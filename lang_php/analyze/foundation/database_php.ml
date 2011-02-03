(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

open Common

open Ast_php

module Ast = Ast_php
module Lib  = Lib_parsing_php
module CG   = Callgraph_php
module Flag = Flag_analyze_php
module EC   = Entity_php

module G = Graph

(*****************************************************************************)
(* PHP info *)
(*****************************************************************************)

(* 
 * See also relation.ml and callgraph.ml. 
 * 
 * Try not to put here stuff that just visits and that is independent 
 * of the database, that does not need the database information.
 *   
 * todo: add info on scope in ast itself ? next to each ident then can 
 *  say to what this ident refers too. As in DrScheme GUI, can see 
 *  arrows between def and use. To be accurate it means that we must
 *  handle well the scope, and also .h files by using cpp_ast_c.ml.
 *  Have the LocalVar and NotLocalVar but maybe more info would be useful.
 * 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type project = 
  Project of Common.dirname * string option (* name of project *)

(* optimisation: the old id (which is now a fullid) was containing a
 * filename, and so could take quite some space when marshalled on the
 * disk, or when used in keys or in list of values as in the callers
 * table. So it's important for the database id to be small.
 * 
 * Also, not bad to use this id rather that the other one as some code,
 * like the confidence call in aliasing_function_c was abusing it a
 * little by using the filename and dir of the id. Clearer to
 * explicitely show that use of such feature by passing a fullid_info
 * function to those codes. 
 *)
type id = Entity_php.id

(* old: was called id *)
type fullid = Entity_php.fullid 

type id_kind = Entity_php.id_kind
type id_string = string (* was IdString, but too heavy *)

(* can be useful in gui to highlight part of text *)
type range_pos = int * int (* charpos *)

(* todo? would be better to have really different types ... 
 * But sometimes we really want to have a single type for all kind of ids,
 * for instance in the callgraph where the caller id can be anything
 * (the id of a function, method, toplevel statement, etc). Maybe
 * union types could help or polymorphic variants.
 *)
type id_function = id
type id_class = id
type id_interface = id
type id_method = id
type id_define = id

type id_other = id 



(*****************************************************************************)
(* Database *)
(*****************************************************************************)

(* 
 * PHP history: 
 * - project, metapath
 * - asts, file_to_ids 
 * - name_defs
 * - caller/callee
 * - id_kinds, names, 
 *   so when search entity in gui, can fastly know the name of an id and kind
 * - symbols, for gui completion
 * - strings, for false positive in deadcode analysis
 * - tokens_of_ast, range_of_ast, str_of_ast to be able to do basic
 *   source to source transformation, get associated comments, get line 
 *   position of ast, etc.
 * - use id_kind, not id_kinds as in PHP one AST defines only one kind
 *   of thing (a func, a class, a method, but not both at the same time
 *   as in C).
 * - nested ids, enclosing id  for method, or nested func
 * - add types from dynamic analysis or static analysis
 * - add id_phpname, so in gui can fastly go from a nested name from its
 *   corresponding it, for instance to later know wether we have type info
 *   about this nested id
 * 
 * 
 * 
 * C history: 
 *  - project, metapath
 *  - objects, file_to_ids 
 * 
 *  - defs
 *  - kinds, name
 * 
 *  - callers_of_f and callees_of_f first version
 * 
 *  - add str_of_ast, for glimpse search
 *  - add range_of_ast, for glimpse highlighting in gui
 * 
 *  - ftype for the function pointer alias analysis and improved callgraph
 *  - callers_of_f and callees_of_f second version, dealing with call
 *    through function pointer.
 * 
 *  - extra, to remember when for optimisation voluntarily forget some
 *    information such as complete list of caller/callees through pointer
 *
 *  - callers_of_f and callees_of_f third version, make it more symetric,
 *    have a id to id relation, not an asymetric id to string.
 * 
 *  - change multiple times the type in caller/callees
 * 
 *  - introduce IdOpt that replace the old Entity_c.id (now fullid) as 
 *    default id.
 *)

(* Right now Mem is actually slower than Disk on huge codebase, probably 
 * because the OCaml Gc has to traverse bigger and bigger graphs of cells
 * when using Mem, and with Disk this graph is always small in memory.
 * This is sad, but at least Mem has some little use for unit testing. See
 * test_analyze_php.ml.
 *)
type db_support = 
  | Disk of Common.dirname (* the path to the Berkeley DB meta data *)
  | Mem


type database = {
  project: project; 
  db_support: db_support; (* using by default Disk (project^"/PFFDB") *)

  (* normally, used and incremented only in database_php_build.index_db1 *)
  mutable next_free_id: int;

  fullid_of_id: (id, fullid) Oassoc.oassoc;
  id_of_fullid: (fullid, id) Oassoc.oassoc; (* used only for a few checks *)

  file_to_topids: (filename, id list) Oassoc.oassoc;
  file_info: (Common.filename, file_info) Oassoc.oassoc;

  (* Some entities are nested in PHP and pfff, for instance a method is
   * part of a class. Moreover even functions can be nested.
   *)
  enclosing_id: (id, id) Oassoc.oassoc;
  children_ids: (id, id list) Oassoc.oassoc;

  defs: database_defs;
  uses: database_uses;

  (* Set of symbols, used by gui to build completion list. Can be derived
   * from defs but may be expensive so strings is a kind of specialised
   * defs.
   *)
  symbols: (string, unit) Oassoc.oassoc; 

  strings: (string, unit) Oassoc.oassoc; (* without the quote *)

  flush_db: unit -> unit;
  close_hook: unit -> unit;

}
  (* -------------------------------------------------------------------- *)
  (* defs *)
  (* -------------------------------------------------------------------- *)
   and database_defs = {
     (* asts, normalized toplevels ? aggregated ? *)
     toplevels:   (id, Ast_php.toplevel) Oassoc.oassoc;

     (* opti: contains only the nested asts. So NEVER USE THIS FIELD
      * directly. Use the ast_of_id helper function!
      *)
     asts: (id, Ast_php.entity) Oassoc.oassoc;

     (* the remaining parsing information, the string and tokens *)
     str_of_topid:    (id, string)                Oassoc.oassoc;
     tokens_of_topid: (id, Parser_php.token list) Oassoc.oassoc;
     range_of_topid: (id, (Parse_info.parse_info * Parse_info.parse_info)) 
       Oassoc.oassoc;

     (* Could have a function_defs, proto_defs, global_defs, struct_def,
      * but this would force for instance in a gui to have different search
      * function for each kind of defs, and so this could duplicate code.
      * So just easier to put all defs together and let the 'kinds' offer
      * the way to easily differentiate them in a post phase. In principle
      * having a specialized function_defs can be faster, but usually people
      * use different name even if the C namespace rules could also the user
      * to overload a name, so in practice I don't think it would be that
      * faster to specialize.
      * 
      * Also with this scheme, and with the 'kinds' table, and a gui that
      * display the kind when there is ambiguity, then can easily go from
      * function to its corresponding prototype. So with one "search entity"
      * button we can in fact support all search.
      * 
      * I think I find the good architecture so that have not too much tables
      * so dont lose time each time I add a new entity to write 
      * lots of boilerplate code.
      *)
     name_defs: (id_string, id list) Oassoc.oassoc;
  
     (* sort of cache. Could get info from asts too, but here lighter, and 
      * also easier for code for instance to filter some id.
      * 
      * update: I was using id_kind list, not just id_kind in kinds table
      * as some ids could have multiple kinds, for instance in C they 
      * can both define a typedef and a structure. This is not the case in PHP
      * so can have just a (id, id_kind) assoc.
      * 
      *)
     id_kind: (id, id_kind) Oassoc.oassoc;

     (* computer statically or dynamically *)
     id_type: (id, Type_php.phptype) Oassoc.oassoc;

     (* reverse of defs, also sort of cache *)
     id_name: (id, id_string) Oassoc.oassoc;

     id_phpname: (id, Ast_php.name) Oassoc.oassoc;

     extra: (id, extra_id_info) Oassoc.oassoc;
   }

  (* -------------------------------------------------------------------- *)
  (* use *)
  (* -------------------------------------------------------------------- *)

  (* history: cf mainly callgraph_c.ml 
   * ex:
   *   int foo() {
   *    ...
   *    x.fnopen();
   *   }
   *   int bar() {
   *    ...
   *    foo_open();
   *   }
   *   int foo_open() { ... }
   * 
   * so the callers_of_f: "foo_open" will be the id of bar * Directcall
   * and the id of foo * Indirect (x.fnopen());
   * 
   * Note that one table may be larger than the other. As a callgraph
   * is essentially a tree, when you add one set of callees in callees_of_f,
   * so one entry with n elements, you have to insert in n elements one
   * new entry (and berkeley DB may be more space efficient in one case,
   * and also the marshalling of ocaml may also factorize more string 
   * in one case).
   *
   * old: callees_of_f: (string, ...) oassoc, but had some pbs. Cf
   *  database_c_build.index_db4
   * old: was using some id_func_pos as key but tedious and use 
   *  redundant space as can get the information from names table.
   * 
   * todo? simplify even more caller/callee ? factorize callsite ?
   * Just id to id ? (or maybe constantly use id_func_pos ?
   * but then more space that what is really needed ?
   * 
   *)
  and database_uses = {
    (* callees and its reverse index *)
    callees_of_f: (id, Callgraph_php.callsites_opt list) Oassoc.oassoc;
    callers_of_f: (id, Callgraph_php.callersinfo_opt list) Oassoc.oassoc; 

    users_of_class: (id (* id class *), id list) Oassoc.oassoc;
    users_of_define: (id (* id define *), id list) Oassoc.oassoc;

    extenders_of_class:        (id_class,     id_class list) Oassoc.oassoc;
    implementers_of_interface: (id_interface, id_class list) Oassoc.oassoc;

    (* This only include the direct includers/includees. For the full
     * set of includers, use the recursive functions: 
     *  - includees_rec_of_file
     *  - includers_rec_of_file
     * 
     * For includees_of_file below, the only advantage over 
     * Include_require_php.increq_of_program is that the path resolution
     * and the subtle differences between include/require/require_module
     * has already been done.
     *)
    includers_of_file: (filename, filename list) Oassoc.oassoc;
    includees_of_file: (filename, filename list) Oassoc.oassoc;
  }
  and file_info = {
    parsing_status: [`OK | `BAD];
  }

    and extra_id_info = {
      tags: Annotation_php.annotation list;
      partial_callers: bool;
      partial_callees: bool;
      todo: unit;
    }

(* ---------------------------------------------------------------------- *)

let default_extra_id_info = {
  tags = [];
  partial_callers = false; 
  partial_callees = false;
  todo = ();
}

let string_of_extra_id_info x = 
  raise Todo


(*****************************************************************************)
(* Extra *)
(*****************************************************************************)

let pfff_special_dirname = "PFFFDB" 
let glimpse_special_dirname = "GLIMPSEDB" 

(* old:
 *  let use_transact = ref false
 *  let use_bdb = ref true
 *  let use_disk = ref true 
 * 
 * For now the interface for opening db on the disk or in mem is 
 * different so no need for those flags. Use call different functions
 * (Database_php.open_db or Database_php_build.create_db ~db_support:Mem)
 * if you want the different behavior
 *)


let is_database_dir ~metapath = 
  Sys.file_exists (metapath ^ "/prj.raw")

let database_tag_filename = "pfff_db.magic"

(* ---------------------------------------------------------------------- *)
type error = 
  | ErrorDb of string

exception Error of error

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let (path_of_project: project -> Common.dirname) = function
  (Project (path,_)) -> path

let (path_of_project_in_database: database -> Common.dirname) = fun db ->
  path_of_project db.project

let (normalize_project: project -> project) = function
  (Project (path, nameopt)) -> 
    Project (Common.chop_dirsymbol path, nameopt)

let (metapath_of_database: database -> Common.dirname) = fun db -> 
  match db.db_support with
  | Disk metapath -> metapath
  | Mem -> failwith "the db has no disk support, and so no metapath"

let (glimpse_metapath_of_database: database -> Common.dirname) = fun db -> 
  let metapath = metapath_of_database db in
  Filename.concat metapath glimpse_special_dirname

let (default_metapath_of_project: project -> Common.dirname) = function
  (Project (path,_)) -> 
    Filename.concat path pfff_special_dirname


(* TODO would be better to have different types for absolute vs 
 * relative/readable paths.
 *)
let absolute_to_readable_filename file db =
  let prj_path = path_of_project db.project in
  Common.filename_without_leading_path prj_path file

let readable_to_absolute_filename file db = 
  if not (Filename.is_relative file)
  then failwith ("expect a relative path, not:" ^ file);
  let prj_path = path_of_project db.project in
  Filename.concat prj_path file

let check_is_database_dir dir = 
  if not(Sys.file_exists (Filename.concat dir database_tag_filename))
  then 
    failwith 
      (spf "was not able to find %s in %s, maybe not a db directory"
          database_tag_filename dir)


(* old: let string_of_id_string (IdString s) = s *)

(*****************************************************************************)
(* nested and enclosing ids manipulation *)
(*****************************************************************************)

let rec recurse_children f db id = 
  f id;
  try 
    let child = db.children_ids#assoc id in
    child +> List.iter (recurse_children f db)
  with Not_found -> ()

let rec enclosing_ids id db = 
  match db.enclosing_id#find_opt id with
  | None -> [id]
  | Some id2 ->
      id::enclosing_ids id2 db

let all_children_ids id db = 
  let res = ref [] in
  recurse_children (fun id ->
    Common.push2 id res
  ) db id;
  !res

(*****************************************************************************)
(* xxx_of_id *)
(*****************************************************************************)

(* could also impose to caller that id is a entity-with-a-name *)
let name_of_id id db = 
  try db.defs.id_name#assoc id
  with Not_found -> "NONAMEID_" ^ EC.str_of_id id

(* for debugging *)
let str_of_id id db = 
  EC.str_of_fullid (db.fullid_of_id#assoc id)


let ast_of_id id db = 
  try 
    let top = db.defs.toplevels#assoc id in
    Lib_parsing_php.toplevel_to_entity top
  with Not_found ->
    db.defs.asts#assoc id

let is_top_id id db = 
  db.defs.toplevels#haskey id

let rec toks_of_topid_of_id id db =
  if is_top_id id db 
  then
    db.defs.tokens_of_topid#assoc id
  else 
    let enclosing = db.enclosing_id#assoc id in
    toks_of_topid_of_id enclosing db





let callees_of_id id db =
  try 
  (db.uses.callees_of_f#assoc id) 
  +> List.map Callgraph_php.callsites_opt_to_callsites 
  +> List.flatten
  with Not_found -> []

let callers_of_id id db =
  if not (List.mem (db.defs.id_kind#assoc id) 
             [EC.Function; EC.Method; EC.StaticMethod])
  then failwith "callers_of_id expect the id of a function or method";

  try
    let callees_of_id id = db.uses.callees_of_f#assoc id in
    (db.uses.callers_of_f#assoc id) 
    +> List.map (Callgraph_php.callersinfo_opt_to_callersinfo ~callees_of_id id)
    +> List.flatten
  with Not_found -> []


(* could be made more efficient by storing in the db the inheritance tree *)
let parent_name_of_id2 id db = 
  let ast = ast_of_id id db in
  match ast with
  | Ast_php.ClassE def ->
      def.c_extends |> Common.fmap (fun (tok, classname) -> 
        Ast.name classname
      )
  | _ ->
      failwith "parent_name_of_id: this is not a class"

let parent_name_of_id a b =
  Common.profile_code "parent_name_of_id" (fun () -> parent_name_of_id2 a b)


(* safer wrappers around the database fields *)
let class_users_of_id id db = 
  if not (List.mem (db.defs.id_kind#assoc id) [EC.Class])
  then failwith "class_users_of_id expect the id of a class";
  try 
    db.uses.users_of_class#assoc id
  with Not_found -> []

let class_extenders_of_id id db =
  if not (List.mem (db.defs.id_kind#assoc id) [EC.Class])
  then failwith "class_extenders_of_id expects the id of a class";
  try 
    db.uses.extenders_of_class#assoc id
  with Not_found -> []

let class_implementers_of_id id db =
  if not (List.mem (db.defs.id_kind#assoc id) [EC.Interface])
  then failwith "class_implementers_of_id expects the id of an interface";
  try 
    db.uses.implementers_of_interface#assoc id
  with Not_found -> []




let classdef_of_nested_id_opt id db = 
  let enclosing = enclosing_ids id db in
  Common.optionise (fun () ->
    enclosing |> Common.find_some (fun id ->
      let id_kind = db.defs.id_kind#assoc id in
      match id_kind with
      | EC.Class -> 
          let id_ast = ast_of_id id db in
          (match id_ast with
          | Ast_php.ClassE def -> Some def
          | _ -> 
              failwith "Impossible: must be a Class"
          )
      | _ -> None
    )
  )

let self_parent_of_nested_id id db =
  (* look if id belongs to a class *)
  match classdef_of_nested_id_opt id db with
  | None -> None, None
  | Some cdef ->
      let self = Some (Ast.name cdef.c_name) in
      let parent = cdef.c_extends |> Common.fmap (fun (tok, classname) ->
        Ast.name classname
      )
      in
      self, parent


let complete_name_of_id id db = 
  try 
    let s = db.defs.id_name#assoc id in
    let id_kind = db.defs.id_kind#assoc id in

    (match id_kind with
    | EC.Method | EC.StaticMethod 
    | EC.ClassConstant | EC.ClassVariable 
    | EC.XhpDecl
      ->

        (match classdef_of_nested_id_opt id db with
        | Some def -> 
            let sclass = Ast.name def.c_name in

            (match id_kind with
            | EC.Method ->
                spf "%s->%s" sclass s
            | EC.StaticMethod ->
                spf "%s::%s" sclass s

            (* todo? something special ? *)
            | EC.ClassConstant | EC.ClassVariable 
            | EC.XhpDecl 
              ->
                spf "%s::%s" sclass s

            | _ -> raise Impossible
            )
        | None ->
            s
        )
    | _ -> s
    )
  with Not_found -> "NONAMEID_" ^ EC.str_of_id id

(*****************************************************************************)
(* id_of_xxx *)
(*****************************************************************************)

let ids_with_kind__of_string2 s db = 
  let ids = 
    try db.defs.name_defs#assoc (s)
    with Not_found -> 
      (* todo: possible ? *)
      []
  in
  let ids = ids +> Common.map_filter (fun id -> 
    try Some (id, db.defs.id_kind#assoc id)
    with Not_found -> None
  )
  in
  ids
let ids_with_kind__of_string a b = 
  Common.profile_code "ids_with_kind__of_string" (fun () -> 
    ids_with_kind__of_string2 a b)


let filter_ids_of_string s kind db = 
  let ids = ids_with_kind__of_string s db in
  ids +> Common.map_filter (fun (id, kind2) -> 
    if  kind = kind2
    then Some (id)
    else None
  )
  

let function_ids__of_string s db = filter_ids_of_string s EC.Function db
let method_ids_of_string s db    = filter_ids_of_string s EC.Method db
let class_ids_of_string s db    = filter_ids_of_string s EC.Class db
let interface_ids_of_string s db    = filter_ids_of_string s EC.Interface db


(* could also have .functions .methods .classes fields in db ?
 * that use name_defs and the appropriate filter ?
 * and the are updated in build_db ?
 *)

let filter_ids_in_db kinds db = 
  (* scalable ? *)
  let xs = db.defs.name_defs#tolist in

  xs +> Common.map_filter (fun (id_str, ids) ->
    let ids_with_kind = ids +> List.map (fun id -> 
      id, db.defs.id_kind#assoc id
    )
    in
    let goodone = ids_with_kind +> Common.filter (fun (id, kind2) -> 
      List.mem kind2 kinds)
    in
    if null goodone
    then None
    else Some (id_str, goodone +> List.map fst)
  )

let functions_in_db db = filter_ids_in_db [EC.Function] db
let classes_in_db db = filter_ids_in_db [EC.Class] db
let methods_in_db db = filter_ids_in_db [EC.Method] db

let functions_or_static_methods_in_db db = 
  filter_ids_in_db [EC.Function;EC.StaticMethod] db

let is_function_id id db = 
  try 
    let kind = db.defs.id_kind#assoc id in
    kind = EC.Function
  with Not_found ->
    pr2 (spf "WEIRD: no id_kind for id: %s" (str_of_id id db));
    false

(* 
 * Are functions, classes, and methods in different namespace in PHP ?
 * well kind of as there is no ambiguity between foo() and $x->foo()
 * or A::foo().
 * 
 * todo? could also take a file as a hint, so that if there is an ambiguity,
 *  the hint file can help to disambiguate
 *)
let id_of_function s db = 
  function_ids__of_string s db +> Common.list_to_single_or_exn

let id_of_class s db = 
  class_ids_of_string s db +> Common.list_to_single_or_exn

let id_of_interface s db = 
  interface_ids_of_string s db +> Common.list_to_single_or_exn


(* todo? do we want to handle inheritance ? also we could allow to have 
 * multiple classes with same name, and the method name can then be used 
 * to disambiguate
 *)
let id_of_method ~theclass:sclass smethod db =
  let idclass = id_of_class sclass db in

  let children = db.children_ids#assoc idclass in
  let methods = 
    children +> List.filter (fun id ->
      let kind = db.defs.id_kind#assoc id in
      (kind = EC.Method || kind = EC.StaticMethod) && 
          db.defs.id_name#assoc id =$= smethod
    )
  in
  Common.list_to_single_or_exn methods


(* 
 * In PHP (and C++) it is ok to do B::foo(), even if B does not define
 * a static method foo, provided that B inherits from a class that 
 * defines such a foo. So have to do some inheritance lookup resolution,
 * statically. See tests/static_method_call2.php
 *
 * todo? should factorize all those id_of_xxx and xxx_ids_of_string stuff 
 *)
let rec static_function_ids_of_strings ~theclass smethod db = 
  let idclasses = class_ids_of_string theclass db in

  idclasses |> Common.map (fun idclass -> 

    let children = db.children_ids#assoc idclass in

    let candidates = 
      children +> List.filter (fun id ->
        let kind = db.defs.id_kind#assoc id in
        kind = EC.StaticMethod && 
            db.defs.id_name#assoc id =$= smethod
      )
    in
    if null candidates
    then 
      let theclass_parent_opt = parent_name_of_id idclass db in
      match theclass_parent_opt with
      | None -> 
          if !Flag.show_analyze_error then begin
            pr2 (spf "could not find parent for class %s" 
                    (name_of_id idclass db));
            pr2 (spf "could not determine static method for %s::%s" 
                    theclass smethod);
          end;
          []
      | Some theclass_parent ->
          static_function_ids_of_strings ~theclass:theclass_parent smethod db

    else candidates
  ) |> List.flatten
    

let id_of_kind_call ?(file_disambiguator="") call db = 
  match call with
  | CG.FunCall s -> id_of_function s db
  | CG.ClassCall (sclass, smethod) 
  | CG.ObjectCall (sclass, smethod) ->
      id_of_method sclass smethod db




let ids_in_file file db = 
  let ids = db.file_to_topids#assoc file in
  let all_ids = ref [] in
  ids +> List.iter (recurse_children (fun id ->
    Common.push2 id all_ids ) db);
  List.rev !all_ids

      
let id_of_phpname name db = 
  let info = Ast.info_of_name name in
  let file = Ast.file_of_info info in
  
  let ids = ids_in_file file db in
  let matching = ids +> List.filter (fun id ->
    try 
      db.defs.id_phpname#assoc id =*= name
    with 
    Not_found -> false
  )
  in
  Common.list_to_single_or_exn matching


(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

let all_files db = 
  db.file_to_topids#tolist |> List.map fst |> Common.sort

let has_parsing_problem file db = 
  try 
    let file_info = db.file_info#assoc file in
    match file_info.parsing_status with
    | `OK -> false
    | `BAD -> true
  with Not_found ->
    pr2 (spf "PB: %s is not in the database" file);
    true



(* One could call those functions below for all the files in the database.
 * Can use memoization to make this tractable.
 *)

let _hmemo_includees = Hashtbl.create 101
let _hmemo_includers = Hashtbl.create 101

let _hmemo_includers_done = Hashtbl.create 101
let _hmemo_includees_done = Hashtbl.create 101

let rec includees_rec_of_file_set file db = 

  Common.memoized _hmemo_includees file (fun () ->
    let direct = 
      try db.uses.includees_of_file#assoc file 
      with Not_found -> []
    in
    
    direct |> List.fold_left (fun acc file -> 
      if Hashtbl.mem _hmemo_includees_done file
      then begin 
        pr2 (spf "WARNING: include cycle for %s" file);
        acc
      end else begin
        Hashtbl.add _hmemo_includees_done file true;

        let acc' = Set_poly.add file acc in
        Set_poly.union acc' (includees_rec_of_file_set file db)
      end
    ) Set_poly.empty
  )

let rec includers_rec_of_file_set file db = 

  Common.memoized _hmemo_includers file (fun () ->
    let direct = 
      try db.uses.includers_of_file#assoc file 
      with Not_found -> []
    in
    
    direct |> List.fold_left (fun acc file -> 
      if Hashtbl.mem _hmemo_includers_done file
      then begin 
        pr2 (spf "WARNING: include cycle for %s" file);
        acc
      end else begin

        Hashtbl.add _hmemo_includers_done file true;
        
        let acc' = Set_poly.add file acc in
        Set_poly.union acc' (includers_rec_of_file_set file db)
      end
    ) Set_poly.empty
  )

let includees_rec_of_file file db = 
  if not (db.file_to_topids#haskey file)
  then failwith (spf "file %s  is not in the database" file);
  includees_rec_of_file_set file db |> Set_poly.elements
   

let includers_rec_of_file file db = 
  if not (db.file_to_topids#haskey file)
  then failwith (spf "file %s  is not in the database" file);
  includers_rec_of_file_set file db |> Set_poly.elements



(* This can be quite expensive as ocamlgraphs are in memory,
 * but at least we can have a structure that is easier to work on
 * when we want to show path to things.
 *)
let mk_graph_of_file ?(depth_limit=None) succ file db = 
  if not (db.file_to_topids#haskey file)
  then failwith (spf "file %s  is not in the database" file);

  let g = G.create () in
  let hdone = Hashtbl.create 101 in

  let has_reached_limit depth =
    match depth_limit with
    | None -> false
    | Some i -> depth >= i
  in

  let rec aux file depth = 
    
    if Hashtbl.mem hdone file || has_reached_limit depth
    then () 
    else begin
      Hashtbl.replace hdone file true;
      g +> G.add_vertex_if_not_present file;
      
      let direct = succ file in
      direct |> List.iter (fun file2 ->
        
        g +> G.add_vertex_if_not_present file2;
        g +> G.add_edge file file2;
        
        aux file2 (depth + 1);
      );
    end
  in
  aux file 0;
  g

let includees_graph_of_file ?depth_limit file db = 
  let succ file =
    try db.uses.includees_of_file#assoc file 
    with Not_found -> []
  in
  mk_graph_of_file ?depth_limit succ file db

let includers_graph_of_file ?depth_limit file db = 
  let succ file =
    try db.uses.includers_of_file#assoc file 
    with Not_found -> []
  in
  mk_graph_of_file ?depth_limit succ file db



(*****************************************************************************)
(* Invariant *)
(*****************************************************************************)

(*
 * todo:
 *  - names is reversed of defs 
 *  - callers is approximately reverse of callees
 *    (via defs, as a funcall name can have multiple function def or macro)
 * 
 * if use id_opt, ensure all current ids are inferior to current limit
 * 
 * less: 
 *  - in file_to_ids, the ids in the vals are different from any other id.
 *  - in kinds, can not have certain combinations
 * 
 * last: the id vs topids. each id has an entry into fullid_of_id 
 * and so on
 *)
let check_db db = 
  raise Todo

(*
  let fake_len = 10 in

  Common.pr2 "checking callees";
  Common.execute_and_show_progress fake_len (fun k -> 
    db.callees_of_f#iter (fun (key, vs) -> 
      k();
      if not (Common.is_set vs)
      then failwith ("not a set" ^ dump vs)
    ));


  Common.pr2 "checking callers";
  Common.execute_and_show_progress fake_len (fun k -> 
    db.callers_of_f#iter (fun (key, vs) -> 
      k();
      if not (Common.is_set vs)
      then let dups = get_duplicates vs in
           failwith ("duplicates:" ^ dump (key, dups));
    );
  );
    
  Common.pr2 "Checking some equality of global numbers (cardinality, ...)";

  (* TODO *)
  let _len_asts = db.objects#length in
  let _len_strs = db.str_of_ast#length in
  let _len_toks = db.tokens_of_ast#length in
  let _len_range = db.range_of_ast#length in

  let _len_extra = db.extra#length in

  let _len_defs = db.defs#length in 
  let _len_kinds = db.kinds#length in

  let _len_names = db.names#length in
  let _len_ftype = db.ftype#length in

  Common.pr2 "checking some tables are subset of other tables regarding ids";
  (* TODO *)

  ()
*)

(*****************************************************************************)
(* Shortcuts *)
(*****************************************************************************)

let filename_of_id id db =
  let fullid = (db.fullid_of_id#assoc id) in
  fullid.EC.file

let line_of_id id db =
  let fullid = (db.fullid_of_id#assoc id) in
  fullid.EC.line

let col_of_id id db =
  let fullid = (db.fullid_of_id#assoc id) in
  fullid.EC.column

let kind_of_id id db =
  try 
    db.defs.id_kind#assoc id
  (* probably because of NotParsedCorrectly entities *)
  with Not_found -> 
    EC.IdMisc

let readable_filename_of_id id db =
  let file = filename_of_id id db in
  let prj_path = path_of_project db.project in
  Common.filename_without_leading_path prj_path file

let parse_info_of_fullid str fullid =
  { Parse_info.token =
      Parse_info.OriginTok ( {
        Parse_info.str = str;
        (* todo? I don't think I use this field fortunately.
         * I could recompute it from line and column but it would
         * force me to reanalyze the file
         *)
        Parse_info.charpos = -1; 

        Parse_info.line = fullid.EC.line;
        Parse_info.column = fullid.EC.column;
        Parse_info.file = fullid.EC.file;
      }
      );
    Parse_info.comments = ();
    Parse_info.transfo = Parse_info.NoTransfo;
  }

let parse_info_of_id id db =
  let fullid = (db.fullid_of_id#assoc id) in
  parse_info_of_fullid (name_of_id id db) fullid


(*****************************************************************************)
(* Open/Close *)
(*****************************************************************************)

(* the real storage backend is in database_php_storage.ml now *)

let open_db_mem prj = 
  let free_id = 1 in
  let mk_assoc () = new Oassoch.oassoch [] in

  { 
      db_support = Mem;
      project = prj;

      next_free_id = free_id;

      fullid_of_id = mk_assoc ();
      id_of_fullid = mk_assoc ();
      enclosing_id = mk_assoc ();
      children_ids = mk_assoc ();

      file_to_topids = mk_assoc ();
      file_info = mk_assoc ();

      symbols = mk_assoc ();
      strings = mk_assoc ();

      defs = {
        toplevels = mk_assoc ();
        asts = mk_assoc ();

        name_defs = mk_assoc ();
        id_kind = mk_assoc ();
        id_type = mk_assoc ();
        id_name = mk_assoc ();
        id_phpname = mk_assoc ();

        str_of_topid = mk_assoc ();
        tokens_of_topid = mk_assoc ();
        range_of_topid = mk_assoc ();

        extra = mk_assoc ();
      };
      
      uses = {
        callers_of_f = mk_assoc ();
        callees_of_f = mk_assoc ();

        users_of_class = mk_assoc ();
        users_of_define = mk_assoc ();

        extenders_of_class = mk_assoc ();
        implementers_of_interface = mk_assoc ();

        includers_of_file = mk_assoc ();
        includees_of_file = mk_assoc ();
      };

      flush_db = (fun () -> ());
      close_hook = (fun () -> ());

    }


(*---------------------------------------------------------------------------*)
let close_db db =
  db.close_hook ()


(*---------------------------------------------------------------------------*)
let _current_open_db_backend = ref (fun metapath ->
  failwith "no db backend set; have you run configure -bdb ?"
)

let with_db ~metapath f = 
  let db = !_current_open_db_backend metapath in
  Common.unwind_protect (fun () -> 
    let res = f db in
    close_db db;
    res
  ) 
  (fun e -> close_db db)



(*****************************************************************************)
(* Error *)
(*****************************************************************************)

let (report_error: error -> string) = fun error -> 
  match error with 
  | ErrorDb s -> "ErrorDb:" ^ s

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-check_db", "   <db>", 
    Common.mk_action_1_arg (fun dbname -> 
      with_db ~metapath:dbname check_db);
]
