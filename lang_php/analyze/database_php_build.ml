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

(* for fields *)
open Ast_php
open Database_php

module Ast  = Ast_php
module Flag = Flag_analyze_php
module V = Visitor_php
module Lib_parsing = Lib_parsing_php
module Entity = Entity_php
module N = Namespace_php
module T = Type_php

module Db = Database_php



module TH   = Token_helpers_php
module EC   = Entity_php
module CG   = Callgraph_php

module A = Annotation_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* We build the full database in multiple steps as some
 * operations need the information computed globally by the
 * previous step:
 * 
 * - we first add the top ASTs in the database.
 * - we then add nested ASTs (methods, class vars, nested funcs), and their ids
 * - we add all strings found in the code in a global table
 * - before adding the callgraph, we need to 
 *   be able to decide given a function call what are the possible entities
 *   that define this function (there can be multiple candidates), so 
 *   we add some string->definitions (function, classes, ...) information. 
 * - then we do the callgraph
 * - we add other reversed index like the callgraph, but for other 
 *   entities than functions, e.g. the places where a class in instantiated,
 *   or inherited.
 * 
 * - TODO given this callgraph we can now run a type inference analysis, as 
 *   having the callgraph helps doing the analysis in a certain order (from
 *   the bottom of the callgraph to the top, with some fixpoint in the 
 *   middle).
 * 
 * time to build the db on www: 
 *   - 15min in bytecode
 *   - 26min when storing the string and tokens
 *   - xxmin when storing method calls
 * cf also score.org !!!
 * 
 *
 * TODO: can take value of _SERVER in params so can
 * partially evaluate things to statically find the 
 * file locations (or hardcode with ~/www ... ) 
 * or use a 'find' ...
 * 
 *    type php_config = ...
 *    type apache_config = ...
 * 
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let _errors = ref []

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag.verbose_database

let pr2_err s = 
  Common.pr2 s;
  Common.push2 s _errors;
  ()

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* should perhaps be moved in database_php.ml ? *)

(* Improve change of locality for analysis by sorting the files.
 * The default ordering of berkeley DB is based on filename but also
 * on their length, so not the best order. 
 * 
 * Does it really help ?
 *)
let iter_files db f = 
  db.file_to_topids#tolist 
  +> Common.sortgen_by_key_lowfirst (* does it really help ? real opti ? *)
  +> Common.index_list_and_total 
  +> List.iter (fun x -> 
    let ((file, topids), i, total) = x in
    try f x 
    with exn -> 
      pr2 (Common.exn_to_s_with_backtrace exn);
      pr2_err (spf "PB with %s, exn = %s" file (Common.exn_to_s exn));
  );
  ()

let iter_files_and_topids db msg f = 
  iter_files db 
   (fun ((file, ids), i, total) -> 
     pr2 (spf "%s: %s %d/%d " msg file i total);
     ids +> List.iter (fun id -> 
       f id file
     )
   )

let iter_files_and_ids db msg f = 
  iter_files_and_topids db msg (fun id file ->
    Db.recurse_children (fun id -> f id file) db id;
  )

(* todo: refactor the code of database_php_build.ml so needs
 * less this specific class case
 *)
let users_of_class_in_any any =
  Defs_uses_php.uses_of_any any +> Common.map_filter (fun (kind, name) ->
    match kind with
    | Database_code.Class -> Some name
    | _ -> None
  )

(*****************************************************************************)
(* Helpers ast and tokens *)
(*****************************************************************************)

(* 
 * Extract some position information from a toplevel element so can then
 * refer to this element through its 'fullid'. Of course we need to have
 * at least one valid info for this toplevel element.
 *)

exception NoII

(* TODO assume the ii are sorted by pos ? *)
let rec first_filepos_origin ii = 
  match ii with
  | [] -> raise NoII
  | x::xs -> 
      if Ast.is_origintok x 
      then x +> Ast.parse_info_of_info +> EC.filepos_of_parse_info
      else first_filepos_origin xs

let (fpos_of_toplevel: Ast.toplevel -> Entity.filepos) = fun top -> 
  let allii = Lib_parsing.ii_of_any (Toplevel top) in
  first_filepos_origin allii 

let (fpos_of_idast: Ast_php.entity -> Entity.filepos) = fun ast ->
  let allii = Lib_parsing_php.ii_of_any (Entity ast) in
  first_filepos_origin allii
  

(* less: similar func in lib_parsing ? *)
let prange_of_origin_tokens toks = 
  let toks = List.filter TH.is_origin toks in
  let ii = toks +> List.map TH.info_of_tok in
  
  try 
    let (min, max) = Lib_parsing.min_max_ii_by_pos ii in
    Ast.parse_info_of_info min, Ast.parse_info_of_info max
  with _ -> 
    (Parse_info.fake_parse_info, Parse_info.fake_parse_info)


let first_comment ast toks =
  let ii = Lib_parsing_php.ii_of_any (Toplevel ast) in
  let (min, max) = Lib_parsing_php.min_max_ii_by_pos ii in

  let min = Ast_php.parse_info_of_info min in

  let toks_before_min = 
    toks +> List.filter (fun tok ->
      Token_helpers_php.pos_of_tok tok < min.Parse_info.charpos
    ) +> List.rev
  in
  let comment_info = 
    match toks_before_min with
    | Parser_php.T_WHITESPACE i1::
        (Parser_php.T_COMMENT i2|Parser_php.T_DOC_COMMENT i2)::xs ->
        if Ast_php.col_of_info i2 = 0
        then Some (Ast_php.parse_info_of_info i2)
        else None

    (* for one-liner comment, there is no newline token before *)
    | Parser_php.T_COMMENT i2::xs ->
        if Ast_php.col_of_info i2 = 0
        then Some (Ast_php.parse_info_of_info i2)
        else None
    | _ -> None
  in
  comment_info

(*****************************************************************************)
(* Helpers to add stuff in database *)
(*****************************************************************************)

(* The goal of those functions are to provide wrappers around adding stuff
 * in the db that automatically handle the consistency of the db when for
 * instance we have both a table and its inverted index.
 *)

let get_newid db =
  let id = EC.Id db.next_free_id in
  db.next_free_id <- db.next_free_id + 1;
  id


let add_toplevel2 file (top, info_item) db = 
  let (str,toks) = info_item in

  let newfullid = 
    try 
      fpos_of_toplevel top 
    with NoII -> 
      pr2_err ("PB: pb NoII with:" ^ file);
      (*
      toput before: let cnt = ref 0 

      pr2 str;
      pr2 ("using info from token then:");
      (try 
          first_filepos_origin_token toks
      with NoII -> 
        pr2 ("PB: pb NoII with tokens too:");
        pr2 (
          toks +> List.map Token_helpers.str_of_tok
               +> Common.join " "
        );
          
        (* if a macro expands to multiple declaration, then have a serie
         * of elements in the ast without connection to the original file.
         * but must still try to generate some fake id for them ?
         *)
        incr cnt;
        { Entity_c.file = file;
          line = -(!cnt);
          column = 0;
        }
      )
      *)
      raise Todo
  in

  if db.id_of_fullid#haskey newfullid
  then begin
    (*
    let newid = db.id_of_fullid#assoc newfullid in
     let str2 = db.str_of_ast#assoc newid in
    pr2_gen (newid, (str, str2));
    *)
    failwith "fullid already in database"
  end;

  let newid = get_newid db in

  let range = prange_of_origin_tokens toks in

  db.fullid_of_id#add2 (newid, newfullid);
  db.id_of_fullid#add2 (newfullid, newid);

  db.defs.toplevels#add2 (newid, top);

  db.defs.str_of_topid#add2 (newid, str);
  db.defs.tokens_of_topid#add2 (newid, toks);
  db.defs.range_of_topid#add2 (newid, range);

  db.defs.extra#add2 (newid, Database_php.default_extra_id_info);

  newid


let (add_filename_and_topids: (filename * id list) -> database -> unit) = 
 fun (file, ids) db -> 
   db.file_to_topids#add2 (file, ids);
   ()


(* have to update tables similar to add_toplevel *)
let (add_nested_id_and_ast: 
  enclosing_id:id -> Ast_php.entity -> database -> id) = 
 fun ~enclosing_id ast db ->

  (* fullid_of_id, extra,   and the specific asts, enclosing and children *)
  let newfullid = 
    try 
      fpos_of_idast ast
    with NoII -> 
      failwith ("PB: pb NoII");
  in
  if db.id_of_fullid#haskey newfullid
  then failwith "fullid already in database";

  let newid = get_newid db in

  db.fullid_of_id#add2 (newid, newfullid);
  db.id_of_fullid#add2 (newfullid, newid);
  db.enclosing_id#add2 (newid, enclosing_id);
  
  db.children_ids#apply_with_default2 enclosing_id
    (fun old -> newid::old) (fun() -> []);

  db.defs.asts#add2 (newid, ast);

  db.defs.extra#add2 (newid, Database_php.default_extra_id_info);

  newid



(*---------------------------------------------------------------------------*)
let (add_def: 
 (id_string * id_kind * id * Ast_php.name option) -> database -> unit) =  
  fun (idstr, idkind, id, nameopt) db -> 

    db.defs.name_defs#apply_with_default2 idstr 
      (fun old -> id::old) (fun() -> []);

    (* the same id can not have multiple kind in PHP, as it can not contain
     * for instance both a variable and struct declaration as in ugly C.
     *)
    if db.defs.id_kind#haskey id 
    then failwith ("WEIRD: An id cant have multiple kinds:" ^ 
                      Db.str_of_id id db);
    db.defs.id_kind#add2 (id, idkind);

    (* old: add2 (id, idkind); *)
    db.defs.id_name#add2 (id, idstr);

    nameopt +> Common.do_option (fun name ->
      db.defs.id_phpname#add2 (id, name);
    );
    db.symbols#add2 (idstr, ());
    ()


(*---------------------------------------------------------------------------*)
(* add_callees_of_f, add in callee table also add in its reversed index
 * caller table. Should be called only one time for each idfpos.  
 * 
 * history: was taking string list, then string set, and now string wrap list.
 * Was quite fast at the beginning, but when adding more and more information
 * (such as remembering all the instances), it gets slower so needed 
 * some optimisations.
 * 
 * optimisations:
 *   - factorization: 
 *     have optimized value (DirectCallIsOpt) that factorize things so
 *     need less call to the callers#add2 method.
 * 
 *   - caching:  
 *     augment buffer size cache for the callers_of_f table as it is very
 *     randomly accessed but often some entries are reaccessed 
 *     and augmented and so want to avoid the disk IO and marshalling
 *     (cf Database.open_db).
 * 
 *   - locality:
 *     try augment the chance of locality by processing files in a certain 
 *     order so that the chance that have similar caller/callee in each file
 *     are quite important. Then the cache will work even better. 
 * 
 *   - compression:
 *     have optmized value that reduce the size of individual 
 *     information, for instance using some id_opt instead of id that
 *     contains lengthy filename, or avoid redundancy that could 
 *     be recomputed easily (from the ast, or from reverse table)
 *     or for instance using position_call_site
 *     instead of full expression or Ast_c.info that are big. 
 *     I guess this is quite useful especially for the caller table 
 *     as even with the augmented buffer size cache, we still need 
 *     sometimes to read entries on the disk and if those entries are big
 *     the problem. 
 *     
 *     So opti: caller/callee again, with 
 *     SameFile of int(*charpos*) | GeneralPos of info ?
 *     Also can just store the offset, no need the (lenthy) filename.
 * 
 *   - todo? preloading of the kinds and name tables used in 
 *     ids_with_kinds__of_string (but apparently not where is bottleneck
 *     on the long run). Put in create_db signature too. How ? simply do:
 *      let oldx = 
 *      let oldy = 
 *      let db = { db with ... }
 *      :)
 *     Could also check that size on disk not too big ? or check the number
 *     of keys ? 
 * 
 *)
let (add_callees_of_id2: (id * (N.nameS Ast_php.wrap list)) -> database -> unit)=
 fun (idcaller, funcalls) db -> 

   (* old: assert(Common.is_set funcs); still true but make less sense
    * now that pass all instances 
    * 
    * old: let directfuncs = funcs +> List.map (fun s -> CG.DirectCall s) in
    * update: now resolve here the possible callees using global information.
    * WWW also give score when multiple possible defs for same string.
    *)
(* old:
   let directfuncs = 
     funcs +> List.map (fun (s, wrap) -> 
       let candidates = 
         Database_c_query.get_functions_or_macros_ids__of_string s db 
       in
       (s,wrap), candidates
     )
   in
   let directcalls = 
     directfuncs +> List.map (fun (swrap, ids) -> 
       ids +> List.map (fun id -> CG.DirectCallTo (id, swrap))
     ) +> List.flatten
   in
*)


   let grouped_instances_same_ident_with_candidates = 
     Common.profile_code "DB.add_callees_of_f_p1" (fun () -> 

       let grouped_instances_same_name = 
         Common.group_by_mapped_key (fun (name, ii) -> name) funcalls in

       grouped_instances_same_name +> List.map (fun (name, name_ii_list) -> 
         let candidates = 
           match name with
           | N.NameS s -> 
               (* can do a few IOs on bdb tables #defs and #kinds *)
               Db.function_ids__of_string s db

           (* Note that because A::foo() and B::foo() may refer to the same
            * id (when B inherits from A without redefining foo), 
            * and because above we group by name, we may generate
            * here for a caller multiple direct call references to the same 
            * id via DirectCallToOpt below. Some invariants can be 
            * broken because of that. So take care for instance in 
            * Callgraph_php.callersinfo_opt_to_callersinfo to handle such
            * corner cases. Another solution would be here to better 
            * group and to identify that the different names 
            * A::foo() and B::foo() actually refers to the same
            * id.
            *)
           | N.NameQualifiedS (sclass, smethod) ->
               Db.static_function_ids_of_strings ~theclass:sclass smethod db
         in
         let s = N.nameS name in
         if null candidates && !Flag.show_analyze_error
         then pr2 (spf "PB: no candidate for function call: %s" s);

         name, name_ii_list |> List.map snd, candidates
       ) 
     )
   in

   (* updating the callees_of_f table *)

   let directcalls =   
     grouped_instances_same_ident_with_candidates 
     |> List.map (fun (a,b,c) -> CG.DirectCallToOpt (a,b,c))
   in
   
   Common.profile_code "DB.add_callees_of_f_p2" (fun () -> 
     db.uses.callees_of_f#add2 (idcaller, directcalls);
   );

   (* updating the callers_of_f table *)

   (* Do we need to use cons or insert_set below ? What is the interest 
    * of insert_set ? to avoid duplication but normally should not call
    * two times add_callees_of_f and so should never add again the same
    * id as the caller of another id.
    * In doubt, if don't trust that the caller adequatly call 
    * add_callees_of_f, then call Database_c.check_db to check
    * the db invariants.
    * 
    * note: This is what takes the most time. This is because 
    * we have to get the value from many different id which are
    * spread into the database hence I guess some disk seek that slow
    * down the process. The numbers of calls to callers_of_f#add
    * is approximately the same than the calls to callees_of_f but
    * in one case we seek far more.
    * 
    *)
(* old:
   directfuncs +> List.iter (fun (swrap, ids) -> 
     ids +> List.iter (fun callee -> 
       db.callers_of_f#apply_with_default callee
         (fun old -> 
           (* insert_set ? *)
           Common.cons (CG.DirectCallerIs (idcaller, swrap)) old
         ) (fun() -> []) +> ignore;
     );
   );
*)

   Common.profile_code "DB.add_callees_of_f_p3" (fun () -> 
   grouped_instances_same_ident_with_candidates 
   +> List.iter (fun (name, infos, ids) -> 
     let nbinstances = List.length infos in 
     (* iter on possible callee candidates *)
     ids +> List.iter (fun callee -> 
       db.uses.callers_of_f#apply_with_default callee
         (fun old -> 
           (* old: idcaller (name, infos), but can be retrieved from
            * reversed table. 
            *)
           Common.cons (CG.DirectCallerIsOpt (idcaller, name, nbinstances)) old
         ) (fun() -> []) +> ignore;
     ));
   );
   ()

let add_callees_of_id a b = 
  Common.profile_code "Db.add_callees_of_f" (fun () -> 
    add_callees_of_id2 a b
  )

(*---------------------------------------------------------------------------*)


(* Similar function but for the indirect function calling part.
 * Also add in reverse index. 
 * 
 * Note that unlike the previous function add_callees_of_f2, 
 * we call add_callees_of_f_indirect multiple times
 * for the same idcaller. Each time for a different indirect function pointer
 * call site which have possibly itself a different set of candidates. 
 * This is why even if we limit in index_db4 the number of candidates
 * to let's say 100, we can still have as in Linux for getfpreg()
 * 6400 callees as we can have 64 indirect pointer function call sites
 * each with 100 candidates.
 * 
 * todo? So need also a way to limit the size of the entries so that
 * a few outliers does not cause the full index_db4 process to slow down.
 * Moreover we can also use the partial_caller info.
 * 
 *)
let add_methodcallees_of_id (idcaller, methods) db = 
  raise Todo


(*****************************************************************************)
(* Build_entity_finder *)
(*****************************************************************************)

(* See Ast_entity_php.mli for the rational behing having both a database
 * type and an entity_finder type.
 *)
let (build_entity_finder: database -> Entity_php.entity_finder) =
 fun db ->
  (fun (id_kind, s) ->
    try (
    match id_kind with
    | Entity_php.Class ->
        Db.class_ids_of_string s db 
        +> List.map (fun id -> Db.ast_of_id id db)
    | Entity_php.Function ->
        Db.function_ids__of_string s db 
        +> List.map (fun id -> Db.ast_of_id id db)
    | Entity_php.StaticMethod ->
        if s =~ "\\(.*\\)::\\(.*\\)"
        then
          let (sclass, smethod) = Common.matched2 s in
          Db.static_function_ids_of_strings ~theclass:sclass smethod db
          +> List.map (fun id -> Db.ast_of_id id db)
        else
          failwith ("wong static method format: " ^ s)
    | _ ->
        raise Todo
    )
    with exn ->
      if !Flag.show_analyze_error
      then 
        pr2 (spf "Entity_finder: pb with '%s', exn = %s" 
              s (Common.exn_to_s exn));

      raise exn
  )

(*****************************************************************************)
(* Build database intermediate steps *)
(*****************************************************************************)

let debug_index_db = ref true

(* ---------------------------------------------------------------------- *)
(* step1:  
 * - store toplevel asts
 * - store file to ids mapping
 *)
let index_db1_2 db files = 

  let nbfiles = List.length files in

  let pbs = ref [] in
  let parsing_stat_list = ref [] in

  files +> Common.index_list +> List.iter (fun (file, i) -> 
    pr2 (spf "PARSING: %s (%d/%d)" file i nbfiles);

    let all_ids = ref [] in
    try (

      Common.timeout_function 20 (fun () ->
      (* parsing, the important call *)
      let (ast2, stat) = Parse_php.parse file in
      let file_info = {
        parsing_status = if stat.Parse_info.bad = 0 then `OK else `BAD;
      }
      in
      db.file_info#add2 (file, file_info);

      Common.push2 stat  parsing_stat_list;

      Common.profile_code "Db.add db1" (fun () ->

        ast2 +> List.iter (fun (topelem, info_item) -> 
          match topelem with
          (* bugfix: the finaldef have the same id as the previous item so
           * do not add it otherwise id will not be a primary key.
           *)
          | Ast.FinalDef _ -> ()
          (* do we want to add the NotParsedCorrectly in the db ? It
           * can be useful in the code visualizer to have all
           * the elements in a file, including the one that do not
           * parse.
           * Note that this id does not have a id_kind for now.
           *)
          | _ ->
              let topelem = Unsugar_php.unsugar_self_parent_toplevel topelem in

              let id = db +> add_toplevel2 file (topelem, info_item) in
              Common.push2 id all_ids;
        );
        db +> add_filename_and_topids (file, (List.rev !all_ids));
      );
      db.flush_db();
    )
    )
    with 
    | Out_of_memory  (*| Stack_overflow*) 
    | Timeout
    -> 
      (* Backtrace.print (); *)
      pr2_err ("PB: BIG PBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB: " ^ file);

      pr2 ("Undoing addition");
      (* undoing partial addition *)
      db +> add_filename_and_topids(file, []);
      !all_ids +> List.iter (fun id -> 
        try 
          let _ = db.defs.toplevels#assoc id in
          db.defs.toplevels#delkey id +> ignore;
          (* todo? del also the fullid and id info ? *)
        with Not_found -> ()
      );
  );
  !parsing_stat_list, !pbs

let index_db1 a b = 
  Common.profile_code "Db.index_db1" (fun () -> index_db1_2 a b)


(* ---------------------------------------------------------------------- *)
(* step2:  
 *  - add defs
 *  - add strings
 *  - add nested ASTs and nested ids
 *    (and also update fullid_of_id, extra and children_ids tables)
 *  - add classes, methods, variables
 *  - add extra information (e.g. the 'kind' of an id)
 * 
 *)

let index_db2_2 db =
  iter_files_and_topids db "ANALYZING2" (fun id file -> 
    let ast = db.defs.toplevels#assoc id in


    (* Extract all strings in the code. Can be used for instance by the
     * deadcode analyzer to remove some false positives if a function
     * is mentionned in a string
     *)
    let strings = Lib_parsing.get_constant_strings_any (Toplevel ast) in
    strings +> List.iter (fun s ->
      db.strings#add2 (s, ());
    );

    (* add stuff not parsed correctly *)
    (match ast with
    | NotParsedCorrectly _infos -> 
        let toks = db.defs.tokens_of_topid#assoc id in
        toks +> List.iter (fun tok -> 
          match tok with
          | Parser_php.T_CONSTANT_ENCAPSED_STRING (s, _) ->
              db.strings#add2 (s, ());
          | Parser_php.T_IDENT(s, _) ->
              db.strings#add2 (s, ());
          | _ -> ()
        );
    | (FinalDef _|Halt _|InterfaceDef _|ClassDef _|FuncDef _|StmtList _)
        -> ()
    );

  (* let's add definitions and nested asts and entities *)
  (*
    let add_def_hook (s, kind) = 
    db +> add_def (IdString s, kind, id)
    in
    let add_type_hook typ = 
    db +> add_type (id, typ);
    in
    ~add_type:add_type_hook
  *)
    
  (* old: Definitions_php.visit_definitions_of_ast ~add_def:add_def_hook ast
   * we now need to add more information in the database (eg enclosing ids),
   * so we have to inline the visit_definitions code here and expand it
   *)

    let enclosing_id = ref id in

    (* Classes can be empty, and so children_ids will not get a chance to 
     * be populated in add_nested_id_and_ast, which can lead to some
     * Not_found when accessing those children in some of our algorithms.
     * It's simpler to have a children_ids that is always valid, and 
     * return [] (not Not_found) for empty classes, hence this code.
     *)
    db.children_ids#add2 (id, []);


    let hooks = { V.default_visitor with
      V.ktop = (fun (k, bigf) x ->
        match x with
        | FuncDef def -> 
            let s = Ast_php.name def.f_name in
            (* pr2 s; *)
            add_def (s, EC.Function, id, Some def.f_name) db;
            (* add_type def.Ast_c.f_type; *)
            k x
        | StmtList stmt -> 
            let s = "__TOPSTMT__" in
            add_def (s, EC.StmtList, id, None) db;
            k x

        | ClassDef class_def ->
            let s = Ast_php.name class_def.c_name in
            add_def (s, EC.Class, id, Some class_def.c_name) db;
            k x
        | InterfaceDef def ->
            let s = Ast_php.name def.i_name in
            add_def (s, EC.Interface, id, Some def.i_name) db;
            k x

        | Halt _ -> ()
        | NotParsedCorrectly _ -> ()
            
        (* right now FinalDef are not in the database, because of possible 
         * fullid ambiguity 
         *)
        | FinalDef _ -> raise Impossible
      );

      (* todo?  could factorize more ... *)
      V.kstmt_and_def = (fun (k, bigf) x ->
        match x with
        | Stmt _ -> ()

        | FuncDefNested def ->
            let newid = add_nested_id_and_ast ~enclosing_id:!enclosing_id
              (Ast_php.FunctionE def) db in
            let s = Ast_php.name def.f_name in
            add_def (s, EC.Function, newid, Some def.f_name) db;

            Common.save_excursion enclosing_id newid  (fun () ->
              k x
            );
            
        | ClassDefNested def ->
            let newid = add_nested_id_and_ast ~enclosing_id:!enclosing_id
              (Ast_php.ClassE def) db in
            let s = Ast_php.name def.c_name in
            add_def (s, EC.Class, newid, Some def.c_name) db;

            Common.save_excursion enclosing_id newid (fun () ->
              k x
            );

        | InterfaceDefNested def ->
            let newid = add_nested_id_and_ast ~enclosing_id:!enclosing_id
              (Ast_php.InterfaceE def) db in
            let s = Ast_php.name def.i_name in
            add_def (s, EC.Interface, newid, Some def.i_name) db;

            Common.save_excursion enclosing_id newid (fun () ->
              k x
            );
      );
      V.kclass_stmt = (fun (k, bigf) x ->
        match x with
        | Method def ->
            let newid = add_nested_id_and_ast  ~enclosing_id:!enclosing_id
              (Ast_php.MethodE def) db in
            let s = Ast_php.name def.m_name in
            let id_kind = 
              if def.m_modifiers |> List.exists (fun (modifier, ii) -> 
                modifier = Ast.Static
              )
              then EC.StaticMethod
              else EC.Method
            in
            (* todo? should we put just the method name, or also add 
             * the class name for the StaticMethod case ? 
             *)

            add_def (s, id_kind, newid, Some def.m_name) db;

            Common.save_excursion enclosing_id newid (fun () ->
              k x
            );

        (* we generate one id per constant. Note that they can not have the 
         * same AST because then we would get some "fullid already in database"
         * error.
         *)
        | ClassConstants (tok1, class_constant_list, tok2) -> 
            class_constant_list +> Ast.uncomma +> List.iter (fun class_cst ->
              let (name, _affect) = class_cst in

              let newid = add_nested_id_and_ast ~enclosing_id:!enclosing_id
                (Ast_php.ClassConstantE(class_cst))
                db
              in
              let s = Ast.name name in
              add_def (s, EC.ClassConstant, newid, None) db;
              
            );
            (* not sure we need to recurse. There can't be more definitions
             * inside class declarations.
             *)
            k x

        | ClassVariables (class_var_modifier, _opt_ty, 
                          class_variable_list, tok) ->
            class_variable_list +> Ast.uncomma +> List.iter (fun class_var ->
              let (dname, _affect) = class_var in

              let modifier = 
                match class_var_modifier with
                | NoModifiers _ -> []
                | VModifiers xs -> 
                    List.map fst xs
              in

              let newid = add_nested_id_and_ast ~enclosing_id:!enclosing_id
                (Ast_php.ClassVariableE(class_var, modifier)) db in

              let s = "$" ^ Ast.dname dname in
              add_def (s, EC.ClassVariable, newid, None) db;
            );

            k x

        | XhpDecl decl ->
            let newid = add_nested_id_and_ast ~enclosing_id:!enclosing_id
              (Ast_php.XhpDeclE decl) db in
            let s = "XHPDECLTODO" in
            add_def (s, EC.XhpDecl, newid, None) db;

            k x
      );
    }
    in
    (V.mk_visitor hooks) (Toplevel ast)

  )


let index_db2 a = 
  Common.profile_code "Db.index_db2" (fun () -> index_db2_2 a)



(* ---------------------------------------------------------------------- *)
(* step3: 
 * - caller/callees global analysis, part 1 
 * - class instantiation reverse index (who creates object of class X)
 * - extends and implements reverse index (who extends/implements class X)
 * - TODO globals-used reverse index
 * 
 * Was before in index_db2. Prefer now to do this step in a separate phase
 * than index_db2. Before they were together but I found this coupling
 * not intellectually satisfactory and it turns out later that we needed
 * anyway to do things in sequence. 
 * 
 * Indeed we would later need in the caller/callees phase to 
 * have run one time index_db2 on all the files to get access to 
 * all the possible definitions of a function call string.  So now split
 * step in two; first add the type, struct def, macro def in db, 
 * and then do the first caller/callee. 
 * 
 * This sequencing also enable to know if the ident corresponds to a
 * macro or not. This can be useful. This also handles recursive calls
 * in an easy way.
 * This also enable to give a score of confidence to DirectCall
 * as for instance a call on a function f defined in the same file is
 * more probable than call to function with same name f in another file 
 * or directory.
 * 
 * Require now some whole-program information such as full list of 
 * definitions, so run after index_db2!
 * 
 * Note that add_callees_of_f can be quite expansive if not optimized, 
 * especially when want to store all call instances and for each instances,
 * even for the directcalls, all the possible targets (because even for
 * directcalls can have ambiguities and multiple candidates).
 * 
 * 
 * 
 * Note that in PHP the type inference will work
 * better if we have the information on the function at the call site,
 * which mean that by doing the callgraph analysis first, and do 
 * some topological sort, we can more easily do a type inference analysis.
 * 
 *
 *)

let index_db3_2 db = 

  (* how assert no modif on those important tables ? *)

  iter_files_and_ids db "ANALYZING3" (fun id file -> 
    let ast = Db.ast_of_id id db in
    (* bugfix: was calling Unsugar_php.unsugar_self_parent_entity ast
     * here but it's too later because an entity can be a nested id
     * which does not have an enclosing class_def to set the classname.
     * So the unsugaring must be done in phase 1
     *)

    let idcaller = id in

    (* the regular function calls sites
     * todo: if the entity is a class, then right now we will consider
     * any calls inside its method. We used to use Visitor2 which was
     * not visiting the class_statements of a class but now that we
     * removed it, we visit everything. Not sure it's an issue.
     *)
    let callees = 
      Callgraph_php.callees_of_any (Entity ast) in

    (* TODO: actually when have parent::foo it does not mean it's
     * a static method. It could be a regular inherited public/protected 
     * method
     *)
    let static_method_callees = 
      Callgraph_php.static_method_callees_of_any (Entity ast) in

    db +> add_callees_of_id (idcaller,  callees ++ static_method_callees);

    (* the new, X::, extends, etc *)
    let classes_used = users_of_class_in_any (Entity ast) in
    let candidates = 
      classes_used +> List.map Ast.name +> Common.set 
      +> Common.map_flatten (fun s -> class_ids_of_string s db)
    in
    candidates +> List.iter (fun idclass -> 
      db.uses.users_of_class#apply_with_default idclass
        (fun old -> id::old) (fun() -> []) +> ignore
    );

    (* the extends and implements *)
    (match ast with
    | Ast.ClassE def ->
        let idB = id in

        def.c_extends |> Common.do_option (fun (tok, classnameA) ->
         (* we are in a situation like: class B extends A *)

          let s = Ast.name classnameA in
          (* There may be multiple classes defining classnameA and 
           * so multiple ids. 
           * 
           * todo: at some point we would like a better
           * scope/file analysis so that there is no ambiguity.
           *)
          let candidates = class_ids_of_string s db in
          candidates |> List.iter (fun idA -> 
            db.uses.extenders_of_class#apply_with_default idA
              (fun old -> idB::old) (fun() -> []) +> ignore
          );
        );

        def.c_implements |> Common.do_option (fun (tok, interface_list) ->
          interface_list |> Ast.uncomma |> List.iter (fun interfacenameA ->
            (* we are in a situation like: class B implements A *)

            let s = Ast.name interfacenameA in
            let candidates = interface_ids_of_string s db in
            candidates |> List.iter (fun idA -> 
              db.uses.implementers_of_interface#apply_with_default idA
                (fun old -> idB::old) (fun() -> []) +> ignore
            );
          );
        );
        
    (* todo? interface can also be extended; maybe should add a 
     * extenders_of_interface at some point if it's useful
     *)
    | Ast.InterfaceE _ ->
        ()

    | Ast.MiscE _
    | Ast.ClassVariableE _
    | Ast.ClassConstantE _
    | Ast.XhpDeclE _
    | Ast.MethodE _
    | Ast.StmtListE _
    | Ast.FunctionE _ 
      ->  ()
    );

    (*
      let global_used = Relation_c.globals_of_ast ~also_def:false ast in
      db +> add_globals_in_toplevel (id, global_used);
      let structs_used = Relation_c.structnames_of_ast ~also_def:false ast in
      db +> add_structs_in_toplevel (id, structs_used);
      let fields_used = Relation_c.fields_used_of_ast ~also_def:false ast in
      db +> add_fields_in_toplevel (id, fields_used);
      let typedefs_used = Relation_c.globals_of_ast ~also_def:false ast in
      db +> add_typedefs_in_toplevel (id, typedefs_used);
    *)
    ()
  )


let index_db3 a = 
  Common.profile_code "Db.index_db3" (fun () -> index_db3_2 a)

(* ---------------------------------------------------------------------- *)
(* step4:
 *  - tags annotations for functions
 *  - local/global annotations
 *)
let index_db4_2 db = 

  let find_entity = build_entity_finder db in
  let msg = "ANALYZING4" in
  iter_files db (fun ((file, ids), i, total) -> 
   pr2 (spf "%s: %s %d/%d " msg file i total);

    
    let asts = ids +> List.map (fun id -> db.defs.toplevels#assoc id) in

    Check_variables_php.check_and_annotate_program 
      ~find_entity:(Some (fun x ->
        (* we just want to annotate here *)
        try 
          find_entity x 
        with Multi_found ->
          raise Not_found
      ))
      asts;

    zip ids asts +> List.iter (fun (id, ast) ->
      db.defs.toplevels#add2 (id, ast);
    );

   ids +> List.iter (fun id ->

    let ast = db.defs.toplevels#assoc id in
    
    (* tags, like @phpsh *)
    let toks = db.defs.tokens_of_topid#assoc id in
    let comment_opt = first_comment ast toks in 

    let tags = ref [] in
    comment_opt +> Common.do_option (fun info ->
      let str = info.Parse_info.str in
      let comment_tags = 
        try A.extract_annotations str 
        with exn ->
          pr2_err (spf "PB: EXTRACT ANNOTATION: %s on %s" 
                  (Common.exn_to_s exn) file);
          []
      in
      tags := comment_tags;
    );

    let callees = Lib_parsing_php.get_funcalls_any (Toplevel ast) in
    (* facebook specific ... *)
    
    if List.mem "THIS_FUNCTION_EXPIRES_ON" callees
    then Common.push2 A.Have_THIS_FUNCTION_EXPIRES_ON tags;

    let extra = db.defs.extra#assoc id in
    let extra' = { extra with
      tags = !tags;
    } in

    db.defs.extra#add2 (id, extra');
   );
  );
  ()

let index_db4 a = 
  Common.profile_code "Db.index_db4" (fun () -> index_db4_2 a)

(*****************************************************************************)
(* create_db *)
(*****************************************************************************)
let max_phase = 4

let create_db 
    ?(verbose_stats=true)
    ?(db_support=Db.Mem)
    ?(phase=max_phase) 
    ?(files=None) 
    prj  
 = 
  let prj = normalize_project prj in 

  let db = 
    match db_support with
    | Disk metapath ->

        let metapath = 
          if metapath = ""
          then Database_php.default_metapath_of_project prj
          else metapath
        in
        
        (*
          if (Sys.file_exists (Filename.concat metapath "notes.txt"))
          then failwith "there is a notes.txt file";
        *)
        
        if not (Common.command2_y_or_no("rm -rf " ^ metapath))
        then failwith "ok we stop";
        
        Common.command2("mkdir -p " ^ metapath);
        Common.command2(spf "touch %s/%s" metapath 
                           Database_php.database_tag_filename);
        Common.write_value prj (metapath ^ "/prj.raw");

        let db = !Database_php._current_open_db_backend metapath in

        let logchan = open_out (metapath ^ "/log.log") in
        Common._chan_pr2 := Some logchan;
        db
    (* note that is is in practice less efficient than Disk :( because
     * probably of the GC that needs to traverse more heap cells
     *)
    | Mem -> 
        open_db_mem prj
  in
  begin

    let files = 
      match files with
      | None ->
          
          let dir = path_of_project db.project in
          
          (* note that if dir is a symlink, files would be an empty list,
           * especially as we chop the / in path_of_project, so add an extra "/"
           * 
           * bugfix: some file like scripts/update_database are php files
           * but do not end in .php ... so not enough to just use 
           * 
           *      let ext = ".*\\.\\(php\\|phpt\\)$" in
           *)
          
          Lib_parsing_php.find_php_files_of_dir_or_files [(dir ^ "/")]

      | Some xs ->     xs
    in

    let nbfiles = List.length files in
    (* does not work well on Centos 5.2
     *
     * Common.check_stack_nbfiles nbfiles; 
     *)
    Common.check_stack_nbfiles nbfiles; 

    (* ?default_depth_limit_cpp *)
    let parsing_stats, bigpbs = 
      index_db1 db files
    in
    db.flush_db();

    if phase >= 2 then index_db2 db;
    db.flush_db();
    if phase >= 3 then index_db3 db;
    db.flush_db();

    if phase >= 4 then index_db4 db;
    db.flush_db();

    if verbose_stats && !Flag.show_analyze_error then begin
      (* Parsing_stat.print_stat_numbers (); *)
      Parse_info.print_parsing_stat_list   parsing_stats;
      !_errors +> List.iter pr2;
    end;
    db
  end


(*****************************************************************************)
(* Fast db construction *)
(*****************************************************************************)

(* The goal here is to build a database containing enough context, enough
 * relevant files, to be able to do interesting global analysis on a file
 * passed as a parameter to one of our command line program.
 * 
 * todo: see facebook/dependencies.ml for now
 * todo: factorize all the db_of_files_or_dirs out there.
 *)
let fast_create_db_mem ?phase files_or_dirs =
  raise Todo

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  (* no -create_db as it is offered as the default action in 
   * main_db,ml
   *)
(*
  "-index_db1", "   <db>", 
    Common.mk_action_1_arg (fun dbname -> 
      with_db ~metapath:dbname index_db1);
*)
  "-index_db2", "   <db>", 
    Common.mk_action_1_arg (fun dbname -> 
      with_db ~metapath:dbname index_db2);
  "-index_db3", "   <db>", 
    Common.mk_action_1_arg (fun dbname -> 
      with_db ~metapath:dbname index_db3);
]
