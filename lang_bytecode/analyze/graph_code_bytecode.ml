(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

module E = Database_code
module G = Graph_code

open JBasics
open JClassLow

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for bytecode. See graph_code.ml and main_codegraph.ml
 * for more information.
 * 
 * As opposed to lang_java/analyze/graph_code_java.ml, no need for:
 *  - package lookup, all names are resolved in the bytecode
 *    (still need a class lookup for fields/methods though ...)
 *  - handling nested classes, they are compiled in another class
 *    with a $ suffix
 *  - handling generics?
 *  - type checking to resolve certain method calls, the bytecode is fully
 *    typed (a bit like TAL), one can get the type of each 'invoke' opcode
 * 
 * I now pass a graph_code_java as a parameter to get the source code
 * location for the entities. The bytecode has attributes
 * such as AttributeSourceFile (and AttributeLineNumberTable) but:
 *  - the filename there does not have any directory information, so
 *    one would need to look for all java files with this name, parse
 *    them, and extract the package name in it to disambiguate
 *  - some opcodes have entries in a LineNumberTable, but what about
 *    empty methods? and what about the LineNumberTable for the class?
 * So for now I just abuse the graph_code for java and try to map a
 * node in graph_code_bytecode to a node in graph_code_java (which by
 * side effects help understand how things are translated).
 * 
 * todo: StaticMethod, StaticField, the bytecode has this information
 * less: put back nested classes inside the other
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  g: Graph_code.graph;
  current: Graph_code.node;

  (* opcodes like getfield, invokevirtual are taking integer parameters
   * that are reference in a constant table containing the full
   * name of the classes/methods/fields.
   *)
  consts: JBasics.constant array;
}

(* We need 3 phases, one to get all the definitions, one to
 * get the inheritance information, and one to get all the Uses.
 * The inheritance is a kind of use, but certain uses like using
 * a field needs the full inheritance tree to already be computed
 * as we may need to lookup entities up in the parents.
 * 
 * Note that if you don't include the java stdlib in the analysis,
 * then you will get not only Not_found lookup for java.xxx classes/methods,
 * but also probably method lookup failures on classes from the program
 * under analysis. Indeed many classes could derive from standard classes
 * and inherits those methods, and without inheritance information, the
 * lookup on those methods will fail.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let _hmemo = Hashtbl.create 101
let parse ~show_parse_error file =
  try 
    Common.memoized _hmemo file (fun () ->
      Parse_bytecode.parse file
    )
  with exn ->
    pr2_once (spf "PARSE ERROR with %s, exn = %s" file 
                  (Common.exn_to_s exn));
    raise exn


let package_and_name_of_str name =
  let xs = Common.split "\\." name in
  let package = List.rev (List.tl (List.rev xs)) in
  (package, name)

let package_and_name_of_cname class_name =
  let name = JBasics.cn_name class_name in
  package_and_name_of_str name


(* quite similar to create_intermediate_directories_if_not_present *)
let create_intermediate_packages_if_not_present g root xs =

  (* ["java";"lang"] -> [[]; ["java"]; ["java";"lang"] ] *)
  let dirs = Common.inits xs in
  let dirs = 
    match dirs with
    | []::xs -> xs
    | _ -> raise Impossible
  in

  let rec aux current xs =
    match xs with
    | [] -> current
    | x::xs ->
      let str = Common.join "." x in
      let entity = str, E.Package in
      if G.has_node entity g
      then aux entity xs
      else begin
        g +> G.add_node entity;
        g +> G.add_edge (current, entity) G.Has;
        aux entity xs
      end
  in
  aux root dirs

let add_use_edge env dst =
  let src = env.current in
  let g = env.g in
  match () with
  | _ when not (G.has_node src g) ->
      pr2 (spf "LOOKUP SRC FAIL %s --> %s, src does not exist???"
              (G.string_of_node src) (G.string_of_node dst));

  | _ when G.has_node dst g -> 
      G.add_edge (src, dst) G.Use g

  | _ -> 
    let (name, kind) = dst in
    let fake_name = 
      (Common.split "\\." name) 
      +> List.map (fun s -> s^"2") 
      +> Common.join "."
    in
    let dst = (fake_name, kind) in
    let parent_target = G.not_found in
    if not (G.has_node dst g)
    then begin 
      let (fake_package, _name) = package_and_name_of_str fake_name in
      let parent = create_intermediate_packages_if_not_present 
        g parent_target fake_package in
      pr2 (spf "PB: lookup fail on %s (in %s)" 
             (G.string_of_node dst) (G.string_of_node src));
      g +> G.add_node dst;
      g +> G.add_edge (parent, dst) G.Has;
    end;
    g +> G.add_edge (src, dst) G.Use;
    ()

let (lookup2: 
  Graph_code.graph -> Graph_code.node -> string -> Graph_code.node option) =
 fun g start fld ->

  let rec depth current =
    if not (G.has_node current g)
    then None
    else 
      let children = G.children current g in
      let full_name = (fst current ^ "." ^ fld) in
      let res =
        children +> Common.find_some_opt (fun (s2, kind) ->
          if full_name =$= s2
          then Some (s2, kind)
          else None
        )
      in
      match res with
      | Some x -> Some x
      | None -> 
          let parents_inheritance = G.succ current G.Use g in
          breath parents_inheritance
  and breath xs = xs +> Common.find_some_opt depth
  in
  depth start

let _hmemo = Hashtbl.create 101 
let lookup g n s =
  Common.profile_code "Graph_bytecode.lookup" (fun () ->
    Common.memoized _hmemo (n,s) (fun () ->
        lookup2 g n s
    )
  )

let unmangle graph_java (full_str_bytecode_name, kind) =
  let xs = Common.split "\\." full_str_bytecode_name in
  let (package, bytecode_name_entity) =
    (match List.rev xs with
    | [] -> raise Impossible
    | x::xs -> List.rev xs, x
    )
  in
  let ys = Common.split "\\$" bytecode_name_entity in
  
  let full_name = package ++ ys in
  Common.join "." full_name, kind
  

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)
let extract_defs ~g ~file ~graph_code_java ast =
  let jclass = ast in

  let (package, name) = package_and_name_of_cname jclass.j_name in
  let current = create_intermediate_packages_if_not_present g G.root package in

  let node = (name, E.Class E.RegularClass) in
  g +> G.add_node node;
  g +> G.add_edge (current, node) G.Has;
  graph_code_java +> Common.do_option (fun g2 ->
    let node' = unmangle g2 node in
    try 
      let nodeinfo = G.nodeinfo node' g2 in
      g +> G.add_nodeinfo node nodeinfo
    with Not_found ->
      pr2 (spf "could not find nodeinfo in the java graph of %s (guess was %s)"
             (G.string_of_node node) (G.string_of_node node'))
  );

  let current = node in

  jclass.j_fields +> List.iter (fun fld ->
    let node = (name ^ "." ^ fld.f_name, E.Field) in
    g +> G.add_node node;
    g +> G.add_edge (current, node) G.Has;
  );
  jclass.j_methods +> List.iter (fun def ->
    let node = (name ^ "." ^ def.m_name, E.Method E.RegularMethod) in

    (* less: for now we just collapse all methods with same name together *)
    if G.has_node node g
    then ()
    else begin
      g +> G.add_node node;
      g +> G.add_edge (current, node) G.Has;
    end
  );
  ()

(*****************************************************************************)
(* Inheritance *)
(*****************************************************************************)

let extract_uses_inheritance ~g ast =
  let jclass = ast in
  let name = JBasics.cn_name jclass.j_name in
  let current = (name, E.Class E.RegularClass) in
  let env = { g; current; consts = jclass.j_consts } in

  let parents = Common.option_to_list jclass.j_super ++ jclass.j_interfaces in
  parents +> List.iter (fun cname ->
    let node = (JBasics.cn_name cname, E.Class E.RegularClass) in
    add_use_edge env node;
  );
  ()

(*****************************************************************************)
(* Uses *)
(*****************************************************************************)

let rec extract_uses ~g ast =
  let jclass = ast in
  let name = JBasics.cn_name jclass.j_name in
  let current = (name, E.Class E.RegularClass) in
  let env = { g; current; consts = jclass.j_consts } in

  jclass.j_attributes +> List.iter (function
  | AttributeCode _ -> failwith "code in j_attributes?"
  | _ -> ()
  );

  jclass.j_fields +> List.iter (fun fld ->
    let node = (name ^ "." ^ fld.f_name, E.Field) in
    let env = { env with current = node } in
    value_type env fld.f_descriptor;

    fld.f_attributes +> List.iter (function
    | AttributeCode _ -> failwith "code in f_attributes?"
    | _ -> ()
    );
  );
  jclass.j_methods +> List.iter (fun def ->
    let node = (name ^ "." ^ def.m_name, E.Method E.RegularMethod) in
    let env = { env with current = node } in
    (* less: dependencies for parameters? ok cmf spirit? and skip? *)

    def.m_attributes +> List.iter (function
    | AttributeCode x -> 
        code env x
    | _ -> ()
    );
  );
  ()

and value_type env = function
  | TBasic _ -> ()
  | TObject obj -> object_type env obj

and object_type env = function
  | TArray x -> value_type env x
  | TClass cname ->
      let node = (JBasics.cn_name cname, E.Class E.RegularClass) in
      add_use_edge env node

and code env x = 
  let x = Lazy.force x in
  x.c_attributes +> List.iter (function
  | AttributeCode _ -> failwith "code in c_attributes?"
  | _ -> ()
  );
  x.c_code +> Array.iteri (fun i op ->
    match op with
    | OpNew i | OpANewArray i ->
        (match env.consts.(i) with
        | ConstValue (ConstClass obj) ->
            object_type env obj
        | x -> 
          pr2 ("Unexpected constant for OpNew");
          pr2_gen x;
        );
    | OpNewArray _java_basic_type -> ()
    | OpAMultiNewArray (i, _) ->
      pr2_once "TODO OpAMultiNewArray"


    | OpGetStatic i | OpPutStatic i
    | OpGetField i | OpPutField i
        ->
        (match env.consts.(i) with
        | ConstField (cname, descr) ->
            let name = JBasics.cn_name cname in
            let fldname = JBasics.fs_name descr in

            let node = (name ^ "." ^ fldname, E.Field) in

            (match lookup env.g (name, E.Class E.RegularClass) fldname with
            | None ->
                add_use_edge env node
            | Some n ->
                add_use_edge env n
            )
        | x -> 
          pr2 ("Unexpected constant for OpGetField");
          pr2_gen x;
        );
    | OpInvokeVirtual i
    | OpInvokeNonVirtual i
    | OpInvokeStatic i
        ->
        (match env.consts.(i) with
        | ConstMethod (TClass cname, descr) ->
            let name = JBasics.cn_name cname in
            let fldname = JBasics.ms_name descr in

            let node = (name ^ "." ^ fldname, E.Method E.RegularMethod) in

            (match lookup env.g (name, E.Class E.RegularClass) fldname with
            | None -> add_use_edge env node
            | Some n -> add_use_edge env n
            )
        | ConstMethod (TArray _, descr) as x -> 
          let fldname = JBasics.ms_name descr in
          (match fldname with
          | "clone" -> ()
          | _ ->
            let ch = IO.output_channel stderr in
            pr2 ("Unexpected constant for OpInvokeVirtual");
            JDumpBasics.dump_constant ch x;
          )

        | x ->
          let ch = IO.output_channel stderr in
          pr2 ("Unexpected constant for OpInvokeVirtual");
          JDumpBasics.dump_constant ch x;
        );

    | OpInvokeInterface (i1, i2) ->
      pr2_once "TODO OpInvokeInterface";
    | OpCheckCast i | OpInstanceOf i ->
      ()
    | _ -> ()
  );
  ()
  
(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) ?(graph_code_java=None) dir_or_file skip_list =
  let root = Common.realpath dir_or_file in
  let all_files = 
    Lib_parsing_bytecode.find_source_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = Skip_code.filter_files ~verbose skip_list root all_files in

  let g = G.create () in
  G.create_initial_hierarchy g;

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then pr2 "\nstep1: extract defs";
  files +> Common_extra.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let ast = parse ~show_parse_error:true file in
      (* it's tempting to put stuff under EXTERNAL under a special E.Dir
       * but then the toplevel com. will be created there and every
       * folloing creation of classes under com will then finish 
       * under EXTERNAL too
       *)
      extract_defs ~g ~file ~graph_code_java ast;
      ()
    ));

  (* step2: creating the 'Use' edges for inheritance *)
  if verbose then pr2 "\nstep2: extract inheritance information";
  files +> Common_extra.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let ast = parse ~show_parse_error:false  file in
     (* we need to extract inheritance information for the builtins too *)
     extract_uses_inheritance ~g ast
   ));

  (* step3: creating the 'Use' edges *)
  if verbose then pr2 "\nstep3: extract uses";
  files +> Common_extra.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let readable = Common.filename_without_leading_path root file in
     if readable =~ "^external" || readable =~ "^EXTERNAL"
     then ()
     else 
       let ast = parse ~show_parse_error:false  file in
       extract_uses ~g ast
   ));
  g
