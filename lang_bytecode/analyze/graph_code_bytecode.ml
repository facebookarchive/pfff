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

module E = Entity_code
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
 *    with a '$' used as a separator instead of '.'
 *  - handling anonymous classes, they are compiled in another class with
 *    a '$N' suffix.
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

(* So "Foo.Bar$FooBar$1" is translated into something more structured,
 * that is: the first anonymous class inside the nested class FooBar
 * of class Bar in the Foo package.
 *)
type bytecode_class_name = {
  package: string list;
  baseclass: string;
  nested_or_anon: dollar_suffix list
}
  and dollar_suffix =
  | DollarNestedClass of string
  | DollarAnonClass of string

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
    if show_parse_error
    then pr2_once (spf "PARSE ERROR with %s, exn = %s" file 
                      (Common.exn_to_s exn));
    raise exn

let bytecode_class_name_of_string name = 
  let xs = Common.split "\\." name in
  let package, bytecode_name_entity  = 
    match List.rev xs with
    | [] -> failwith ("wrong format for bytecode class name: " ^ name)
    | [x] -> [], x
    | x::xs -> List.rev xs, x
  in
  let ys = Common.split "\\$" bytecode_name_entity in
  match ys with
  | [] -> failwith ("wrong format for bytecode class name: " ^ name)
  | y::ys ->
    { package; baseclass = y;
      nested_or_anon = ys +> List.map (fun y ->
        if y =~ "[0-9]+" 
        then DollarAnonClass y
        else DollarNestedClass y
      );
    }
let java_class_name_of_bytecode_class_name x =
  Common.join "." (x.package @ [x.baseclass]) ^
  (x.nested_or_anon +> List.map (function
   (* todo: need look in original java file, or have a more compatible
    * convention of how I name anon class entities in graph_code_java.ml
    *)
   | DollarAnonClass _ -> raise Todo
   | DollarNestedClass s -> "." ^ s
   ) +> Common.join ""
  )

(* quite similar to create_intermediate_directories_if_not_present *)
let create_intermediate_packages_if_not_present g root xs =

  (* ["java";"lang"] -> [[]; ["java"]; ["java";"lang"] ] *)
  let dirs = Common2.inits xs in
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
      (* probably inaccurate for fields/methods *)
      let fake = bytecode_class_name_of_string fake_name in
      let parent = create_intermediate_packages_if_not_present 
        g parent_target fake.package in
      (match name with
      (* on unit test code, we have this failure that we don't want show *)
      | "java.lang.Object.<init>" -> ()
      | "java.lang.Object" -> ()
      | _ ->
        pr2 (spf "PB: lookup fail on %s (in %s)" 
               (G.string_of_node dst) (G.string_of_node src));
      );
      g +> G.add_node dst;
      g +> G.add_edge (parent, dst) G.Has;
    end;
    g +> G.add_edge (src, dst) G.Use;
    ()

(*****************************************************************************)
(* Lookup *)
(*****************************************************************************)

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

(*****************************************************************************)
(* Bytecode<->Java connection *)
(*****************************************************************************)

let unmangle _graph_java (full_str_bytecode_name, kind) =

  let class_ = bytecode_class_name_of_string full_str_bytecode_name in
  
  let class_ = { class_ with
    (* todo: look in graph_java for the corresponding anon_xxx at some point *)
    nested_or_anon = class_.nested_or_anon +> List.filter (function
    | DollarAnonClass _ -> false
    | DollarNestedClass _ -> true
    );
  }
  in
  java_class_name_of_bytecode_class_name class_, kind

let java_basename_of_jclass jclass =
  jclass.j_attributes +> Common.find_some (function
  (* note that this always contain just a basename, e.g. "Foo.java", never
   * a full path like "fb4a/com/facebook/Foo.java"
   *)
  | AttributeSourceFile f -> Some f
  | _ -> None
  )

let java_basename_of_bytecode_classname x =
  x.baseclass ^ ".java"

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)
let extract_defs2 ~g ~file ~graph_code_java ~hjavabasename_to_fullpath ast =
  let jclass = ast in

  let name = JBasics.cn_name jclass.j_name in
  let class_ = bytecode_class_name_of_string name in
  let current = 
    create_intermediate_packages_if_not_present g G.root class_.package in

  let node = (name, E.Class) in
  (try 
      g +> G.add_node node;
  with Graph_code.Error (Graph_code.NodeAlreadyPresent node) ->
    let nodeinfo = G.nodeinfo node g in
    pr2 (spf "DUPE: %s" (G.string_of_node node));
    pr2 (spf " orig = %s" (nodeinfo.G.pos.Parse_info.file));
    pr2 (spf " dupe = %s" file);
    raise (Graph_code.Error (Graph_code.NodeAlreadyPresent node))
  );
  let nodeinfo = { G.
    pos = { Parse_info.
      str = "";
      line = -1;
      charpos = -1;
      column = -1;
      file = file;
    };
    props = [];
    typ = None;
  } in
  g +> G.add_nodeinfo node nodeinfo;
  (match class_.nested_or_anon with
  | [] -> g +> G.add_edge (current, node) G.Has;
  (* this will be done later in adjust_parents_nested_anon2 *)
  | _ -> ()
  );
  graph_code_java +> Common.do_option (fun g2 ->
    let node' = unmangle g2 node in
    try 
      let nodeinfo = G.nodeinfo node' g2 in
      g +> G.add_nodeinfo node nodeinfo
    with Not_found ->
      let java_filename =
        try 
          java_basename_of_jclass jclass 
        with Not_found ->
          pr2 (spf "no AttributeSourceFile for %s" (G.string_of_node node));
          java_basename_of_bytecode_classname class_
      in
      (* todo: could disambiguate those files by parsing them and looking
       * at their .package field.
       *)
      let java_possible_files =
        try 
          Hashtbl.find hjavabasename_to_fullpath java_filename
        with Not_found -> []
      in
      if null java_possible_files
      then pr2 (spf "no java source file for %s" (G.string_of_node node))
      else pr2 (spf "no nodeinfo in the java graph for %s (guess was %s)"
             (G.string_of_node node) (G.string_of_node node'))
  );

  let current = node in

  jclass.j_fields +> List.iter (fun fld ->
    let node = (name ^ "." ^ fld.f_name, E.Field) in
    g +> G.add_node node;
    g +> G.add_edge (current, node) G.Has;
  );
  jclass.j_methods +> List.iter (fun def ->
    let node = (name ^ "." ^ def.m_name, E.Method) in

    (* less: for now we just collapse all methods with same name together *)
    if G.has_node node g
    then ()
    else begin
      g +> G.add_node node;
      g +> G.add_edge (current, node) G.Has;
    end
  );
  ()
let extract_defs ~g ~file ~graph_code_java ~hjavabasename_to_fullpath ast =
  Common.profile_code "GC_bytecode.extract_defs" (fun () ->
    extract_defs2 ~g ~file ~graph_code_java ~hjavabasename_to_fullpath ast
  )


(* this must be run after all the classes has been created as we
 * need to lookup for classes.
 *)
let adjust_parents_nested_anon2 g =
  g +> G.iter_nodes (fun n ->
    let (str, kind) = n in
    (match kind with
    | E.Class ->
       let class_ = bytecode_class_name_of_string str in
       (match class_.nested_or_anon with
       | [] -> ()
       | _ ->
         let new_parent = java_class_name_of_bytecode_class_name 
           {class_ with nested_or_anon = [] }
         in
         (* old: not sure why but remove_edge seems extremly slow
          * let _old_parent = G.parent n g in
          * G.remove_edge (old_parent, n) G.Has g;
          *)
         G.add_edge ((new_parent, E.Class), n) G.Has g;
       )
    | _ -> ()
    )
  )

let adjust_parents_nested_anon a = 
  Common.profile_code "GC_bytecode.adjust_parents" (fun () ->
    adjust_parents_nested_anon2 a)

(*****************************************************************************)
(* Inheritance *)
(*****************************************************************************)

let extract_uses_inheritance2 ~g ast =
  let jclass = ast in
  let name = JBasics.cn_name jclass.j_name in
  let current = (name, E.Class) in
  let env = { g; current; consts = jclass.j_consts } in

  let parents = Common2.option_to_list jclass.j_super @ jclass.j_interfaces in
  parents +> List.iter (fun cname ->
    let node = (JBasics.cn_name cname, E.Class) in
    add_use_edge env node;
  );
  ()

let extract_uses_inheritance ~g ast =
  Common.profile_code "GC_bytecode.extract_inheritance" (fun () ->
    extract_uses_inheritance2 ~g ast)

(*****************************************************************************)
(* Uses *)
(*****************************************************************************)

let rec extract_uses2 ~g ast =
  let jclass = ast in
  let name = JBasics.cn_name jclass.j_name in
  let current = (name, E.Class) in
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
    let node = (name ^ "." ^ def.m_name, E.Method) in
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
      let node = (JBasics.cn_name cname, E.Class) in
      add_use_edge env node

and code env x = 
  let x = Lazy.force x in
  x.c_attributes +> List.iter (function
  | AttributeCode _ -> failwith "code in c_attributes?"
  | _ -> ()
  );
  x.c_code +> Array.iteri (fun _i op ->
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
    | OpAMultiNewArray (_i, _) ->
      pr2_once "TODO OpAMultiNewArray"


    | OpGetStatic i | OpPutStatic i
    | OpGetField i | OpPutField i
        ->
        (match env.consts.(i) with
        | ConstField (cname, descr) ->
            let name = JBasics.cn_name cname in
            let fldname = JBasics.fs_name descr in
            let node = (name ^ "." ^ fldname, E.Field) in
            (match lookup env.g (name, E.Class) fldname with
            | None -> add_use_edge env node
            | Some n -> add_use_edge env n
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

            let node = (name ^ "." ^ fldname, E.Method) in

            (match lookup env.g (name, E.Class) fldname with
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

    | OpInvokeInterface (_i1, _i2) ->
      pr2_once "TODO OpInvokeInterface";
    | OpCheckCast _i | OpInstanceOf _i ->
      ()
    | _ -> ()
  );
  ()

let extract_uses ~g ast =
  Common.profile_code "GC_bytecode.extract_uses" (fun () ->
    extract_uses2 ~g ast)
  
(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) ?(graph_code_java=None) root files =
  let java_files = Lib_parsing_java.find_source_files_of_dir_or_files [root] in

  (* less? Skip_code.filter_files skip_list root all_java_files in *)
  let hjavabasename_to_fullpath =
    java_files 
    +> List.map (fun file -> Filename.basename file, file)
    +> Common.group_assoc_bykey_eff
    +> Common.hash_of_list
  in

  let g = G.create () in
  G.create_initial_hierarchy g;

  (* step1: creating the nodes and 'Has' edges, the defs *)
  if verbose then pr2 "\nstep1: extract defs";
  files +> Console.progress ~show:verbose (fun k -> 
    List.iter (fun file ->
      k();
      let ast = parse ~show_parse_error:true file in
      (* it's tempting to put stuff under EXTERNAL under a special E.Dir
       * but then the toplevel com. will be created there and every
       * folloing creation of classes under com will then finish 
       * under EXTERNAL too
       *)
      extract_defs ~g ~file ~graph_code_java ~hjavabasename_to_fullpath ast;
      ()
    ));
  if verbose then pr2 "\nstep1 bis: adjust parents of nested/anon classes";
  adjust_parents_nested_anon g;

  (* step2: creating the 'Use' edges for inheritance *)
  if verbose then pr2 "\nstep2: extract inheritance information";
  files +> Console.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let ast = parse ~show_parse_error:false  file in
     (* we need to extract inheritance information for the builtins too *)
     extract_uses_inheritance ~g ast
   ));

  (* step3: creating the 'Use' edges *)
  if verbose then pr2 "\nstep3: extract uses";
  files +> Console.progress ~show:verbose (fun k -> 
   List.iter (fun file ->
     k();
     let readable = Common.readable ~root file in
     if readable =~ "^external" || readable =~ "^EXTERNAL"
     then ()
     else 
       let ast = parse ~show_parse_error:false  file in
       extract_uses ~g ast
   ));
  g
