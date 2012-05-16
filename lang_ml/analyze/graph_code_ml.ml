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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph of dependencies for OCaml. See graph_code.ml and main_codegraph.ml
 * for more information.
 * 
 * todo? if give edge weight, then need to modulate depending on
 * the type of the reference. 2 references to a function in another
 * module is more important than 10 references to some constructors.
 * Type|Exception > Function|Class|Global > Constructors|constants ?
 * If we do some pattern matching on 20 constructors, is it more
 * important than 2 functions calls? Need to differentiate those
 * different use of the qualifier
 * 
 * todo? there is no edge weight? But is it useful in an ocaml context?
 * We can't have mutually dependent files or directories; the ocaml compiler
 * imposes a layering, so the in-edge will be enough information to give
 * more weight to some nodes. Thx to this layering the connected components
 * module of gephi also does some good work.
 * 
 * notes:
 *  - ml vs mli? just get rid of mli? but one can also want to
 *    care only about mli dependencies, like I did with my 'make doti'. 
 *    We can introduce a Module entity that is the parent of the
 *    ml and mli file (this entity has-graph unify many things).
 *    TODO but there is still the issue about where to put the edge
 *    when one module call a function in another module. Do we
 *    link the call to the def in the mli or in the ml?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers graph_code *)
(*****************************************************************************)

(* todo: put in graph_code.ml at some point, this is generic code *)
let create_intermediate_directories_if_not_present g dir =
  let dirs = Common.inits_of_relative_dir dir in

  let rec aux current xs =
    match xs with
    | [] -> ()
    | x::xs ->
        let entity = x, E.Dir in
        if G.has_node entity g
        then aux entity xs
        else begin
          g +> G.add_node entity;
          g +> G.add_edge (current, entity) G.Has;
          aux entity xs
        end
  in
  aux G.root dirs


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let parse file =
  Common.save_excursion Flag_parsing_ml.show_parsing_error false (fun ()->
    try 
      Parse_ml.parse_program file 
    with Parse_ml.Parse_error _ ->
      (* pr2 ("PARSING problem in: " ^ file); *)
      []
  )

(* Adjust the set of files, to exclude noisy modules (e.g. tests in external/)
 * or modules that would introduce false positive when we do the
 * modulename->file lookup.
 *)
let filter_ml_files files =
  files +> Common.exclude (fun file ->
    (* less: could also do a pfff_dependencies that just care about mli
     * like my make doti
     *)
    let (d,b,e) = Common.dbe_of_filename file in
    let xs = Common.split "/" d in
    let ml_file = Common.filename_of_dbe (d,b,"ml") in

    (* todo: pfff specific ... *)
    let is_test_in_external =
      List.mem "external" xs &&
        xs +> List.exists (fun s ->
          match s with
          | "examples" | "tests" |  "test" 
          (* ocamlgraph and ocamlgtk specific *)
          | "dgraph" | "editor" | "view_graph" | "applications"
              -> true
          | _ -> false
        )
    in
    let _is_test =
      xs +> List.exists (fun s ->
        match s with
        | "examples" | "tests" |  "test" -> true
        | _ -> false
      )
    in
    let is_generated_dupe = 
      List.mem "_build" xs ||
      b = "myocamlbuild"
    in

    (* pad specific *)
    let is_old = List.mem "old" xs in

    (* some files like in pfff/external/core/ do not have a .ml
     * so at least index the mli. otherwise skip the mli
     *)
    let is_mli_with_a_ml =
      e = "mli" && Sys.file_exists ml_file
    in
    is_test_in_external || (*is_test || *)
    is_mli_with_a_ml || 
    is_old || 
    is_generated_dupe ||
    false
  )

(*****************************************************************************)
(* Defs *)
(*****************************************************************************)

(* 
 * For now we just create the Dir, File, and Module entities.
 * 
 * todo: extract Function, Type, Constructor, Field, etc.
 *)
let extract_defs ~g ~ast ~readable ~file =
  let dir = Common.dirname readable in
  create_intermediate_directories_if_not_present g dir;

  (* dir -> module -> file (.ml and mli) *)
  let dir = (dir, E.Dir) in
  let m = (Module_ml.module_name_of_filename file, E.Module) in
  let file = (readable, E.File) in

  if G.has_node m g
  then
    (match G.parents m g with
    (* probably because processed .mli or .ml before which created the node *)
    | [] -> 
        raise Impossible
    | [p] when p =*= dir -> 
        ()
    | _ ->
        (match fst m with
        (* we attach to two parents when we are almost sure that
         * nobody will reference this module (e.g. because it's an 
         * entry point)
         *)
        | s when (s =~ "Main.*") || s =~ "Demo.*" || s =~ "Test.*" ->
            g +> G.add_edge (dir, m) G.Has
        | _ ->
            pr2 (spf "PB: module %s is already present (%s and %s)"
                    (fst m) (fst dir) (fst (G.parent m g)))
        )
    )
  else begin
    g +> G.add_node m;
    g +> G.add_edge (dir, m) G.Has;
  end;
  g +> G.add_node file;
  g +> G.add_edge (m, file) G.Has;
  ()

(*****************************************************************************)
(* Uses *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) dir =
  let root = Common.realpath dir in
  let files = Lib_parsing_ml.find_ml_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = filter_ml_files files in

  let g = G.create () in
  g +> G.add_node G.root;

  (* step1: creating the nodes and 'Has' edges, the defs *)
  files +> Common_extra.progress ~show:verbose (fun k -> List.iter (fun file ->
    k();
    let readable = Common.filename_without_leading_path root file in
    let ast = parse file in
    extract_defs ~g ~ast ~readable ~file;
  ));

  (* step2: creating the 'Use' edges, the uses *)
  files +> Common_extra.progress ~show:verbose (fun k -> List.iter (fun file ->
    k();
  ));

  g
