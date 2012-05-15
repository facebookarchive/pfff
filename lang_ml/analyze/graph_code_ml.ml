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
 * todo?
 *  - ml vs mli? just get rid of mli? but could also do a pfff_dependencies
 *    that just care about mli like my 'make doti' ?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let parse file =
  Common.save_excursion Flag_parsing_ml.show_parsing_error false (fun ()->
    Parse_ml.parse_program file 
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
    let is_test =
      xs +> List.exists (fun s ->
        match s with
        | "examples" | "tests" |  "test" -> true
        | _ -> false
      )
    in

    (* pad specific *)
    let is_old = List.mem "old" xs in

    (* some files like in pfff/external/core/ do not have a .ml
     * so at least index the mli. otherwise skip the mli
     *)
    let is_mli_with_a_ml =
      e = "mli" && Sys.file_exists ml_file
    in
    is_test_in_external || is_mli_with_a_ml || is_old || is_test
  )



(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let build ?(verbose=true) dir =
  let root = Common.realpath dir in
  let files = Lib_parsing_ml.find_ml_files_of_dir_or_files [root] in

  (* step0: filter noisy modules/files *)
  let files = filter_ml_files files in

  let g = G.create () in

  (* step1: creating the nodes and 'Has' edges, the defs *)
  files +> Common_extra.progress ~show:verbose (fun k -> List.iter (fun file ->
    k();
    let _readable = Common.filename_without_leading_path root file in
    let _ast = parse file in
    raise Todo
  ));

  (* step2: creating the 'Use' edges, the uses *)
  files +> Common_extra.progress ~show:verbose (fun k -> List.iter (fun file ->
    k();
  ));

  g
