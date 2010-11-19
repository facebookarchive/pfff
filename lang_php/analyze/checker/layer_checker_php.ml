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


(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * Structure similar to other layer generator.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* could generate constructor for each *)
let properties = [
]

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let info_of_error_and_kind err =
  raise Todo

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let gen_layer ~root ~output errors = 

(*
  (*  a layer needs readable paths, hence the root *)
  let root = Common.realpath dir in

  let files = Lib_parsing_php.find_php_files_of_dir_or_files [root] in

  files +> List.iter (fun file ->
    let find_entity = None in
    Check_all_php.check_file ~find_entity file
  );

  let errors = !Error_php._errors in
*)

  let infos = errors +> List.map info_of_error_and_kind in

  let layer = Layer_code.simple_layer_of_parse_infos ~root properties infos in
  pr2 ("generating layer in " ^ output);
  Layer_code.save_layer layer output;
  ()


