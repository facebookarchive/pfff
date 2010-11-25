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

module Db = Database_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * Structure similar to other layer generator.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

let properties = [
  "dead", "red";
  "unknown", "white";
]


(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let infos_and_kinds_of_dead_ids dead_ids db =
  raise Todo

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let gen_layer ~db ~output = 

  let root = Db.path_of_project_in_database db in

  let dead_ids = raise Todo in
  let infos =
    infos_and_kinds_of_dead_ids dead_ids db
  in

  let layer = Layer_code.simple_layer_of_parse_infos ~root infos properties in
  pr2 ("generating layer in " ^ output);
  Layer_code.save_layer layer output;
  ()


(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

let actions () = [
  "-gen_deadcode_layer", " <db> <output>",
  Common.mk_action_2_arg (fun dbpath output ->
    Database_php.with_db ~metapath:dbpath (fun db ->
      gen_layer ~db ~output
    )
  )
]
