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
  "dead function", "red";
  "unknown", "white";
]


(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let infos_and_kinds_of_dead_ids dead_ids db =
  dead_ids +> List.map (fun (_s, id) ->
    Db.parse_info_of_id id db, "dead function"
  )


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let gen_layer ~db ~output = 

  let root = Db.path_of_project_in_database db in

  let hooks = { Deadcode_php.default_hooks with
    (* Deadcode_php.is_probable_dynamic_funcname = 
      False_positives_deadcode.is_probable_dynamic_funcname;
    is_valid_author = Fb_employees.is_still_fb_employee;
    is_valid_file = (fun file -> 
      not (False_positives_deadcode.exclude_from_deadcode_patch_generation file)
    );
    *)
    Deadcode_php.print_diff = false;
    Deadcode_php.with_blame = false;
  }
  in
  let dead_ids = 
    Deadcode_php.finding_dead_functions hooks db
  in
  let infos =
    infos_and_kinds_of_dead_ids dead_ids db
  in

  let layer = Layer_code.simple_layer_of_parse_infos 
    ~title:"Dead code"
    ~description:"Mostly functions without any callers (static analysis)"
    ~root infos properties in
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
