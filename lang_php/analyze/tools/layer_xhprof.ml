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

module X = Xhprof

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
  "live", "green";
  "dead", "red";
  "unknown", "white";
]


(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let infos_and_kinds_of_xhprof_entities xhprof_entities db =
  xhprof_entities +> Common.map (fun e ->
    try (
    match e with
    | X.Function funcname ->
        (* ugly: should perhaps do that in the parse function *)
        let funcname = X.remove_at_suffix funcname in

        let ids = Db.function_ids__of_string funcname db in
        ids +> List.map (fun id ->
          Db.parse_info_of_id id db, "live"
        )
    | X.Method (classname, methodname) ->
        let methodname = X.remove_at_suffix methodname in

        let ids =
          [Db.id_of_method ~theclass:classname methodname db]
        in
        ids +> List.map (fun id ->
          Db.parse_info_of_id id db, "live"
        )

    | X.RunInit file -> 
        (* todo? *)
        []
    | X.Main | X.PruneChild ->  
        []
    | X.Misc s -> 
        pr2 ("not handled: " ^ s);
        []
    )
    with exn ->
      (match exn with
      | Not_found | Multi_found ->
          pr2 (spf "PB with %s, exn = %s"
                (X.string_of_xhprof_entity e)
                (Common.string_of_exn exn));
          []
      | _ -> raise exn
      )
  ) +> List.flatten


(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let gen_layer ~xhprof_file ~db ~output = 

  let root = Db.path_of_project_in_database db in

  let xhprof_entities =
    Common.cat xhprof_file +> List.map (Xhprof.parse_xhprof_entity_string)
  in
  let infos =
    infos_and_kinds_of_xhprof_entities xhprof_entities db
  in

  let layer = Layer_code.simple_layer_of_parse_infos 
    ~title:"Live code"
    ~description:"Use xhprof to determine live code"
    ~root infos properties in
  pr2 ("generating layer in " ^ output);
  Layer_code.save_layer layer output;
  ()


(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

let actions () = [
  "-gen_xhprof_layer", " <file> <db> <output>",
  Common.mk_action_3_arg (fun file dbpath output ->
    Database_php.with_db ~metapath:dbpath (fun db ->
      gen_layer ~xhprof_file:file ~db ~output
    )
  )
]
