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

let properties = [
  "live", "green";
  "dead", "red";
  "unknown", "white";
]


(*****************************************************************************)
(* Code *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let gen_layer ~xhprof_file ~db ~output = 

  let root = raise Todo in
  let infos = raise Todo in

  let layer = Layer_code.simple_layer_of_parse_infos ~root infos properties in
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
