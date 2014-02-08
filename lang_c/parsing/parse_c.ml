(* Yoann Padioleau
 *
 * Copyright (C) 2012 Yoann Padioleau
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Just a small wrapper around the C++ parser
 *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let _done_init = ref false

let parse file =
  if not !_done_init
  then begin 
    Parse_cpp.init_defs !Flag_parsing_cpp.macros_h;
    _done_init := true
  end;
  Parse_cpp.parse ~lang:Flag_parsing_cpp.C file


let parse_program file =
  let (program2, _stat) = parse file in
  Parse_cpp.program_of_program2 program2
