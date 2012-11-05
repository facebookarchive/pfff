(* Copyright (C) 2012 Yoann Padioleau
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

module PI = Parse_info

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_parse xs =
  raise Todo

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)


let actions () = [
  "-parse_bytecode", "   <file or dir>", 
  Common.mk_action_n_arg test_parse;

]
