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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse2 file = 
  let ch = open_in file in
  let io = IO.input_channel ch in
  let c = JParse.parse_class_low_level io in
  IO.close_in io;
  c

let parse a = 
  Common.profile_code "Parse_bytecode.parse" (fun () -> parse2 a)
