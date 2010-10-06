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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
type info = Parse_info.info
and tok = info

(* ------------------------------------------------------------------------- *)
(* Names  *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Type expressions *)
(* ------------------------------------------------------------------------- *)
(* core language, module language, class language *)


(* ------------------------------------------------------------------------- *)
(* Value expressions *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* Toplevel phrases *)
(* ------------------------------------------------------------------------- *)

and toplevel =
  | Ok of info

  | NotParsedCorrectly of info list
  | FinalDef of info (* EOF *)

 and program = toplevel list
 (* with tarzan *)


(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

let str_of_info x = Parse_info.str_of_info x
let col_of_info x = Parse_info.col_of_info x
let line_of_info x = Parse_info.line_of_info x
let pos_of_info x = Parse_info.pos_of_info x
