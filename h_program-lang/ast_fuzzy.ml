(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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
(* Types *)
(*****************************************************************************)

type tok = Parse_info.info

type tree =
  | Braces of tok * tree list * tok
  (* todo: comma *)
  | Parens of tok * tree list * tok
  | Angle  of tok * tree list * tok
  | Tok of tok
 (* with tarzan *)

(*****************************************************************************)
(* Vof *)
(*****************************************************************************)

let vof_token t =
  Ocaml.VString (Parse_info.str_of_info t)
  (* Parse_info.vof_token t*)

let rec vof_multi_grouped =
  function
  | Braces ((v1, v2, v3)) ->
      let v1 = vof_token v1
      and v2 = Ocaml.vof_list vof_multi_grouped v2
      and v3 = vof_token v3
      in Ocaml.VSum (("Braces", [ v1; v2; v3 ]))
  | Parens ((v1, v2, v3)) ->
      let v1 = vof_token v1
      and v2 = Ocaml.vof_list vof_multi_grouped v2
      and v3 = vof_token v3
      in Ocaml.VSum (("Parens", [ v1; v2; v3 ]))
  | Angle ((v1, v2, v3)) ->
      let v1 = vof_token v1
      and v2 = Ocaml.vof_list vof_multi_grouped v2
      and v3 = vof_token v3
      in Ocaml.VSum (("Angle", [ v1; v2; v3 ]))
  | Tok v1 -> let v1 = vof_token v1 in Ocaml.VSum (("Tok", [ v1 ]))

let vof_trees xs =
  Ocaml.VList (xs +> List.map vof_multi_grouped)

