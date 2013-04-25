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
  | Braces of tok * trees * tok
  (* todo: comma *)
  | Parens of tok * trees * tok
  | Angle  of tok * trees * tok
  | Tok of tok
and trees = tree list
 (* with tarzan *)

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)

type visitor_out = trees -> unit

type visitor_in = {
  ktree: (tree -> unit) * visitor_out -> tree -> unit;
  ktok: (tok -> unit) * visitor_out -> tok -> unit;
}

let (default_visitor : visitor_in) = 
  { ktree = (fun (k, _) x -> k x);
    ktok  = (fun (k, _) x -> k x);
  }

let (mk_visitor: visitor_in -> visitor_out) = fun vin ->

  let rec v_tree x =
    let rec k x = match x with
      | Braces ((v1, v2, v3)) ->
        let _v1 = v_tok v1 and _v2 = v_trees v2 and _v3 = v_tok v3 in ()
      | Parens ((v1, v2, v3)) ->
        let _v1 = v_tok v1 and _v2 = v_trees v2 and _v3 = v_tok v3 in ()
      | Angle ((v1, v2, v3)) ->
        let _v1 = v_tok v1 and _v2 = v_trees v2 and _v3 = v_tok v3 in ()
      | Tok v1 -> let _v1 = v_tok v1 in ()
    in
    vin.ktree (k, all_functions) x
 and v_trees v = v_list v_tree v
 and v_list f x = List.iter f x
 and v_tok x =
    let rec k x = () in
    vin.ktok (k, all_functions) x

  and all_functions x = v_trees x in
  all_functions

(*****************************************************************************)
(* Extractor *)
(*****************************************************************************)

let (ii_of_trees: trees -> Parse_info.info list) = fun trees ->
  let globals = ref [] in
  let hooks = { default_visitor with
    ktok = (fun (k, _) i -> Common.push2 i globals)
  } in
  begin
    let vout = mk_visitor hooks in
    vout trees;
    List.rev !globals
  end

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

