(*s: json_ast_php.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009-2010 Facebook
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
(*e: Facebook copyright *)

open Common 

module J = Json_type 

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * It can be useful for people who don't like OCaml to still benefit 
 * from pfff parsing by having at least a JSON representation
 * of the Ast, hence this file. Other parts of pfff generates JSON
 * data (see flib_navigator/, fb_phpunit/, h_visualization/).
 *
 *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let string_of_expr x = 
  x |> Meta_ast_php.vof_expr |> Ocaml.json_of_v |> Json_out.string_of_json
let string_of_toplevel x = 
  x |> Meta_ast_php.vof_toplevel |> Ocaml.json_of_v |> Json_out.string_of_json
let string_of_program x = 
  Common.profile_code "json_of_program" (fun () ->
    x |> Meta_ast_php.vof_program |> Ocaml.json_of_v |> Json_out.string_of_json
  )

let string_of_program_fast x = 
  Common.profile_code "json_of_program_fast" (fun () ->
    let json = x |> Meta_ast_php.vof_program |> Ocaml.json_of_v 
    in
    Common.profile_code "string_of_json" (fun () ->
      Json_io.string_of_json ~compact:true ~recursive:false json
    )
  )


(*e: json_ast_php.ml *)
