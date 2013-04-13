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
(* floats are the norm in graphics *)
open Common2.ArithFloatInfix

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* TODO: factorize with pfff/code_map/model2.ml ? *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type world_client = {
  rects: Treemap.treemap_rendering;

  (* viewport, device coordinates 
   * todo: factorize with code_map/ and put in the graphics_context class type?
   *)
  width:  int;
  height: int;

  orig_coord_width: float;
  orig_coord_height: float;
  width_text_etalon_normalized_coord: float;
}

(*****************************************************************************)
(* Coordinate system *)
(*****************************************************************************)

(* in treemap.ml: xy_ratio = 1.71 *)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

type context = Dom_html.canvasRenderingContext2D Js.t
