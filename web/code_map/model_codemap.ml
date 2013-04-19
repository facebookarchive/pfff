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
open Common2.ArithFloatInfix

module F = Figures
module T = Treemap

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* TODO: factorize with pfff/code_map/model2.ml ? *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type world_client = {
  project: string;
  (* readable path *)
  path: string;
  size: string;

  rects: Treemap.treemap_rendering;

  (* to show labels without leading path *)
  root: Common.dirname;

  (* viewport, device coordinates  *)
  width:  int;
  height: int;
}

type fileinfo_client = {
  nblines: float; (* more convenient than int *)

  style: file_rendering_style;
}

  and file_rendering_style =
    | Regular of string list (* lines *)
    | Fancy of (lines * Highlight_code.category option * Common2.filepos) list
    | Nothing
  and lines = 
   (string, unit) Common2.either list

(*****************************************************************************)
(* Coordinate system *)
(*****************************************************************************)

(* in treemap.ml: xy_ratio = 1.71 *)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

type context = Dom_html.canvasRenderingContext2D Js.t

(*****************************************************************************)
(* Point -> treemap info *)
(*****************************************************************************)

(* alt: could use Cairo_bigarray and the pixel trick if
 * it takes too long to detect which rectangle is under the cursor.
 * coud also sort the rectangles ... or have some kind of BSP.
 *)
let find_rectangle_at_user_point w user =
  let rects = w.rects in

  if List.length rects = 1
  then 
    (* we are fully zommed, this treemap will have tr_depth = 1 but we return
     * it *)
    let x = List.hd rects in
    Some (x, [], x)
  else 
   let matching_rects = rects 
    +> List.filter (fun r -> 
      F.point_is_in_rectangle user r.T.tr_rect
      && r.T.tr_depth > 1
    ) 
    +> List.map (fun r -> r, r.T.tr_depth) 
    +> Common.sort_by_val_highfirst 
    +> List.map fst
   in
   match matching_rects with
   | [] -> None
   | [x] -> Some (x, [], x)
   | _ -> Some (Common2.head_middle_tail matching_rects)
