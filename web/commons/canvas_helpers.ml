(* Yoann Padioleau
 * 
 * Copyright (C) 2011, 2013 Facebook
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
open Common_client

open Figures
module F = Figures
module Color = Simple_color

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * See Dom_html.mli and also http://www.w3schools.com/tags/ref_canvas.asp
 *)

(*****************************************************************************)
(* Color *)
(*****************************************************************************)
type css_color = string

let css_color_of_rgbf (r,g,b) alpha =
  let f_to_i f = int_of_float (100. * f) in
  let (r, g, b) = f_to_i r, f_to_i g, f_to_i b in
  spf "rgba(%d%%, %d%%, %d%%, %f)" r g b alpha

let css_color_of_color ?(alpha=1.) ~color () = 
  let (r,g,b) = color +> Color.rgbf_of_string in
  css_color_of_rgbf (r,g,b) alpha


(*****************************************************************************)
(* Oo interface *)
(*****************************************************************************)


(* Build a graphical context where the original dimension of the canvas
 * (width x height) are then scaled to a (xy_ratio x 1) drawing world.
 *)
class context ~ctx ~width ~height ~xy_ratio =

  (* ugly hack because html5 canvas does not handle using float size for
   * fonts when printing text in a scaled context.
   * less: find better font?
   *)
  let _ = ctx##font <- Js.string (spf "bold 12 px serif" ) in
  let text = "MM" in
  let metric = ctx##measureText (Js.string text) in
  let width_text_etalon_orig_coord = metric##width / 2.0 in
  (*pr2 (spf "width text orig coord = %f" width_text_etalon_orig_coord);*)

  let orig_coord_width = float_of_int width in
  let orig_coord_height = float_of_int height in
  let normalized_coord_width = xy_ratio in
  let _normalized_coord_height = 1.0 in

  let width_text_etalon_normalized_coord = 
    (normalized_coord_width * width_text_etalon_orig_coord) /
      orig_coord_width
  in
  (* pr2 (spf "width text normalized coord = %f" 
         width_text_etalon_normalized_coord); *)


object (self)

  method orig_coord_height = orig_coord_height
  method orig_coord_width = orig_coord_width

  method canvas_ctx = (ctx : Dom_html.canvasRenderingContext2D Js.t)

  initializer
    begin
      ctx##setTransform (1.,0.,0.,1.,0.,0.);
      ctx##scale (
        (float_of_int width / xy_ratio),
        (float_of_int height));
    end

(*****************************************************************************)
(* state *)
(*****************************************************************************)
method fillStyle ?alpha color =
  ctx##fillStyle <- Js.string (css_color_of_color ?alpha ~color ())

method strokeStyle ?alpha color =
  ctx##strokeStyle <- Js.string (css_color_of_color ?alpha ~color ())

(*****************************************************************************)
(* Figures *)
(*****************************************************************************)

(*
method draw_line (color, size, (x1, y1), (x2, y2)) =
  ctx##strokeStyle <- (Js.string color);
  ctx##lineWidth <- size;
  ctx##beginPath();
  ctx##moveTo(x1, y1);
  ctx##lineTo(x2, y2);
  ctx##stroke()
*)

method draw_rectangle ?alpha ~color ~line_width r =

  ctx##strokeStyle <- Js.string (css_color_of_color ?alpha ~color ());
  ctx##lineWidth <- line_width;

  ctx##beginPath();
  ctx##moveTo (r.p.x, r.p.y);
  ctx##lineTo (r.q.x, r.p.y);
  ctx##lineTo (r.q.x, r.q.y);
  ctx##lineTo (r.p.x, r.q.y);
  ctx##lineTo (r.p.x, r.p.y);
  ctx##closePath();

  ctx##stroke();
  ()

method fill_rectangle_xywh ?alpha ~x ~y ~w ~h ~color () = 

  ctx##fillStyle <- Js.string (css_color_of_color ?alpha ~color ());
  
  ctx##beginPath();
  ctx##moveTo(x, y);
  ctx##lineTo(x+w, y);
  ctx##lineTo(x+w, y+h);
  ctx##lineTo(x, y+h);
  ctx##closePath();

  ctx##fill();
  ()

method fill_rectangle ?alpha ~color r = 
  ctx##fillStyle <- Js.string (css_color_of_color ?alpha ~color ());
  (* pr2 (Figures.s_of_rectangle r); *)
  (* need the begin/close thing in canvas *)
  ctx##beginPath();
  ctx##moveTo (r.p.x, r.p.y);
  ctx##lineTo (r.q.x, r.p.y);
  ctx##lineTo (r.q.x, r.q.y);
  ctx##lineTo (r.p.x, r.q.y);
  ctx##lineTo (r.p.x, r.p.y);
  ctx##closePath();

  ctx##fill();
  ()

(*****************************************************************************)
(* Distance conversion *)
(*****************************************************************************)
method device_to_user ~x ~y =
  ((float_of_int x) / orig_coord_width) * xy_ratio,
  (float_of_int y / orig_coord_height)

method device_to_user_size size =
  (float_of_int size / orig_coord_height)

(*****************************************************************************)
(* Text *)
(*****************************************************************************)

method fill_text_scaled_return_width ?(rotate=0.) ~x ~y ~size str =
  ctx##save ();
  ctx##setTransform (1.,0.,0.,1.,0.,0.);

  (* y should be between 0 and 1 *)
  let y' = y * orig_coord_height in
  (* x should be between 0 and 1.71 *)
  let x' = (x / xy_ratio) * orig_coord_width in
  
  ctx##translate (x', y');
  ctx##rotate (rotate);

  (* ugly *)
  let size = size * 0.9 in

  let scale_factor = size / width_text_etalon_normalized_coord in

  ctx##scale (scale_factor, scale_factor);
  ctx##fillText (Js.string str, 0., 0.);
  let metric = ctx##measureText (Js.string str) in
  ctx##restore ();
  metric##width * scale_factor * xy_ratio / orig_coord_width

method fill_text_scaled ?rotate ~x ~y ~size str =
  ignore(self#fill_text_scaled_return_width ?rotate ~x ~y ~size str)


(* todo: can probably compute the extend without scaling and so on,
 * just by playing with w.xxx info.
 * note: you can also use ctx##textAlign <- Js.string "center"
 * which alleviate the need for this method sometimes.
 *)
method text_extents_scaled str ~size =
(*
  ctx##save ();
  ctx##setTransform (1.,0.,0.,1.,0.,0.);

  let scale_factor = size / w.width_text_etalon_normalized_coord in

  ctx##scale (scale_factor, scale_factor);
  (* does not work, it returns value on unscaled world *)
  let metric = ctx##measureText (Js.string str) in
  let width = metric##width in
  let metric = ctx##measureText (Js.string "X") in
  let height = metric##width in
  ctx##restore ();
  width, height
*)

  (* ugly *)
  let size = size * 0.9 in
  
  (* rough approximation *)

  let width = size * ((float_of_int (String.length str)) * 0.8) in
  let height = size * 0.9 in
  width, height


method user_to_device_font_size size =
  (* CairoH.user_to_device_font_size cr font_size  *)
  size * orig_coord_width (* TODO *)



end

