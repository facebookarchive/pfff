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
open Common
(* floats are the norm in graphics *)
open Common.ArithFloatInfix

open Figures
module CairoH = Cairo_helpers3

open Model3
module Ctl = Controller3

module DM = Dependencies_matrix_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Coordinate system *)
(*****************************************************************************)

(* On my 30' monitor, when I run codegraph and expand it to take the
 * whole screen, then the Grab utility tells me that the drawing area
 * is 2560 x 1490 (on my laptop it's 1220 x 660).
 * So if we want a uniform coordinate system that is
 * still aware of the proportion (like I did in Treemap.xy_ratio),
 * then 1.71 x 1 is a good choice.
 *)
let xy_ratio = 1.71

let scale_coordinate_system cr w =
  Cairo.scale cr
    (float_of_int w.width / xy_ratio)
    (float_of_int w.height);
  ()

(*****************************************************************************)
(* Layout *)
(*****************************************************************************)

(* this assumes a xy_ratio of 1.71 *)
let x_start_matrix_left = 0.2
let y_start_matrix_up = 0.15
let x_end_matrix_right = 1.55
  
(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

(* assumes cr is setup with uniform coordinate system *)
let draw_matrix cr w =
  (* clear the screen *)
  CairoH.fill_rectangle ~cr ~x:0.0 ~y:0.0 ~w:xy_ratio ~h:1.0 
    ~color:"DarkSlateGray" ();

  (* draw matrix enclosing rectangle *)
  CairoH.draw_rectangle ~cr ~line_width:0.001 ~color:"wheat"
    { p = { x = x_start_matrix_left; y = y_start_matrix_up };
      q = { x = x_end_matrix_right; y = 1.0 };
    };
  Cairo.select_font_face cr "serif"
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_BOLD;

  (* draw cells *)
  let nb_elts = Array.length w.m.DM.matrix in
  let width_cell = 
    (x_end_matrix_right - x_start_matrix_left) / (float_of_int nb_elts) in
  let height_cell = 
    (1.0 - y_start_matrix_up) / (float_of_int nb_elts) in

  for i = 0 to nb_elts -.. 1 do
    for j = 0 to nb_elts -.. 1 do
      (* the matrix is accessed as matrix.(row).(col), but here y corresponds
       * to the row, and x to the column, hence the association of j to x
       * and i to y.
       *)
      let x = (float_of_int j) * width_cell + x_start_matrix_left in
      let y = (float_of_int i) * height_cell + y_start_matrix_up in

      (* less: could also display intra dependencies *)
      if i = j then
        CairoH.fill_rectangle ~cr ~x ~y ~w:width_cell ~h:height_cell
          ~color:"wheat" ()
      else begin

        CairoH.draw_rectangle ~cr ~line_width:0.0005 ~color:"wheat"
          { p = { x = x; y = y; };
            q = { x = x + width_cell; y = y + height_cell };
          };
        let n = w.m.DM.matrix.(i).(j) in
        if n > 0 then begin
          let txt = string_of_int n in
          let font_size = 
            match n with
            | _ when n <= 10 -> 
                width_cell / 2.
            | _ ->
                width_cell / (float_of_int (String.length txt))
          in
          CairoH.set_font_size cr font_size;
          (* todo: optimize? *)
          let extent = CairoH.text_extents cr txt in
          let tw = extent.Cairo.text_width in
          let th = extent.Cairo.text_height in
          
          let x = x + (width_cell / 2.) - (tw / 2.0) in
          let y = y + (height_cell / 2.) + (th / 2.0) in
          Cairo.move_to cr x y;
          CairoH.show_text cr txt;
        end;
      end
    done
  done;

  (* draw left rows *)
  let font_size = x_start_matrix_left / 20. in
  CairoH.set_font_size cr font_size;
  (* peh because it exercises the spectrum of high letters *)
  let extent = CairoH.text_extents cr "peh" in
  let _base_tw = extent.Cairo.text_width / 3. in
  let th = extent.Cairo.text_height in

  for j = 0 to nb_elts -.. 1 do
    let x = 0. in
    let y = (float_of_int j) * height_cell + y_start_matrix_up in
    CairoH.draw_rectangle ~cr ~line_width:0.0005 ~color:"wheat"
        { p = { x = x; y = y; };
          q = { x = x_start_matrix_left; y = y + height_cell };
        };
    (* align on the left *)
    Cairo.move_to cr (x + 0.02) (y + (height_cell /2.) + (th / 2.0));
    let node = Hashtbl.find w.m.DM.i_to_name j in
    let (txt, _kind) = node in
    CairoH.show_text cr txt;
  done;
    
  (* draw up columns *)
  for i = 0 to nb_elts (* not -.. 1, cos we draw lines here, not rectangles *)do
    let x = (float_of_int i) * width_cell + x_start_matrix_left in
    let y = y_start_matrix_up in

    CairoH.set_source_color ~cr ~color:"wheat" ();
    Cairo.move_to cr x y;
    (* because of the xy_ratio, this actually does not do a 45 deg line.
     * old: Cairo.line_to cr (x + (y_start_matrix_up / atan (pi / 4.)))  0.; 
     *)
    Cairo.line_to cr (x + (y_start_matrix_up / atan (pi / 2.8)))  0.; 
    Cairo.stroke cr;

    if i < nb_elts then begin
      let node = Hashtbl.find w.m.DM.i_to_name i in
      let (txt, _kind) = node in
      
      Cairo.move_to cr (x + (width_cell / 2.0) + (th / 2.0)) (y - 0.001);
      let angle = -. (pi / 4.) in
      Cairo.rotate cr ~angle:angle;
      CairoH.show_text cr txt;
      Cairo.rotate cr ~angle:(-. angle);
    end;
  done;
  
  ()

(*****************************************************************************)
(* Painting entry point *)
(*****************************************************************************)

(* 'Paint' creates the cairo context and adjust the scaling if needed
 * and then call 'draw' functions.
 *)
let paint w =
  let cr = Cairo.create w.base in
  scale_coordinate_system cr w;
  draw_matrix cr w;
  !Ctl._refresh_drawing_area ();
  ()

(*****************************************************************************)
(* Events *)
(*****************************************************************************)

let button_action da w ev =
  pr2 "View_matrix.Button_action";
  true
