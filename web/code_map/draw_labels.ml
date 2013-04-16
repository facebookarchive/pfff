(* Yoann Padioleau
 * 
 * Copyright (C) 2010-2013 Facebook
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
open Common_client

open Figures (* for the fields *)
open Model_codemap (* for the fields *)

module Flag = Flag_visual

module T = Treemap
module F = Figures
module Color = Simple_color

(*****************************************************************************)
(* Label *)
(*****************************************************************************)

let _hmemo_text_extent = Hashtbl.create 101

(* This can be quite cpu intensive. CairoH.text_extents is quite slow
 * so you must avoid calling it too much. A simple optimisation
 * when the treemap is big is to avoid trying to draw labels
 * that are too tiny already.
 * 
 * note that this function is also called when we mouse over a rectangle
 * in which case we redraw the label in a different color
 * 
 * design: good to have a color different for dir and files.
 * 
 * design: could decide to give different colors to dirs depending on its
 * depth, like red for toplevel dir, green, and so on, like I do for 
 * my code sections, but feel simpler to just have one.
 * The rectangles will already have different colors and in the end
 * the depth does not have that much meaning in projects. For instance
 * in java code there are lots of nested directories (org/apache/...),
 * in some projects there is always an intermediate src/ directory;
 * each software have different conventions.
 *)
let rec draw_treemap_rectangle_label_maybe ctx ?(color=None) rect =

  let lbl = rect.T.tr_label in
  (* we do some opti in server_codemap now which can lead to empty txt *)
  if lbl <> "" then begin

  let base = Filename.basename lbl in
  (* old: Common.is_directory_eff lbl *)
  let is_directory = rect.T.tr_is_node in

  let txt = 
    if is_directory
    then base ^ "/"
    else base
  in
  let color = 
    match color with
    | None -> 
        if is_directory 
        then "NavyBlue"
        else "black"
    | Some c -> c
  in

  let font_size, minus_alpha = 
    if not !Flag.boost_label_size
    then 
     (match rect.T.tr_depth with
    | 1 -> 0.1, 0.8
    | 2 -> 0.05, 0.2
    | 3 -> 0.03, 0.4
    | 4 -> 0.02, 0.5
    | 5 -> 0.02, 0.65
    | 6 -> 0.02, 0.7
    | _ -> 0.02, 0.8
     )
    else
    (match rect.T.tr_depth with
    | 1 -> 0.1, 0.8
    | 2 -> 0.06, 0.2
    | 3 -> 0.045, 0.3
    | 4 -> 0.041, 0.35
    | 5 -> 0.04, 0.4
    | 6 -> 0.03, 0.45
    | _ -> 0.02, 0.5
    )
  in
  let zoom = 1. in (* still used? *)
  let font_size = font_size / (zoom) (* use zoom factor inversely *) in
  let alpha = 1. - (minus_alpha / zoom) in

  try_draw_label 
    ctx
    ~font_size_orig:font_size
    ~color ~alpha 
    ~rect txt
  end

and try_draw_label 
    (ctx: Canvas_helpers.context) ~font_size_orig ~color ~alpha ~rect txt =

(* ugly: sometimes labels are too big. Should provide a way to
 * shorten them.
 * let txt = 
 * if true
 * then if txt =~ "org.eclipse.\\(.*\\)" then Common.matched1 txt else txt
 * else txt
 * in
 *)
(*
  let txt = 
  if true
  then if txt =~ "[^-]*-\\(.*\\)" then Common.matched1 txt else txt
  else txt
  in
*)
  let r = rect.T.tr_rect in
  
  let w = F.rect_width r in
  let h = F.rect_height r in

  let is_file = 
    (* old: try Common.is_file_eff rect.T.tr_label with _ -> false *)
    not rect.T.tr_is_node
  in

  
  let rec aux ~font_size ~step =

  ctx#fillStyle ~alpha color;

    (* opti: this avoid lots of computation *)
    let font_size_real = ctx#user_to_device_font_size font_size in

    if font_size_real < !Flag.threshold_draw_label_font_size_real
    then ()
    else begin

     (* opti:
      *  was let extent = CairoH.text_extents cr txt
      *)
     let th, base_tw = 
       Common.memoized _hmemo_text_extent (font_size, font_size_real) (fun ()->
         (* peh because it exercises the spectrum of high letters *)
         
         let tw, th = ctx#text_extents_scaled "peh" ~size:font_size in
         th, tw
       )
     in
     let tw = float_of_int (String.length txt) * base_tw / 3. in
           
     
     (* will try first horizontally at a certain size, then
      * diagonally at a certain size, and if can't then reduce
      * the font_size (up to a certain limit) and try again
      * (first horizontally, then diagonally).
      *)
     match step with
     | 1 | 4 | 7 | 10 when tw < w && th < h && rect.T.tr_depth > 1 ->
           (* see http://cairographics.org/tutorial/#L3showtext 
            * for the logic behind the placement of the text
            *)
     
           let x = r.p.x + w / 2.0 - (tw / 2.0) in
           let y = r.p.y + h / 2.0 + (th / 2.0) in
       
           ctx#fill_text_scaled txt ~x ~y ~size:font_size;

           (* '<= 2' actually means "the toplevel entries" as 
            * the root is at depth=1
            *)
           if rect.T.tr_depth <= 2 then begin

             ctx#fillStyle ~alpha "red";
             ctx#fill_text_scaled (String.sub txt 0 1) ~x ~y ~size:font_size
           end
           
   
     | 2 | 5 | 8 | 11 when 
           tw < sqrt (w * w + h * h) && 
           th < min w h &&
             rect.T.tr_depth > 1 ->
           (* todo: try vertically ... *)
   
         (* you have to draw on a paper to understand this code below ... *)
   
         let tangent = h / w in
         let angle = atan tangent in
   
         (* right now we don't handle the fact that the text itself has
          * a height but below the x and y positions are just the
          * place of the very bottom of the first letter. In some way we trace a
          * line below the text in the diagonal of the rectangle. This means all
          * the text is on the top of the diagonal. It should be in the middle
          * of the diagonal. 
          * As a first fix we can artificially augment the angle ... ugly
          *)
         let angle = 
           min (angle + angle / 10.) (Math.pi / 2.) 
         in
   
         (* I love basic math *)
         let x = r.p.x + w / 2.0 - (cos angle * (tw / 2.0)) in
         let y = r.p.y + h / 2.0 - (sin angle * (tw / 2.0)) in
           
         ctx#fill_text_scaled txt ~x ~y ~size:font_size ~rotate:angle;
         
         if rect.T.tr_depth <= 2 then begin
           ctx#fillStyle ~alpha "red";
           ctx#fill_text_scaled (String.sub txt 0 1) ~x ~y ~size:font_size
            ~rotate:angle;
         end;
   
     | 3 ->
         (* I am ok to go down to 70% *)
         let font_size = font_size_orig * 0.7 in
         aux ~step:4 ~font_size
     | 6 ->
         (* I am ok to go down to 50% of original *)
         let font_size = font_size_orig * 0.5 in
         aux ~step:7 ~font_size
   
     | 9 ->
         (* I am ok to go down to 30% of original for file only  *)
         if is_file
         then 
           let font_size = font_size_orig * 0.25 in
           aux ~step:10 ~font_size
         else ()
   
     (* this case is taken only for the first cases (1, 2, ..) when the
      * associated 'when' expression is false
      *)
     | n -> 
         if n >= 12
         then ()
         else aux ~step:(Pervasives.(+) n 1) ~font_size
    end
  in
  aux ~font_size:font_size_orig ~step:1 
