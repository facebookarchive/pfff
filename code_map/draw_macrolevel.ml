(*s: draw_macrolevel.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2010-2012 Facebook
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
open Common2.ArithFloatInfix

open Figures (* for the fields *)

module T = Treemap
module F = Figures
module Color = Simple_color

module Db = Database_code
module HC = Highlight_code

module CairoH = Cairo_helpers
module  Parsing = Parsing2

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Drawing a treemap rectangle *)
(*****************************************************************************)

(*s: draw_treemap_rectangle() *)
let draw_treemap_rectangle2 ~cr ?(color=None) ?(alpha=1.) rect =
  let r = rect.T.tr_rect in

  (let (r,g,b) = 
    let (r,g,b) = rect.T.tr_color +> Color.rgb_of_color +> Color.rgbf_of_rgb in
    match color with
    | None -> (r,g,b)
    | Some c -> 
        let (r2,g2,b2) = c +> Color.rgbf_of_string in
        (r2 + r / 20., g2 + g / 20., b2 + b / 20.)
  in
  Cairo.set_source_rgba cr r g b (alpha);
  );

  Cairo.move_to cr r.p.x r.p.y;
  Cairo.line_to cr r.q.x r.p.y;
  Cairo.line_to cr r.q.x r.q.y;
  Cairo.line_to cr r.p.x r.q.y;

  Cairo.fill cr;
  ()

let draw_treemap_rectangle ~cr ?color ?alpha a =
  Common.profile_code "View.draw_treemap_rectangle" (fun () -> 
    draw_treemap_rectangle2 ~cr ?color ?alpha a)
(*e: draw_treemap_rectangle() *)


(*****************************************************************************)
(* Layers macrolevel *)
(*****************************************************************************)

(* How should we draw layer information at the macro level ?
 *   
 *  - fill the rectangle with the color of one layer ? 
 *  - separate equally among layers ? 
 *  - draw on top of the existing archi color ?
 *  - draw circles instead of rectangle so have quantitative information too 
 *    (like I was doing when display git related commit information).
 * 
 * It is maybe good to not draw on top of the existing archi_code color. 
 * Too many colors kill colors. Also we can not convey quantitative 
 * information by coloring with full rectangles (instead of the random
 * circles trick) but for some layers like security it is probably better.
 * Don't care so much about how many bad calls; care really about
 * number of files with bad calls in them.
 * 
 * So for now we just fill rectangles with colors from the layer and
 * when a file matches multiple layers we split the rectangle in equal
 * parts.
 *)

let draw_trect_using_layers ~cr layers_with_index rect =
  let _r = rect.T.tr_rect in

  (* don't use archi_code color. Just black and white *)
  let is_file = not rect.T.tr_is_node in
  let color = if is_file then "white" else "black" in
  draw_treemap_rectangle ~cr ~color:(Some color) rect;

  if is_file then begin
    let file = rect.T.tr_label in
    
    let color_info = 
      try Hashtbl.find layers_with_index.Layer_code.macro_index file
      with Not_found -> []
    in
    (* What to draw ? TODO a splitted rectangle ? 
     *)
    let sorted = Common2.sort_by_key_highfirst color_info in
    (match sorted with
    | [] -> ()
    | (_float, color)::_rest ->
      draw_treemap_rectangle ~cr ~color:(Some color) rect;
    );
  end

(*****************************************************************************)
(* File Summary *)
(*****************************************************************************)

(*s: draw_summary_content *)
(* todo: should base this on the current max in the current view.
 * Also bad that can not compare function use by just looking
 * at their size :(
*)
(*
let threshold_print_summary_entity_users = 10

let draw_summary_content2 ~cr ~w_per_column ~context ~nblines ~file rect =
  ignore(nblines);
  ignore(file);

  let r = rect.T.tr_rect in
  let file = rect.T.tr_label in

  let model = Async.async_get context.model in

  let files_entities = model.Model2.hfiles_entities in
  let entities = 
    try Hashtbl.find files_entities file
    with Not_found -> []
  in
  let entities = 
    entities 
    +> Common.take_safe 5
    +> List.filter (fun e ->
      e.Db.e_number_external_users > threshold_print_summary_entity_users
    )
  in

  let _w = F.rect_width r in
  let h = F.rect_height r in

  (* todo: bad to use w cos its give advantage to huge file.
   * w_per_column on the opposite is rougly the same on a screen.
   *)
  let font_size = w_per_column / 2. in

  let font_size_real = CairoH.user_to_device_font_size cr font_size in
  
  let space_per_line_summary = 
    h / 6.
  in
  entities +> Common.index_list_1 +> List.iter (fun (e, i) ->

    let nb_use = e.Db.e_number_external_users in
    (* todo: move this func elsewhere, in database_code ?  *)
    let use_arity = Parsing.use_arity_of_use_count nb_use in
    
    (* would like to reuse code used when drawing content but
     * I've copy paste to allow specifics anamorphic config
     *)
    let font_size_multiplier = 
      (* should reuse Style2.mulitplier_use ? *)
      match use_arity with
      | HC.HugeUse when nb_use > 1000 -> 4.
      | HC.HugeUse -> 3.
      | HC.LotsOfUse -> 2.
      | HC.MultiUse -> 1.1
      | HC.SomeUse | HC.UniqueUse -> 0.7
      | HC.NoUse -> 0.5
    in
    let size_font_multiplier_multiplier = 
      (*- 0.2 * font_size_real + 2. *)
      match font_size_real with
      | n when n < 3. -> 2.
      | n when n < 8. -> 1.5
      | n when n < 10. -> 1.
      | _ -> 0.5
    in

    let final_font_size = 
      Draw_common.final_font_size_when_multiplier 
        ~multiplier:font_size_multiplier
        ~size_font_multiplier_multiplier
        ~font_size
        ~font_size_real
    in

  (* TODO use the appropriate color for entity *)
    (let (r,g,b) = 
      0.2, 0.0, 0.0
    in
    let alpha = 0.5 in
    Cairo.set_source_rgba cr r g b alpha;
    );
    Cairo.set_font_size cr final_font_size;
    
    let x = r.p.x in
    let y = r.p.y + (space_per_line_summary * (float_of_int i)) in

    let str = e.Db.e_name in
    Cairo.move_to cr x y;
    CairoH.show_text cr str;
    let str = spf "(%d)" e.Db.e_number_external_users in
    CairoH.show_text cr str;
  );
  ()

let draw_summary_content 
 ~cr ~w_per_column ~context ~nblines ~file rect =
  Common.profile_code "View.draw_summary_content" (fun () ->
    draw_summary_content2 ~cr ~w_per_column ~context ~nblines ~file rect)
*)
(*e: draw_summary_content *)
(*e: draw_macrolevel.ml *)
