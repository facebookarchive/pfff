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
module F = Figures
module Color = Simple_color

module T = Treemap

module CanvasH = Canvas_helpers
open Model_codemap (* for the fields *)

module Flag = Flag_visual
module Style = Style_codemap

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* note: some types below could be 'int' but it's more convenient to have
 * everything as a float because arithmetic with OCaml sucks when have
 * multiple numeric types
 *)
type draw_content_layout = {
  font_size: float;
  split_nb_columns: float;
  w_per_column:float;
  space_per_line: float;
  l_nblines: float;
}

(*****************************************************************************)
(* globals *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Anamorphic entities *)
(*****************************************************************************)

let final_font_size_when_multiplier 
    ~multiplier ~size_font_multiplier_multiplier 
    ~font_size ~font_size_real 
   = 
  let size_font_multiplier = multiplier in
  
  let font_size_adjusted = 
    if size_font_multiplier = 1.
    then font_size
    else 
      max 
       (font_size * size_font_multiplier * size_font_multiplier_multiplier)
       (font_size * 1.5)
  in
  
  let final_font_size = 
    Common2.borne ~min:font_size ~max:(font_size * 30.) font_size_adjusted
  in
  final_font_size

let final_font_size_of_categ ~font_size ~font_size_real categ = 

  let multiplier = Style.size_font_multiplier_of_categ ~font_size_real categ in
  (* as we zoom in, we don't want to be as big, and as
   * we zoom out we want to be bigger
   *)
  let size_font_multiplier_multiplier = 
     (*- 0.2 * font_size_real + 2. *)
    match font_size_real with
    | n when n < 3. -> 2.
    | n when n < 8. -> 1.5
    | n when n < 10. -> 1.
    | _ -> 0.5
  in

  final_font_size_when_multiplier 
    ~multiplier
    ~size_font_multiplier_multiplier
    ~font_size
    ~font_size_real

let rgba_and_font_size_of_categ 
  ~ctx ~font_size ~font_size_real (*~is_matching_line*)
 categ 
 =
  let is_matching_line = false in

  let attrs =
    match categ with
    | None -> Highlight_code.info_of_category Highlight_code.Normal
    | Some categ -> Highlight_code.info_of_category categ
  in
  
  let final_font_size = 
    final_font_size_of_categ ~font_size ~font_size_real categ in
  let final_font_size = 
    if is_matching_line
    then final_font_size * 1.
    else final_font_size
  in
  
  let _alpha_adjust =
    let ratio = final_font_size / font_size in
    match ratio with
    | _ when ratio > 4. -> 0.
    | _ when ratio > 2. -> 0.3
    | _ when ratio >= 1. -> 0.5
    | _ -> 0.3
  in
  
  attrs +> Common.find_some (fun attr ->
    match attr with
    | `FOREGROUND s 
    | `BACKGROUND s (* todo: should really draw the background of the text *)
      -> 
        let (r,g,b) = Color.rgbf_of_string s in
        let alpha = 1. in
        Some (((r, g, b), alpha), final_font_size);
    | _ -> None
  )

(*****************************************************************************)
(* Columns *)
(*****************************************************************************)

let font_size_when_have_x_columns ~nblines ~chars_per_column ~w ~h ~with_n_columns =
  let size_x = (w / with_n_columns) / chars_per_column in
  let size_y = (h / (nblines / with_n_columns)) in

  let min_font = min size_x size_y in
  min_font
   
(* Given a file with nblines and nbcolumns (usually 80) and
 * a rectangle of w width and h height, what is the optimal
 * number of columns. The principle is to start at 1 column
 * and see if by adding columns we can have a bigger font.
 * We try to maximize the font_size.
 *)
let optimal_nb_columns ~nblines ~chars_per_column ~w ~h = 
  
  let rec aux current_font_size current_nb_columns = 
    let min_font = font_size_when_have_x_columns 
      ~nblines ~chars_per_column ~w ~h ~with_n_columns:current_nb_columns
    in
    if min_font > current_font_size
    then aux min_font (current_nb_columns + 1.)
    else 
      (* regression, then go back on step *)
      current_nb_columns - 1.
  in
  aux 0.0   1.


let draw_column_bars ctx ~split_nb_columns ~font_size ~w_per_column r = 
  let ctx2 = ctx#canvas_ctx in
  for i = 1 to int_of_float (split_nb_columns - 1.) do
    let i = float_of_int i in

    ctx2##fillStyle <- Js.string (CanvasH.css_color_of_rgbf (0.0,0.0,1.0) 0.2);
      
    let font_size_real = ctx#user_to_device_font_size font_size in
    let width = 
      if font_size_real > 5.
      then  (font_size / 10.)
      else font_size
    in
    ctx2##lineWidth <- width;
    ctx2##moveTo (r.p.x + w_per_column * i, r.p.y);
    ctx2##lineTo (r.p.x + w_per_column * i, r.q.y);
    ctx2##stroke();
  done


(*****************************************************************************)
(* File Content *)
(*****************************************************************************)

let draw_content (ctx:Canvas_helpers.context) ~layout fileinfo rect =
  let r = rect.T.tr_rect in
  let font_size = layout.font_size in
  let font_size_real = ctx#user_to_device_font_size font_size in

  if font_size_real > Flag.threshold_draw_dark_background_font_size_real
  then begin

    (* erase what was done at the macrolevel *)
(*
    if Hashtbl.length context.layers_microlevel > 0 then begin
      Draw_macrolevel.draw_treemap_rectangle ~cr ~color:(Some "white") 
        ~alpha:1.0 rect;
    end;
*)

    let nb_rects_on_screen = 1 in (* TODO *)
    let alpha = 
      match (*context.*)nb_rects_on_screen with
      | n when n <= 1 -> 0.95
      | n when n <= 2 -> 0.8
      | n when n <= 10 -> 0.6
      | _ -> 0.3
    in
    Draw_macrolevel.draw_treemap_rectangle ctx ~color:(Some "DarkSlateGray") 
        ~alpha rect;
    (* draw a thin rectangle with aspect color *)
    (*
      CairoH.draw_rectangle_bis ~cr ~color:(rect.T.tr_color) 
      ~line_width:(font_size / 2.) rect.T.tr_rect;
    *)

    let nblines_per_column = 
      (layout.l_nblines / layout.split_nb_columns) +> ceil +> int_of_float in

    let line = ref 1 in

    (match fileinfo.style with
    | Fancy tokens_with_categ ->

        let column = ref 0 in
        let line_in_column = ref 1 in

        let x = ref 
          (r.p.x + (float_of_int !column) * layout.w_per_column) in
        let y = ref 
          (r.p.y + (layout.space_per_line * (float_of_int !line_in_column))) in
        
(*
    let model = Async.async_get context.model in
    let entities = model.Model2.hentities in
*)

        tokens_with_categ +> List.iter (fun (xs, categ, filepos) ->

          let (((red,g,b), alpha), final_font_size) =
            rgba_and_font_size_of_categ 
              ~ctx ~font_size ~font_size_real
              (*~is_matching_line:(Hashtbl.mem hmatching_lines !line)*)
              categ in
          ctx#canvas_ctx##fillStyle <- 
            Js.string (CanvasH.css_color_of_rgbf (red,g,b) alpha);

          xs +> List.iter (function
          | Common2.Left s -> 
              (*
                let pt = Cairo.get_current_point cr in
                Common.push2 (s, filepos, pt) text_with_user_pos;
              *)
              
              let width = 
                ctx#fill_text_scaled_return_width 
                  s ~x:!x ~y:!y ~size:final_font_size in
              x := !x + width;

          | Common2.Right () ->
              
              incr line_in_column;
              incr line;
              
              if !line_in_column > nblines_per_column
              then begin 
                incr column;
                line_in_column := 1;
              end;
              
              x := r.p.x + 
                (float_of_int !column) * layout.w_per_column;
              y := r.p.y + 
                (layout.space_per_line * (float_of_int !line_in_column));
              
          (* must be done before the move_to below ! *)
          (*
            (match Common2.hfind_option !line hmatching_lines with
            | None -> ()
            | Some color ->
            CairoH.fill_rectangle ~cr 
            ~alpha:0.25
            ~color
            ~x 
            ~y:(y - layout.space_per_line) 
                ~w:layout.w_per_column 
            ~h:(layout.space_per_line * 3.)
            ()
            );
          *)
          )
        )

    | Regular lines ->
      (* This was causing some "out_of_memory" cairo error on linux. Not
       * sure why.
       *)
        ctx#canvas_ctx##fillStyle <- 
          Js.string (CanvasH.css_color_of_rgbf (0.0,0.0,0.0) 0.9);

        let xs = lines in
        let xxs = Common2.pack_safe nblines_per_column xs in
        
        (* I start at 0 for the column because the x displacement
         * is null at the beginning, but at 1 for the line because
         * the y displacement must be more than 0 at the
         * beginning
         *)
        Common.index_list_0 xxs +> List.iter (fun (xs, column) ->
          Common.index_list_1 xs +> List.iter (fun (s, line_in_column) ->
            
            let x = r.p.x + 
              (float_of_int column) * layout.w_per_column in
            let y = r.p.y + 
              (layout.space_per_line * (float_of_int line_in_column)) in
            
            ctx#fill_text_scaled s ~x ~y ~size:font_size;
            
            incr line;
          );
        );
    | Nothing ->
        ()
    )
  end


let draw_treemap_rectangle_content_maybe ctx fileinfo rect =
  let r = rect.T.tr_rect in
  let w = F.rect_width r in
  let h = F.rect_height r in

  let nblines = fileinfo.nblines in

  let font_size_estimate = h / 100. in
  let font_size_real_estimate = 
    ctx#user_to_device_font_size font_size_estimate in

  if font_size_real_estimate > 0.4
  then begin

    (* assume our code follow certain conventions. Could infer from file. 
     * we should put 80, but a font is higher than large, so 
     * I manually readjust things. todo: should readjust something
     * else.
     *)
    let chars_per_column = 41.0 in
    
    let split_nb_columns = 
      optimal_nb_columns ~nblines ~chars_per_column ~h ~w in
    let font_size = 
      font_size_when_have_x_columns ~nblines ~chars_per_column ~h ~w 
        ~with_n_columns:split_nb_columns in
    let w_per_column = 
      w / split_nb_columns in
    let space_per_line = 
      font_size in
    
    draw_column_bars ctx ~split_nb_columns ~font_size ~w_per_column r;

    let font_size_real = ctx#user_to_device_font_size font_size in
    
    let layout = {
      font_size = font_size;
      split_nb_columns = split_nb_columns;
      w_per_column = w_per_column;
      space_per_line = space_per_line;
      l_nblines = nblines;
    } 
    in

    if font_size_real > !Flag.threshold_draw_content_font_size_real 
       (* && not (is_big_file_with_few_lines ~nblines file) *)
       && nblines < !Flag.threshold_draw_content_nblines
    then draw_content ctx ~layout fileinfo rect
    else ()

  end
