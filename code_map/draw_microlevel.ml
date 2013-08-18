(*s: draw_microlevel.ml *)
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
module F = Figures
module Color = Simple_color

open Model2 (* for the fields *)

module Flag = Flag_visual
module Style = Style2

module T = Treemap
module CairoH = Cairo_helpers

module FT = File_type
module Parsing = Parsing2

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* There are many different coordinates relevant to the lines of a file:
 *  - line number in the file
 *  - column and line in column for the file when rendered in multiple columns
 *  - x,y position relative to the current treemap rectangle
 *  - x,y position on the screen
 * We have many functions below to go from one to the other.
 * 
 * note: some types below could be 'int' but it's more convenient to have
 * everything as a float because arithmetic with OCaml sucks when have
 * multiple numeric types.
 *)

type line = int

type line_in_column = {
  column: float; (* int *)
  line_in_column: float; (* int *)
}

type pos = float (* x *) * float (* y *)

type point = Cairo.point

(*s: type draw_content_layout *)
type draw_content_layout = {
  font_size: float;
  split_nb_columns: float; (* int *)
  w_per_column:float;
  space_per_line: float;
  nblines: float; (* int *)
  nblines_per_column: float; (* int *)
}
(*e: type draw_content_layout *)

(*****************************************************************************)
(* globals *)
(*****************************************************************************)

(* ugly, see mli for explanation *)
let text_with_user_pos = ref []

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let is_big_file_with_few_lines ~nblines file = 
  nblines < 20. && Common2.filesize_eff file > 4000

let use_fancy_highlighting file =
  match FT.file_type_of_file file with
  | ( FT.PL (FT.Web (FT.Php _))
    | FT.PL (FT.Web (FT.Js))
    | FT.PL (FT.Web (FT.Html))
    | FT.PL (FT.ML _)
    | FT.PL (FT.Cplusplus _ | FT.C _)
    | FT.PL (FT.Thrift)
    | FT.Text ("nw" | "tex"  | "texi" | "web" | "org")
    | FT.PL (FT.Lisp _)
    | FT.PL (FT.Haskell _)
    | FT.PL (FT.Python)
    | FT.PL (FT.Csharp)
    | FT.PL (FT.Java)
    (*    | FT.PL (FT.Prolog _) *)
    | FT.PL (FT.Erlang)
    | FT.PL (FT.Opa)
    ) -> true
  | (FT.Text "txt") when Common2.basename file =$= "info.txt" -> true
  | _ -> false

(*****************************************************************************)
(* Coordinate conversion *)
(*****************************************************************************)

let line_in_column_to_pos lc r layout =
  let x = r.p.x + (lc.column * layout.w_per_column) in
  let y = r.p.y + (lc.line_in_column * layout.space_per_line) in
  x, y

let line_to_line_in_column line layout =
  let line = (float_of_int line) - 1. in
  let column = floor (line / layout.nblines_per_column) in
  let line_in_column = 
    line - (column * layout.nblines_per_column) in
  { column; line_in_column }

let line_to_rectangle line r layout =
  let lc = line_to_line_in_column line layout in
  let x, y = line_in_column_to_pos lc r layout in
  { p = { x; y };
    q = { x = x + layout.w_per_column; y = y + layout.space_per_line };
  }

let point_to_line pt r layout =
  let x = pt.Cairo.x - r.p.x in
  let y = pt.Cairo.y - r.p.y in
  let line_in_column = floor (y / layout.space_per_line) in
  let column = floor (x / layout.w_per_column) in
  (column * layout.nblines_per_column + line_in_column + 1.) +> int_of_float

(*****************************************************************************)
(* Anamorphic entities *)
(*****************************************************************************)

(*s: final_font_size_of_categ *)
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
  Draw_common.final_font_size_when_multiplier 
    ~multiplier
    ~size_font_multiplier_multiplier
    ~font_size
    ~font_size_real
(*e: final_font_size_of_categ *)

let set_source_rgba_and_font_size_of_categ 
  ~cr ~font_size ~font_size_real ~is_matching_line
 categ 
 =
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
  
  Cairo.set_font_size cr final_font_size;
  
  let final_font_size_real = 
    CairoH.user_to_device_font_size cr final_font_size in
  
  attrs +> List.iter (fun attr ->
    match attr with
    | `FOREGROUND s 
    | `BACKGROUND s (* todo: should really draw the background of the text *)
      -> 
        let (r,g,b) = Color.rgbf_of_string s in
        (* this seems needed only on old version of Cario, or at least
         * on the cairo I am using under Linux. Under Mac I don't need
         * this; I put alpha = 1. for everything and the rendering
         * is fine.
         *)
        let alpha = 
          if CairoH.is_old_cairo () then
            match () with
            | _ when final_font_size_real < 1. -> 0.2
            | _ when final_font_size_real < 3. -> 0.4
            | _ when final_font_size_real < 5. -> 0.9
                
            | _ when final_font_size_real < 8. 
                  -> 1. (* TODO - alpha_adjust, do that only when not in
                           fully zoomed mode *)
                    
            | _ -> 1.
          else 1.
        in
        Cairo.set_source_rgba cr r g b alpha;
    | _ -> ()
  );
  ()

(*****************************************************************************)
(* Columns *)
(*****************************************************************************)

(*s: font_size_when_have_x_columns *)
let font_size_when_have_x_columns ~nblines ~chars_per_column ~w ~h ~with_n_columns = 
  let size_x = (w / with_n_columns) / chars_per_column in
  let size_y = (h / (nblines / with_n_columns)) in

  let min_font = min size_x size_y in
  min_font
(*e: font_size_when_have_x_columns *)
   
(*s: optimal_nb_columns *)
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
(*e: optimal_nb_columns *)

(*s: draw_column_bars *)
let draw_column_bars2 ~cr layout r = 
  for i = 1 to int_of_float (layout.split_nb_columns - 1.) do
    let i = float_of_int i in
      
    Cairo.set_source_rgba cr 0.0 0.0 1. 0.2;

    let font_size_real = CairoH.user_to_device_font_size cr layout.font_size in
    let width = 
      if font_size_real > 5.
      then  (layout.font_size / 10.)
      else layout.font_size
    in
    Cairo.set_line_width cr width;

    Cairo.move_to cr (r.p.x + layout.w_per_column * i) r.p.y;
    Cairo.line_to cr (r.p.x + layout.w_per_column * i) r.q.y;
    Cairo.stroke cr ;
  done
let draw_column_bars ~cr layout rect =
  Common.profile_code "View.draw_bars" (fun () ->
    draw_column_bars2 ~cr layout rect)
(*e: draw_column_bars *)


(*****************************************************************************)
(* File Content *)
(*****************************************************************************)

(*s: draw_content *)
let draw_content2 ~cr ~layout ~context ~file rect =

  let r = rect.T.tr_rect in

  let font_size = layout.font_size in
  let font_size_real = CairoH.user_to_device_font_size cr font_size in

  if font_size_real > Style.threshold_draw_dark_background_font_size_real
  then begin

    (* erase what was done at the macrolevel *)
    if Hashtbl.length context.layers_microlevel > 0 then begin
      Draw_macrolevel.draw_treemap_rectangle ~cr ~color:(Some "white") 
        ~alpha:1.0 rect;
    end;

    let alpha = 
      match context.nb_rects_on_screen with
      | n when n <= 1 -> 0.95
      | n when n <= 2 -> 0.8
      | n when n <= 10 -> 0.6
      | _ -> 0.3
    in
    (* unset when used when debugging the layering display *)
    if Hashtbl.length context.layers_microlevel = 0 || true
    then begin
      Draw_macrolevel.draw_treemap_rectangle ~cr ~color:(Some "DarkSlateGray") 
        ~alpha rect;
      (* draw a thin rectangle with aspect color *)
      CairoH.draw_rectangle_bis ~cr ~color:(rect.T.tr_color) 
        ~line_width:(font_size / 2.) rect.T.tr_rect;
    end
  end;

  (* highlighting layers (and grep-like queries at one point) *)
  let hmatching_lines = 
    try Hashtbl.find context.layers_microlevel file
    with Not_found -> Hashtbl.create 0
  in

  (* todo: make sgrep_query a form of layer *)
  let matching_grep_lines = 
    try Hashtbl.find_all context.grep_query file
    with Not_found -> []
  in
  matching_grep_lines +> List.iter (fun line ->
    Hashtbl.add hmatching_lines line "purple"
  );

  let line = ref 1 in

  (* ugly *)
  text_with_user_pos := [];

  (* coupling: with parsing2.ml *)
  (if use_fancy_highlighting file then begin

    let column = ref 0 in
    let line_in_column = ref 1 in

    let x, y = 
      line_in_column_to_pos 
        { column = float_of_int !column; 
          line_in_column = float_of_int !line_in_column
        } r layout in

    Cairo.move_to cr x y;

    let model = Async.async_get context.model in
    let entities = model.Model2.hentities in

    let tokens_with_categ = Parsing.tokens_with_categ_of_file file entities in

    tokens_with_categ +> List.iter (fun (s, categ, filepos) ->

      set_source_rgba_and_font_size_of_categ 
        ~cr ~font_size ~font_size_real
        ~is_matching_line:(Hashtbl.mem hmatching_lines !line)
        categ;
      
      let xs = Common2.lines_with_nl_either s in
      
      xs +> List.iter (function
      | Common2.Left s -> 
        let pt = Cairo.get_current_point cr in
        Common.push2 (s, filepos, pt) text_with_user_pos;

        CairoH.show_text cr s
      | Common2.Right () ->
        
        incr line_in_column;
        incr line;

        if !line_in_column > int_of_float layout.nblines_per_column
        then begin 
          incr column;
          line_in_column := 1;
        end;

        let x, y = 
          line_in_column_to_pos 
            { column = float_of_int !column; 
              line_in_column = float_of_int !line_in_column
            } r layout in

          (* must be done before the move_to below ! *)
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
        Cairo.move_to cr x y;
        
        
      );
    )
  end else begin
    match FT.file_type_of_file file with
    | FT.PL _ | FT.Text _ ->      
    (* This was causing some "out_of_memory" cairo error on linux. Not
     * sure why.
     *)

      Cairo.set_font_size cr font_size ;
      Cairo.set_source_rgba cr 0.0 0.0 0.0 0.9;
      
      let xs = Common.cat file in
      let xxs = Common2.pack_safe (int_of_float layout.nblines_per_column) xs in

    (* I start at 0 for the column because the x displacement
     * is null at the beginning, but at 1 for the line because
     * the y displacement must be more than 0 at the
     * beginning
     *)
      Common.index_list_0 xxs +> List.iter (fun (xs, column) ->
        Common.index_list_1 xs +> List.iter (fun (s, line_in_column) ->
          let x, y = 
            line_in_column_to_pos 
              { column = float_of_int column; 
                line_in_column = float_of_int line_in_column
              } r layout in

          Cairo.move_to cr x y;
          CairoH.show_text cr s;
        );
      );
      ()
    | _ ->
      ()
  end);
  { line_to_rectangle = 
      (fun line -> line_to_rectangle line r layout);
    pos_to_line = 
      (fun pt -> point_to_line pt r layout);
    content = [||];
  }



let draw_content ~cr ~layout ~context ~file rect =
  Common.profile_code "View.draw_content" (fun () ->
    draw_content2 ~cr ~layout ~context ~file rect)
(*e: draw_content *)


(*s: draw_treemap_rectangle_content_maybe *)
let draw_treemap_rectangle_content_maybe2 ~cr ~clipping ~context rect  =
  let r = rect.T.tr_rect in

  if F.intersection_rectangles r clipping = None
  then (* pr2 ("not drawing: " ^ file) *) None
  else begin

    let file = rect.T.tr_label in

    (* if the file is not textual, or contain weird characters, then
     * it confuses cairo which then can confuse computation done in gtk
     * idle callbacks
     *)
    if Common2.lfile_exists_eff file && File_type.is_textual_file file
    then begin
      let w = F.rect_width r in
      let h = F.rect_height r in

      let font_size_estimate = h / 100. in
      let font_size_real_estimate = 
        CairoH.user_to_device_font_size cr font_size_estimate in
      if font_size_real_estimate > 0.4
      then begin
        
       (* Common.nblines_with_wc was really slow. Forking sucks.
        * alt: we could store the nblines of a file in the db.
        *)
        let nblines = Common2.nblines_eff file +> float_of_int in
        
       (* Assume our code follow certain conventions. Could infer from file. 
        * We should put 80, but a font is higher than large, so I 
        * manually readjust things.
        *)
        let chars_per_column = 41.0 in
    
        let split_nb_columns = 
          optimal_nb_columns ~nblines ~chars_per_column ~h ~w in
        let font_size = 
          font_size_when_have_x_columns ~nblines ~chars_per_column ~h ~w 
            ~with_n_columns:split_nb_columns in

        let layout = {
          nblines;
          font_size;
          split_nb_columns;
          w_per_column = w / split_nb_columns;
          space_per_line = font_size;
          nblines_per_column = (nblines / split_nb_columns) +> ceil;
        } 
        in
        
        draw_column_bars ~cr layout r;
        
       (* todo: does not work :(
          let font_option = Cairo.Font_Options.make [`ANTIALIAS_SUBPIXEL] in
       
       (try 
         Cairo.set_font_options cr font_option;
       with exn ->
         let status = Cairo.status cr in
         let s2 = Cairo.string_of_status status in
         failwith s2;
       );
       *)
        Cairo.select_font_face cr Style.font_text
          Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
    
        let font_size_real = CairoH.user_to_device_font_size cr font_size in
       (*pr2 (spf "file: %s, font_size_real = %f" file font_size_real);*)
    
        if font_size_real > !Flag.threshold_draw_content_font_size_real 
            && not (is_big_file_with_few_lines ~nblines file)
            && nblines < !Flag.threshold_draw_content_nblines
        then Some (draw_content ~cr ~layout ~context ~file rect)
        else 
          if context.settings.draw_summary 
            (* draw_summary_content ~cr ~layout ~context ~file  rect *)
          then raise Todo
          else None
      end
      else None
    end
    else None
  end
let draw_treemap_rectangle_content_maybe ~cr ~clipping ~context rect = 
  Common.profile_code "View.draw_content_maybe" (fun () ->
    draw_treemap_rectangle_content_maybe2 ~cr ~clipping ~context rect)
(*e: draw_treemap_rectangle_content_maybe *)
    

(*e: draw_microlevel.ml *)
