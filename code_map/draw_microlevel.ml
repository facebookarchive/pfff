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
open Model2 (* for the fields *)
module F = Figures
module M = Model2
module T = Treemap
module Color = Simple_color
module CairoH = Cairo_helpers
module HC = Highlight_code
module Flag = Flag_visual
module Style = Style2
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
 *  - x,y position on the screen in normalized coordinates
 *  - x,y position on the screen in pixels
 * We have many functions below to go from one to the other.
 * 
 * note: some types below could be 'int' but it's more convenient to have
 * everything as a float because arithmetic with OCaml sucks when have
 * multiple numeric types.
 * 
 * Below line numbers starts at 0, not at 1 as in emacs.
 *)

type _line = Model2.line

type line_in_column = {
  column: float; (* int *)
  line_in_column: float; (* int *)
}

type _pos = float (* x *) * float (* y *)

type _point = Cairo.point

(*s: type draw_content_layout *)
(*e: type draw_content_layout *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let is_big_file_with_few_lines ~nblines file = 
  nblines < 20. && Common2.filesize_eff file > 4000

(* coupling: with parsing2.ml, todo move in parsing2.ml? *)
let use_fancy_highlighting file =
  match FT.file_type_of_file file with
  | ( FT.PL (FT.Web (FT.Php _))
    | FT.PL (FT.Web (FT.Js))
    | FT.PL (FT.Web (FT.Html))
    | FT.PL (FT.ML _)
    | FT.PL (FT.Cplusplus _ | FT.C _ | FT.ObjectiveC _)
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
    | FT.PL (FT.Rust)
    ) -> true
  | (FT.Text "txt") when Common2.basename file =$= "info.txt" -> true
  | _ -> false

(*****************************************************************************)
(* Coordinate conversion *)
(*****************************************************************************)

let line_in_column_to_bottom_pos lc r layout =
  let x = r.p.x + (lc.column * layout.width_per_column) in
  (* to draw text in cairo we need to be one line below, hence the +1
   * as y goes down but the text is drawn above
   *)
  let y = r.p.y + ((lc.line_in_column + 1.) * layout.height_per_line) in
  x, y

let line_to_line_in_column line layout =
  let (Line line) = line in
  let line = float_of_int line in
  let column = floor (line / layout.nblines_per_column) in
  let line_in_column = 
    line - (column * layout.nblines_per_column) in
  { column; line_in_column }

let line_to_rectangle line r layout =
  let lc = line_to_line_in_column line layout in
  (* this is the bottom pos, so we need to substract height_per_line
   * if we want to draw above the bottom pos
   *)
  let x, y = line_in_column_to_bottom_pos lc r layout in
  { p = { x; 
          y = y - layout.height_per_line };
    q = { x = x + layout.width_per_column; 
          y = y + 0.2 * layout.height_per_line };
  }

let point_to_line pt r layout =
  let x = pt.Cairo.x - r.p.x in
  let y = pt.Cairo.y - r.p.y in
  let line_in_column = floor (y / layout.height_per_line) in
  let column = floor (x / layout.width_per_column) in
  Line ((column * layout.nblines_per_column + line_in_column) +> int_of_float)

(*****************************************************************************)
(* Content properties *)
(*****************************************************************************)

(* Anamorphic entities *)
(*s: final_font_size_of_categ *)
let final_font_size_of_categ ~font_size ~font_size_real categ = 

  let multiplier = Style.size_font_multiplier_of_categ ~font_size_real categ in
  (* as we zoom in, we don't want to be as big, and as we zoom out we want
   * to be bigger
   *)
  let multiplier = 
     (*- 0.2 * font_size_real + 2. *)
    match font_size_real with
    | n when n < 3. -> 2.0 * multiplier
    | n when n < 8. -> 0.8 * multiplier
    | n when n < 10. -> 0.7 * multiplier
    | _ -> 0.5 * multiplier
  in
  Common2.borne ~min:font_size ~max:(font_size * 30.) (font_size * multiplier)

(*e: final_font_size_of_categ *)

let color_of_categ categ =
  let attrs =
    match categ with
    | None ->       Highlight_code.info_of_category Highlight_code.Normal
    | Some categ -> Highlight_code.info_of_category categ
  in
  attrs +> Common.find_some (fun attr ->
    match attr with
    | `FOREGROUND s 
    | `BACKGROUND s (* todo: should really draw the background of the text *)
      -> 
        Some (s)
    | _ -> None
  )

let glyphs_of_file ~font_size ~font_size_real model_async file 
  : (glyph list) array option =

  (* real position is set later in draw_content *)
  let pos = { Cairo.x = 0.; y = 0. } in

  match FT.file_type_of_file file with
  | _ when use_fancy_highlighting file ->

    let entities = 
      match Async.async_get_opt model_async with
      | Some model -> model.Model2.hentities 
      | None -> Hashtbl.create 0
    in

    (* if you have some cache in tokens_with_categ_of_file, then it
     * must be invalidated when a file has changed on the disk, otherwise
     * we can get some Array out of bound exceptions as the number of lines
     * returned by nblines_eff may be different
     *)
    let nblines = Common2.nblines_eff file in
    let arr = Array.make nblines [] in
    let tokens_with_categ = Parsing.tokens_with_categ_of_file file entities in

    let line = ref 0 in
    let acc = ref [] in
    (try
     tokens_with_categ +> List.iter (fun (s, categ, _filepos) ->
      let final_font_size = 
        final_font_size_of_categ ~font_size ~font_size_real categ in
      let color = 
        color_of_categ categ in

      let xs = Common2.lines_with_nl_either s in
      xs +> List.iter (function
      | Common2.Left str ->
          Common.push { M. str; font_size=final_font_size; color; categ;pos } 
            acc;
      | Common2.Right () ->
          arr.(!line) <- List.rev !acc;
          acc := [];
          incr line;
      )
    );
    if !acc <> []
    then arr.(!line) <- List.rev !acc;
    Some arr
   with Invalid_argument("index out of bounds") ->
      failwith (spf "try on %s, nblines = %d, line = %d" file nblines !line)
    )

  | FT.PL _ | FT.Text _ ->      
      Common.cat file
      +> List.map (fun str -> 
        [{ M.str; font_size; color = "black"; categ=None; pos }])
      +> Array.of_list
      +> (fun x -> Some x)

  | _ -> None

let defs_of_glyphs glyphs =
  let res = ref [] in
  glyphs +> Array.iteri (fun line_0_indexed glyphs ->
    glyphs +> List.iter (fun glyph ->
      glyph.categ +> Common.do_option (fun categ ->
        Database_code.entity_kind_of_highlight_category_def categ 
        +> Common.do_option (fun kind ->
              Common.push (Line line_0_indexed, (glyph.str, kind)) res
        ))));
  List.rev !res

(*****************************************************************************)
(* Columns *)
(*****************************************************************************)

(*s: font_size_when_have_x_columns *)
let font_size_when_have_x_columns ~nblines ~chars_per_column ~w ~h ~with_n_columns = 
  let size_x = (w / with_n_columns) / chars_per_column in
  let size_y = (h / (nblines / with_n_columns)) in
  min size_x size_y
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
    (* regression, then go back on step *)
    else current_nb_columns - 1.
  in
  aux 0.0 1.0
(*e: optimal_nb_columns *)

(*s: draw_column_bars *)
let draw_column_bars2 cr layout r = 
  for i = 1 to int_of_float (layout.split_nb_columns - 1.) do
    let i = float_of_int i in
      
    Cairo.set_source_rgba cr 0.0 0.0 1. 0.2;

    let font_size_real = CairoH.user_to_device_font_size cr layout.lfont_size in
    let width = 
      if font_size_real > 5.
      then layout.lfont_size / 10.
      else layout.lfont_size
    in
    Cairo.set_line_width cr width;

    Cairo.move_to cr (r.p.x + layout.width_per_column * i) r.p.y;
    Cairo.line_to cr (r.p.x + layout.width_per_column * i) r.q.y;
    Cairo.stroke cr ;
  done
let draw_column_bars cr layout rect =
  Common.profile_code "View.draw_bars" (fun () ->
    draw_column_bars2 cr layout rect)
(*e: draw_column_bars *)


(*****************************************************************************)
(* File Content *)
(*****************************************************************************)

(*s: draw_content *)
let draw_content2 cr layout context tr =

  let r = tr.T.tr_rect in
  let file = tr.T.tr_label in

  let font_size = layout.lfont_size in
  let font_size_real = CairoH.user_to_device_font_size cr font_size in

  if font_size_real > Style.threshold_draw_dark_background_font_size_real
  then begin

    (* erase what was done at the macrolevel *)
    if Hashtbl.length context.layers_microlevel > 0 then begin
      Draw_macrolevel.draw_treemap_rectangle ~cr ~color:(Some "white") 
        ~alpha:1.0 tr;
    end;

    let alpha = 
      match context.nb_rects_on_screen with
      | n when n <= 1 -> 0.95
      | n when n <= 2 -> 0.8
      | n when n <= 10 -> 0.6
      | _ -> 0.3
    in
    (* unset when used when debugging the layering display *)
    (* if Hashtbl.length context.layers_microlevel = 0 *)

    Draw_macrolevel.draw_treemap_rectangle ~cr ~color:(Some "DarkSlateGray") 
      ~alpha tr;
    (* draw a thin rectangle with aspect color *)
    CairoH.draw_rectangle_bis ~cr ~color:(tr.T.tr_color) 
      ~line_width:(font_size / 2.) tr.T.tr_rect;
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
    let (Line iline) = line in
    Hashtbl.add hmatching_lines (iline+..1) "purple"
  );

  (* the important function call, getting the decorated content *)
  let glyphs_opt = 
    glyphs_of_file ~font_size ~font_size_real context.model2 file in

  glyphs_opt +> Common.do_option (fun glyphs ->
    glyphs +> Array.iteri (fun line_0_indexed _glyph ->
      let lc = line_to_line_in_column (Line line_0_indexed) layout in
      let x, y = line_in_column_to_bottom_pos lc r layout in
      Cairo.move_to cr x y;
      
      glyphs.(line_0_indexed) +> List.iter (fun glyph ->
        let pos = Cairo.get_current_point cr in
        glyph.pos <- pos;
        Cairo.set_font_size cr glyph.M.font_size;
        let (r,g,b) = Color.rgbf_of_string glyph.color in
        Cairo.set_source_rgba cr r g b 1.;
        CairoH.show_text cr glyph.M.str;
      );

      (* hmatching_lines comes from layer_microlevel which is 1-index based *)
      let line = line_0_indexed +.. 1 in
      (match Common2.hfind_option line hmatching_lines with
      | None -> ()
      | Some color ->
        CairoH.fill_rectangle ~cr 
          ~alpha:0.25
          ~color
          ~x 
          (* 'y' is from line_in_column_to_bottom_pos() so it's a bottom pos
           * todo? do we want to show 3 lines centered, hence the * 2 below?
           *)
          ~y:(y - (layout.height_per_line (* * 2. *)) )
          ~w:layout.width_per_column 
          ~h:(layout.height_per_line (* * 3. *))
          ()
      );
    );
  );

  { line_to_rectangle = 
      (fun line -> line_to_rectangle line r layout);
    point_to_line = 
      (fun pt -> point_to_line pt r layout);
    layout;
    container = tr;
    content = glyphs_opt;
    defs = (match glyphs_opt with None -> [] | Some x -> defs_of_glyphs x);
  }

let draw_content cr layout context tr =
  Common.profile_code "View.draw_content" (fun () ->
    draw_content2 cr layout context tr)
(*e: draw_content *)


(*s: draw_treemap_rectangle_content_maybe *)
let draw_treemap_rectangle_content_maybe2 cr clipping context tr  =
  let r = tr.T.tr_rect in

  if F.intersection_rectangles r clipping = None
  then (* pr2 ("not drawing: " ^ file) *) None
  else begin
    let file = tr.T.tr_label in

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
        * We should put 80, but a font is higher than large, so I readjusted.
        *)
        let chars_per_column = 41.0 in
    
        let split_nb_columns = 
          optimal_nb_columns ~nblines ~chars_per_column ~h ~w in
        let font_size = 
          font_size_when_have_x_columns ~nblines ~chars_per_column ~h ~w 
            ~with_n_columns:split_nb_columns in

        let layout = {
          nblines;
          lfont_size = font_size;
          split_nb_columns;
          width_per_column = w / split_nb_columns;
          height_per_line = font_size;
          nblines_per_column = (nblines / split_nb_columns) +> ceil;
        } 
        in
        draw_column_bars cr layout r;
        
        Cairo.select_font_face cr Style.font_text
          Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
    
        let font_size_real = CairoH.user_to_device_font_size cr font_size in
       (*pr2 (spf "file: %s, font_size_real = %f" file font_size_real);*)
    
        if font_size_real > !Flag.threshold_draw_content_font_size_real 
            && not (is_big_file_with_few_lines ~nblines file)
            && nblines < !Flag.threshold_draw_content_nblines
        then Some (draw_content cr layout context tr)
        else None
      end
      else None
    end
    else None
  end
let draw_treemap_rectangle_content_maybe cr clipping context rect = 
  Common.profile_code "View.draw_content_maybe" (fun () ->
    draw_treemap_rectangle_content_maybe2 cr clipping context rect)
(*e: draw_treemap_rectangle_content_maybe *)

(*****************************************************************************)
(* Magnifyer Content *)
(*****************************************************************************)

(* alt: digital zoom? good enough? need rendering at better resolution? *)
let draw_magnify_line ?(honor_color=true) cr line microlevel =
  match microlevel.content with
  | None -> ()
  | Some glyphs ->
    let r = microlevel.container.T.tr_rect in
    let layout = microlevel.layout in

    let lc = line_to_line_in_column line layout in
    let x, y = line_in_column_to_bottom_pos lc r layout in
    Cairo.move_to cr x y;
    
    let (Line iline) = line in
    (* because of the way we layout code in multiple columns with different
     * fonts, we may not use the whole rectangle to draw the content of
     * a file and so the cursor could be far below the last line of
     * the file
     *)
    if iline < Array.length glyphs then begin
     glyphs.(iline) 
     +> (fun xs ->
      match xs with
      | [] -> []
      | x::xs ->
        if x.M.str =~ "[ \t]+" then xs
        else x::xs
    )
    +> List.iter (fun glyph ->
      (* let font_size = glyph.M.font_size * 3. in *)
      let font_size_real = 15. in
      let font_size = CairoH.device_to_user_size cr font_size_real in
      Cairo.set_font_size cr font_size;
      let color = 
        if honor_color 
        then glyph.color
        else "wheat"
      in
      let (r,g,b) = Color.rgbf_of_string color in
      let alpha = 1. in
      Cairo.set_source_rgba cr r g b alpha;
      CairoH.show_text cr glyph.M.str;
    )
    end
(*e: draw_microlevel.ml *)
