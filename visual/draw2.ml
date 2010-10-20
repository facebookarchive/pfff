(*s: draw2.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2010 Facebook
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

(* 
 * Floats are the norm in graphics. 
 * note: with ocaml 3.12 could also use the Float.(...) local open extension
 *)
open Common.ArithFloatInfix

module Color = Simple_color

open Figures (* for the fields *)
open Model2 (* for the fields *)

module Style = Style2
module FT = File_type
module Parsing = Parsing2

module Flag = Flag_visual

module HC = Highlight_code

module Db = Database_code

module CairoH = Cairo_helpers
module T = Treemap

module F = Figures

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* ugly *)

let text_with_user_pos = ref []

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*s: type draw_content_layout *)
type draw_content_layout = {
  font_size: float;
  split_nb_columns: float;
  w_per_column:float;
  space_per_line: float;
}
(*e: type draw_content_layout *)

(*s: type context *)
(* a slice of Model2.drawing *)
type context = {
  model: Model2.model Model2.async;
  settings:Model2.settings;
  nb_rects_on_screen: int;
  current_grep_query: (Common.filename, int) Hashtbl.t;
}
(*e: type context *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let is_big_file_with_few_lines ~nblines fullpath = 
  nblines < 20. && 
  Common.filesize_eff fullpath > 4000

(*****************************************************************************)
(* Basics *)
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
(* Color of entity *)
(*****************************************************************************)

(*****************************************************************************)
(* Anamorphic entities *)
(*****************************************************************************)

(*s: final_font_size_when_multiplier *)
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
    Common.borne ~min:font_size ~max:(font_size * 30.) font_size_adjusted
  in
  final_font_size
(*e: final_font_size_when_multiplier *)

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

  final_font_size_when_multiplier 
    ~multiplier
    ~size_font_multiplier_multiplier
    ~font_size
    ~font_size_real
(*e: final_font_size_of_categ *)

(*****************************************************************************)
(* Columns *)
(*****************************************************************************)

(*s: font_size_when_have_x_columns *)
let font_size_when_have_x_columns ~nblines ~nbcolumns ~w ~h ~with_n_columns = 
  let size_x = (w / with_n_columns) / nbcolumns in
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
let optimal_nb_columns ~nblines ~nbcolumns ~w ~h = 
  
  let rec aux current_font_size current_nb_columns = 
    let min_font = font_size_when_have_x_columns 
      ~nblines ~nbcolumns ~w ~h ~with_n_columns:current_nb_columns
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
let draw_column_bars2 ~cr ~split_nb_columns ~font_size ~w_per_column rect = 
  let r = rect.T.tr_rect in
  for i = 1 to int_of_float (split_nb_columns - 1.) do
    let i = float_of_int i in
      
    Cairo.set_source_rgba cr 0.0 0.0 1. 0.2;

    let font_size_real = CairoH.user_to_device_font_size cr font_size in
    let width = 
      if font_size_real > 5.
      then  (font_size / 10.)
      else font_size
    in
    Cairo.set_line_width cr width;

    Cairo.move_to cr (r.p.x + w_per_column * i) r.p.y;
    Cairo.line_to cr (r.p.x + w_per_column * i) r.q.y;
    Cairo.stroke cr ;
  done
let draw_column_bars ~cr ~split_nb_columns ~font_size ~w_per_column rect =
  Common.profile_code "View.draw_bars" (fun () ->
    draw_column_bars2 ~cr ~split_nb_columns ~font_size ~w_per_column rect)
(*e: draw_column_bars *)


(*****************************************************************************)
(* File Summary *)
(*****************************************************************************)

(*s: draw_summary_content *)
(* todo: should base this on the current max in the current view.
 * Also bad that can not compare function use by just looking
 * at their size :(
*)
let threshold_print_summary_entity_users = 10

let draw_summary_content2 ~cr ~layout ~context ~nblines ~file rect =

  let w_per_column  = layout.w_per_column in

  let r = rect.T.tr_rect in
  let file = rect.T.tr_label in

  let model = Model2.async_get context.model in

  let files_entities = model.Model2.hfiles_entities in
  let entities = 
    try Hashtbl.find files_entities file
    with Not_found -> []
  in
  let entities = 
    entities 
    +> Common.take_safe 5
    +> Common.filter (fun e ->
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
      final_font_size_when_multiplier 
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
 ~cr ~layout ~context ~nblines ~file rect =
  Common.profile_code "View.draw_summary_content" (fun () ->
    draw_summary_content2 ~cr ~layout ~context ~nblines ~file rect)
(*e: draw_summary_content *)

(*****************************************************************************)
(* File Content *)
(*****************************************************************************)

(*s: draw_content *)
let draw_content2 ~cr ~layout ~context ~nblines ~file rect =

  let font_size = layout.font_size in
  let split_nb_columns = layout.split_nb_columns in
  let w_per_column  = layout.w_per_column in
  let space_per_line = layout.space_per_line in

  let r = rect.T.tr_rect in
  let font_size_real = CairoH.user_to_device_font_size cr font_size in


  (* highlighting grep-like queries *)
  let matching_lines = 
    try Hashtbl.find_all context.current_grep_query file
    with Not_found -> []
  in
  let hmatching_lines = Common.hashset_of_list matching_lines in
  let is_matching_line i = 
    Hashtbl.mem hmatching_lines i
  in
  pr2_gen matching_lines;

  let line = ref 1 in

  let nblines_per_column = 
    (nblines / split_nb_columns) +> ceil +> int_of_float in

  (* ugly *)
  text_with_user_pos := [];

  (match FT.file_type_of_file file with
  | (
      FT.PL (FT.Web (FT.Php _))
    | FT.PL (FT.Web (FT.Js _))
    | FT.PL (FT.ML _)
    | FT.PL (FT.Cplusplus | FT.C)
    | FT.PL (FT.Thrift)
    | FT.Text ("nw" | "tex"  | "texi" | "web")
    ) ->

    let column = ref 0 in
    let line_in_column = ref 1 in

    let x = r.p.x + (float_of_int !column) * w_per_column in
    let y = r.p.y + (space_per_line * (float_of_int !line_in_column)) in
        
    Cairo.move_to cr x y;

    let model = Model2.async_get context.model in
    let entities = model.Model2.hentities in

    let tokens_with_categ = 
      Parsing.tokens_with_categ_of_file file entities
    in
    if font_size_real > Style.threshold_draw_dark_background_font_size_real
    then begin
      let alpha = 
        match context.nb_rects_on_screen with
        | n when n <= 2 -> 0.8
        | n when n <= 10 -> 0.6
        | _ -> 0.3
      in
      draw_treemap_rectangle ~cr ~color:(Some "DarkSlateGray") ~alpha rect;
      CairoH.draw_rectangle_bis ~cr ~color:(rect.T.tr_color) 
        ~line_width:font_size rect.T.tr_rect;
    end;

    tokens_with_categ +> List.iter (fun (s, categ, filepos) ->
      let attrs =
        match categ with
        | None -> Highlight_code.info_of_category Highlight_code.Normal
        | Some categ -> Highlight_code.info_of_category categ
      in

      let final_font_size = 
        final_font_size_of_categ ~font_size ~font_size_real categ in
      let final_font_size = 
        if is_matching_line !line 
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

      attrs |> List.iter (fun attr ->
        match attr with
        | `FOREGROUND s -> 
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
      
      let xs = Common.lines_with_nl_either s in
      
      xs +> List.iter (function
      | Left s -> 
          let pt = Cairo.get_current_point cr in
          Common.push2 (s, filepos, pt) text_with_user_pos;

          CairoH.show_text cr s
      | Right () ->
          
          incr line_in_column;
          incr line;

          if !line_in_column > nblines_per_column
          then begin 
            incr column;
            line_in_column := 1;
          end;

          let x = r.p.x + (float_of_int !column) * w_per_column in
          let y = r.p.y + (space_per_line * (float_of_int !line_in_column)) in

          (* must be done before the move_to below ! *)
          if is_matching_line !line
          then begin
            CairoH.fill_rectangle ~cr 
              ~alpha:0.5
              ~color:"magenta"
              ~x 
              ~y:(y - space_per_line) 
              ~w:w_per_column 
              ~h:(space_per_line * 3.)
              ()
          end;
          
          Cairo.move_to cr x y;
          
          
      );
    )
  | FT.PL _ | FT.Text _ ->      
   (* This was causing some "out_of_memory" cairo error on linux. Not
    * sure why.
    *)

    Cairo.set_font_size cr font_size ;
    Cairo.set_source_rgba cr 0.0 0.0 0.0 0.9;
      
    let xs = Common.cat file in
    let xxs = Common.pack_safe nblines_per_column xs in

    (* I start at 0 for the column because the x displacement
     * is null at the beginning, but at 1 for the line because
     * the y displacement must be more than 0 at the
     * beginning
     *)
    Common.index_list_0 xxs +> List.iter (fun (xs, column) ->
      Common.index_list_1 xs +> List.iter (fun (s, line_in_column) ->
      
        let x = r.p.x + (float_of_int column) * w_per_column in
        let y = r.p.y + (space_per_line * (float_of_int line_in_column)) in
        
        Cairo.move_to cr x y;
        CairoH.show_text cr s;

        incr line;
      );
    );
      ()
  | _ ->
      ()
  )

let draw_content ~cr ~layout ~context ~nblines ~file rect =
  Common.profile_code "View.draw_content" (fun () ->
    draw_content2 ~cr ~layout ~context ~nblines ~file rect)
(*e: draw_content *)


(*s: draw_treemap_rectangle_content_maybe *)
let draw_treemap_rectangle_content_maybe2 ~cr ~clipping ~context rect  =
  let r = rect.T.tr_rect in
  let file = rect.T.tr_label in

  if intersection_rectangles r clipping = None
  then (* pr2 ("not drawing: " ^ file) *) ()
  else begin

  let w = F.rect_width r in
  let h = F.rect_height r in

  (* pr2_gen (dw.path, file); *)
  let fullpath = file in

  (* if the file is not textual, or contain weird characters, then
   * it confuses cairo which then can confuse computation done in gtk
   * idle callbacks
   *)
  if Common.lfile_exists_eff fullpath && File_type.is_textual_file fullpath
  then begin
    let font_size_estimate = h / 100. in
    let font_size_real_estimate = 
      CairoH.user_to_device_font_size cr font_size_estimate in
    if font_size_real_estimate > 0.4
    then begin

    (* Common.nblines_with_wc was really slow. fork sucks.
     * alternative: we could store the nblines of a file in the db but
     * we would need a fast absolute_to_readable then.
     *)
    let nblines = Common.nblines_eff fullpath +> float_of_int in
    (* assume our code follow certain conventions. Could infer from file. 
     * we should put 80, but a font is higher than large, so 
     * I manually readjust things. todo: should readjust something
     * else.
     *)

    let nbcolumns = 41.0 in
    
    let split_nb_columns = 
      optimal_nb_columns ~nblines ~nbcolumns ~h ~w
    in
    let font_size = 
      font_size_when_have_x_columns ~nblines ~nbcolumns ~h ~w 
        ~with_n_columns:split_nb_columns
    in
    let w_per_column = w / split_nb_columns in
    
    let space_per_line = font_size in
    
    (* draw the columns bars *)
    draw_column_bars ~cr ~split_nb_columns ~font_size ~w_per_column rect;


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
    Cairo.select_font_face cr "serif" 
      Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
    
    let font_size_real = CairoH.user_to_device_font_size cr font_size in
    (*pr2 (spf "file: %s, font_size_real = %f" file font_size_real);*)
    
    let layout = {
      font_size = font_size;
      split_nb_columns = split_nb_columns;
      w_per_column = w_per_column;
      space_per_line = space_per_line;
    } 
    in

    if font_size_real > !Flag.threshold_draw_content_font_size_real 
       && not (is_big_file_with_few_lines ~nblines fullpath)
       && nblines < !Flag.threshold_draw_content_nblines
    then draw_content ~cr ~layout ~context ~nblines ~file:fullpath rect
    else 
     if context.settings.draw_summary 
     then 
       draw_summary_content ~cr ~layout ~context ~nblines ~file:fullpath  rect
    end
  end
  end
let draw_treemap_rectangle_content_maybe ~cr ~clipping ~context rect = 
  Common.profile_code "View.draw_content_maybe" (fun () ->
    draw_treemap_rectangle_content_maybe2 ~cr ~clipping ~context rect)
(*e: draw_treemap_rectangle_content_maybe *)
    

(*****************************************************************************)
(* Label *)
(*****************************************************************************)

(*s: draw_treemap_rectangle_label_maybe *)
let _hmemo_text_extent = Hashtbl.create 101

(* This can be quite cpu intensive. CairoH.text_extents is quite slow
 * so you must avoid calling it too much. A simple optimisation
 * when the treemap is big is to avoid trying to draw labels
 * that are too tiny already.
 *)
let rec draw_treemap_rectangle_label_maybe2 ~cr ~zoom ~color rect =
  if !Flag.disable_fonts then ()
  else begin

  let lbl = rect.T.tr_label in
  let base = Filename.basename lbl in
  (* old: Common.is_directory_eff lbl *)
  let is_directory = rect.T.tr_is_node in
  let txt = 
    if is_directory
    then base ^ "/"
    else base
  in

  Cairo.select_font_face cr "serif" 
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_BOLD;
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
  let font_size = font_size / (zoom) (* use zoom factor inversely *) in
  let alpha = 1. - (minus_alpha / zoom) in

    try_draw_label 
      ~font_size_orig:font_size
      ~color ~alpha 
      ~cr ~rect txt
  end


and try_draw_label ~font_size_orig ~color ~alpha ~cr ~rect txt =

(* ugly: sometimes labels are too big. Should provide a way to
 * shorten them.
 * let txt = 
 * if true
 * then if txt =~ "org.eclipse.\\(.*\\)" then Common.matched1 txt else txt
 * else txt
 * in
 *)

  let r = rect.T.tr_rect in
  
  let w = F.rect_width r in
  let h = F.rect_height r in

  let is_file = 
    (* old: try Common.is_file_eff rect.T.tr_label with _ -> false *)
    not rect.T.tr_is_node
  in

  (let (r,g,b) = color +> Color.rgbf_of_string in
   Cairo.set_source_rgba cr r g b alpha;
  );

  let rec aux ~font_size ~step =

    (* opti: this avoid lots of computation *)
    let font_size_real = CairoH.user_to_device_font_size cr font_size in

    if font_size_real < !Flag.threshold_draw_label_font_size_real
    then ()
    else begin

     CairoH.set_font_size cr font_size;

     (* opti:
      *  was let extent = CairoH.text_extents cr txt
      *)
     let th, base_tw = 
       Common.memoized _hmemo_text_extent (font_size, font_size_real) (fun ()->
         (* peh because it exercises the spectrum of high letters *)
         let extent = CairoH.text_extents cr "peh" in
         let tw = extent.Cairo.text_width in
         let th = extent.Cairo.text_height in
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
       
           Cairo.move_to cr x y;
           CairoH.show_text cr txt;
   
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
         Cairo.move_to cr x y;
   
           
         Cairo.rotate cr ~angle:angle;
         CairoH.show_text cr txt;
         Cairo.rotate cr ~angle:(-. angle);
   
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

let draw_treemap_rectangle_label_maybe ~cr ~zoom ~color rect =
  Common.profile_code "View.draw_label_maybe" (fun () ->
    draw_treemap_rectangle_label_maybe2 ~cr ~zoom ~color rect)
(*e: draw_treemap_rectangle_label_maybe *)

(*e: draw2.ml *)
