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
open Common_client

open Figures
module Color = Simple_color

open Model_codegraph (* for the fields *)
module M = Model_codegraph

module E = Database_code
module DM = Dependencies_matrix_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* TODO: factorize with pfff/code_graph/view_matrix.ml *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* similar to highlight_code.ml *)
let color_of_node (_, kind) =
  match kind with
  | E.Function -> "gold"
  | E.Prototype -> "gold4"
  (* less: different color for interfaces and traits? *)
  | E.Class _ -> "coral"
  | E.Module -> "chocolate"
  | E.Package -> "chocolate"
  | E.Type -> "YellowGreen"
  | E.Constant -> "pink"
  | E.Global -> "cyan"
  | E.GlobalExtern -> "cyan4"
  | E.Macro -> "pink1"
  | E.Exception -> "orchid"
  | E.TopStmts -> "black"

  | E.Method _ -> "gold3"
  | E.Field -> "MediumPurple1"
  | E.ClassConstant -> "pink3"
  | E.Constructor -> "pink3"

  | E.Other s -> raise Todo

  | E.Dir -> "SteelBlue2"
  | E.MultiDirs -> "SteelBlue3"
  | E.File -> "wheat"

(* we often keep fully qualified names in the graph_code to avoid
 * ambiguities between entities, but in code_graph we prefer to
 * display short names as the parents already contain the
 * information.
 *)
let txt_of_node (s, kind) = 
  match kind with
  | E.Dir | E.File | E.MultiDirs -> Common2.basename s
  | E.Package | E.Module
  | E.Class _ 
  | E.Field | E.Constructor | E.Method _  | E.ClassConstant
  | E.Function | E.Type | E.Constant | E.Global
  | E.Exception 
    ->
      let xs = Common.split "[.]" s in
      Common2.list_last xs
  | _ -> s

(* todo: style/font_of_node_kind? so put in bold directories *)

let line_width_of_depth l d =
  let h = l.height_cell in
  match d with
  | 0 -> h / 8.
  | 1 -> h / 20.
  | 2 -> h / 40.
  | _ -> h / 80.

let line_color_of_depth d =
  match d with
  | 0 -> "wheat"
  | 1 -> "grey80"
  | 2 -> "grey65"
  | 3 -> "grey50"
  | _ -> "grey30"

(*****************************************************************************)
(* Matrix Coord -> XY Coord  *)
(*****************************************************************************)

let rect_of_cell i j l =
  (* the matrix is accessed as matrix.(row).(col), but here y corresponds
   * to the row, and x to the column, hence the association of j to x
   * and i to y.
   *)
  let x = (float_of_int j) * l.width_cell + l.x_start_matrix_left in
  let y = (float_of_int i) * l.height_cell + l.y_start_matrix_up in
  { p = { x = x; y = y; };
    q = { x = x + l.width_cell; y = y + l.height_cell };
  }
    
let rect_of_column j l =
  let x = (float_of_int j) * l.width_cell + l.x_start_matrix_left in
  let y = l.y_start_matrix_up in
  { p = { x; y };
    q = { x = x + l.width_cell; y = l.y_end_matrix_down }
  }

let rect_of_line i l =
  let x = l.x_start_matrix_left in
  let y = (float_of_int i) * l.height_cell + l.y_start_matrix_up in
  { p = { x; y };
    q = { x = l.x_end_matrix_right; y = y + l.height_cell }
  }


let rect_of_label_left i l =
  let x = 0.0 in
  let y = (float_of_int i) * l.height_cell + l.y_start_matrix_up in
  { p = { x = x; y = y; };
    q = { x = l.x_start_matrix_left; y = y + l.height_cell };
  }

(*****************************************************************************)
(* Drawing helpers *)
(*****************************************************************************)

let draw_cells (ctx: Canvas_helpers.context) w ~interactive_regions =
  let l = M.layout_of_w w in

  for i = 0 to l.nb_elts -.. 1 do
    for j = 0 to l.nb_elts -.. 1 do
      let rect = rect_of_cell i j l in
      Common.push (Cell (i, j), rect) interactive_regions;
      
      (* less: could also display intra dependencies *)
      if i = j then
        ctx#fill_rectangle ~color:"wheat" rect
      else begin
        (* old: this is now done in draw_left_rows
         *  let _line_width = line_width_of_depth l depth in
         *  CairoH.draw_rectangle ~cr ~line_width ~color:"wheat" rect; 
         *)
        (* todo: heatmap? *)
        let n = w.m.DM.matrix.(i).(j) in
        if n > 0 then begin
          let txt = string_of_int n in
          let font_size = 
            match n with
            | _ when n <= 10 -> 
                l.width_cell / 2.
            | _ ->
                l.width_cell / (float_of_int (String.length txt))
          in
          let tw, th = ctx#text_extents_scaled txt ~size:font_size in
          (* pr2 (spf "tw = %f, th = %f" tw th); *)
          (* let tw, th = 0. , 0. in *)

          let x = rect.p.x in
          let y = rect.p.y in
          
          (* note: could also use ctx##textAlign <- Js.string "center"
           * to avoid this contorsion, but that way we are portable
           * with cairo
           *)
          let x = x + (l.width_cell / 2.) - (tw / 2.0) in
          let y = y + (l.height_cell / 2.) + (th / 2.0) in
          ctx#fill_text_scaled ~x ~y ~size:font_size txt;
        end;
      end
    done
  done;
  ()

let draw_left_tree (ctx: Canvas_helpers.context) w ~interactive_regions =
  let l = M.layout_of_w w in
  let font_size_default = 
    min (l.height_cell/1.5) (l.x_start_matrix_left/10.) in

  let i = ref 0 in
  let rec aux depth tree =
    match tree with
    (* a leaf *)
    | DM.Node (node, []) ->
        (* draw box around label *)
        let x = float_of_int depth * l.width_vertical_label in
        let y = (float_of_int !i) * l.height_cell + l.y_start_matrix_up in
        let rect = { 
          p = { x = x; y = y; };
          q = { x = l.x_start_matrix_left; y = y + l.height_cell };
        } in
        let line_width = line_width_of_depth l depth in
        let color = line_color_of_depth depth in
        ctx#draw_rectangle ~line_width ~color rect;

        Common.push (Row !i, rect) interactive_regions;

        (* draw horizontal lines around cells *)
        let rect2 = {
          p = { x = l.x_start_matrix_left; y = y; };
          q = { x = l.x_end_matrix_right; y = y + l.height_cell };
        } in
        ctx#draw_rectangle ~line_width ~color rect2;
       
        (* draw vertical lines around cells *)
        let x' = (float_of_int !i) * l.width_cell + l.x_start_matrix_left in
        let y'  = l.y_start_matrix_up in
        let rect3 = {
          p = { x = x'; y = y'; };
          q = { x = x' + l.width_cell; y = l.y_end_matrix_down};
        } in
        ctx#draw_rectangle ~line_width ~color rect3;

        (* old: let node = Hashtbl.find w.m.DM.i_to_name i in *)
        let color = color_of_node node in
        let txt = txt_of_node node in
        let tw, _ = ctx#text_extents_scaled txt ~size:font_size_default in

        let width_for_label = l.x_start_matrix_left - x in
        (* todo: could try different settings until it works? like in cm? *)
        let font_size_final =
          if tw > width_for_label 
          then (font_size_default / (tw / width_for_label))
          else font_size_default
        in

        (* align text on the left *)
        let _, th = ctx#text_extents_scaled txt ~size:font_size_final in
        let x = (x + 0.002) in
        let y = (y + (l.height_cell /2.) + (th / 2.0)) in
        ctx#fillStyle color;
        ctx#fill_text_scaled ~x ~y ~size:font_size_final txt;
        incr i

    (* a node, draw the label vertically *)
    | DM.Node (node, xs) ->
        let x = float_of_int depth * l.width_vertical_label in
        let y = (float_of_int !i) * l.height_cell + l.y_start_matrix_up in
        let n = float_of_int (List.length (DM.final_nodes_of_tree tree)) in
        let rect = {
          p = { x; y; };
          q = { x = x + l.width_vertical_label; y = y + n * l.height_cell};
        } in

        let line_width = line_width_of_depth l depth in
        ctx#draw_rectangle ~line_width ~color:"SteelBlue2" rect;
        (* todo? push2 ?? interactive_regions *)

        let color = color_of_node node in
        ctx#fillStyle color;
        let txt = txt_of_node node in
        let font_size_default = 
          min (l.width_vertical_label/1.5) ((n * l.height_cell) /10.) in

        let tw,_ = ctx#text_extents_scaled txt ~size:font_size_default in

        let width_for_label = n * l.height_cell in
        (* todo: could try different settings until it works? like in cm? *)
        let font_size_final =
          if tw > width_for_label 
          then (font_size_default / (tw / width_for_label))
          else font_size_default
        in

        (* center the text *)
        let tw, th = ctx#text_extents_scaled txt ~size:font_size_final in
        let angle = -. (Common2.pi / 2.) in
        let x = ((x + l.width_vertical_label / 2.) + (th / 2.0)) in
        let y = (y + ((n * l.height_cell) /2.) + (tw / 2.0)) in
        ctx#fill_text_scaled ~x ~y ~size:font_size_final
          ~rotate:angle txt;

        xs +> List.iter (aux (depth +.. 1))
  in
  (* use dm.config, not w.config which is not necessaraly ordered *)
  let config = w.m.DM.config in
  (match config with
  | DM.Node (_root, xs) -> xs +> List.iter (aux 0)
  )

let draw_up_columns (ctx: Canvas_helpers.context) w ~interactive_regions =
  let l = M.layout_of_w w in
  let font_size_default = min (l.height_cell/1.5) (l.x_start_matrix_left/10.) in

  (* peh because it exercises the spectrum of high letters *)
  let _, th = ctx#text_extents_scaled ~size:font_size_default "peh" in

  (* not -.. 1, cos we draw lines here, not rectangles *)
  for j = 0 to l.nb_elts do
    let x = (float_of_int j) * l.width_cell + l.x_start_matrix_left in
    let y = l.y_start_matrix_up in
    let rect = {
      (* fake rectangle *)
      p = { x = x; y = 0. };
      q = { x = x + l.width_cell; y = l.y_start_matrix_up };
    } in
    Common.push (Column j, rect) interactive_regions;

    ctx#strokeStyle "wheat";
    let ctx2 = ctx#canvas_ctx in
    ctx2##moveTo(x, y);
    (* because of the xy_ratio, this actually does not do a 45 deg line.
     * old: Cairo.line_to cr (x + (y_start_matrix_up / atan (pi / 4.)))  0.; 
     *)
    ctx2##lineTo (x + (l.y_start_matrix_up / atan (Common2.pi / 2.8)),   0.); 
    ctx2##stroke();

    if j < l.nb_elts then begin
      let node = w.m.DM.i_to_name.(j) in
      let x = (x + (l.width_cell / 2.0) + (th / 2.0)) in
      let y = (y - 0.001) in
      let angle = -. (Common2.pi / 4.) in
      let color = color_of_node node in
      let txt = txt_of_node node in
      ctx#fillStyle color;
      ctx#fill_text_scaled ~rotate:angle ~x ~y ~size:font_size_default txt;
      ()
    end;
  done;
  ()

(*****************************************************************************)
(* Drawing entry point *)
(*****************************************************************************)

let draw_matrix (ctx: Canvas_helpers.context) w =

  (* clear the screen *)
  ctx#fill_rectangle_xywh ~x:0.0 ~y:0.0 ~w:xy_ratio ~h:1.0
    ~color:"DarkSlateGray" ();

  let l = M.layout_of_w w in

  (* draw matrix enclosing rectangle *)
  ctx#draw_rectangle ~line_width:0.001 ~color:"wheat"
    { p = { x = l.x_start_matrix_left; y = l.y_start_matrix_up };
      q = { x = l.x_end_matrix_right; y = l.y_end_matrix_down };
    };

  let interactive_regions = ref [] in
  draw_cells      ctx w ~interactive_regions;
  draw_left_tree  ctx w ~interactive_regions;
  draw_up_columns ctx w ~interactive_regions;

  w.interactive_regions <- !interactive_regions;

  ()
