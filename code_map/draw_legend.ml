(*s: draw_legend.ml *)
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

module CairoH = Cairo_helpers
module L = Layer_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let draw_legend_of_color_string_pairs ~cr xs = 

  Cairo.select_font_face cr "serif" 
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
  let size = 25.  in

  Cairo.set_font_size cr (size * 0.6);
  Cairo.set_source_rgba cr 0. 0. 0.    1.0;
  
  xs +> Common.index_list_1 +> List.iter (fun ((color,s), i) ->
    let x = 10. in
    let y = float_of_int i * size in

    let w = size in
    let h = size in

    CairoH.fill_rectangle ~cr ~color ~x ~y ~w ~h ();
    Cairo.set_source_rgba cr 0. 0. 0.    1.0;
    Cairo.move_to cr (x + size * 2.) (y + size * 0.8);
    Cairo.show_text cr s;
  )
  
(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

(*s: paint_legend *)
(* todo: make the architecture a layer so no need for special case *)
let draw_legend ~cr =

  let archis = Archi_code.source_archi_list in
  let grouped_archis = archis +> Common.group_by_mapped_key (fun archi ->
    (* I tend to favor the darker variant of the color in treemap_pl.ml hence
     * the 3 below
     *)
    Treemap_pl.color_of_source_archi archi ^ "3"
  )
  in
  let grouped_archis = grouped_archis +> List.map (fun (color, kinds) ->
    color, kinds +> List.map Archi_code.s_of_source_archi +> Common.join ", "
  ) in
  draw_legend_of_color_string_pairs ~cr grouped_archis

(*e: paint_legend *)

let draw_legend_layer ~cr layers_idx = 
  let pairs = 
    layers_idx.L.layers +> Common.map_filter (fun (layer, is_active) ->
      if is_active
      then Some layer.L.kinds
      else None
    ) +> List.flatten +> List.map (fun (a, b) -> (b, a))
  in
  draw_legend_of_color_string_pairs ~cr pairs

(*e: draw_legend.ml *)
