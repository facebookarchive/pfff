(*s: draw_legend.ml *)
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

open Common.ArithFloatInfix

module CairoH = Cairo_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

(*s: paint_legend *)
let draw_legend ~cr =

  Cairo.select_font_face cr "serif" 
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
  let size = 25.  in

  Cairo.set_font_size cr (size * 0.6);

  Cairo.set_source_rgba cr 0. 0. 0.    1.0;
  
  let archis = Archi_code.source_archi_list in

  let grouped_archis = archis +> Common.group_by_mapped_key (fun archi ->
    let color = Treemap_pl.color_of_source_archi archi in
    (* I tend to favor the darker variant of the color in treemap_pl.ml hence
     * the 3 below
     *)
    let color = color ^ "3" in
    color
  )
  in
  
  grouped_archis +> Common.index_list_1 +> List.iter (fun ((color,kinds), i) ->
    
    let x = 10. in
    let y = float_of_int i * size in

    let w = size in
    let h = size in

    CairoH.fill_rectangle ~cr ~color ~x ~y ~w ~h ();

    let s = 
      kinds +> List.map Archi_code.s_of_source_archi +> Common.join ", " in

    Cairo.set_source_rgba cr 0. 0. 0.    1.0;
    Cairo.move_to cr (x + size * 2.) (y + size * 0.8);
    Cairo.show_text cr s;
  );
  ()
(*e: paint_legend *)

(*e: draw_legend.ml *)
