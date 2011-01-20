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

open Common

module Controller = Controller2

module L = Layer_code
module M = Model2

open Model2

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let choose_layer ~root layer_title_opt dw_ref =
  let dw = !dw_ref in

  let original_layers = dw.M.layers.L.layers +> List.map fst in
  let layers_idx = 
    Layer_code.build_index_of_layers 
      ~root
      (original_layers +> List.map (fun layer ->
        layer, 
        match layer_title_opt with
        | None -> false
        | Some title -> title =$= layer.L.title
      ))
  in
  dw_ref := 
    Model2.init_drawing 
      ~width:dw.width
      ~height:dw.height
      ~width_minimap:dw.width_minimap
      ~height_minimap:dw.height_minimap
      dw.treemap_func 
      dw.dw_model 
      layers_idx
      [root];
  View_mainmap.paint !dw_ref;
  !Controller._refresh_da ();
  !Controller._refresh_legend ();
  ()
