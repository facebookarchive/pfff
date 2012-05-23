(* Yoann Padioleau
 * 
 * Copyright (C) 2012 Facebook
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

module CairoH = Cairo_helpers3

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* The code model *)
(*****************************************************************************)

type model = {
  g: Graph_code.graph;
  root: Common.dirname;
}

(*****************************************************************************)
(* The drawing model *)
(*****************************************************************************)

(* All the 'float' below are to be intepreted as user coordinates except when
 * explicitely mentioned. All the 'int' are usually device coordinates.
 *)
type world = {
  model: model;

  mutable config: Dependencies_matrix_code.config;
  (* cache of Dependencies_matrix_code.build config g *)
  mutable m: Dependencies_matrix_code.dm;

  (* device coordinates *)
  mutable pm: GDraw.pixmap;
  mutable overlay: [ `Any ] Cairo.surface;

  (* viewport, device coordinates *)
  mutable width: int;
  mutable height: int;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* todo: factorize with model2.ml *)
let new_pixmap ~width ~height =
  let drawable = GDraw.pixmap ~width ~height () in
  drawable#set_foreground `WHITE;
  drawable#rectangle ~x:0 ~y:0 ~width ~height ~filled:true () ;
  drawable

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* width/height are a first guess. The first configure ev will force a resize*)
let init_world ?(width = 600) ?(height = 600) config model =
  let m = 
    Common.profile_code2 "Model.building matrix" (fun () -> 
      Dependencies_matrix_code.build config model.g 
    )
  in
  let pm = new_pixmap ~width ~height in
  {
    model; config;
    m;
    width; height;
    pm;
    overlay = Cairo.surface_create_similar (CairoH.surface_of_pixmap pm)
      Cairo.CONTENT_COLOR_ALPHA width height;
  }
