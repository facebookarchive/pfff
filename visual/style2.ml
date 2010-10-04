(*s: style2.ml *)
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

module SH = Highlight_code

module Flag = Flag_visual

(*****************************************************************************)
(* Visual style *)
(*****************************************************************************)
(* see also model2.settings *)

(*s: zoom_factor_incruste_mode *)
(* TODO: should be automatically computed. Should have instead a
 * wanted_real_font_size_when_incruste_mode = 9.
 *)
let zoom_factor_incruste_mode = 10. (* was 18 *)
(*e: zoom_factor_incruste_mode *)

(*s: threshold_draw_dark_background_font_size_real *)
(* CONFIG *)
let threshold_draw_dark_background_font_size_real = 1.
(*e: threshold_draw_dark_background_font_size_real *)

(*s: size_font_multiplier_of_categ() *)
let multiplier_use x = 
  match x with
  | SH.HugeUse -> 3.3
  | SH.LotsOfUse -> 2.7
  | SH.MultiUse -> 2.1
  | SH.SomeUse -> 1.7
  | SH.UniqueUse -> 1.3
  | SH.NoUse -> 0.9

let size_font_multiplier_of_categ ~font_size_real categ =
    match categ with
    | Some (SH.Class SH.Def2 use) -> 5. *. multiplier_use use
    | Some (SH.Module SH.Def) -> 5.
    | Some (SH.Function (SH.Def2 use)) -> 3.5 *. multiplier_use use
    | Some (SH.TypeDef SH.Def) -> 5.
    | Some (SH.Global (SH.Def2 use)) -> 3. *. multiplier_use use
    | Some (SH.FunctionDecl use) -> 2.5 *. multiplier_use use
    | Some (SH.Macro (SH.Def2 use)) -> 2. *. multiplier_use use
    | Some (SH.MacroVar (SH.Def2 use)) -> 2. *. multiplier_use use
    | Some (SH.Method (SH.Def2 use)) -> 3.5 *. multiplier_use use
    | Some (SH.StaticMethod (SH.Def2 use)) -> 3.5 *. multiplier_use use
    | Some (SH.Field (SH.Def2 use)) -> 1.7 *. multiplier_use use
        
    | Some (SH.Global (SH.Use2 _)) when font_size_real > 7.
          -> 1.5
    | Some (SH.Method (SH.Use2 _)) when font_size_real > 7.
          -> 1.5

        

    | Some (SH.CommentSection0) -> 5.
    | Some (SH.CommentSection1) -> 4.
    | Some (SH.CommentSection2) -> 3.5
    | Some (SH.CommentEstet) -> 1.0
    | Some (SH.CommentCopyright) -> 0.5

    | Some (SH.CommentSyncweb) -> 1.

(*
    | Some (SH.Comment) when font_size_real > 7.
          -> 1.5
*)

    | Some (SH.BadSmell) -> 2.5

    (* ocaml *)
    | Some (SH.UseOfRef) -> 2.
    (* php, C, etc *)
    | Some (SH.PointerCall) -> 3.
        
    | _ -> 
        (* the cases above should have covered all the cases *)
        categ +> Common.do_option (fun categ ->
          if Highlight_code.is_entity_def_category categ
          then failwith "You should update size_font_multiplier_of_categ";
        );


        1. 
(*e: size_font_multiplier_of_categ() *)

(*s: windows_params() *)
let windows_params screen_size =
  let width, height, minimap_hpos, minimap_vpos = 
    match screen_size with
    | 1 ->
        1350, 800, 1100, 150
    | 2 ->
        2560, 1580, 2350, 100 (* was 2200 and 280 *)
    | 3 ->
        7000, 4000, 6900, 100
    | 4 ->
        16000, 9000, 15900, 100
    | 5 ->
        20000, 12000, 19900, 100
    | 6 ->
        25000, 15000, 24900, 100
    | _ ->
        failwith "not valid screen_size"
  in
  width, height, minimap_hpos, minimap_vpos
(*e: windows_params() *)

(*e: style2.ml *)
