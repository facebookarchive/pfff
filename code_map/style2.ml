(*s: style2.ml *)
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

module E = Entity_code
module HC = Highlight_code
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
let threshold_draw_dark_background_font_size_real = 5.
(*e: threshold_draw_dark_background_font_size_real *)

let font_size_filename_cursor = 30.

(* see also Cairo_helpers.prepare_string which may double the spaces *)
let font_text = 
  match 0 with
  | 0 -> "serif"

  | 1 -> "helvetica"
  | 2 -> "courier"
  | 3 -> "arial"
  | 4 -> "consolas"
  | 5 -> "dejavu"
  | 6 -> "terminal"
  | _ -> raise Impossible

(*s: size_font_multiplier_of_categ() *)
let multiplier_use x = 
  match x with
  | HC.HugeUse -> 3.3
  | HC.LotsOfUse -> 2.7
  | HC.MultiUse -> 2.1
  | HC.SomeUse -> 1.7
  | HC.UniqueUse -> 1.3
  | HC.NoUse -> 0.9

let size_font_multiplier_of_categ ~font_size_real categ =
  match categ with
  | None -> 1.
  | Some x ->
    (match x with
    (* entities defs *)

    | HC.Entity (E.Module, (HC.Def2 use))   -> 5. *. multiplier_use use
    | HC.Entity (E.Type, (HC.Def2 use))     -> 5. *. multiplier_use use
    | HC.Entity (E.Class, (HC.Def2 use))    -> 5. *. multiplier_use use

    | HC.Entity (E.Function, (HC.Def2 use)) -> 3.5 *. multiplier_use use
    | HC.Entity (E.Method, (HC.Def2 use))   -> 3.5 *. multiplier_use use
    | HC.Entity (E.Global, (HC.Def2 use))   -> 3. *. multiplier_use use

    | HC.Entity (E.Constant, (HC.Def2 use)) -> 2. *. multiplier_use use
    | HC.Entity (E.Exception, (HC.Def2 use))    -> 2. *. multiplier_use use
    | HC.Entity (E.Macro, (HC.Def2 use))    -> 2. *. multiplier_use use
    | HC.Entity (E.Field, (HC.Def2 use))    -> 1.7 *. multiplier_use use
    | HC.Entity (E.Constructor, (HC.Def2 use)) -> 1.2 *. multiplier_use use

    | HC.FunctionDecl use -> 2.5 *. multiplier_use use
    | HC.StaticMethod (HC.Def2 use) -> 3.5 *. multiplier_use use
    | HC.StructName (HC.Def) -> 3.

    | HC.GrammarRule -> 2.5
        
    (* entities uses *)
    | HC.Entity (E.Global, (HC.Use2 _)) when font_size_real > 7.
          -> 1.5

    (* | HC.Method (HC.Use2 _) when font_size_real > 7.  -> 1.2 *)
        
    (* "literate programming" *)
    | HC.CommentSection0 -> 5.
    | HC.CommentSection1 -> 3.
    | HC.CommentSection2 -> 2.0
    | HC.CommentSection3 -> 1.2
    | HC.CommentSection4 -> 1.1
    | HC.CommentEstet -> 1.0
    | HC.CommentCopyright -> 0.5

    | HC.CommentSyncweb -> 1.

 (* | HC.Comment when font_size_real > 7. -> 1.5 *)

    (* semantic visual feedback *)

    | HC.BadSmell -> 2.5

    (* ocaml *)
    | HC.UseOfRef -> 2.

    (* php, C, etc *)
    | HC.PointerCall -> 5.
    | HC.ParameterRef -> 2.
    | HC.CallByRef -> 3.

    (* misc *)
    | HC.Local (HC.Def) -> 1.2
        
    | _ -> 
        (* the cases above should have covered all the cases *)
        if Database_code.is_entity_def_category x
        then failwith "You should update size_font_multiplier_of_categ";

        1. 
    )
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
