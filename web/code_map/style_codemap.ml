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

module SH = Highlight_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* diff with flag_visual? *)

(*****************************************************************************)
(* Visual style *)
(*****************************************************************************)

let font_size_filename_cursor_device_world = 15


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

    (* entities defs *)

    | Some (SH.Class SH.Def2 use) -> 5. *. multiplier_use use
    | Some (SH.Module SH.Def) -> 5.
    | Some (SH.Function (SH.Def2 use)) -> 3.5 *. multiplier_use use
    | Some (SH.TypeDef SH.Def) -> 5.
    | Some (SH.Global (SH.Def2 use)) -> 3. *. multiplier_use use
    | Some (SH.FunctionDecl use) -> 2.5 *. multiplier_use use
    | Some (SH.Macro (SH.Def2 use)) -> 2. *. multiplier_use use
    | Some (SH.Constant (SH.Def2 use)) -> 2. *. multiplier_use use
    | Some (SH.Method (SH.Def2 use)) -> 3.5 *. multiplier_use use
    | Some (SH.StaticMethod (SH.Def2 use)) -> 3.5 *. multiplier_use use
    | Some (SH.Field (SH.Def2 use)) -> 1.7 *. multiplier_use use

    | Some (SH.Constructor (SH.Def2 (use))) -> 1.2 *. multiplier_use use

    | Some (SH.GrammarRule) -> 2.5
        
    (* entities uses *)
    | Some (SH.Global (SH.Use2 _)) when font_size_real > 7.
          -> 1.5
(*
    | Some (SH.Method (SH.Use2 _)) when font_size_real > 7.
          -> 1.2
*)
        
    (* "literate programming" *)
    | Some (SH.CommentSection0) -> 5.
    | Some (SH.CommentSection1) -> 3.
    | Some (SH.CommentSection2) -> 2.0
    | Some (SH.CommentSection3) -> 1.2
    | Some (SH.CommentSection4) -> 1.1
    | Some (SH.CommentEstet) -> 1.0
    | Some (SH.CommentCopyright) -> 0.5

    | Some (SH.CommentSyncweb) -> 1.

(*
    | Some (SH.Comment) when font_size_real > 7.
          -> 1.5
*)

    (* semantic visual feedback *)

    | Some (SH.BadSmell) -> 2.5

    (* ocaml *)
    | Some (SH.UseOfRef) -> 2.

    (* php, C, etc *)
    | Some (SH.PointerCall) -> 3.
    | Some (SH.ParameterRef) -> 2.
    | Some (SH.CallByRef) -> 3.

    (* misc *)
    | Some (SH.Local (SH.Def)) -> 1.2
        
    | _ -> 
        (* the cases above should have covered all the cases *)
        categ +> Common.do_option (fun categ ->
          if Database_code.is_entity_def_category categ
          then failwith "You should update size_font_multiplier_of_categ";
        );


        1. 
