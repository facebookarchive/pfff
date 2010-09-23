(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

exception FT_Error of int
let _ = Callback.register_exception "FT_exn" (FT_Error 0)

type ft_library
type ft_face

external init_freetype : unit -> ft_library = "ml_FT_Init_FreeType"
external done_freetype : ft_library -> unit = "ml_FT_Done_FreeType"

external new_face  : ft_library -> ?index:int -> string -> ft_face = "ml_FT_New_Face"
external done_face : ft_face -> unit = "ml_FT_Done_Face"

type fc_pattern
external fc_name_parse   : ?options:Cairo.Font_Options.t -> string -> fc_pattern = "ml_FcNameParse"
external fc_name_unparse : fc_pattern -> string = "ml_FcNameUnparse"

type font_face = [`Any|`FT] Cairo.font_face

external font_face_create_for_pattern : fc_pattern -> font_face     = "ml_cairo_ft_font_face_create_for_pattern"
external font_face_create_for_ft_face : ft_face -> int -> font_face = "ml_cairo_ft_font_face_create_for_ft_face"

let downcast_font_face f =
  match Cairo.font_face_get_type f with
  | `FT -> (Obj.magic f : font_face)
  | _ -> invalid_arg "Cairo_ft: font face downcast"
let downcast_scaled_font sf =
  match Cairo.Scaled_Font.get_type sf with
  | `FT -> (Obj.magic sf : [`Any|`FT] Cairo.Scaled_Font.t)
  | _ -> invalid_arg "Cairo_ft: scaled font downcast"

external font_lock_face   : [> `FT] Cairo.Scaled_Font.t -> ft_face = "ml_cairo_ft_scaled_font_lock_face"
external font_unlock_face : [> `FT] Cairo.Scaled_Font.t -> unit    = "ml_cairo_ft_scaled_font_unlock_face"
