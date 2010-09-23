(**************************************************************************)
(*                Lablgtk                                                 *)
(*                                                                        *)
(*    This program is free software; you can redistribute it              *)
(*    and/or modify it under the terms of the GNU Library General         *)
(*    Public License as published by the Free Software Foundation         *)
(*    version 2, with the exception described in file COPYING which       *)
(*    comes with the library.                                             *)
(*                                                                        *)
(*    This program is distributed in the hope that it will be useful,     *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU Library General Public License for more details.                *)
(*                                                                        *)
(*    You should have received a copy of the GNU Library General          *)
(*    Public License along with this program; if not, write to the        *)
(*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         *)
(*    Boston, MA 02111-1307  USA                                          *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: gtkXmHTML.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gtk

type string_direction = [
  | `R_TO_L
  | `L_TO_R
]

type position = [
  | `END
  | `CENTER
  | `BEGINNING
]

type line_type = [
  | `SOLID
  | `DASHED
  | `SINGLE
  | `DOUBLE
  | `STRIKE
  | `UNDER
  | `NONE
]

type dither_type = [
  | `QUICK
  | `BEST
  | `FAST
  | `SLOW
  | `DISABLED
]

type xmhtml = [widget|`container|`xmhtml]

external create : unit -> xmhtml obj = "ml_gtk_xmhtml_new"
external freeze : [> `xmhtml] obj -> unit = "ml_gtk_xmhtml_freeze"
external thaw : [> `xmhtml] obj -> unit = "ml_gtk_xmhtml_thaw"
external source : [> `xmhtml] obj -> string -> unit = "ml_gtk_xmhtml_source"
(* external get_source : [> `xmhtml] obj -> string = "ml_gtk_xmhtml_get_source" *)
external set_string_direction : [> `xmhtml] obj -> string_direction -> unit
  = "ml_gtk_xmhtml_set_string_direction"
external set_alignment : [> `xmhtml] obj -> position -> unit
  = "ml_gtk_xmhtml_set_alignment"
(* external set_outline : [> `xmhtml] obj -> bool -> unit
  = "ml_gtk_xmhtml_outline" *)
external set_font_familty :
  [> `xmhtml] obj -> family:string -> sizes:string -> unit
  = "ml_gtk_xmhtml_set_font_familty"
external set_font_familty_fixed :
  [> `xmhtml] obj -> family:string -> sizes:string -> unit
  = "ml_gtk_xmhtml_set_font_familty_fixed"
external set_font_charset : [> `xmhtml] obj -> string -> unit
  = "ml_gtk_xmhtml_set_font_charset"
external set_allow_body_colors : [> `xmhtml] obj -> bool -> unit
  = "ml_gtk_xmhtml_set_allow_body_colors"
external set_hilight_on_enter : [> `xmhtml] obj -> bool -> unit
  = "ml_gtk_xmhtml_set_hilight_on_enter"
external set_anchor_underline_type : [> `xmhtml] obj -> line_type list -> unit
  = "ml_gtk_xmhtml_set_anchor_underline_type"
external set_anchor_visited_underline_type :
  [> `xmhtml] obj -> line_type list -> unit
  = "ml_gtk_xmhtml_set_anchor_visited_underline_type"
external set_anchor_target_underline_type :
  [> `xmhtml] obj -> line_type list -> unit
  = "ml_gtk_xmhtml_set_anchor_target_underline_type"
external set_allow_color_switching : [> `xmhtml] obj -> bool -> unit
  = "ml_gtk_xmhtml_set_allow_color_switching"
external set_dithering : [> `xmhtml] obj -> dither_type -> unit
  = "ml_gtk_xmhtml_set_dithering"
external set_allow_font_switching : [> `xmhtml] obj -> bool -> unit
  = "ml_gtk_xmhtml_set_allow_font_switching"
external set_max_image_colors : [> `xmhtml] obj -> int -> unit
  = "ml_gtk_xmhtml_set_max_image_colors"
external set_allow_images : [> `xmhtml] obj -> bool -> unit
  = "ml_gtk_xmhtml_set_allow_images"
external set_plc_intervals :
  [> `xmhtml] obj -> min:int -> max:int -> default:int -> unit
  = "ml_gtk_xmhtml_set_plc_intervals"
(*
external set_def_body_image_url : [> `xmhtml] obj -> string -> unit
  = "ml_gtk_xmhtml_set_def_body_image_url"
*)
external set_anchor_buttons : [> `xmhtml] obj -> bool -> unit
  = "ml_gtk_xmhtml_set_anchor_buttons"
external set_anchor_cursor : [> `xmhtml] obj -> Gdk.cursor option -> unit
  = "ml_gtk_xmhtml_set_anchor_cursor"
external set_topline : [> `xmhtml] obj -> int -> unit
  = "ml_gtk_xmhtml_set_topline"
external get_topline : [> `xmhtml] obj -> int
  = "ml_gtk_xmhtml_get_topline"
external set_freeze_animations : [> `xmhtml] obj -> bool -> unit
  = "ml_gtk_xmhtml_set_freeze_animations"
external set_screen_gamma : [> `xmhtml] obj -> float -> unit
  = "ml_gtk_xmhtml_set_screen_gamma"
external set_perfect_colors : [> `xmhtml] obj -> bool -> unit
  = "ml_gtk_xmhtml_set_perfect_colors"
external set_uncompress_command : [> `xmhtml] obj -> string -> unit
  = "ml_gtk_xmhtml_set_uncompress_command"
external set_strict_checking : [> `xmhtml] obj -> bool -> unit
  = "ml_gtk_xmhtml_set_strict_checking"
external set_bad_html_warnings : [> `xmhtml] obj -> bool -> unit
  = "ml_gtk_xmhtml_set_bad_html_warnings"
external set_allow_form_coloring : [> `xmhtml] obj -> bool -> unit
  = "ml_gtk_xmhtml_set_allow_form_coloring"
external set_imagemap_draw : [> `xmhtml] obj -> bool -> unit
  = "ml_gtk_xmhtml_set_imagemap_draw"
external set_alpha_processing : [> `xmhtml] obj -> bool -> unit
  = "ml_gtk_xmhtml_set_alpha_processing"
