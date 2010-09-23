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

(* $Id: pango.ml 1501 2010-04-11 21:07:18Z oandrieu $ *)

open Gaux
open Gobject

type context = [`pangocontext] obj
type font = [`pangofont] obj
type font_map = [`pangofontmap] obj
type font_description
type font_metrics
type language
type layout = [`pangolayout] obj
type units = int
type rectangle = {x:int; y:int; width:int; height:int}

external _pango_init : unit -> unit = "ml_pango_init"
let () = _pango_init ()

module Tags = struct
  type style =
      [ `NORMAL | `OBLIQUE | `ITALIC ]

  type weight_internal =
      [ `ULTRALIGHT | `LIGHT | `NORMAL |`BOLD | `ULTRABOLD |`HEAVY ]
  type weight = [ weight_internal | `CUSTOM of int]

  type variant =
        [ `NORMAL | `SMALL_CAPS ]
    
  type stretch =
      [ `ULTRA_CONDENSED | `EXTRA_CONDENSED 
      | `CONDENSED | `SEMI_CONDENSED 
      | `NORMAL | `SEMI_EXPANDED
      | `EXPANDED | `EXTRA_EXPANDED | `ULTRA_EXPANDED ]

  type scale =
      [ `XX_SMALL | `X_SMALL | `SMALL | `MEDIUM 
      | `LARGE | `X_LARGE | `XX_LARGE
      | `CUSTOM of float ]
  external scale_to_float : scale -> float = "ml_Pango_scale_val"

  type underline = [ `NONE | `SINGLE | `DOUBLE | `LOW ]

  type wrap_mode = [ `WORD | `CHAR | `WORD_CHAR ]

  type ellipsize_mode = [ `NONE | `START | `MIDDLE | `END ]

  open Gpointer
  external _get_tables : unit ->
      style variant_table
    * weight_internal variant_table
    * variant variant_table
    * stretch variant_table
    * underline variant_table
    * wrap_mode variant_table
    * ellipsize_mode variant_table
    = "ml_pango_get_tables"

  let style, weight, variant, stretch, underline, wrap_mode,
    ellipsize_mode = _get_tables ()

  let weight_to_int (w : weight) =
    match w with 
      | `CUSTOM b -> b 
      | #weight_internal as w -> encode_variant weight w
end

module Font = struct
  open Tags
  external from_string : string -> font_description = 
    "ml_pango_font_description_from_string"
  external to_string : font_description -> string = 
    "ml_pango_font_description_to_string"
  external copy : font_description -> font_description = 
    "ml_pango_font_description_copy"
  external set_family : font_description -> string -> unit =
    "ml_pango_font_description_set_family"
  external get_family : font_description -> string =
    "ml_pango_font_description_get_family"
  external set_style : font_description -> style -> unit =
    "ml_pango_font_description_set_style"
  external get_style : font_description -> style =
    "ml_pango_font_description_get_style"
  external set_variant : font_description -> variant -> unit =
    "ml_pango_font_description_set_variant"
  external get_variant : font_description -> variant =
    "ml_pango_font_description_get_variant"
  external set_weight : font_description -> int -> unit =
    "ml_pango_font_description_set_weight"
  let set_weight fd w = set_weight fd (weight_to_int w)
  external get_weight : font_description -> int =
    "ml_pango_font_description_get_weight"
  external set_stretch : font_description -> stretch -> unit =
    "ml_pango_font_description_set_stretch"
  external get_stretch : font_description -> stretch =
    "ml_pango_font_description_get_stretch"
  external set_size : font_description -> int -> unit =
    "ml_pango_font_description_set_size"
  external get_size : font_description -> int =
    "ml_pango_font_description_get_size"
  let modify fd ?family ?style ?variant ?weight ?stretch ?size () =
    let may_set set_x x = may x ~f:(set_x fd) in
    may_set set_family family;
    may_set set_style style;
    may_set set_stretch stretch;
    may_set set_variant variant;
    may_set set_weight weight;
    may_set set_size size

  external get_metrics : font -> language -> font_metrics =
    "ml_pango_font_get_metrics"

  external get_ascent : font_metrics -> units =
    "ml_pango_font_metrics_get_ascent"
  external get_descent : font_metrics -> units =
    "ml_pango_font_metrics_get_descent"
  external get_approximate_char_width : font_metrics -> units =
    "ml_pango_font_metrics_get_approximate_char_width"
  external get_approximate_digit_width : font_metrics -> units =
    "ml_pango_font_metrics_get_approximate_digit_width"
end

module FontMap = struct
  external load_font : font_map -> context -> font_description -> font
    = "ml_pango_font_map_load_font"
end

module Language = struct
  external from_string : string -> language = "ml_pango_language_from_string"
  external to_string : language -> string = "ml_pango_language_to_string"
  external matches : language -> string -> bool = "ml_pango_language_matches"
  let none : language = Obj.magic Gpointer.boxed_null
end

module Context = struct
  let cast w : context = Gobject.try_cast w "PangoContext"
  external get_font_description : context -> font_description =
    "ml_pango_context_get_font_description"
  external set_font_description : context -> font_description -> unit =
    "ml_pango_context_set_font_description"
  external get_language : context -> language =
    "ml_pango_context_get_language"
  external set_language : context -> language -> unit =
    "ml_pango_context_set_language"
  external load_font : context -> font_description -> font =
    "ml_pango_context_load_font"
  external load_fontset : context -> font_description -> language -> font =
    "ml_pango_context_load_fontset"
  external get_metrics :
    context -> font_description -> language option -> font_metrics =
    "ml_pango_context_get_metrics"
end

external scale : unit -> int = "ml_PANGO_SCALE"
let scale = scale ()

module Layout = struct
  open Tags
  let cast w : layout = Gobject.try_cast w "PangoLayout"
  external create : context -> layout = "ml_pango_layout_new"
  external copy : layout -> layout = "ml_pango_layout_copy"
  external get_context : layout -> context = "ml_pango_layout_get_context"
  external get_text : layout -> string = "ml_pango_layout_get_text"
  external set_text : layout -> string -> unit = "ml_pango_layout_set_text"
  external set_markup : layout -> string -> unit = "ml_pango_layout_set_markup"
  external set_markup_with_accel : layout -> string -> Glib.unichar -> unit
    = "ml_pango_layout_set_markup_with_accel"
  external set_font_description : layout -> font_description -> unit
    = "ml_pango_layout_set_font_description"
  external get_width : layout -> int = "ml_pango_layout_get_width"
  external set_width : layout -> int -> unit = "ml_pango_layout_set_width"
  external get_indent : layout -> int = "ml_pango_layout_get_indent"
  external set_indent : layout -> int -> unit = "ml_pango_layout_set_indent"
  external get_spacing : layout -> int = "ml_pango_layout_get_spacing"
  external set_spacing : layout -> int -> unit = "ml_pango_layout_set_spacing"
  external get_wrap : layout -> wrap_mode = "ml_pango_layout_get_wrap"
  external set_wrap : layout -> wrap_mode -> unit = "ml_pango_layout_set_wrap"
  external get_justify : layout -> bool = "ml_pango_layout_get_justify"
  external set_justify : layout -> bool -> unit = "ml_pango_layout_set_justify"
  external get_single_paragraph_mode : layout -> bool
    = "ml_pango_layout_get_single_paragraph_mode"
  external set_single_paragraph_mode : layout -> bool -> unit
    = "ml_pango_layout_set_single_paragraph_mode"
  external context_changed : layout -> unit = "ml_pango_layout_context_changed"
  external get_size : layout -> units * units = "ml_pango_layout_get_size"
  external get_pixel_size : layout -> int * int
    = "ml_pango_layout_get_pixel_size"
  external get_extent : layout -> rectangle = "ml_pango_layout_get_extent"
  external get_pixel_extent : layout -> rectangle
    = "ml_pango_layout_get_pixel_extent"
  external index_to_pos : layout -> int -> rectangle
    = "ml_pango_layout_index_to_pos"
  external xy_to_index : layout -> x:int -> y:int -> int * int * bool
    = "ml_pango_layout_xy_to_index"
  external set_ellipsize : layout -> ellipsize_mode -> unit
    = "ml_pango_layout_set_ellipsize"
  external get_ellipsize : layout -> ellipsize_mode
    = "ml_pango_layout_get_ellipsize"
end
