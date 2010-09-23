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

(* $Id: gPango.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gaux
open Pango
open Font

let to_pixels (x : units) = (x-1) / scale + 1
let from_pixels x : units = x * scale

class metrics obj = object
  method ascent = get_ascent obj
  method descent = get_descent obj
  method approx_char_width = get_approximate_char_width obj
  method approx_digit_width = get_approximate_digit_width obj
end

class font_description fd = object
  method fd = fd
  method copy = new font_description (copy fd)
  method to_string = to_string fd
  method family = get_family fd
  method style = get_style fd
  method variant = get_variant fd
  method weight = get_weight fd
  method stretch = get_stretch fd
  method size = get_size fd
  method modify = modify fd
end

let font_description = from_string

open Context

class context obj = object (self)
  val obj = obj
  method as_context = obj
  method font_description = get_font_description obj
  method font_name = Font.to_string (get_font_description obj)
  method language = Language.to_string (get_language obj)
  method load_font desc = load_font obj (Font.from_string desc)
  method load_fontset
      ?(desc = self#font_description) ?(lang = self#language) () =
    load_fontset obj desc (Language.from_string lang)
  method get_metrics
      ?(desc = self#font_description) ?(lang = self#language) () =
    new metrics (get_metrics obj desc (Some (Language.from_string lang)))
  method create_layout = Layout.create obj
end

class context_rw obj = object
  inherit context obj
  method set_font_description desc =
    set_font_description obj desc
  method set_font_by_name desc =
    set_font_description obj (Font.from_string desc)
  method set_language lang = set_language obj (Language.from_string lang)
end
