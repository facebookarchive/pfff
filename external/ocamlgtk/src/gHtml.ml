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

(* $Id: gHtml.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gaux
open Gtk
open GtkBase
open GObj
open GtkXmHTML

class xmhtml obj = object (self)
  inherit widget_full (obj : GtkXmHTML.xmhtml obj)
  method event = new GObj.event_ops obj
  method freeze = freeze obj
  method thaw = thaw obj
  method source = source obj
  method set_fonts = set_font_familty obj
  method set_fonts_fixed = set_font_familty_fixed obj
  method set_anchor_buttons = set_anchor_buttons obj
  method set_anchor_cursor = set_anchor_cursor obj
  method set_anchor_underline = set_anchor_underline_type obj
  method set_anchor_visited_underline = set_anchor_visited_underline_type obj
  method set_anchor_target_underline = set_anchor_target_underline_type obj
  method set_topline = set_topline obj
  method topline = get_topline obj
  method set_strict_checking = set_strict_checking obj
  method set_bad_html_warnings = set_bad_html_warnings obj
  method set_imagemap_draw = set_imagemap_draw obj
end

let xmhtml ?source ?border_width ?width ?height ?packing ?show () =
  let w = create () in
  Container.set w ?border_width ?width ?height;
  may source ~f:(GtkXmHTML.source w);
  pack_return (new xmhtml w) ~packing ~show
