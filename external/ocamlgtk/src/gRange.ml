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

(* $Id: gRange.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gaux
open Gtk
open GtkBase
open GtkRange
open OgtkRangeProps
open GObj

class progress_bar obj = object
  inherit [Gtk.progress_bar] widget_impl obj
  method connect = new widget_signals_impl obj
  method event = new GObj.event_ops obj
  inherit progress_bar_props
  method pulse () = ProgressBar.pulse obj
end

let progress_bar =
  ProgressBar.make_params [] ~cont:(fun pl ?packing ?show () ->
    pack_return (new progress_bar (ProgressBar.create pl)) ~packing ~show)

class range_signals obj = object
  inherit widget_signals_impl obj
  inherit range_sigs
end

class range obj = object
  inherit ['a] widget_impl obj
  method connect = new range_signals obj
  method event = new GObj.event_ops obj
  inherit range_props
end

class scale obj = object
  inherit range (obj : Gtk.scale obj)
  inherit scale_props
end

let scale dir ?adjustment =
  Scale.make_params [] ~cont:(
  Range.make_params ?adjustment:(may_map GData.as_adjustment adjustment)
    ~cont:(fun pl ?packing ?show params ->
      pack_return (new scale (Scale.create dir pl)) ~packing ~show))

let scrollbar dir ?adjustment =
  Range.make_params [] ?adjustment:(may_map GData.as_adjustment adjustment)
    ~cont:(fun pl ?packing ?show params ->
      pack_return (new range (Scrollbar.create dir pl)) ~packing ~show)

class ruler obj = object
  inherit ['a] widget_impl obj
  method connect = new widget_signals_impl obj
  method event = new GObj.event_ops obj
  inherit ruler_props
  method set_metric = Ruler.set_metric obj
end

let ruler dir ?metric =
  Ruler.make_params [] ~cont:(fun pl ?packing ?show params ->
    let w = new ruler (Ruler.create dir pl) in
    may w#set_metric metric;
    pack_return w ~packing ~show)
