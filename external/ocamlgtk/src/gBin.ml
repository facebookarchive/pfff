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

(* $Id: gBin.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gaux
open Gtk
open GtkBinProps
open GtkBase
open GtkBin
open GObj
open OgtkBinProps
open GContainer

let param = Gobject.param

class scrolled_window obj = object
  inherit [Gtk.scrolled_window] bin_impl obj
  inherit scrolled_window_props
  method connect = new container_signals_impl obj
  method add_with_viewport w =
    ScrolledWindow.add_with_viewport obj (as_widget w)
end

let scrolled_window ?hadjustment ?vadjustment =
  ScrolledWindow.make_params []
    ?hadjustment:(may_map ~f:GData.as_adjustment hadjustment)
    ?vadjustment:(may_map ~f:GData.as_adjustment vadjustment)
    ~cont:(
  pack_container ~create:(fun pl ->
    new scrolled_window (ScrolledWindow.create pl)))

class event_box obj = object
  inherit bin obj
  method connect = new container_signals_impl obj
  method event = new GObj.event_ops (obj :> Gtk.event_box obj)
end

let event_box =
  pack_container [] ~create:(fun pl -> new event_box (EventBox.create pl))

class invisible obj = object
  inherit bin obj
  method connect = new container_signals_impl obj
  method event = new GObj.event_ops (obj :> Gtk.invisible obj)
end

let invisible =
  pack_container [] ~create:(fun pl -> new invisible (Invisible.create pl))

class handle_box_signals (obj : [> handle_box] obj) = object
  inherit container_signals_impl obj
  inherit handle_box_sigs
end

class handle_box obj = object
  inherit [Gtk.handle_box] bin_impl obj
  method connect = new handle_box_signals obj
  method event = new GObj.event_ops obj
  inherit handle_box_props
end

let handle_box =
  HandleBox.make_params [] ~cont:(
  pack_container ~create:(fun pl -> new handle_box (HandleBox.create pl)))

class frame_skel obj = object
  inherit [[> frame]] bin_impl obj
  inherit frame_props
end

class frame obj = object
  inherit frame_skel (obj : Gtk.frame obj)
  method connect = new container_signals_impl obj
end

let frame =
  Frame.make_params [] ~cont:(
  pack_container ~create:(fun pl -> new frame (Frame.create pl)))

class aspect_frame obj = object
  inherit frame_skel (obj : Gtk.aspect_frame obj)
  method connect = new container_signals_impl obj
  inherit aspect_frame_props
end

let aspect_frame =
  AspectFrame.make_params [] ~cont:(
  Frame.make_params ~cont:(
  pack_container ~create:(fun pl -> new aspect_frame (AspectFrame.create pl))))

class viewport obj = object
  inherit [Gtk.viewport] bin_impl obj
  method connect = new container_signals_impl obj
  method event = new event_ops obj
  inherit viewport_props
end

let viewport ?hadjustment ?vadjustment =
  Viewport.make_params []
    ?hadjustment:(may_map ~f:GData.as_adjustment hadjustment)
    ?vadjustment:(may_map ~f:GData.as_adjustment vadjustment) ~cont:(
  pack_container ~create:(fun pl -> new viewport (Viewport.create pl)))

class alignment obj = object
  inherit [Gtk.alignment] bin_impl obj
  method connect = new container_signals_impl obj
  inherit alignment_props
end

let alignment ?padding =
  let pl = match padding with 
  | None -> [] 
  | Some (t, b, l, r) -> [ param Alignment.P.top_padding t ;
			   param Alignment.P.bottom_padding b ;
			   param Alignment.P.left_padding l ;
			   param Alignment.P.right_padding r ] in
  Alignment.make_params pl ~cont:(
  pack_container ~create:(fun pl -> new alignment (Alignment.create pl)))
  
let alignment_cast w = new alignment (Alignment.cast w#as_widget)

class expander_signals obj = object
  inherit GContainer.container_signals_impl (obj : [> Gtk.expander] Gtk.obj)
  inherit OgtkBinProps.expander_sigs
end

class expander obj = object
  inherit [[> Gtk.expander]] GContainer.bin_impl obj
  inherit OgtkBinProps.expander_props
  method connect = new expander_signals obj
end

let expander =
  GtkBin.Expander.make_params [] ~cont:(
  GContainer.pack_container ~create:(fun pl ->
    new expander (GtkBin.Expander.create pl)))
