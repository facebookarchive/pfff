(**************************************************************************)
(*                                                                        *)
(*  This file is part of OcamlGraph.                                      *)
(*                                                                        *)
(*  Copyright (C) 2009                                                    *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1, with a linking exception.                    *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the file ../LICENSE for more details.                             *)
(*                                                                        *)
(*  Authors:                                                              *)
(*    - Jean-Denis Koeck (jdkoeck@gmail.com)                              *)
(*    - Julien Signoles  (Julien.Signoles@cea.fr)                         *)
(*                                                                        *)
(**************************************************************************)

open DGraphModel
open DGraphViewItem

open GnoCanvas

(** View class *)

(**
   Simple widget derived from the Gnome Canvas
   Supports zooming and scrolling *)

class ['vertex, 'edge, 'cluster] view :
  GnomeCanvas.canvas Gtk.obj ->
  ('vertex, 'edge, 'cluster) DGraphModel.abstract_model ->
  object
    inherit canvas
      
    (** Model from DGraphModel *)
    method model : ('vertex, 'edge, 'cluster) DGraphModel.abstract_model
      
    (** Getters *)
    method get_node : 'vertex -> 'vertex view_node
    method get_edge : 'edge -> 'edge view_edge
    method get_cluster : 'cluster -> 'cluster view_cluster

    (** Iterators *)
    method iter_nodes :  ('vertex view_node -> unit) -> unit
    method iter_edges : ('vertex view_node -> 'vertex view_node -> unit) -> unit
    method iter_edges_e :  ('edge view_edge -> unit) -> unit
    method iter_clusters : ('cluster view_cluster -> unit) -> unit

    method iter_succ :   ('vertex view_node -> unit) -> 'vertex view_node -> unit
    method iter_pred :   ('vertex view_node -> unit) -> 'vertex view_node -> unit
    method iter_succ_e : ('edge view_edge -> unit) -> 'vertex view_node -> unit
    method iter_pred_e : ('edge view_edge -> unit) -> 'vertex view_node -> unit

    (** Membership functions *)
    method mem_edge : 'vertex view_node -> 'vertex view_node -> bool
    method find_edge : 'vertex view_node -> 'vertex view_node -> 'edge view_edge
    method src : 'edge view_edge -> 'vertex view_node
    method dst : 'edge view_edge -> 'vertex view_node

    method zoom_factor : float
    method zoom_to : float -> unit
    method zoom_in : unit -> unit
    method zoom_out : unit -> unit
    method adapt_zoom : unit -> unit
  end

val view :
  ?aa:bool -> (** Anti aliasing *)
  ('vertex, 'edge, 'cluster) DGraphModel.abstract_model ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) ->
  ?show:bool -> unit
  -> ('vertex, 'edge, 'cluster) view

(** Same widget augmented with highlighting and focus
    Hover to highlight, double click to focus
*)
class ['vertex, 'edge, 'cluster] highlight_focus_view :
  GnomeCanvas.canvas Gtk.obj ->
  ('vertex, 'edge, 'cluster) DGraphModel.abstract_model ->
  ['vertex, 'edge, 'cluster] view

val highlight_focus_view :
  ?aa:bool -> (** Anti aliasing *)
  ('vertex, 'edge, 'cluster) DGraphModel.abstract_model ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) ->
  ?show:bool -> unit
  -> ('vertex, 'edge, 'cluster) highlight_focus_view

(** Same widget augmented with a label displaying the current node *)
class ['vertex, 'edge, 'cluster] labeled_view :
  GnomeCanvas.canvas Gtk.obj ->
  ('vertex, 'edge, 'cluster) DGraphModel.abstract_model -> GMisc.label ->
  ['vertex, 'edge, 'cluster] view

val labeled_view :
  ?aa:bool -> (** Anti aliasing *)
  ('vertex, 'edge, 'cluster) DGraphModel.abstract_model -> GMisc.label ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) ->
  ?show:bool -> unit
  -> ('vertex, 'edge, 'cluster) highlight_focus_view

(* Same widget augmented with highlighting, focus
    and the ability to drag the canvas (click'n hold)
*)
(* class ['vertex, 'edge, 'cluster] drag_view : *)
(*   GnomeCanvas.canvas Gtk.obj -> *)
(*   ('vertex, 'edge, 'cluster) DGraphModel.abstract_model -> *)
(*   ['vertex, 'edge, 'cluster] view *)

(* val drag_view : *)
(*   ?aa:bool -> (\** Anti aliasing *\) *)
(*   ('vertex, 'edge, 'cluster) DGraphModel.abstract_model -> *)
(*   ?border_width:int -> *)
(*   ?width:int -> *)
(*   ?height:int -> *)
(*   ?packing:(GObj.widget -> unit) -> *)
(*   ?show:bool -> unit *)
(*   -> ('vertex, 'edge, 'cluster) highlight_focus_view *)
