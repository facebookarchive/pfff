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

open GnoCanvas

(* Shape *)

(** Shape properties *)
type shape_p = [ `FILL_COLOR of string
               | `OUTLINE_COLOR of string
	       | `WIDTH_UNITS of float
	       | `DASH of float * float array ]

class type shape = object
  inherit GnoCanvas.base_item
  val obj : GnomeCanvas.item Gtk.obj
  (** Sets properties *)
  method set : shape_p list -> unit
  (** Undoes the last call to "set" *)
  method undo : unit -> unit
end

(* Text *)

(** Derived text class
   Uses a properties queue to undo changes
   (when dehighlighting for instance).
*)

class graph_text :
  GnomeCanvas.text Gtk.obj ->
  size_points:float ->
  props:GnomeCanvas.text_p list ->
object
  inherit text
  (** Undoes the last call to "set" *)
  method undo : unit -> unit
  (** Resizes the text *)
  method resize : float -> unit
  (** Initial size in points *)
  method init_size : float
end

(* View items *)

(** Tiny module to easily create and combine view_item properties *)
module VP : sig
  
  (** A pair of shape properties and text properties*)
  type t = shape_p list * GnomeCanvas.text_p list

  (** Monoid interface *)
  val empty : t
  val combine : t -> t -> t

  (** Simple properties *)
  type color = string
  val shp_color : color -> t
  val shp_fill_color : color -> t
  val txt_color : color -> t
  val shp_width : float -> t
  val txt_weight : int -> t
end

class type common_view = object
  inherit canvas
  method zoom_factor : float
  method adapt_zoom : unit -> unit
end

(** ViewItem class
    Group of shapes and texts
*)
class view_item :
  view:common_view ->
  pos:float * float ->
  ops_list:XDotDraw.operation list list ->
  (** Highlight view item properties *)
  hl_vip:VP.t ->
object
  inherit group

  method shapes : shape list
  method texts : graph_text list

  method iter_shapes : (shape -> unit) -> unit
  method iter_texts : (graph_text -> unit) -> unit

  method highlight : unit -> unit
  method dehighlight : unit -> unit
  method select : unit -> unit
  method deselect : unit -> unit

  method show : unit -> unit
  method hide : unit -> unit
  method is_showed : bool
  method is_hidden : bool

  method center : unit -> unit
  (* method connect : *)
  (*   < event : callback:(GnoCanvas.item_event -> bool) -> GtkSignal.id list > *)
end


class type ['vertex] view_node =
  object
    inherit view_item
    method vertex : 'vertex
  end

class type ['edge] view_edge =
  object
    inherit view_item
    method edge : 'edge
  end

class type ['cluster] view_cluster =
  object
    inherit view_item
    method cluster : 'cluster
  end

val view_node :
  ?hl_vip:VP.t ->
  view:common_view ->
  vertex:'vertex ->
  layout:XDot.node_layout ->
  unit ->
  'vertex view_node

val view_edge :
  ?hl_vip:VP.t ->
  view:common_view ->
  edge:'edge ->
  layout:XDot.edge_layout ->
  unit ->
  'edge view_edge

val view_cluster :
  view:common_view ->
  cluster:'cluster ->
  layout:XDot.cluster_layout ->
  unit ->
  'cluster view_cluster
