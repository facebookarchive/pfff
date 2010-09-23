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

(* $Id: gDraw.mli 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gdk

(** Offscreen drawables *)

(** {3 Colors} *)

(** @gtkdoc gdk gdk-Colormaps-and-Colors *)
type color =
  [ `COLOR of Gdk.color
  | `WHITE
  | `BLACK
  | `NAME of string
  | `RGB of int * int * int]

val color : ?colormap:colormap -> color -> Gdk.color

type optcolor =
  [ `COLOR of Gdk.color
  | `WHITE
  | `BLACK
  | `NAME of string
  | `RGB of int * int * int
  | `DEFAULT ]

val optcolor : ?colormap:colormap -> optcolor -> Gdk.color option

(** {3 GdkDrawable} *)

(** Functions for drawing points, lines, arcs, and text
   @gtkdoc gdk gdk-Drawing-Primitives *)
class drawable : ?colormap:colormap -> ([>`drawable] Gobject.obj as 'a) ->
  object
    val mutable gc : gc
    val w : 'a
    method arc :
      x:int ->
      y:int ->
      width:int ->
      height:int ->
      ?filled:bool -> ?start:float -> ?angle:float -> unit -> unit
    method color : color -> Gdk.color
    method colormap : colormap
    method depth : int
    method gc : gc
    method set_gc : gc -> unit
    method gc_values : GC.values
    method line : x:int -> y:int -> x:int -> y:int -> unit
    method point : x:int -> y:int -> unit
    method polygon : ?filled:bool -> (int * int) list -> unit
    method put_layout :
      x: int -> y: int -> ?fore:color -> ?back:color -> Pango.layout -> unit
    method put_image :
      x:int -> y:int ->
      ?xsrc:int -> ?ysrc:int -> ?width:int -> ?height:int -> image -> unit
    method put_pixmap :
      x:int -> y:int ->
      ?xsrc:int -> ?ysrc:int -> ?width:int -> ?height:int -> pixmap -> unit
    method put_rgb_data :
      width:int -> height:int ->
      ?x:int -> ?y:int -> ?dither:Gdk.Tags.rgb_dither ->
      ?row_stride:int -> Gpointer.region -> unit
    method put_pixbuf :
      x:int -> y:int ->
      ?width:int -> ?height:int ->
      ?dither:Gdk.Tags.rgb_dither ->
      ?x_dither:int ->
      ?y_dither:int -> ?src_x:int -> ?src_y:int -> GdkPixbuf.pixbuf -> unit
    method rectangle :
      x:int ->
      y:int -> width:int -> height:int -> ?filled:bool -> unit -> unit
    method set_background : color -> unit
    method set_foreground : color -> unit
    method set_clip_region : region -> unit
    method set_clip_origin : x:int -> y:int -> unit
    method set_clip_mask : bitmap -> unit
    method set_clip_rectangle : Rectangle.t -> unit
    method set_line_attributes :
      ?width:int ->
      ?style:GC.gdkLineStyle ->
      ?cap:GC.gdkCapStyle -> ?join:GC.gdkJoinStyle -> unit -> unit
    method size : int * int
    method string : string -> font:font -> x:int -> y:int -> unit
    method points : (int * int) list -> unit
    method lines : (int * int) list -> unit
    method segments : ((int * int) * (int * int)) list -> unit
  end

(** {3 GdkPixmap} *)

(** Offscreen drawables
   @gtkdoc gdk gdk-Bitmaps-and-Pixmaps *)
class pixmap :
  ?colormap:colormap -> ?mask:bitmap -> Gdk.pixmap ->
  object
    inherit drawable
    val w : Gdk.pixmap
    val bitmap : drawable option
    val mask : bitmap option
    method mask : bitmap option
    method pixmap : Gdk.pixmap
  end

class type misc_ops =
  object
    method colormap : colormap
    method realize : unit -> unit
    method visual_depth : int
    method window : window
  end

(** @gtkdoc gdk gdk-Bitmaps-and-Pixmaps *)
val pixmap :
  width:int -> height:int -> ?mask:bool ->
  ?window:< misc : #misc_ops; .. > -> ?colormap:colormap ->
  unit -> pixmap
val pixmap_from_xpm :
  file:string ->
  ?window:< misc : #misc_ops; .. > ->
  ?colormap:colormap -> ?transparent:color -> unit -> pixmap
val pixmap_from_xpm_d :
  data:string array ->
  ?window:< misc : #misc_ops; .. > ->
  ?colormap:colormap -> ?transparent:color -> unit -> pixmap

(** {3 GdkDragContext} *)

(** @gtkdoc gdk gdk-Drag-and-Drop *)
class drag_context : Gdk.drag_context ->
  object
    val context : Gdk.drag_context
    method status : ?time:int32 -> Tags.drag_action option -> unit
    method suggested_action : Tags.drag_action
    method targets : string list
  end
