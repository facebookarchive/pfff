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

(* $Id: gnoCanvas.mli 1515 2010-06-08 08:50:23Z garrigue $ *)

(** [libgnomecanvas] bindings *)

type items_properties = [ 
  | `NO_WIDGET
  | `NO_FILL_COLOR
  | `NO_OUTLINE_COLOR
  | `NO_FONT
  | `NO_TEXT
  | `NO_BPATH
  | `NO_PIXBUF
  | `ANCHOR of Gtk.Tags.anchor_type
  | `ARROW_SHAPE_A of float
  | `ARROW_SHAPE_B of float
  | `ARROW_SHAPE_C of float
  | `BPATH of GnomeCanvas.PathDef.t
  | `CAP_STYLE of Gdk.GC.gdkCapStyle
  | `CLIP of bool
  | `CLIP_HEIGHT of float
  | `CLIP_WIDTH of float
  | `CURSOR_BLINK of bool
  | `CURSOR_VISIBLE of bool
  | `DASH of float * float array
  | `EDITABLE of bool
  | `FAMILY of string
  | `FILL_COLOR of string
  | `FILL_COLOR_RGBA of int32
  | `FILL_STIPPLE of Gdk.bitmap
  | `FIRST_ARROWHEAD of bool
  | `FONT of string
  | `GROW_HEIGHT of bool
  | `HEIGHT of float
  | `JOIN_STYLE of Gdk.GC.gdkJoinStyle
  | `JUSTIFICATION of Gtk.Tags.justification
  | `LAST_ARROWHEAD of bool
  | `LEFT_MARGIN of int
  | `LINE_STYLE of Gdk.GC.gdkLineStyle
  | `MARKUP of string
  | `OUTLINE_COLOR of string
  | `OUTLINE_COLOR_RGBA of int32
  | `OUTLINE_STIPPLE of Gdk.bitmap
  | `PIXBUF of GdkPixbuf.pixbuf
  | `POINTS of float array
  | `RIGHT_MARGIN of int
  | `RISE of int
  | `SCALE of float
  | `SIZE of int
  | `SIZE_PIXELS of bool
  | `SIZE_POINTS of float
  | `SMOOTH of bool
  | `TEXT of string
  | `VISIBLE of bool
  | `WEIGHT of int
  | `WIDGET of GObj.widget
  | `WIDTH of float
  | `WIDTH_PIXELS of int
  | `WIDTH_UNITS of float
  | `X of float
  | `X1 of float
  | `X2 of float
  | `X_OFFSET of float
  | `Y of float
  | `Y1 of float
  | `Y2 of float
  | `Y_OFFSET of float
] 
      
val propertize : [< items_properties] -> string * unit Gobject.data_set

type item_event = [
  | `BUTTON_PRESS of GdkEvent.Button.t
  | `TWO_BUTTON_PRESS of GdkEvent.Button.t
  | `THREE_BUTTON_PRESS of GdkEvent.Button.t
  | `BUTTON_RELEASE of GdkEvent.Button.t
  | `MOTION_NOTIFY of GdkEvent.Motion.t
  | `KEY_PRESS of GdkEvent.Key.t
  | `KEY_RELEASE of GdkEvent.Key.t
  | `ENTER_NOTIFY of GdkEvent.Crossing.t
  | `LEAVE_NOTIFY of GdkEvent.Crossing.t
  | `FOCUS_CHANGE of GdkEvent.Focus.t ]

class item_signals :
  'b Gtk.obj ->
  object
    constraint 'b = [> GnomeCanvas.item]
    inherit GObj.gtkobj_signals
    val obj : 'b Gtk.obj
    method event : callback:(item_event -> bool) -> GtkSignal.id
  end

(** @gtkdoc libgnomecanvas GnomeCanvasItem *)
class base_item : ([> GnomeCanvas.item] as 'b) Gtk.obj ->
  object
    inherit GObj.gtkobj
    val obj : 'b Gtk.obj

    method parent : group
    method reparent : group -> unit

    method as_item : GnomeCanvas.item Gtk.obj
    method connect : item_signals

    method get_bounds : float array
    method grab : Gdk.Tags.event_mask list -> Gdk.cursor -> int32 -> unit
    method grab_focus : unit -> unit
    method hide : unit -> unit
    method i2c_affine : float array
    method i2w : x:float -> y:float -> float * float
    method i2w_affine : float array
    method lower : int -> unit
    method lower_to_bottom : unit -> unit
    method move : x:float -> y:float -> unit
    method canvas : canvas
    method xform : [`IDENTITY|`TRANSL of float array|`AFFINE of float array]
    method affine_relative : float array -> unit
    method affine_absolute : float array -> unit
    method raise : int -> unit
    method raise_to_top : unit -> unit
    method show : unit -> unit
    method ungrab : int32 -> unit
    method w2i : x:float -> y:float -> float * float
  end

(** @gtkdoc libgnomecanvas GnomeCanvasGroup *)
and group : GnomeCanvas.group Gtk.obj ->
  object 
    inherit base_item
    val obj : GnomeCanvas.group Gtk.obj
    method as_group : GnomeCanvas.group Gtk.obj
    method get_items : base_item list
    method set : GnomeCanvas.group_p list -> unit
  end

(** @gtkdoc libgnomecanvas GnomeCanvas *)
and canvas : GnomeCanvas.canvas Gtk.obj ->
  object
    inherit GPack.layout
    val obj : GnomeCanvas.canvas Gtk.obj
    method aa : bool
    method c2w : cx:float -> cy:float -> float * float
    method get_center_scroll_region : bool
    method get_item_at : x:float -> y:float -> base_item (** @raise Not_found . *)
    method get_scroll_offsets : int * int
    method get_scroll_region : float array
    method root : group
    method scroll_to : x:int -> y:int -> unit
    method set_center_scroll_region : bool -> unit
    method set_pixels_per_unit : float -> unit
    method set_scroll_region :
      x1:float -> y1:float -> x2:float -> y2:float -> unit
    method update_now : unit -> unit
    method w2c : wx:float -> wy:float -> int * int
    method w2c_affine : float array
    method w2c_d : wx:float -> wy:float -> float * float
    method window_to_world : winx:float -> winy:float -> float * float
    method world_to_window : wox:float -> woy:float -> float * float
  end

(** @gtkdoc libgnomecanvas GnomeCanvasItem *)
class ['p] item : ([> GnomeCanvas.item] as 'a) Gtk.obj -> 
  object
    inherit base_item
    val obj : 'a Gtk.obj
    constraint 'p = [< items_properties]
    method set : 'p list -> unit
  end

(** @gtkdoc libgnomecanvas GnomeCanvas *)
val canvas :
  ?aa:bool ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) ->
  ?show:bool ->
  unit -> canvas 

(** @gtkdoc libgnomecanvas GnomeCanvasGroup *)
val group : ?x:float -> ?y:float -> #group -> group

val wrap_item : 
  [> GnomeCanvas.item] Gtk.obj -> ('a, 'p) GnomeCanvas.Types.t -> 'p item

type rect = GnomeCanvas.re_p item

(** @gtkdoc libgnomecanvas GnomeCanvasRect *)
val rect :
  ?x1:float -> ?y1:float -> 
  ?x2:float -> ?y2:float -> 
  ?fill_color:string ->
  ?props:GnomeCanvas.re_p list ->
  #group -> rect

type ellipse = GnomeCanvas.re_p item

(** @gtkdoc libgnomecanvas GnomeCanvasEllipse *)
val ellipse :
  ?x1:float -> ?y1:float -> 
  ?x2:float -> ?y2:float -> 
  ?fill_color:string ->
  ?props:GnomeCanvas.re_p list ->
  #group -> ellipse

(** @gtkdoc libgnomecanvas GnomeCanvasText *)
class text : GnomeCanvas.text Gtk.obj ->
  object
    inherit [GnomeCanvas.text_p] item
    val obj : GnomeCanvas.text Gtk.obj
    method text_height : float
    method text_width : float
  end

(** @gtkdoc libgnomecanvas GnomeCanvasText *)
val text :
  ?x:float -> ?y:float -> ?text:string ->
  ?font:string -> ?size:int -> ?anchor:Gtk.Tags.anchor_type ->
  ?props:GnomeCanvas.text_p list ->
  #group -> text

type line = GnomeCanvas.line_p item

(** @gtkdoc libgnomecanvas GnomeCanvasLine *)
val line :
  ?points:float array ->
  ?fill_color:string ->
  ?props:GnomeCanvas.line_p list ->
  #group -> line

type bpath = GnomeCanvas.bpath_p item

(** @gtkdoc libgnomecanvas GnomeCanvasBpath *)
val bpath :
  ?bpath:GnomeCanvas.PathDef.t ->
  ?fill_color:string ->
  ?props:GnomeCanvas.bpath_p list ->
  #group -> bpath

type pixbuf = GnomeCanvas.pixbuf_p item

(** @gtkdoc libgnomecanvas GnomeCanvasPixbuf *)
val pixbuf :
  ?x:float -> ?y:float -> ?pixbuf:GdkPixbuf.pixbuf ->
  ?width:float -> ?height:float ->
  ?props:GnomeCanvas.pixbuf_p list ->
  #group -> pixbuf

type polygon = GnomeCanvas.polygon_p item

(** @gtkdoc libgnomecanvas GnomeCanvasPolygon *)
val polygon :
  ?points:float array ->
  ?fill_color:string ->
  ?props:GnomeCanvas.polygon_p list ->
  #group -> polygon

type widget = GnomeCanvas.widget_p item

(** @gtkdoc libgnomecanvas GnomeCanvasWidget *)
val widget :
  ?widget:< coerce: GObj.widget; .. > ->
  ?x:float -> ?y:float -> 
  ?width:float -> ?height:float ->
  ?props:GnomeCanvas.widget_p list ->
  #group -> widget

(** @gtkdoc libgnomecanvas GnomeCanvasRichtext *)
class rich_text : GnomeCanvas.rich_text Gtk.obj ->
  object
    inherit [GnomeCanvas.rich_text_p] item
    val obj : GnomeCanvas.rich_text Gtk.obj
    method copy_clipboard : unit -> unit
    method cut_clipboard : unit -> unit
    method paste_clipboard : unit -> unit
    method get_buffer : GText.buffer
  end

(** @gtkdoc libgnomecanvas GnomeCanvasRichtext *)
val rich_text :
  ?x:float -> ?y:float ->
  ?text:string ->
  ?width:float -> ?height:float ->
  ?props:GnomeCanvas.rich_text_p list ->
  #group -> rich_text
