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

(* $Id: gnomeCanvas.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

type canvas = [Gtk.layout|`canvas]
type item   = [`gtk|`canvasitem]
type group  = [item|`canvasgroup]
type clipgroup = [group|`canvasclipgroup]
type shape  = [item|`canvasshape]
type r_e    = [shape|`canvasre]
type rect   = [r_e|`canvasrectangle]
type ellipse = [r_e|`canvasellipse]
type bpath  = [shape|`canvasbpath]
type polygon = [shape|`canvaspolygon]
type text   = [item|`canvastext]
type line   = [item|`canvasline]
type pixbuf = [item|`canvaspixbuf]
type widget = [item|`canvaswidget]
type rich_text = [item|`canvasrichtext]

type path_def

(* GnomeCanvas *)

module Canvas = 
  struct
external new_canvas    : unit -> canvas Gobject.obj = "ml_gnome_canvas_new"
external new_canvas_aa : unit -> canvas Gobject.obj = "ml_gnome_canvas_new_aa"

external root : [> canvas] Gobject.obj -> group Gobject.obj = "ml_gnome_canvas_root"
external set_scroll_region : [> canvas] Gobject.obj -> x1:float -> y1:float -> x2:float -> y2:float -> unit = "ml_gnome_canvas_set_scroll_region"
external get_scroll_region : [> canvas] Gobject.obj -> float array = "ml_gnome_canvas_get_scroll_region"
external set_center_scroll_region : [> canvas] Gobject.obj -> bool -> unit = "ml_gnome_canvas_set_center_scroll_region"
external get_center_scroll_region : [> canvas] Gobject.obj -> bool = "ml_gnome_canvas_get_center_scroll_region"
external set_pixels_per_unit : [> canvas] Gobject.obj -> float -> unit = "ml_gnome_canvas_set_pixels_per_unit"
external scroll_to : [> canvas] Gobject.obj -> x:int -> y:int -> unit = "ml_gnome_canvas_scroll_to"
external get_scroll_offsets : [> canvas] Gobject.obj -> int * int = "ml_gnome_canvas_get_scroll_offsets"
external update_now : [> canvas] Gobject.obj -> unit = "ml_gnome_canvas_update_now"
external get_item_at : [> canvas] Gobject.obj -> x:float -> y:float -> item Gobject.obj = "ml_gnome_canvas_get_item_at"
external w2c_affine : [> canvas] Gobject.obj -> float array = "ml_gnome_canvas_w2c_affine"
external w2c : [> canvas] Gobject.obj -> wx:float -> wy:float -> int * int = "ml_gnome_canvas_w2c"
external w2c_d : [> canvas] Gobject.obj -> wx:float -> wy:float -> float * float = "ml_gnome_canvas_w2c_d"
external c2w : [> canvas] Gobject.obj -> cx:float -> cy:float -> float * float = "ml_gnome_canvas_c2w"
external window_to_world : [> canvas] Gobject.obj -> winx:float -> winy:float -> float * float = "ml_gnome_canvas_window_to_world"
external world_to_window : [> canvas] Gobject.obj -> wox:float -> woy:float -> float * float = "ml_gnome_canvas_world_to_window"

end

module PathDef =
  struct
  type t = Gpointer.boxed
  external new_path : ?size:int -> unit -> t = "ml_gnome_canvas_path_def_new"
  external duplicate : t -> t = "ml_gnome_canvas_path_def_duplicate"
  external concat : t list -> t = "ml_gnome_canvas_path_def_concat"
  external reset : t -> unit = "ml_gnome_canvas_path_def_reset"
  external moveto : t -> float -> float -> unit = "ml_gnome_canvas_path_def_moveto"
  external lineto : t -> float -> float -> unit = "ml_gnome_canvas_path_def_lineto"
  external lineto_moving : t -> float -> float -> unit = "ml_gnome_canvas_path_def_lineto_moving"
  external curveto : t -> float -> float -> float -> float -> float -> float -> unit = "ml_gnome_canvas_path_def_curveto_bc" "ml_gnome_canvas_path_def_curveto"
  external closepath : t -> unit = "ml_gnome_canvas_path_def_closepath"
  external closepath_current : t -> unit = "ml_gnome_canvas_path_def_closepath_current"
  external length : t -> int = "ml_gnome_canvas_path_def_length"
  external is_empty : t -> bool = "ml_gnome_canvas_path_def_is_empty"
  external has_currentpoint : t -> bool = "ml_gnome_canvas_path_def_has_currentpoint"
end


type group_p = [`X of float| `Y of float]
type shape_p = [`FILL_COLOR of string| `OUTLINE_COLOR of string
               | `NO_FILL_COLOR| `NO_OUTLINE_COLOR
               | `FILL_COLOR_RGBA of int32| `FILL_STIPPLE of Gdk.bitmap
               | `OUTLINE_COLOR_RGBA of int32| `OUTLINE_STIPPLE of Gdk.bitmap
               | `WIDTH_UNITS of float| `WIDTH_PIXELS of int
	       | `DASH of float * float array
	       | `CAP_STYLE of Gdk.GC.gdkCapStyle
	       | `JOIN_STYLE of Gdk.GC.gdkJoinStyle]
type re_p = [shape_p| `X1 of float| `Y1 of float| `X2 of float| `Y2 of float]
type text_p = [`X of float| `Y of float| `TEXT of string| `FONT of string
              | `NO_TEXT| `NO_FONT| `NO_FILL_COLOR
              | `SIZE of int| `SIZE_POINTS of float| `FILL_COLOR of string
              | `FILL_COLOR_RGBA of int32 | `FILL_STIPPLE of Gdk.bitmap
	      | `CLIP of bool| `CLIP_WIDTH of float| `CLIP_HEIGHT of float
	      | `X_OFFSET of float| `Y_OFFSET of float
	      | `JUSTIFICATION of Gtk.Tags.justification
	      | `ANCHOR of Gtk.Tags.anchor_type| `FAMILY of string
	      | `MARKUP of string | `RISE of int | `SCALE of float
	      | `WEIGHT of int]
type line_p = [`ARROW_SHAPE_A of float| `ARROW_SHAPE_B of float| `ARROW_SHAPE_C of float
              | `FILL_COLOR of string| `NO_FILL_COLOR
	      | `WIDTH_UNITS of float| `WIDTH_PIXELS of int
              | `POINTS of float array| `FIRST_ARROWHEAD of bool
	      | `LAST_ARROWHEAD of bool| `SMOOTH of bool
              | `FILL_COLOR_RGBA of int32 | `FILL_STIPPLE of Gdk.bitmap
	      | `CAP_STYLE of Gdk.GC.gdkCapStyle| `JOIN_STYLE of Gdk.GC.gdkJoinStyle
	      | `LINE_STYLE of Gdk.GC.gdkLineStyle]
type bpath_p = [shape_p| `BPATH of PathDef.t| `NO_BPATH]
type pixbuf_p = [`X of float| `Y of float
                | `WIDTH of float| `HEIGHT of float
		| `ANCHOR of Gtk.Tags.anchor_type
		| `PIXBUF of GdkPixbuf.pixbuf| `NO_PIXBUF]
type polygon_p = [shape_p| `POINTS of float array]
type widget_p = [`X of float| `Y of float
                | `WIDTH of float| `HEIGHT of float
		| `SIZE_PIXELS of bool
		| `ANCHOR of Gtk.Tags.anchor_type
		| `WIDGET of GObj.widget| `NO_WIDGET]
type rich_text_p = [`X of float| `Y of float
                  | `TEXT of string
                  | `WIDTH of float|  `HEIGHT of float
		  | `EDITABLE of bool | `VISIBLE of bool
		  | `JUSTIFICATION of Gtk.Tags.justification
		  | `ANCHOR of Gtk.Tags.anchor_type
		  | `CURSOR_VISIBLE of bool| `CURSOR_BLINK of bool
		  | `GROW_HEIGHT of bool
		  | `LEFT_MARGIN of int| `RIGHT_MARGIN of int]

module Types : sig
  type ('a, 'b) t constraint 'a = [> `gtk|`canvasitem]

  val group : (group, group_p) t
  val rect : ([item|`canvasshape|`canvasRE|`canvasrect], re_p) t
  val ellipse : ([item|`canvasshape|`canvasRE|`canvasellipse], re_p) t
  val text : ([item|`canvastext], text_p) t
  val line : ([item|`canvasline], line_p) t
  val bpath : ([item|`canvasshape|`canvasbpath], bpath_p) t
  val pixbuf : ([item|`canvaspixbuf], pixbuf_p) t
  val polygon : ([item|`canvasshape|`canvaspolygon], polygon_p) t
  val widget : ([item|`canvaswidget], widget_p) t
  val rich_text : (rich_text, rich_text_p) t
  val shape : ([item|`canvasshape], shape_p) t
  val rect_ellipse : ([item|`canvasshape|`canvasRE], re_p) t

  val points : Gobject.g_type
  val is_a : 'a Gobject.obj -> ('b, 'c) t -> bool
  val name : ('a, 'b) t -> string
end = 
  struct
  type ('a, 'b) t = Gobject.g_type constraint 'a = [> `gtk|`canvasitem]
  external register_types : unit -> Gobject.g_type array
     = "ml_gnome_canvas_register_types"
  let canvas_types = register_types ()

  let group = canvas_types.(4)
  let rect = canvas_types.(11)
  let ellipse = canvas_types.(3)
  let text = canvas_types.(14)
  let line = canvas_types.(6)
  let bpath = canvas_types.(1)
  let pixbuf = canvas_types.(7)
  let polygon = canvas_types.(9)
  let shape = canvas_types.(13)
  let rect_ellipse = canvas_types.(10)
  let widget = canvas_types.(2)
  let rich_text = canvas_types.(12)
  let points = canvas_types.(8)

  let is_a obj typ =
    Gobject.Type.is_a (Gobject.get_type obj) typ
  let name = Gobject.Type.name
  end

(* GnomeCanvasItem *)
type item_event =
    [ `BUTTON_PRESS | `TWO_BUTTON_PRESS | `THREE_BUTTON_PRESS | `BUTTON_RELEASE
    | `MOTION_NOTIFY | `KEY_PRESS | `KEY_RELEASE | `ENTER_NOTIFY | `LEAVE_NOTIFY 
    | `FOCUS_CHANGE ] Gdk.event

module Item =
  struct
external new_item : [> group] Gobject.obj -> ('a, 'b) Types.t -> 'a Gobject.obj = "ml_gnome_canvas_item_new"
external parent : [> item] Gobject.obj -> group Gobject.obj = "ml_gnome_canvas_item_parent"
external canvas : [> item] Gobject.obj -> canvas Gobject.obj = "ml_gnome_canvas_item_canvas"
external xform :  [> item] Gobject.obj -> [`IDENTITY|`TRANSL of float array|`AFFINE of float array] = "ml_gnome_canvas_item_xform"
external affine_relative : [> item] Gobject.obj -> float array -> unit = "ml_gnome_canvas_item_affine_relative"
external affine_absolute : [> item] Gobject.obj -> float array -> unit = "ml_gnome_canvas_item_affine_absolute"
external set : [> item] Gobject.obj -> unit = "ml_gnome_canvas_item_set"
  (* Must call [set] after using [Gobject.Property.set] *)
external move : [> item] Gobject.obj -> x:float -> y:float -> unit = "ml_gnome_canvas_item_move"
external raise : [> item] Gobject.obj -> int -> unit = "ml_gnome_canvas_item_raise"
external lower : [> item] Gobject.obj -> int -> unit = "ml_gnome_canvas_item_lower"
external raise_to_top : [> item] Gobject.obj -> unit = "ml_gnome_canvas_item_raise_to_top"
external lower_to_bottom : [> item] Gobject.obj -> unit = "ml_gnome_canvas_item_lower_to_bottom"
external show : [> item] Gobject.obj -> unit = "ml_gnome_canvas_item_show"
external hide : [> item] Gobject.obj -> unit = "ml_gnome_canvas_item_hide"
external grab : [> item] Gobject.obj -> Gdk.Tags.event_mask list -> Gdk.cursor -> int32 -> unit = "ml_gnome_canvas_item_grab"
external ungrab : [> item] Gobject.obj -> int32 -> unit = "ml_gnome_canvas_item_ungrab"
external w2i : [> item] Gobject.obj -> x:float -> y:float -> float * float = "ml_gnome_canvas_item_w2i"
external i2w : [> item] Gobject.obj -> x:float -> y:float -> float * float = "ml_gnome_canvas_item_i2w"
external i2w_affine : [> item] Gobject.obj -> float array = "ml_gnome_canvas_item_i2w_affine"
external i2c_affine : [> item] Gobject.obj -> float array = "ml_gnome_canvas_item_i2c_affine"
external reparent : [> item] Gobject.obj -> group Gobject.obj -> unit = "ml_gnome_canvas_item_reparent"
external grab_focus : [> item] Gobject.obj -> unit = "ml_gnome_canvas_item_grab_focus"
external get_bounds : [> item] Gobject.obj -> float array = "ml_gnome_canvas_item_get_bounds"
module Signals = struct
  let marshal = GtkBase.Widget.Signals.Event.marshal
  let event : ([> `canvasitem], item_event -> bool) GtkSignal.t =
    { GtkSignal.name = "event"; 
      GtkSignal.classe = `canvasitem; 
      GtkSignal.marshaller = marshal; }
  end
end

(* GnomeCanvasGroup *)
module Group = 
  struct
external get_items : [> group] Gobject.obj -> item Gobject.obj list  = "ml_gnome_canvas_group_get_items"
end

module Text =
  struct
    let text_width = { Gobject.name = "text-width"; Gobject.conv = Gobject.Data.double }
    let text_height = { Gobject.name = "text-height"; Gobject.conv = Gobject.Data.double }
end

(* GnomeCanvasRichText *)
module RichText =
  struct
external cut_clipboard : [> rich_text] Gobject.obj -> unit = "ml_gnome_canvas_rich_text_cut_clipboard"
external copy_clipboard : [> rich_text] Gobject.obj -> unit = "ml_gnome_canvas_rich_text_copy_clipboard"
external paste_clipboard : [> rich_text] Gobject.obj -> unit = "ml_gnome_canvas_rich_text_paste_clipboard"
external get_buffer : [> rich_text] Gobject.obj -> Gtk.text_buffer = "ml_gnome_canvas_rich_text_get_buffer"
end

(* Conversion  functions for properties *)
module Conv = struct
external convert_points : float array -> Gpointer.boxed
  = "ml_gnome_canvas_convert_points"
external convert_dash : float -> float array -> Gpointer.boxed
  = "ml_gnome_canvas_convert_dash"
external get_points : Gpointer.boxed -> float array
  = "ml_gnome_canvas_get_points"
external get_dash : Gpointer.boxed -> float * float array
  = "ml_gnome_canvas_get_dash"
open Gaux
open Gobject
let points =
  { kind = `OTHER (Type.from_name "GnomeCanvasPoints");
    inj = (fun x -> `POINTER (may_map convert_points x));
    proj = (fun x -> may_map get_points (Data.pointer.proj x)) }
let art_vpath_dash =
  { kind = `POINTER;
    inj = (fun x -> `POINTER (may_map (fun (x,y) -> convert_dash x y) x));
    proj = (fun x -> may_map get_dash (Data.pointer.proj x)) }
let path_def =
  { kind = `POINTER; inj = Data.unsafe_pointer_option.inj; proj =
    (fun x -> may_map PathDef.duplicate (Data.unsafe_pointer_option.proj x)) }
end
