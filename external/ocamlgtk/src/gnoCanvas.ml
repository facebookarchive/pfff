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

(* $Id: gnoCanvas.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open GnomeCanvas

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
  | `BPATH of PathDef.t
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

let encode tbl v = `INT (Gpointer.encode_variant tbl v)
let propertize = function
  | `ANCHOR a -> "anchor", encode GtkEnums.anchor_type a
  | `ARROW_SHAPE_A v -> "arrow_shape_a", `FLOAT v
  | `ARROW_SHAPE_B v -> "arrow_shape_b", `FLOAT v
  | `ARROW_SHAPE_C v -> "arrow_shape_c", `FLOAT v
  | `BPATH p -> "bpath" , `POINTER (Some p)
  | `CAP_STYLE c -> "cap_style", encode GdkEnums.cap_style c
  | `CLIP b -> "clip", `BOOL b
  | `CLIP_HEIGHT v -> "clip_height", `FLOAT v
  | `CLIP_WIDTH v -> "clip_width", `FLOAT v
  | `CURSOR_BLINK b -> "cursor_blink", `BOOL b
  | `CURSOR_VISIBLE b -> "cursor_visible", `BOOL b
  | `DASH (off, d) -> "dash", `POINTER (Some (Conv.convert_dash off d))
  | `EDITABLE b -> "editable", `BOOL b
  | `FAMILY s -> "family", `STRING (Some s)
  | `FILL_COLOR c -> "fill_color", `STRING (Some c)
  | `FILL_COLOR_RGBA c -> "fill_color_rgba", `INT32 c
  | `FILL_STIPPLE (d : Gdk.bitmap) -> "fill_stipple", `OBJECT (Some (Gobject.coerce d))
  | `FIRST_ARROWHEAD b -> "first_arrowhead", `BOOL b
  | `FONT t -> "font", `STRING (Some t)
  | `GROW_HEIGHT b -> "grow_height", `BOOL b
  | `HEIGHT v -> "height", `FLOAT v
  | `JOIN_STYLE c -> "join_style", encode GdkEnums.join_style c
  | `JUSTIFICATION j -> "justification", encode GtkEnums.justification j
  | `LAST_ARROWHEAD  b -> "last_arrowhead", `BOOL b
  | `LEFT_MARGIN i -> "left_margin", `INT i
  | `LINE_STYLE c -> "line_style", encode GdkEnums.line_style c
  | `OUTLINE_COLOR c -> "outline_color", `STRING (Some c)
  | `OUTLINE_COLOR_RGBA c -> "outline_color_rgba", `INT32 c
  | `OUTLINE_STIPPLE (d : Gdk.bitmap) -> "outline_stipple", `OBJECT (Some (Gobject.coerce d))
  | `MARKUP s -> "markup", `STRING (Some s)
  | `PIXBUF (p : GdkPixbuf.pixbuf) -> "pixbuf", `OBJECT (Some (Gobject.coerce p))
  | `POINTS p -> "points", `POINTER (Some (Conv.convert_points p))
  | `RIGHT_MARGIN i -> "right_margin", `INT i
  | `RISE i -> "rise", `INT i
  | `SCALE f -> "scale", `FLOAT f
  | `SIZE i -> "size", `INT i
  | `SIZE_PIXELS b -> "size_pixels", `BOOL b
  | `SIZE_POINTS f -> "size-points", `FLOAT f
  | `SMOOTH b -> "smooth", `BOOL b
  | `TEXT t -> "text", `STRING (Some t)
  | `VISIBLE b -> "visible", `BOOL b
  | `WEIGHT i -> "weight", `INT i
  | `WIDGET (w : GObj.widget) -> "widget", `OBJECT (Some (Gobject.coerce w#as_widget))
  | `WIDTH v -> "width", `FLOAT v
  | `WIDTH_PIXELS v -> "width_pixels", `INT v
  | `WIDTH_UNITS v -> "width_units", `FLOAT v
  | `X v -> "x", `FLOAT v
  | `X1 v -> "x1", `FLOAT v
  | `X2 v -> "x2", `FLOAT v
  | `X_OFFSET v -> "x_offset", `FLOAT v
  | `Y v -> "y", `FLOAT v
  | `Y1 v -> "y1", `FLOAT v
  | `Y2 v -> "y2", `FLOAT v
  | `Y_OFFSET v -> "y_offset", `FLOAT v
  | `NO_FILL_COLOR -> "fill_color", `STRING None
  | `NO_OUTLINE_COLOR -> "outline_color", `STRING None
  | `NO_FONT -> "font", `STRING None
  | `NO_TEXT -> "text", `STRING None
  | `NO_BPATH -> "bpath", `POINTER None
  | `NO_PIXBUF -> "pixbuf", `OBJECT None
  | `NO_WIDGET -> "widget", `OBJECT None

let set_properties obj p =
  List.iter
    (fun p -> let p, d = propertize p in Gobject.Property.set_dyn obj p d)
    p;
  Item.set obj

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

let event_proxy : (item_event -> bool) -> GnomeCanvas.item_event -> bool = 
  fun cb ev -> match GdkEvent.get_type ev with
  | `BUTTON_PRESS ->
      cb (`BUTTON_PRESS (GdkEvent.unsafe_cast ev))
  | `TWO_BUTTON_PRESS ->
      cb (`TWO_BUTTON_PRESS (GdkEvent.unsafe_cast ev))
  | `THREE_BUTTON_PRESS ->
      cb (`THREE_BUTTON_PRESS (GdkEvent.unsafe_cast ev))
  | `BUTTON_RELEASE ->
      cb (`BUTTON_RELEASE (GdkEvent.unsafe_cast ev))
  | `MOTION_NOTIFY ->
      cb (`MOTION_NOTIFY (GdkEvent.unsafe_cast ev))
  | `KEY_PRESS ->
      cb (`KEY_PRESS (GdkEvent.unsafe_cast ev))
  | `KEY_RELEASE ->
      cb (`KEY_RELEASE (GdkEvent.unsafe_cast ev))
  | `ENTER_NOTIFY ->
      cb (`ENTER_NOTIFY (GdkEvent.unsafe_cast ev))
  | `LEAVE_NOTIFY ->
      cb (`LEAVE_NOTIFY (GdkEvent.unsafe_cast ev))
  | `FOCUS_CHANGE ->
      cb (`FOCUS_CHANGE (GdkEvent.unsafe_cast ev))

class item_signals obj = object (self)
  inherit GObj.gtkobj_signals_impl obj
  method event ~callback =
    self#connect Item.Signals.event ~callback:(event_proxy callback)
end

class type base_item_t =
  object
    inherit GObj.gtkobj
    val obj : 'a Gtk.obj
    constraint 'a = [> GnomeCanvas.item]

    method parent : group_t
    method reparent : group_t -> unit

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
    method canvas : canvas_t
    method xform : [`IDENTITY|`TRANSL of float array|`AFFINE of float array]
    method affine_relative : float array -> unit
    method affine_absolute : float array -> unit
    method raise : int -> unit
    method raise_to_top : unit -> unit
    method show : unit -> unit
    method ungrab : int32 -> unit
    method w2i : x:float -> y:float -> float * float
  end

and group_t =
  object 
    inherit base_item_t
    val obj : GnomeCanvas.group Gtk.obj
    method as_group : GnomeCanvas.group Gtk.obj
    method get_items : base_item_t list
    method set : GnomeCanvas.group_p list -> unit
  end

and canvas_t =
  object
    inherit GPack.layout
    val obj : GnomeCanvas.canvas Gtk.obj
    method aa : bool
    method c2w : cx:float -> cy:float -> float * float
    method get_center_scroll_region : bool
    method get_item_at : x:float -> y:float -> base_item_t
    method get_scroll_offsets : int * int
    method get_scroll_region : float array
    method root : group_t
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

let new_group : ('a Gtk.obj -> group_t) ref = ref (fun _ -> assert false)
let new_base_item : ('a Gtk.obj -> base_item_t) ref = ref (fun _ -> assert false)

class base_item obj = object
  inherit GObj.gtkobj obj
  method as_item = (obj :> GnomeCanvas.item Gtk.obj)
  method connect = new item_signals (obj :> GnomeCanvas.item Gtk.obj)
  method parent = !new_group (Item.parent obj)
  method reparent grp = Item.reparent obj (grp : group_t)#as_group
  method canvas = new canvas (Item.canvas obj)
  method xform = Item.xform obj
  method affine_relative = Item.affine_relative obj
  method affine_absolute = Item.affine_absolute obj
  method move = Item.move obj
  method raise = Item.raise obj
  method lower = Item.lower obj
  method raise_to_top () = Item.raise_to_top obj
  method lower_to_bottom () = Item.lower_to_bottom obj
  method show () = Item.show obj
  method hide () = Item.hide obj
  method grab = Item.grab obj
  method ungrab = Item.ungrab obj
  method w2i = Item.w2i obj
  method i2w = Item.i2w obj
  method i2w_affine = Item.i2w_affine obj
  method i2c_affine = Item.i2c_affine obj
  method grab_focus () = Item.grab_focus obj
  method get_bounds = Item.get_bounds obj
end

and canvas obj = object
  inherit GPack.layout (obj : GnomeCanvas.canvas Gtk.obj)
  val aa = { Gobject.name = "aa"; Gobject.conv = Gobject.Data.boolean }
  method root = !new_group (Canvas.root obj)
  method aa = Gobject.get aa obj
  method set_scroll_region = Canvas.set_scroll_region obj
  method get_scroll_region = Canvas.get_scroll_region obj
  method set_center_scroll_region = Canvas.set_center_scroll_region obj
  method get_center_scroll_region = Canvas.get_center_scroll_region obj
  method set_pixels_per_unit = Canvas.set_pixels_per_unit obj
  method scroll_to = Canvas.scroll_to obj
  method get_scroll_offsets = Canvas.get_scroll_offsets obj
  method update_now () = Canvas.update_now obj
  method get_item_at ~x ~y = !new_base_item (Canvas.get_item_at obj ~x ~y)
  method w2c_affine = Canvas.w2c_affine obj
  method w2c = Canvas.w2c obj
  method w2c_d = Canvas.w2c_d obj
  method c2w = Canvas.c2w obj
  method window_to_world = Canvas.window_to_world obj
  method world_to_window   = Canvas.world_to_window obj
end

let () = new_base_item := new base_item

class group grp_obj = object
  inherit base_item (grp_obj : GnomeCanvas.group Gtk.obj)
  method as_group = grp_obj 
  method get_items = List.map (new base_item) (Group.get_items grp_obj)
  method set (p : GnomeCanvas.group_p list) = set_properties grp_obj p
end

let () = new_group := new group

class ['p] item obj = 
  object
    inherit base_item obj
    method set (p : 'p list) = set_properties obj p
  end

let canvas ?(aa=false) =
  GContainer.pack_container [] ~create:(fun pl ->
    let w =
      if aa then Canvas.new_canvas_aa () else Canvas.new_canvas () in
    Gobject.set_params w pl;
    new canvas w)

let wrap_item o (typ : (_, 'p) Types.t) =
  if not (Types.is_a o typ)
  then raise (Gobject.Cannot_cast (Gobject.Type.name (Gobject.get_type o), 
				   Types.name typ)) ;
  (new item o : 'p item)

let construct_item (typ : (_, 'p) Types.t) ~props parent =
  let i = Item.new_item parent#as_group typ in
  let o = (new item i : 'p item) in
  if props <> [] then o#set props ;
  o

let unoption_list ~rest l =
  List.fold_right (fun o acc -> match o with Some v -> v :: acc | None -> acc) l rest

let group ?x ?y parent =
  let i = Item.new_item parent#as_group Types.group in
  let g = new group i in
  let props = unoption_list ~rest:[]
      [ ( match x with None -> None | Some v -> Some (`X v) ) ;
	( match y with None -> None | Some v -> Some (`Y v) ) ; ] in
  if props <> [] then g#set props ;
  g

type rect = GnomeCanvas.re_p item
let rect ?x1 ?y1 ?x2 ?y2 ?fill_color ?(props=[]) p = 
  let props = unoption_list ~rest:props
      [ ( match x1 with None -> None | Some v -> Some (`X1 v) ) ;
	( match y1 with None -> None | Some v -> Some (`Y1 v) ) ;
	( match x2 with None -> None | Some v -> Some (`X2 v) ) ;
	( match y2 with None -> None | Some v -> Some (`Y2 v) ) ;
	( match fill_color with None -> None | Some v -> Some (`FILL_COLOR v) ) ;
      ] in
  construct_item Types.rect ~props p

type ellipse = GnomeCanvas.re_p item
let ellipse ?x1 ?y1 ?x2 ?y2 ?fill_color ?(props=[]) p = 
  let props = unoption_list ~rest:props
      [ ( match x1 with None -> None | Some v -> Some (`X1 v) ) ;
	( match y1 with None -> None | Some v -> Some (`Y1 v) ) ;
	( match x2 with None -> None | Some v -> Some (`X2 v) ) ;
	( match y2 with None -> None | Some v -> Some (`Y2 v) ) ;
	( match fill_color with None -> None | Some v -> Some (`FILL_COLOR v) ) ;
      ] in
  construct_item Types.ellipse ~props p

class text txt_obj = object
  inherit [GnomeCanvas.text_p] item (txt_obj : GnomeCanvas.text Gtk.obj)
  method text_height =
    Gobject.Property.get txt_obj GnomeCanvas.Text.text_height
  method text_width =
    Gobject.Property.get txt_obj GnomeCanvas.Text.text_width
end
let text ?x ?y ?text ?font ?size ?anchor ?(props=[]) p =
  let props = unoption_list ~rest:props
      [ ( match x with None -> None | Some v -> Some (`X v) ) ;
	( match y with None -> None | Some v -> Some (`Y v) ) ;
	( match text with None -> None | Some v -> Some (`TEXT v) ) ;
	( match font with None -> None | Some v -> Some (`FONT v) ) ;
	( match size with None -> None | Some v -> Some (`SIZE v) ) ;
	( match anchor with None -> None | Some v -> Some (`ANCHOR v) ) ; 
      ] in
  let i = Item.new_item p#as_group Types.text in
  let o = new text i in
  if props <> [] then o#set props ;
  o

type line = GnomeCanvas.line_p item
let line ?points ?fill_color ?(props=[]) p = 
  let props = unoption_list ~rest:props
      [ ( match points with None -> None | Some v -> Some (`POINTS v) ) ;
	( match fill_color with None -> None | Some v -> Some (`FILL_COLOR v) ) ;
      ] in
  construct_item Types.line ~props p

type bpath = GnomeCanvas.bpath_p item
let bpath ?bpath ?fill_color ?(props=[]) p = 
  let props = unoption_list ~rest:props
      [ ( match bpath with None -> None | Some v -> Some (`BPATH v) ) ;
	( match fill_color with None -> None | Some v -> Some (`FILL_COLOR v) ) ;
      ] in
  construct_item Types.bpath ~props p

type pixbuf = GnomeCanvas.pixbuf_p item
let pixbuf ?x ?y ?pixbuf ?width ?height ?(props=[]) p =
  let width = match (width, pixbuf) with
  | (None, Some p) -> Some (`WIDTH (float (GdkPixbuf.get_width p)))
  | (None, _) -> None
  | (Some v, _) -> Some (`WIDTH v) in
  let height = match (height, pixbuf) with
  | (None, Some p) -> Some (`HEIGHT (float (GdkPixbuf.get_height p)))
  | (None, _) -> None
  | (Some v, _) -> Some (`HEIGHT v) in
  let props = unoption_list ~rest:props
      [ ( match x with None -> None | Some v -> Some (`X v) ) ;
	( match y with None -> None | Some v -> Some (`Y v) ) ;
	( match pixbuf with None -> None | Some v -> Some (`PIXBUF v) ) ;
	width ; height ;
      ] in
  construct_item Types.pixbuf ~props p

type polygon = GnomeCanvas.polygon_p item
let polygon ?points ?fill_color ?(props=[]) p =
  let props = unoption_list ~rest:props
      [ ( match points with None -> None | Some v -> Some (`POINTS v) ) ;
	( match fill_color with None -> None | Some v -> Some (`FILL_COLOR v) ) ;
      ] in
  construct_item Types.polygon ~props p

type widget = GnomeCanvas.widget_p item
let widget ?widget ?x ?y ?width ?height ?(props=[]) p =
  let w = match widget with None -> None | Some wi -> Some (`WIDGET wi#coerce) in
  let props = unoption_list ~rest:props
      [ ( match x with None -> None | Some v -> Some (`X v) ) ;
	( match y with None -> None | Some v -> Some (`Y v) ) ;
	( match width with None -> None | Some v -> Some (`WIDTH v) ) ;
	( match height with None -> None | Some v -> Some (`HEIGHT v) ) ;
	w ] in
  construct_item Types.widget ~props p

class rich_text rchtxt_obj = object
  inherit [GnomeCanvas.rich_text_p] item (rchtxt_obj : GnomeCanvas.rich_text Gtk.obj)
  method cut_clipboard () = RichText.cut_clipboard obj
  method copy_clipboard () = RichText.copy_clipboard obj
  method paste_clipboard () = RichText.paste_clipboard obj
  method get_buffer = new GText.buffer (RichText.get_buffer obj)
end

let rich_text ?x ?y ?text ?width ?height ?(props=[]) p =
  let props = unoption_list ~rest:props
      [ ( match x with None -> None | Some v -> Some (`X v) ) ;
	( match y with None -> None | Some v -> Some (`Y v) ) ;
	( match width with None -> None | Some v -> Some (`WIDTH v) ) ;
	( match height with None -> None | Some v -> Some (`HEIGHT v) ) ;
	( match text with None -> None | Some t -> Some (`TEXT t) ) ;
      ] in
  let i = Item.new_item p#as_group Types.rich_text in
  let o = new rich_text i in
  if props <> [] then o#set props ;
  o
