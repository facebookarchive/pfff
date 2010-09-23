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

(* $Id: gdk.ml 1452 2009-05-08 10:15:38Z garrigue $ *)

open StdLabels
open Gaux
open Gobject

type color
type colormap
type visual
type screen = [`gdkscreen] obj
type region
type gc
type window = [`drawable|`gdkwindow] obj
type pixmap = [`drawable|`gdkpixmap] obj
type bitmap = [`drawable|`gdkpixmap|`gdkbitmap] obj
type font
type image = [`gdkimage] obj
type atom
type keysym = int
type +'a event
type drag_context = [`dragcontext] Gobject.obj
type cursor
type xid = int32
type device
type display

exception Error of string
let _ = Callback.register_exception "gdkerror" (Error"")

external _gdk_init : unit -> unit = "ml_gdk_init"
let () = _gdk_init ()

module Tags = struct
  type event_type =
    [ `NOTHING | `DELETE | `DESTROY | `EXPOSE | `MOTION_NOTIFY
    | `BUTTON_PRESS | `TWO_BUTTON_PRESS | `THREE_BUTTON_PRESS | `BUTTON_RELEASE
    | `KEY_PRESS | `KEY_RELEASE
    | `ENTER_NOTIFY | `LEAVE_NOTIFY | `FOCUS_CHANGE
    | `CONFIGURE | `MAP | `UNMAP | `PROPERTY_NOTIFY
    | `SELECTION_CLEAR | `SELECTION_REQUEST | `SELECTION_NOTIFY
    | `PROXIMITY_IN | `PROXIMITY_OUT
    | `DRAG_ENTER | `DRAG_LEAVE | `DRAG_MOTION | `DRAG_STATUS
    | `DROP_START | `DROP_FINISHED | `CLIENT_EVENT | `VISIBILITY_NOTIFY
    | `NO_EXPOSE | `SCROLL | `WINDOW_STATE | `SETTING ]

  type event_mask =
    [ `EXPOSURE
    | `POINTER_MOTION | `POINTER_MOTION_HINT
    | `BUTTON_MOTION | `BUTTON1_MOTION | `BUTTON2_MOTION | `BUTTON3_MOTION
    | `BUTTON_PRESS | `BUTTON_RELEASE
    | `KEY_PRESS | `KEY_RELEASE
    | `ENTER_NOTIFY | `LEAVE_NOTIFY | `FOCUS_CHANGE
    | `STRUCTURE | `PROPERTY_CHANGE | `VISIBILITY_NOTIFY
    | `PROXIMITY_IN | `PROXIMITY_OUT
    | `SUBSTRUCTURE | `SCROLL
    | `ALL_EVENTS ]

  type extension_mode =
    [ `NONE | `ALL | `CURSOR ]

  type visibility_state =
    [ `UNOBSCURED | `PARTIAL | `FULLY_OBSCURED ]

  type input_source =
    [ `MOUSE | `PEN | `ERASER | `CURSOR ]

  type scroll_direction =
    [ `UP | `DOWN | `LEFT | `RIGHT ]

  type notify_type =
    [ `ANCESTOR | `VIRTUAL | `INFERIOR | `NONLINEAR
    | `NONLINEAR_VIRTUAL | `UNKNOWN ] 

  type crossing_mode = [ `NORMAL | `GRAB | `UNGRAB ]

  type setting_action = [ `NEW | `CHANGED | `DELETED ]

  type window_state =
    [ `WITHDRAWN | `ICONIFIED | `MAXIMIZED | `STICKY ]

  type modifier =
    [ `SHIFT | `LOCK | `CONTROL | `MOD1 | `MOD2 | `MOD3 | `MOD4 | `MOD5
    | `BUTTON1 | `BUTTON2 | `BUTTON3 | `BUTTON4 | `BUTTON5 ]

  type drag_action =
    [ `DEFAULT | `COPY | `MOVE | `LINK | `PRIVATE | `ASK ]

  type rgb_dither = 
    [ `NONE | `NORMAL | `MAX]

  type property_state = [ `NEW_VALUE | `DELETE ]

  type property_mode = [ `REPLACE | `PREPEND | `APPEND ]

  type xdata =
    [ `BYTES of string
    | `SHORTS of int array
    | `INT32S of int32 array ]

  type xdata_ret = [ xdata | `NONE ]

  type gravity =
    [ `NORTH_WEST | `NORTH | `NORTH_EAST | `WEST | `CENTER | `EAST
    | `SOUTH_WEST | `SOUTH | `SOUTH_EAST | `STATIC ]

  type window_type_hint =
    [ `NORMAL | `DIALOG | `MENU | `TOOLBAR | `SPLASHSCREEN | `UTILITY
    | `DOCK | `DESKTOP ]
end
open Tags

module Convert = struct
  external test_modifier : modifier -> int -> bool
      = "ml_test_GdkModifier_val"
  let modifier i =
    List.filter [`SHIFT;`LOCK;`CONTROL;`MOD1;`MOD2;`MOD3;`MOD4;`MOD5;
		 `BUTTON1;`BUTTON2;`BUTTON3;`BUTTON4;`BUTTON5]
      ~f:(fun m -> test_modifier m i)
  external test_window_state : window_state -> int -> bool
      = "ml_test_GdkWindowState_val"
  let window_state i =
    List.filter [ `WITHDRAWN; `ICONIFIED; `MAXIMIZED; `STICKY ]
      ~f:(fun m -> test_window_state m i)
end

module Atom = struct
  external intern : string -> bool -> atom = "ml_gdk_atom_intern"
  let intern ?(dont_create=false) name = intern name dont_create
  external name : atom -> string = "ml_gdk_atom_name"
  let none = intern "NONE"
  let primary = intern "PRIMARY"
  let secondary = intern "SECONDARY"
  let clipboard = intern "CLIPBOARD"
  let string = intern "STRING"
end

module Property = struct
  external change :
      window ->
      property:atom -> typ:atom -> mode:property_mode -> xdata -> unit
      = "ml_gdk_property_change"
  let change ~window ~typ ?(mode=`REPLACE) property data =
    change window ~property ~typ ~mode data
  external get :
      window -> property:atom ->
      max_length:int -> delete:bool -> (atom * xdata) option
      = "ml_gdk_property_get"
  let get ~window ?(max_length=65000) ?(delete=false) property =
    get window ~property ~max_length ~delete
  external delete : window:window -> atom -> unit
      = "ml_gdk_property_delete"
end

module Screen = struct
  external get_width : screen -> int = "ml_gdk_screen_get_width"
  external width : unit -> int = "ml_gdk_screen_width"
  let width ?screen () =
    match screen with None -> width () | Some s -> get_width s
  external get_height : screen -> int = "ml_gdk_screen_get_height"
  external height : unit -> int = "ml_gdk_screen_height"
  let height ?screen () =
    match screen with None -> height () | Some s -> get_height s
  external get_pango_context_for : screen -> Pango.context =
    "ml_gdk_pango_context_get_for_screen"
  external get_pango_context : unit -> Pango.context =
    "ml_gdk_pango_context_get"
  let get_pango_context ?screen () =
    match screen with None -> get_pango_context ()
    | Some s -> get_pango_context_for s

  (* Only with Gtk-2.2 *)
  external default : unit -> screen = "ml_gdk_screen_get_default"
end

module Visual = struct
  type visual_type =
    [ `STATIC_GRAY|`GRAYSCALE|`STATIC_COLOR
     |`PSEUDO_COLOR|`TRUE_COLOR|`DIRECT_COLOR ]

  external get_best : ?depth:int -> ?kind:visual_type -> unit -> visual
      = "ml_gdk_visual_get_best"
  external get_type : visual -> visual_type = "ml_GdkVisual_type"
  external depth : visual -> int = "ml_GdkVisual_depth"
  external red_mask : visual -> int = "ml_GdkVisual_red_mask"
  external red_shift : visual -> int = "ml_GdkVisual_red_shift"
  external red_prec : visual -> int = "ml_GdkVisual_red_prec"
  external green_mask : visual -> int = "ml_GdkVisual_green_mask"
  external green_shift : visual -> int = "ml_GdkVisual_green_shift"
  external green_prec : visual -> int = "ml_GdkVisual_green_prec"
  external blue_mask : visual -> int = "ml_GdkVisual_blue_mask"
  external blue_shift : visual -> int = "ml_GdkVisual_blue_shift"
  external blue_prec : visual -> int = "ml_GdkVisual_blue_prec"
end

module Image = struct
  type image_type =
    [ `NORMAL|`SHARED|`FASTEST ]

  let cast w : image = Gobject.try_cast w "GdkImage"
  let destroy = Gobject.unsafe_unref

  external create : kind: image_type -> visual: visual -> 
    width: int -> height: int -> image
      = "ml_gdk_image_new"
  external get :
      [>`drawable] obj -> x: int -> y: int -> width: int -> height: int -> image
      = "ml_gdk_drawable_get_image"
  external put_pixel : image -> x: int -> y: int -> pixel: int -> unit
    = "ml_gdk_image_put_pixel"
  external get_pixel : image -> x: int -> y: int -> int
    = "ml_gdk_image_get_pixel"
  external width : image -> int = "ml_gdk_image_width"
  external height : image -> int = "ml_gdk_image_height"
  external depth : image -> int = "ml_gdk_image_depth"
  external get_visual : image -> visual = "ml_gdk_image_visual"
end

module Color = struct
  external color_white : colormap -> color = "ml_gdk_color_white"
  external color_black : colormap -> color = "ml_gdk_color_black"
  external color_parse : string -> color = "ml_gdk_color_parse"
  external color_alloc : colormap -> color -> bool = "ml_gdk_color_alloc"
  external color_create : red:int -> green:int -> blue:int -> color
      = "ml_GdkColor"

  external get_system_colormap : unit -> colormap
      = "ml_gdk_colormap_get_system"
  external colormap_new : visual -> privat:bool -> colormap
      = "ml_gdk_colormap_new"
  let get_colormap ?(privat=false) vis = colormap_new vis ~privat
  external get_visual : colormap -> visual
      = "ml_gdk_colormap_get_visual"

  type spec = [ `BLACK | `NAME of string | `RGB of int * int * int | `WHITE]
  let color_alloc ~colormap color =
    if not (color_alloc colormap color) then raise (Error"Color.alloc");
    color
  let alloc ~colormap color =
    match color with
      `WHITE -> color_white colormap
    | `BLACK -> color_black colormap
    | `NAME s -> color_alloc ~colormap (color_parse s)
    | `RGB (red,green,blue) ->
	color_alloc ~colormap (color_create ~red ~green ~blue)

  external red : color -> int = "ml_GdkColor_red"
  external blue : color -> int = "ml_GdkColor_blue"
  external green : color -> int = "ml_GdkColor_green"
  external pixel : color -> int = "ml_GdkColor_pixel"
end

module Rectangle = struct
  type t
  external create : x:int -> y:int -> width:int -> height:int -> t
      = "ml_GdkRectangle"
  external x : t -> int = "ml_GdkRectangle_x"
  external y : t -> int = "ml_GdkRectangle_y"
  external width : t -> int = "ml_GdkRectangle_width"
  external height : t -> int = "ml_GdkRectangle_height"
end

module Drawable = struct
  let cast w : [`drawable] obj = Gobject.try_cast w "GdkDrawable"
  external get_visual : [>`drawable] obj -> visual
    = "ml_gdk_drawable_get_visual"
  external get_depth : [>`drawable] obj -> int
    = "ml_gdk_drawable_get_depth"
  external get_colormap : [>`drawable] obj -> colormap
    = "ml_gdk_drawable_get_colormap"
  external get_size : [>`drawable] obj -> int * int
    = "ml_gdk_drawable_get_size"
end

module Window = struct
  let cast w : window = Gobject.try_cast w "GdkWindow"
  type background_pixmap = [ `NONE | `PARENT_RELATIVE | `PIXMAP of pixmap]
  external get_parent : window -> window = "ml_gdk_window_get_parent"
  external get_position : window -> int * int = "ml_gdk_window_get_position"
  external get_pointer_location : window -> int * int =
    "ml_gdk_window_get_pointer_location"
  external root_parent : unit -> window = "ml_GDK_ROOT_PARENT"
  external set_back_pixmap : window -> pixmap -> int -> unit = 
    "ml_gdk_window_set_back_pixmap"
  external set_cursor : window -> cursor -> unit = 
    "ml_gdk_window_set_cursor"
  external clear : window -> unit = "ml_gdk_window_clear"
  external get_xwindow : [>`drawable] obj -> xid = "ml_GDK_WINDOW_XWINDOW"

  let set_back_pixmap w pix = 
    let null_pixmap = (Obj.magic Gpointer.boxed_null : pixmap) in
    match pix with
      `NONE -> set_back_pixmap w null_pixmap 0
    | `PARENT_RELATIVE -> set_back_pixmap w null_pixmap 1
    | `PIXMAP(pixmap) -> set_back_pixmap w pixmap 0 
       (* anything OK, Maybe... *) 

  (* for backward compatibility for lablgtk1 programs *)	  
  let get_visual = Drawable.get_visual
end

module PointArray = struct
  type t = { len: int}
  external create : len:int -> t = "ml_point_array_new"
  external set : t -> pos:int -> x:int -> y:int -> unit = "ml_point_array_set"
  let set arr ~pos =
    if pos < 0 || pos >= arr.len then invalid_arg "PointArray.set";
    set arr ~pos
end

module SegmentArray = struct
  type t = { len: int}
  external create : len:int -> t = "ml_segment_array_new"
  external set : t -> pos:int -> x1:int -> y1:int -> x2:int -> y2: int -> unit = "ml_segment_array_set_bc" "ml_segment_array_set"
  let set arr ~pos =
    if pos < 0 || pos >= arr.len then invalid_arg "SegmentArray.set";
    set arr ~pos
end

module Region = struct
  type gdkFillRule = [ `EVEN_ODD_RULE|`WINDING_RULE ]
  type gdkOverlapType = [ `IN|`OUT|`PART ]
  external create : unit -> region = "ml_gdk_region_new"
  external destroy : region -> unit = "ml_gdk_region_destroy"
  external polygon : PointArray.t -> gdkFillRule -> region 
      = "ml_gdk_region_polygon"
  let polygon l =
    let len = List.length l in
    let arr = PointArray.create ~len in
    List.fold_left l ~init:0
      ~f:(fun pos (x,y) -> PointArray.set arr ~pos ~x ~y; pos+1);
    polygon arr
  external copy : region -> region
      = "ml_gdk_region_copy"
  external intersect : region -> region -> region
      = "ml_gdk_region_intersect"
  external union : region -> region -> region 
      = "ml_gdk_region_union"
  external subtract : region -> region -> region 
      = "ml_gdk_region_subtract"
  external xor : region -> region -> region 
      = "ml_gdk_region_xor"
  external union_with_rect : region -> Rectangle.t -> region
      = "ml_gdk_region_union_with_rect"
  let intersect r1 r2 = let r3 = copy r1 in intersect r3 r2; r3
  let union r1 r2 = let r3 = copy r1 in union r3 r2; r3
  let subtract r1 r2 = let r3 = copy r1 in subtract r3 r2; r3
  let xor r1 r2 = let r3 = copy r1 in xor r3 r2; r3
  let union_with_rect r1 r2 = let r3 = copy r1 in union_with_rect r3 r2; r3
  external offset : region -> x:int -> y:int -> unit = "ml_gdk_region_offset"
  external shrink : region -> x:int -> y:int -> unit = "ml_gdk_region_shrink"
  external empty : region -> bool = "ml_gdk_region_empty"
  external equal : region -> region -> bool = "ml_gdk_region_equal"
  external point_in : region -> x:int -> y:int -> bool 
      = "ml_gdk_region_point_in"
  external rect_in : region -> Rectangle.t -> gdkOverlapType
      = "ml_gdk_region_rect_in"
  external get_clipbox : region -> Rectangle.t -> unit
      = "ml_gdk_region_get_clipbox"
end
      

module GC = struct
  type gdkFunction = [ `COPY|`INVERT|`XOR ]
  type gdkFill = [ `SOLID|`TILED|`STIPPLED|`OPAQUE_STIPPLED ]
  type gdkSubwindowMode = [ `CLIP_BY_CHILDREN|`INCLUDE_INFERIORS ]
  type gdkLineStyle = [ `SOLID|`ON_OFF_DASH|`DOUBLE_DASH ]
  type gdkCapStyle = [ `NOT_LAST|`BUTT|`ROUND|`PROJECTING ]
  type gdkJoinStyle = [ `MITER|`ROUND|`BEVEL ]
  external create : [>`drawable] obj -> gc = "ml_gdk_gc_new"
  external set_foreground : gc -> color -> unit = "ml_gdk_gc_set_foreground"
  external set_background : gc -> color -> unit = "ml_gdk_gc_set_background"
  external set_font : gc -> font -> unit = "ml_gdk_gc_set_font"
  external set_function : gc -> gdkFunction -> unit = "ml_gdk_gc_set_function"
  external set_fill : gc -> gdkFill -> unit = "ml_gdk_gc_set_fill"
  external set_tile : gc -> pixmap -> unit = "ml_gdk_gc_set_tile"
  external set_stipple : gc -> pixmap -> unit = "ml_gdk_gc_set_stipple"
  external set_ts_origin : gc -> x:int -> y:int -> unit
      = "ml_gdk_gc_set_ts_origin"
  external set_clip_origin : gc -> x:int -> y:int -> unit
      = "ml_gdk_gc_set_clip_origin"
  external set_clip_mask : gc -> bitmap -> unit = "ml_gdk_gc_set_clip_mask"
  external set_clip_rectangle : gc -> Rectangle.t -> unit
      = "ml_gdk_gc_set_clip_rectangle"
  external set_clip_region : gc -> region -> unit = "ml_gdk_gc_set_clip_region"
  external set_subwindow : gc -> gdkSubwindowMode -> unit
      = "ml_gdk_gc_set_subwindow"
  external set_exposures : gc -> bool -> unit = "ml_gdk_gc_set_exposures"
  external set_line_attributes :
      gc -> width:int -> style:gdkLineStyle -> cap:gdkCapStyle ->
      join:gdkJoinStyle -> unit
      = "ml_gdk_gc_set_line_attributes"
  external set_dashes : gc -> offset:int -> int list -> unit =
    "ml_gdk_gc_set_dashes"
  external copy : dst:gc -> gc -> unit = "ml_gdk_gc_copy"
  type values = {
      foreground : color;
      background : color;
      font : font option;
      fonction : gdkFunction;
      fill : gdkFill;
      tile : pixmap option;
      stipple : pixmap option;
      clip_mask : bitmap option;
      subwindow_mode : gdkSubwindowMode;
      ts_x_origin : int;
      ts_y_origin : int;
      clip_x_origin : int;
      clip_y_origin : int;
      graphics_exposures : bool;
      line_width : int;
      line_style : gdkLineStyle;
      cap_style : gdkCapStyle;
      join_style : gdkJoinStyle;
    }
  external get_values : gc -> values = "ml_gdk_gc_get_values"
end

module Pixmap = struct
  let cast w : pixmap = Gobject.try_cast w "GdkPixmap"
  let destroy = Gobject.unsafe_unref
  open Gpointer
  external create :
      window optboxed -> width:int -> height:int -> depth:int -> pixmap
      = "ml_gdk_pixmap_new"
  let create ?window ~width ~height ?(depth = -1) () =
    try create (optboxed window) ~width ~height ~depth
    with _ -> failwith "Gdk.Pixmap.create"
  external create_from_data :
      window optboxed -> string -> width:int -> height:int -> depth:int ->
      fg:color -> bg:color -> pixmap
      = "ml_gdk_pixmap_create_from_data_bc" "ml_gdk_pixmap_create_from_data"
  let create_from_data ?window ~width ~height ?(depth = -1) ~fg ~bg data =
    try create_from_data (optboxed window) data ~width ~height ~depth ~fg ~bg
    with _ -> failwith "Gdk.Pixmap.create_from_data"
  external create_from_xpm :
      ?window:window -> ?colormap:colormap -> ?transparent:color ->
      file:string -> unit -> pixmap * bitmap
      = "ml_gdk_pixmap_colormap_create_from_xpm"
  external create_from_xpm_d :
      ?window:window -> ?colormap:colormap -> ?transparent:color ->
      data:string array -> unit -> pixmap * bitmap
      = "ml_gdk_pixmap_colormap_create_from_xpm_d"
end

module Bitmap = struct
  let cast w : bitmap =
    let w = Gobject.try_cast w "GdkPixmap" in
    if Drawable.get_depth w <> 1 then
      raise (Gobject.Cannot_cast("GdkPixmap","GdkBitmap"));
    w
  open Gpointer
  let create ?window ~width ~height () : bitmap =
    Gobject.unsafe_cast (Pixmap.create ?window ~width ~height ~depth:1 ())
  external create_from_data :
      window optboxed -> string -> width:int -> height:int -> bitmap
      = "ml_gdk_bitmap_create_from_data"
  let create_from_data ?window ~width ~height data =
    try create_from_data (optboxed window) data ~width ~height
    with _ -> failwith "Gdk.Bitmap.create_from_data"
end

module Font = struct
  external load : string -> font = "ml_gdk_font_load"
  external load_fontset : string -> font = "ml_gdk_fontset_load"
  external string_width : font -> string -> int = "ml_gdk_string_width"
  external char_width : font -> char -> int = "ml_gdk_char_width"
  external string_height : font -> string -> int = "ml_gdk_string_height"
  external char_height : font -> char -> int = "ml_gdk_char_height"
  external string_measure : font -> string -> int = "ml_gdk_string_measure"
  external char_measure : font -> char -> int = "ml_gdk_char_measure"
  external get_type : font -> [`FONT | `FONTSET] = "ml_GdkFont_type"
  external ascent : font -> int = "ml_GdkFont_ascent"
  external descent : font -> int = "ml_GdkFont_descent"
end

module Draw = struct
  external point : [>`drawable] obj -> gc -> x:int -> y:int -> unit
      = "ml_gdk_draw_point"
  external line :
      [>`drawable] obj -> gc -> x:int -> y:int -> x:int -> y:int -> unit
      = "ml_gdk_draw_line_bc" "ml_gdk_draw_line"
  external rectangle :
      [>`drawable] obj -> gc ->
      filled:bool -> x:int -> y:int -> width:int -> height:int -> unit
      = "ml_gdk_draw_rectangle_bc" "ml_gdk_draw_rectangle"
  let rectangle w gc ~x ~y ~width ~height ?(filled=false) () =
    rectangle w gc ~x ~y ~width ~height ~filled
  external arc :
      [>`drawable] obj -> gc -> filled:bool -> x:int -> y:int ->
      width:int -> height:int -> start:int -> angle:int -> unit
      = "ml_gdk_draw_arc_bc" "ml_gdk_draw_arc"
  let arc w gc ~x ~y ~width ~height ?(filled=false) ?(start=0.)
      ?(angle=360.) () =
    arc w gc ~x ~y ~width ~height ~filled
      ~start:(truncate(start *. 64.))
      ~angle:(truncate(angle *. 64.))

  let f_pointarray f l = 
    let array_of_points l =
      let len = List.length l in
      let arr = PointArray.create ~len in
      List.fold_left l ~init:0
      	~f:(fun pos (x,y) -> PointArray.set arr ~pos ~x ~y; pos+1);
      arr
    in
    f (array_of_points l)

  let f_segmentarray f l = 
    let array_of_segments l =
      let len = List.length l in
      let arr = SegmentArray.create ~len in
      List.fold_left l ~init:0
      	~f:(fun pos ((x1,y1),(x2,y2)) -> 
	  SegmentArray.set arr ~pos ~x1 ~y1 ~x2 ~y2; pos+1);
      arr
    in
    f (array_of_segments l)

  external polygon :
    [>`drawable] obj -> gc -> filled:bool -> PointArray.t -> unit
    = "ml_gdk_draw_polygon"
  let polygon w gc ?(filled=false) = function
    | [] -> ()
    | l -> f_pointarray (polygon w gc ~filled) l
  external string :
    [>`drawable] obj -> font: font -> gc -> x: int -> y: int -> string -> unit
    = "ml_gdk_draw_string_bc" "ml_gdk_draw_string"	
  external layout :
    [>`drawable] obj -> gc -> x: int -> y: int -> Pango.layout ->
    ?fore:color -> ?back:color -> unit
    = "ml_gdk_draw_layout_with_colors_bc" "ml_gdk_draw_layout_with_colors"
  external image_ : [>`drawable] obj -> gc -> image -> 
    xsrc: int -> ysrc: int -> xdest: int -> ydest: int -> 
    width: int -> height: int -> unit
    = "ml_gdk_draw_image_bc" "ml_gdk_draw_image"
  let image w gc ?(xsrc=0) ?(ysrc=0) ?(xdest=0) ?(ydest=0)
      ?(width= -1) ?(height= -1) image =
    image_ w gc image ~xsrc ~ysrc ~xdest ~ydest ~width ~height
(*
  external bitmap : [>`drawable] obj -> gc -> bitmap: bitmap -> 
    xsrc: int -> ysrc: int -> xdest: int -> ydest: int -> 
    width: int -> height: int -> unit
      = "ml_gdk_draw_bitmap_bc" "ml_gdk_draw_bitmap"
*)
  external pixmap_ : [>`drawable] obj -> gc -> pixmap -> 
    xsrc: int -> ysrc: int -> xdest: int -> ydest: int -> 
    width: int -> height: int -> unit
    = "ml_gdk_draw_pixmap_bc" "ml_gdk_draw_pixmap"
  let pixmap w gc ?(xsrc=0) ?(ysrc=0) ?(xdest=0) ?(ydest=0)
      ?(width= -1) ?(height= -1) pixmap =
    pixmap_ w gc pixmap ~xsrc ~ysrc ~xdest ~ydest ~width ~height

  external points : [>`drawable] obj -> gc -> PointArray.t -> unit
      = "ml_gdk_draw_points"
  let points w gc l = f_pointarray (points w gc) l
  external lines : [>`drawable] obj -> gc -> PointArray.t -> unit
      = "ml_gdk_draw_lines"
  let lines w gc l = f_pointarray (lines w gc) l
  external segments : [>`drawable] obj -> gc -> SegmentArray.t -> unit
      = "ml_gdk_draw_segments"
  let segments w gc = function
    | [] -> ()
    | l -> f_segmentarray (segments w gc) l
end

module Rgb = struct
  external init : unit -> unit = "ml_gdk_rgb_init"
  external get_visual : unit -> visual = "ml_gdk_rgb_get_visual"
  external get_cmap : unit -> colormap = "ml_gdk_rgb_get_cmap"
  external draw_image_ :
    [>`drawable] obj -> gc -> x:int -> y:int -> width:int -> height:int ->
    dither:rgb_dither -> buf:Gpointer.region -> row_stride:int -> unit
    = "ml_gdk_draw_rgb_image_bc" "ml_gdk_draw_rgb_image"
  let draw_image w gc ~width ~height ?(x=0) ?(y=0) ?(dither=`NORMAL)
    ?(row_stride=width*3) buf =
    if height <= 0 || width <= 0 || row_stride < width * 3
    || Gpointer.length buf < row_stride * (height - 1) + width
    then invalid_arg "Gdk.Rgb.draw_image";
    draw_image_ w gc ~x ~y ~width ~height ~dither ~buf ~row_stride
end

module DnD = struct
  external drag_status : drag_context -> drag_action option -> time:int32 -> unit
      = "ml_gdk_drag_status"
  external drag_context_suggested_action : drag_context -> drag_action
      = "ml_GdkDragContext_suggested_action"
  external drag_context_targets : drag_context -> atom list
      = "ml_GdkDragContext_targets"
end

module Truecolor = struct
  (* Truecolor quick color query *) 

  type visual_shift_prec = {
      red_shift : int;
      red_prec : int;
      green_shift : int;
      green_prec : int;
      blue_shift : int;
      blue_prec : int
    }
 
  let shift_prec visual = {
    red_shift = Visual.red_shift visual;
    red_prec = Visual.red_prec visual;
    green_shift = Visual.green_shift visual;
    green_prec = Visual.green_prec visual;
    blue_shift = Visual.blue_shift visual;
    blue_prec = Visual.blue_prec visual;
  }

  let color_creator visual =
    match Visual.get_type visual with
      `TRUE_COLOR | `DIRECT_COLOR ->
	let shift_prec = shift_prec visual in
        (* Format.eprintf "red : %d %d, "
	  shift_prec.red_shift shift_prec.red_prec;
	Format.eprintf "green : %d %d, "
	  shift_prec.green_shift shift_prec.green_prec;
	Format.eprintf "blue : %d %d"
	  shift_prec.blue_shift shift_prec.blue_prec;
	Format.pp_print_newline Format.err_formatter (); *)
	let red_lsr = 16 - shift_prec.red_prec
	and green_lsr = 16 - shift_prec.green_prec
	and blue_lsr = 16 - shift_prec.blue_prec in
	fun ~red: red ~green: green ~blue: blue ->
	  (((red lsr red_lsr) lsl shift_prec.red_shift) lor 
    	   ((green lsr green_lsr) lsl shift_prec.green_shift) lor
    	   ((blue lsr blue_lsr) lsl shift_prec.blue_shift))
    | _ -> raise (Invalid_argument "Gdk.Truecolor.color_creator")

  let color_parser visual =
    match Visual.get_type visual with
      `TRUE_COLOR | `DIRECT_COLOR ->
	let shift_prec = shift_prec visual in
	let red_lsr = 16 - shift_prec.red_prec
	and green_lsr = 16 - shift_prec.green_prec
	and blue_lsr = 16 - shift_prec.blue_prec in
	let mask = 1 lsl 16 - 1 in
	fun pixel ->
	  ((pixel lsr shift_prec.red_shift) lsl red_lsr) land mask,
	  ((pixel lsr shift_prec.green_shift) lsl green_lsr) land mask,
	  ((pixel lsr shift_prec.blue_shift) lsl blue_lsr) land mask
    | _ -> raise (Invalid_argument "Gdk.Truecolor.color_parser")
end

module X = struct
  (* X related functions *)
  external flush : unit -> unit
      = "ml_gdk_flush"
  external beep : unit -> unit
      = "ml_gdk_beep"
end

module Cursor = struct
  type cursor_type = [
    | `X_CURSOR
    | `ARROW
    | `BASED_ARROW_DOWN
    | `BASED_ARROW_UP
    | `BOAT
    | `BOGOSITY
    | `BOTTOM_LEFT_CORNER
    | `BOTTOM_RIGHT_CORNER
    | `BOTTOM_SIDE
    | `BOTTOM_TEE
    | `BOX_SPIRAL
    | `CENTER_PTR
    | `CIRCLE
    | `CLOCK
    | `COFFEE_MUG
    | `CROSS
    | `CROSS_REVERSE
    | `CROSSHAIR
    | `DIAMOND_CROSS
    | `DOT
    | `DOTBOX
    | `DOUBLE_ARROW
    | `DRAFT_LARGE
    | `DRAFT_SMALL
    | `DRAPED_BOX
    | `EXCHANGE
    | `FLEUR
    | `GOBBLER
    | `GUMBY
    | `HAND1
    | `HAND2
    | `HEART
    | `ICON
    | `IRON_CROSS
    | `LEFT_PTR
    | `LEFT_SIDE
    | `LEFT_TEE
    | `LEFTBUTTON
    | `LL_ANGLE
    | `LR_ANGLE
    | `MAN
    | `MIDDLEBUTTON
    | `MOUSE
    | `PENCIL
    | `PIRATE
    | `PLUS
    | `QUESTION_ARROW
    | `RIGHT_PTR
    | `RIGHT_SIDE
    | `RIGHT_TEE
    | `RIGHTBUTTON
    | `RTL_LOGO
    | `SAILBOAT
    | `SB_DOWN_ARROW
    | `SB_H_DOUBLE_ARROW
    | `SB_LEFT_ARROW
    | `SB_RIGHT_ARROW
    | `SB_UP_ARROW
    | `SB_V_DOUBLE_ARROW
    | `SHUTTLE
    | `SIZING
    | `SPIDER
    | `SPRAYCAN
    | `STAR
    | `TARGET
    | `TCROSS
    | `TOP_LEFT_ARROW
    | `TOP_LEFT_CORNER
    | `TOP_RIGHT_CORNER
    | `TOP_SIDE
    | `TOP_TEE
    | `TREK
    | `UL_ANGLE
    | `UMBRELLA
    | `UR_ANGLE
    | `WATCH
    | `XTERM
  ]
  external create : cursor_type -> cursor = "ml_gdk_cursor_new"
  external create_from_pixmap :
    pixmap -> mask:bitmap ->
    fg:color -> bg:color -> x:int -> y:int -> cursor
    = "ml_gdk_cursor_new_from_pixmap_bc" "ml_gdk_cursor_new_from_pixmap"
  external create_from_pixbuf :
    [`pixbuf] obj -> x:int -> y:int -> cursor
    = "ml_gdk_cursor_new_from_pixbuf" (** @since GTK 2.4 *)
  external get_image : cursor -> [`pixbuf] obj
    = "ml_gdk_cursor_get_image"       (** @since GTK 2.8 *)
end

module Display = struct
    (* since Gtk+-2.2 *)

  external default :
    unit -> display
    = "ml_gdk_display_get_default"
  external get_window_at_pointer :
    display -> (window * int * int) option
    = "ml_gdk_display_get_window_at_pointer"
  let window_at_pointer ?display () =
    get_window_at_pointer
      (match display with None -> default ()
      | Some disp -> disp)
end

module Windowing = struct
  external get : unit -> [`QUARTZ | `WIN32 | `X11] = "ml_gdk_get_platform"
  let platform = get ()
end
