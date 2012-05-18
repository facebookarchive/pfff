(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

type status =
    SUCCESS
  | NO_MEMORY
  | INVALID_RESTORE
  | INVALID_POP_GROUP
  | NO_CURRENT_POINT
  | INVALID_MATRIX
  | INVALID_STATUS
  | NULL_POINTER
  | INVALID_STRING
  | INVALID_PATH_DATA
  | READ_ERROR
  | WRITE_ERROR
  | SURFACE_FINISHED
  | SURFACE_TYPE_MISMATCH
  | PATTERN_TYPE_MISMATCH
  | INVALID_CONTENT
  | INVALID_FORMAT
  | INVALID_VISUAL
  | FILE_NOT_FOUND
  | INVALID_DASH
  | INVALID_DSC_COMMENT
  | INVALID_INDEX
  | CLIP_NOT_REPRESENTABLE
exception Error of status
let init = Callback.register_exception "cairo_status_exn" (Error NULL_POINTER)

external version_encode : int -> int -> int -> int = "ml_CAIRO_VERSION_ENCODE"
external run_time_version        : unit -> int = "ml_cairo_version"
external run_time_version_string : unit -> string = "ml_cairo_version_string"
external _version        : unit -> int = "ml_CAIRO_VERSION"
external _version_string : unit -> string = "ml_CAIRO_VERSION_STRING"
let compile_time_version = _version ()
let compile_time_version_string = _version_string ()

type t
type -'a surface
type -'a pattern
type -'a font_face

type surface_type = [
  | `Image 
  | `PDF | `PS | `SVG
  | `Xlib | `XCB
  | `Glitz | `Quartz | `Win32 | `BeOS | `DirectFB ]
type pattern_type = [
  | `Solid
  | `Surface
  | `Linear | `Radial ]
type font_type = [
  | `TOY
  | `FT
  | `Win32
  | `ATSUI ]

type content =
    CONTENT_COLOR
  | CONTENT_ALPHA
  | CONTENT_COLOR_ALPHA

type point = { x : float ; y : float }
type matrix = {
    xx : float ; yx : float ;
    xy : float ; yy : float ;
    x0 : float ; y0 : float 
  }

external create : [> `Any] surface -> t = "ml_cairo_create"
external save : t -> unit = "ml_cairo_save"
external restore : t -> unit = "ml_cairo_restore"

external _push_group : t -> unit = "ml_cairo_push_group"
external _push_group_with_content : t -> content -> unit = "ml_cairo_push_group_with_content"
let push_group ?content cr =
  match content with
  | None   -> _push_group cr
  | Some c -> _push_group_with_content cr c
external pop_group : t -> [`Any] pattern = "ml_cairo_pop_group"
external pop_group_to_source : t -> unit = "ml_cairo_pop_group_to_source"

type operator =
    OPERATOR_CLEAR

  | OPERATOR_SOURCE
  | OPERATOR_OVER
  | OPERATOR_IN
  | OPERATOR_OUT
  | OPERATOR_ATOP

  | OPERATOR_DEST
  | OPERATOR_DEST_OVER
  | OPERATOR_DEST_IN
  | OPERATOR_DEST_OUT
  | OPERATOR_DEST_ATOP

  | OPERATOR_XOR
  | OPERATOR_ADD
  | OPERATOR_SATURATE

external set_operator : t -> operator -> unit = "ml_cairo_set_operator"

external set_source_rgb  : t -> red:float -> green:float -> blue:float -> unit = "ml_cairo_set_source_rgb"
external set_source_rgba : t -> red:float -> green:float -> blue:float -> alpha:float ->unit = "ml_cairo_set_source_rgba"
external set_source : t -> [> `Any] pattern -> unit = "ml_cairo_set_source"
external set_source_surface : t -> [> `Any] surface -> float -> float -> unit = "ml_cairo_set_source_surface"

external set_tolerance : t -> float -> unit = "ml_cairo_set_tolerance"

type antialias =
    ANTIALIAS_DEFAULT
  | ANTIALIAS_NONE
  | ANTIALIAS_GRAY
  | ANTIALIAS_SUBPIXEL
external set_antialias : t -> antialias -> unit = "ml_cairo_set_antialias"

type fill_rule =
    FILL_RULE_WINDING
  | FILL_RULE_EVEN_ODD
external set_fill_rule : t -> fill_rule -> unit = "ml_cairo_set_fill_rule"
external set_line_width : t -> float -> unit = "ml_cairo_set_line_width"
type line_cap =
    LINE_CAP_BUTT
  | LINE_CAP_ROUND
  | LINE_CAP_SQUARE
external set_line_cap : t -> line_cap -> unit = "ml_cairo_set_line_cap"
type line_join =
    LINE_JOIN_MITER
  | LINE_JOIN_ROUND
  | LINE_JOIN_BEVEL
external set_line_join : t -> line_join -> unit = "ml_cairo_set_line_join"
external set_dash : t -> float array -> float -> unit = "ml_cairo_set_dash"
external set_miter_limit : t -> float -> unit = "ml_cairo_set_miter_limit"
external translate : t -> tx:float -> ty:float -> unit = "ml_cairo_translate"
external scale : t -> sx:float -> sy:float -> unit = "ml_cairo_scale"
external rotate : t -> angle:float -> unit = "ml_cairo_rotate"
external transform : t -> matrix -> unit = "ml_cairo_transform"
external set_matrix : t -> matrix -> unit = "ml_cairo_set_matrix"
external identity_matrix : t -> unit = "ml_cairo_identity_matrix"
external user_to_device : t -> point -> point = "ml_cairo_user_to_device"
external user_to_device_distance : t -> point -> point = "ml_cairo_user_to_device_distance"
external device_to_user : t -> point -> point = "ml_cairo_device_to_user"
external device_to_user_distance : t -> point -> point = "ml_cairo_device_to_user_distance"

external new_path : t -> unit = "ml_cairo_new_path"
external move_to : t -> x:float -> y:float -> unit = "ml_cairo_move_to"
let move_to_point cr { x = x ; y = y } = move_to cr ~x ~y
external new_sub_path : t -> unit = "ml_cairo_new_sub_path"
external line_to : t -> x:float -> y:float -> unit = "ml_cairo_line_to"
let line_to_point cr { x = x ; y = y } = line_to cr ~x ~y
external curve_to : t -> x1:float -> y1:float -> x2:float -> y2:float -> x3:float -> y3:float -> unit = "ml_cairo_curve_to_bc" "ml_cairo_curve_to"
let curve_to_point cr {x=x1; y=y1} {x=x2; y=y2} {x=x3; y=y3} = curve_to cr ~x1 ~y1 ~x2 ~y2 ~x3 ~y3
external arc : t -> xc:float -> yc:float -> radius:float -> angle1:float -> angle2:float -> unit = "ml_cairo_arc_bc" "ml_cairo_arc"
external arc_negative : t -> xc:float -> yc:float -> radius:float -> angle1:float -> angle2:float -> unit = "ml_cairo_arc_negative_bc" "ml_cairo_arc_negative"
external rel_move_to : t -> dx:float -> dy:float -> unit = "ml_cairo_rel_move_to"
external rel_line_to : t -> dx:float -> dy:float -> unit = "ml_cairo_rel_line_to"
external rel_curve_to : t -> dx1:float -> dy1:float -> dx2:float -> dy2:float -> dx3:float -> dy3:float -> unit = "ml_cairo_rel_curve_to_bc" "ml_cairo_rel_curve_to"
external rectangle : t -> x:float -> y:float -> width:float -> height:float -> unit = "ml_cairo_rectangle"
external close_path : t -> unit = "ml_cairo_close_path"

external paint : t -> unit = "ml_cairo_paint"
external paint_with_alpha : t -> float -> unit = "ml_cairo_paint_with_alpha"

external mask : t -> [> `Any] pattern -> unit = "ml_cairo_mask"
external mask_surface : t -> [> `Any] surface -> surface_x:float -> surface_y:float -> unit = "ml_cairo_mask_surface"

external stroke : t -> unit = "ml_cairo_stroke"
external stroke_preserve : t -> unit = "ml_cairo_stroke_preserve"
external fill : t -> unit = "ml_cairo_fill"
external fill_preserve : t -> unit = "ml_cairo_fill_preserve"
external copy_page : t -> unit = "ml_cairo_copy_page"
external show_page : t -> unit = "ml_cairo_show_page"

external in_stroke : t -> point -> bool = "ml_cairo_in_stroke"
external in_fill : t -> point -> bool = "ml_cairo_in_fill"

external stroke_extents : t -> float * float * float * float = "ml_cairo_stroke_extents"
external fill_extents : t -> float * float * float * float = "ml_cairo_fill_extents"
external clip : t -> unit = "ml_cairo_clip"
external clip_preserve : t -> unit = "ml_cairo_clip_preserve"
external reset_clip : t -> unit = "ml_cairo_reset_clip"


type glyph = { index : int; glyph_x : float; glyph_y : float; }
type text_extents = 
  { x_bearing   : float ;
    y_bearing   : float ;
    text_width  : float ;
    text_height : float ;
    x_advance   : float ;
    y_advance   : float }
type font_extents =
  { ascent : float;
    descent : float;
    font_height : float;
    max_x_advance : float;
    max_y_advance : float }
type font_slant =
    FONT_SLANT_NORMAL
  | FONT_SLANT_ITALIC
  | FONT_SLANT_OBLIQUE
type font_weight =
    FONT_WEIGHT_NORMAL
  | FONT_WEIGHT_BOLD
type subpixel_order =
    SUBPIXEL_ORDER_DEFAULT
  | SUBPIXEL_ORDER_RGB
  | SUBPIXEL_ORDER_BGR
  | SUBPIXEL_ORDER_VRGB
  | SUBPIXEL_ORDER_VBGR
type hint_style =
    HINT_STYLE_DEFAULT
  | HINT_STYLE_NONE
  | HINT_STYLE_SLIGHT
  | HINT_STYLE_MEDIUM
  | HINT_STYLE_FULL
type hint_metrics =
    HINT_METRICS_DEFAULT
  | HINT_METRICS_OFF
  | HINT_METRICS_ON

let font_type_of_int = function
  | 0 -> `TOY
  | 1 -> `FT
  | 2 -> `Win32
  | 3 -> `ATSUI
  | _ -> `Any

external _font_face_get_type : [> `Any] font_face -> int = "ml_cairo_font_face_get_type"
let font_face_get_type f =
  font_type_of_int (_font_face_get_type f)
let font_face_downcast_to_toy f =
  match font_face_get_type f with
  | `TOY -> (Obj.magic f : [`Any|`TOY] font_face)
  | _ -> invalid_arg "Cairo: font face downcast"

module Font_Options = struct
  type t
  external create : unit -> t = "ml_cairo_font_options_create"
  external merge : t -> t -> unit = "ml_cairo_font_options_merge"
  external get_antialias : t -> antialias = "ml_cairo_font_options_get_antialias"
  external set_antialias : t -> antialias -> unit = "ml_cairo_font_options_set_antialias"
  external get_subpixel_order : t -> subpixel_order = "ml_cairo_font_options_get_subpixel_order"
  external set_subpixel_order : t -> subpixel_order -> unit = "ml_cairo_font_options_set_subpixel_order"
  external get_hint_style : t -> hint_style = "ml_cairo_font_options_get_hint_style"
  external set_hint_style : t -> hint_style -> unit = "ml_cairo_font_options_set_hint_style"
  external get_hint_metrics : t -> hint_metrics = "ml_cairo_font_options_get_hint_metrics"
  external set_hint_metrics : t -> hint_metrics -> unit = "ml_cairo_font_options_set_hint_metrics"
    type all =
      [ `ANTIALIAS_DEFAULT
      | `ANTIALIAS_GRAY
      | `ANTIALIAS_NONE
      | `ANTIALIAS_SUBPIXEL
      | `HINT_METRICS_DEFAULT
      | `HINT_METRICS_OFF
      | `HINT_METRICS_ON
      | `HINT_STYLE_DEFAULT
      | `HINT_STYLE_FULL
      | `HINT_STYLE_MEDIUM
      | `HINT_STYLE_NONE
      | `HINT_STYLE_SLIGHT
      | `SUBPIXEL_ORDER_BGR
      | `SUBPIXEL_ORDER_DEFAULT
      | `SUBPIXEL_ORDER_RGB
      | `SUBPIXEL_ORDER_VBGR
      | `SUBPIXEL_ORDER_VRGB ]
  let make l =
    let o = create () in
    List.iter (function
      | `ANTIALIAS_DEFAULT -> set_antialias o ANTIALIAS_DEFAULT
      | `ANTIALIAS_NONE -> set_antialias o ANTIALIAS_NONE
      | `ANTIALIAS_GRAY -> set_antialias o ANTIALIAS_GRAY
      | `ANTIALIAS_SUBPIXEL -> set_antialias o ANTIALIAS_SUBPIXEL
      | `SUBPIXEL_ORDER_DEFAULT -> set_subpixel_order o SUBPIXEL_ORDER_DEFAULT
      | `SUBPIXEL_ORDER_RGB -> set_subpixel_order o SUBPIXEL_ORDER_RGB
      | `SUBPIXEL_ORDER_BGR -> set_subpixel_order o SUBPIXEL_ORDER_BGR
      | `SUBPIXEL_ORDER_VRGB -> set_subpixel_order o SUBPIXEL_ORDER_VRGB
      | `SUBPIXEL_ORDER_VBGR -> set_subpixel_order o SUBPIXEL_ORDER_VBGR
      | `HINT_STYLE_DEFAULT -> set_hint_style o HINT_STYLE_DEFAULT
      | `HINT_STYLE_NONE -> set_hint_style o HINT_STYLE_NONE
      | `HINT_STYLE_SLIGHT -> set_hint_style o HINT_STYLE_SLIGHT
      | `HINT_STYLE_MEDIUM -> set_hint_style o HINT_STYLE_MEDIUM
      | `HINT_STYLE_FULL -> set_hint_style o HINT_STYLE_FULL
      | `HINT_METRICS_DEFAULT -> set_hint_metrics o HINT_METRICS_DEFAULT
      | `HINT_METRICS_OFF -> set_hint_metrics o HINT_METRICS_OFF
      | `HINT_METRICS_ON -> set_hint_metrics o HINT_METRICS_ON)
      l ;
    o
end

(* scaled fonts *)
module Scaled_Font = struct
type -'a t

external create : ([>`Any] as 'a) font_face -> matrix -> matrix -> Font_Options.t -> 'a t = "ml_cairo_scaled_font_create"
external _get_type : [> `Any] t -> int = "ml_cairo_scaled_font_get_type"
let get_type f = font_type_of_int (_get_type f)
let downcast_to_toy f =
  if get_type f = `TOY 
  then (Obj.magic f : [`Any|`TOY] t) 
  else invalid_arg "Cairo: scaled font downcast"
external font_extents : [> `Any] t -> font_extents = "ml_cairo_scaled_font_extents"
external text_extents : [> `Any] t -> string -> text_extents = "ml_cairo_scaled_font_text_extents"
external glyph_extents : [> `Any] t -> glyph array -> text_extents = "ml_cairo_scaled_font_glyph_extents"
external get_font_face : ([>`Any] as 'a) t -> 'a font_face = "ml_cairo_scaled_font_get_font_face"
external get_font_matrix : ([>`Any] as 'a) t -> matrix = "ml_cairo_scaled_font_get_font_matrix"
external get_ctm : ([>`Any] as 'a) t -> matrix = "ml_cairo_scaled_font_get_ctm"
external _get_font_options : ([>`Any] as 'a) t -> Font_Options.t -> unit = "ml_cairo_scaled_font_get_font_options"
let get_font_options sf = 
  let o = Font_Options.create () in
  _get_font_options sf o ;
  o
end

external select_font_face : t -> string -> font_slant -> font_weight -> unit = "ml_cairo_select_font_face"
external set_font_size : t -> float -> unit = "ml_cairo_set_font_size"
external set_font_matrix : t -> matrix -> unit = "ml_cairo_set_font_matrix"
external get_font_matrix : t -> matrix = "ml_cairo_get_font_matrix"
external set_font_options : t -> Font_Options.t -> unit = "ml_cairo_set_font_matrix"
external _get_font_options : t -> Font_Options.t -> unit = "ml_cairo_get_font_options"
let merge_font_options cr o' =
  let o = Font_Options.create () in
  _get_font_options cr o ;
  Font_Options.merge o o' ;
  set_font_options cr o
let get_font_options cr =
  let o = Font_Options.create () in
  _get_font_options cr o ;
  o
external set_scaled_font : t -> [> `Any] Scaled_Font.t -> unit = "ml_cairo_set_scaled_font"
external show_text : t -> string -> unit = "ml_cairo_show_text"
external show_glyphs : t -> glyph array -> unit = "ml_cairo_show_glyphs"
external get_font_face : t -> [`Any] font_face = "ml_cairo_get_font_face"
external font_extents : t -> font_extents = "ml_cairo_font_extents"
external set_font_face : t -> [> `Any] font_face -> unit = "ml_cairo_set_font_face"
external text_extents : t -> string -> text_extents = "ml_cairo_text_extents"
external glyph_extents : t -> glyph array -> text_extents = "ml_cairo_glyph_extents"
external text_path : t -> string -> unit = "ml_cairo_text_path"
external glyph_path : t -> glyph array -> unit = "ml_cairo_glyph_path"

external get_operator : t -> operator = "ml_cairo_get_operator"
external get_source : t -> [`Any] pattern = "ml_cairo_get_source"
external get_tolerance : t -> float = "ml_cairo_get_tolerance"
external get_antialias : t -> antialias = "ml_cairo_get_antialias"
external get_current_point : t -> point = "ml_cairo_get_current_point"
external get_fill_rule : t -> fill_rule = "ml_cairo_get_fill_rule"
external get_line_width : t -> float = "ml_cairo_get_line_width"
external get_line_cap : t -> line_cap = "ml_cairo_get_line_cap"
external get_line_join : t -> line_join = "ml_cairo_get_line_join"
external get_miter_limit : t -> float = "ml_cairo_get_miter_limit"
external get_matrix : t -> matrix = "ml_cairo_get_matrix"
external get_target : t -> [`Any] surface = "ml_cairo_get_target"
external get_group_target : t -> [`Any] surface = "ml_cairo_get_group_target"

type flat_path = [
  | `MOVE_TO of point
  | `LINE_TO of point
  | `CLOSE ]
type path = [
  | flat_path
  | `CURVE_TO of point * point * point ]
external fold_path      : t -> ('a -> [> path] -> 'a) -> 'a -> 'a = "ml_cairo_copy_path"
external fold_path_flat : t -> ('a -> [> flat_path] -> 'a) -> 'a -> 'a  = "ml_cairo_copy_path_flat"

let append_path cr = function
  | `MOVE_TO p -> move_to_point cr p
  | `LINE_TO p -> line_to_point cr p
  | `CLOSE -> close_path cr
  | `CURVE_TO (p1, p2, p3) -> curve_to_point cr p1 p2 p3

external status : t -> status = "ml_cairo_status"
external surface_status : [> `Any] surface -> status = "ml_cairo_surface_status"
external pattern_status : [> `Any] pattern -> status = "ml_cairo_pattern_status"
external font_face_status : [> `Any] font_face -> status = "ml_cairo_font_face_status"
external scaled_font_status : [> `Any] Scaled_Font.t -> status = "ml_cairo_scaled_font_status"
external font_options_status : Font_Options.t -> status = "ml_cairo_font_options_status"
external string_of_status : status -> string = "ml_cairo_status_to_string"



(* surface *)
external surface_create_similar : [> `Any] surface -> content -> width:int -> height:int -> [`Any] surface = "ml_cairo_surface_create_similar"

external surface_finish : [> `Any] surface -> unit = "ml_cairo_surface_finish"

external _surface_get_type : [> `Any] surface -> int = "ml_cairo_surface_get_type"
let surface_get_type s =
  match _surface_get_type s with
  | 0 -> `Image
  | 1 -> `PDF
  | 2 -> `PS
  | 3 -> `Xlib
  | 4 -> `XCB
  | 5 -> `Glitz
  | 6 -> `Quartz
  | 7 -> `Win32
  | 8 -> `BeOS
  | 9 -> `DirectFB
  | 10 -> `SVG
  | _  -> `Any

external surface_get_content : [> `Any] surface -> content = "ml_cairo_surface_get_content"

external _surface_get_font_options : [> `Any] surface -> Font_Options.t -> unit = "ml_cairo_surface_get_font_options"
let surface_get_font_options s =
  let o = Font_Options.create () in
  _surface_get_font_options s o ;
  o

external surface_flush : [> `Any] surface -> unit = "ml_cairo_surface_flush"
external mark_dirty    : [> `Any] surface -> unit = "ml_cairo_surface_mark_dirty"
external mark_dirty_rectangle : [> `Any] surface -> int -> int -> int -> int -> unit = "ml_cairo_surface_mark_dirty_rectangle"

external surface_set_device_offset : [> `Any] surface -> float -> float -> unit = "ml_cairo_surface_set_device_offset"
external surface_get_device_offset : [> `Any] surface -> float * float = "ml_cairo_surface_get_device_offset"

external surface_set_fallback_resolution : [> `Any] surface -> float -> float -> unit = "ml_cairo_surface_set_fallback_resolution"


type image_surface = [`Any|`Image] surface

type format =
    FORMAT_ARGB32
  | FORMAT_RGB24
  | FORMAT_A8
  | FORMAT_A1

external image_surface_create : format -> width:int -> height:int -> image_surface = "ml_cairo_image_surface_create"
external image_surface_get_format  : [>`Image] surface -> format = "ml_cairo_image_surface_get_format"
external image_surface_get_width  : [>`Image] surface -> int = "ml_cairo_image_surface_get_width"
external image_surface_get_height : [>`Image] surface -> int = "ml_cairo_image_surface_get_height"
external image_surface_get_stride  : [>`Image] surface -> int = "ml_cairo_image_surface_get_stride"



(* pattern *)
type extend = 
    EXTEND_NONE
  | EXTEND_REPEAT
  | EXTEND_REFLECT

type filter =
    FILTER_FAST
  | FILTER_GOOD
  | FILTER_BEST
  | FILTER_NEAREST
  | FILTER_BILINEAR
  | FILTER_GAUSSIAN

type solid_pattern = [`Any|`Solid] pattern
type surface_pattern  = [`Any|`Surface] pattern
type gradient_pattern = [`Any|`Gradient] pattern

module Pattern = struct
external _get_type : [> `Any] pattern -> int = "ml_cairo_pattern_get_type"
let get_type p =
  match _get_type p with
  | 0 -> `Solid
  | 1 -> `Surface
  | 2 -> `Linear
  | 3 -> `Radial
  | _ -> `Any
let downcast_to_solid p =
  if get_type p = `Solid
  then (Obj.magic p : [`Solid|`Any] pattern)
  else invalid_arg "Cairo.Pattern: pattern downcast"
let downcast_to_surface p =
  if get_type p = `Surface
  then (Obj.magic p : [`Surface|`Any] pattern)
  else invalid_arg "Cairo.Pattern: pattern downcast"
let downcast_to_gradient p =
  match get_type p with
  | `Linear | `Radial -> (Obj.magic p : gradient_pattern)
  | _ -> invalid_arg "Cairo.Pattern: pattern downcast"
external create_rgb  : red:float -> green:float -> blue:float -> solid_pattern = "ml_cairo_pattern_create_rgb"
external create_rgba : red:float -> green:float -> blue:float -> alpha:float -> solid_pattern = "ml_cairo_pattern_create_rgba"
external create_for_surface : [> `Any] surface -> surface_pattern = "ml_cairo_pattern_create_for_surface"
external create_linear : x0:float -> y0:float -> x1:float -> y1:float -> gradient_pattern = "ml_cairo_pattern_create_linear"
external create_radial : cx0:float -> cy0:float -> radius0:float -> cx1:float -> cy1:float -> radius1:float -> gradient_pattern = "ml_cairo_pattern_create_radial_bc" "ml_cairo_pattern_create_radial"

external add_color_stop_rgb  : [>`Gradient] pattern -> off:float -> red:float -> green:float -> blue:float -> unit = "ml_cairo_pattern_add_color_stop_rgb"
external add_color_stop_rgba : [>`Gradient] pattern -> off:float -> red:float -> green:float -> blue:float -> alpha:float -> unit = "ml_cairo_pattern_add_color_stop_rgba_bc" "ml_cairo_pattern_add_color_stop_rgba"

external set_matrix : [> `Any] pattern -> matrix -> unit = "ml_cairo_pattern_set_matrix"
external get_matrix : [> `Any] pattern -> matrix = "ml_cairo_pattern_get_matrix"

external set_extend : [> `Surface] pattern -> extend -> unit = "ml_cairo_pattern_set_extend"
external get_extend : [> `Surface] pattern -> extend = "ml_cairo_pattern_get_extend"

external set_filter : [> `Surface] pattern -> filter -> unit = "ml_cairo_pattern_set_filter"
external get_filter : [> `Surface] pattern -> filter = "ml_cairo_pattern_get_filter"
end


(* matrix *)
module Matrix = struct
let init_identity = { xx = 1.; yx = 0.; xy = 0.; yy = 1.; x0 = 0.; y0 = 0. }
let init_translate x y = { xx = 1.; yx = 0.; xy = 0.; yy = 1.; x0 = x; y0 = y }
let init_scale x y = { xx = x; yx = 0.; xy = 0.; yy = y; x0 = 0.; y0 = 0. }
let init_rotate a =
  let s = sin a in
  let c = cos a in
  { xx = c; yx = s; xy = ~-. s; yy = c; x0 = 0.; y0 = 0. }

external translate : matrix -> float -> float -> matrix = "ml_cairo_matrix_translate"
external scale     : matrix -> float -> float -> matrix = "ml_cairo_matrix_scale"
external rotate    : matrix -> float -> matrix = "ml_cairo_matrix_rotate"
external invert    : matrix -> matrix = "ml_cairo_matrix_invert"
external multiply  : matrix -> matrix -> matrix = "ml_cairo_matrix_multiply"

external transform_distance : matrix -> point -> point = "ml_cairo_matrix_transform_distance"
external transform_point    : matrix -> point -> point = "ml_cairo_matrix_transform_point"
end
