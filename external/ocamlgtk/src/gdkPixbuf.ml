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

(* $Id: gdkPixbuf.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gaux
open Gobject
open Gdk

type pixbuf = [`pixbuf] obj
type colorspace = [ `RGB ]
type alpha_mode = [ `BILEVEL | `FULL ]
type interpolation = [ `NEAREST | `TILES | `BILINEAR | `HYPER ]

type gdkpixbuferror =
  | ERROR_CORRUPT_IMAGE
  | ERROR_INSUFFICIENT_MEMORY
  | ERROR_BAD_OPTION
  | ERROR_UNKNOWN_TYPE
  | ERROR_UNSUPPORTED_OPERATION
  | ERROR_FAILED
exception GdkPixbufError of gdkpixbuferror * string
external _init : unit -> unit = "ml_gdkpixbuf_init"
let () = _init () ; Callback.register_exception "gdk_pixbuf_error" (GdkPixbufError (ERROR_CORRUPT_IMAGE, ""))
external set_marshal_use_rle : bool -> unit = "ml_gdk_pixbuf_set_marshal_use_rle"

(* Accessors *)

external get_n_channels : pixbuf -> int = "ml_gdk_pixbuf_get_n_channels"
external get_has_alpha : pixbuf -> bool = "ml_gdk_pixbuf_get_has_alpha"
external get_bits_per_sample : pixbuf -> int
  = "ml_gdk_pixbuf_get_bits_per_sample"
external get_width : pixbuf -> int = "ml_gdk_pixbuf_get_width"
external get_height : pixbuf -> int = "ml_gdk_pixbuf_get_height"
external get_rowstride : pixbuf -> int = "ml_gdk_pixbuf_get_rowstride"

external _get_pixels : pixbuf -> Obj.t * int = "ml_gdk_pixbuf_get_pixels"
let get_pixels pixbuf =
  let obj, pos = _get_pixels pixbuf in
  let get_length (_, pixbuf) =
    get_rowstride pixbuf * get_height pixbuf + pos
  in
  let r =
    Gpointer.unsafe_create_region ~path:[|0|] ~get_length (obj, pixbuf) in
  Gpointer.sub ~pos r

(* Constructors *)

external _create :
  colorspace:colorspace -> has_alpha:bool ->
  bits:int -> width:int -> height:int -> pixbuf
  = "ml_gdk_pixbuf_new"
let create ~width ~height ?(bits=8) ?(colorspace=`RGB) ?(has_alpha=false) () =
  _create ~colorspace ~has_alpha ~bits ~width ~height

let cast o : pixbuf = Gobject.try_cast o "GdkPixbuf"

external copy : pixbuf -> pixbuf = "ml_gdk_pixbuf_copy" 
external subpixbuf : pixbuf -> src_x:int -> src_y:int -> width:int -> height:int -> pixbuf 
  = "ml_gdk_pixbuf_new_subpixbuf"
external from_file : string -> pixbuf = "ml_gdk_pixbuf_new_from_file"
external get_file_info : string -> string * int * int = "ml_gdk_pixbuf_get_file_info"
external from_file_at_size : string -> width:int -> height:int -> pixbuf 
  = "ml_gdk_pixbuf_new_from_file_at_size"
external from_xpm_data : string array -> pixbuf
  = "ml_gdk_pixbuf_new_from_xpm_data"

external _from_data :
  Gpointer.region -> has_alpha:bool -> bits:int ->
  width:int -> height:int -> rowstride:int -> pixbuf
  = "ml_gdk_pixbuf_new_from_data_bc" "ml_gdk_pixbuf_new_from_data"
let from_data ~width ~height ?(bits=8) ?rowstride ?(has_alpha=false) data =
  let nc = if has_alpha then 4 else 3 in
  let rowstride = match rowstride with None -> width * nc | Some r -> r in
  if bits <> 8 || rowstride < width * nc || width <= 0 || height <= 0
  || Gpointer.length data < rowstride * (height - 1) + width * nc
  then invalid_arg "GdkPixbuf.from_data";
  _from_data data ~has_alpha ~bits ~width ~height ~rowstride

external _get_from_drawable :
  pixbuf -> [>`drawable] obj -> colormap -> src_x:int -> src_y:int ->
  dest_x:int -> dest_y:int -> width:int -> height:int -> unit
  = "ml_gdk_pixbuf_get_from_drawable_bc" "ml_gdk_pixbuf_get_from_drawable"
let get_from_drawable ~dest ?(dest_x=0) ?(dest_y=0) ?width ?height
    ?(src_x=0) ?(src_y=0) ?(colormap=Gdk.Rgb.get_cmap()) src =
  let dw, dh = Gdk.Drawable.get_size src in
  let mw = min (dw - src_x) (get_width dest - dest_x)
  and mh = min (dh - src_y) (get_height dest - dest_y) in
  let width = default mw ~opt:width and height = default mh ~opt:height in
  if src_x < 0 || src_y < 0 || dest_x < 0 || dest_y < 0
  || width <= 0 || height <= 0 || width > mw || height > mh
  then invalid_arg "GdkPixbuf.get_from_drawable";
  _get_from_drawable dest src colormap ~src_x ~src_y ~dest_x ~dest_y
    ~width ~height

(* Render *)

external _render_alpha :
  src:pixbuf -> bitmap -> src_x:int -> src_y:int ->
  dest_x:int -> dest_y:int -> width:int -> height:int -> threshold:int -> unit
  = "ml_gdk_pixbuf_render_threshold_alpha_bc"
    "ml_gdk_pixbuf_render_threshold_alpha"
let render_alpha bm ?(dest_x=0) ?(dest_y=0) ?width ?height ?(threshold=128)
    ?(src_x=0) ?(src_y=0) src =
  let width = may_default get_width src ~opt:width
  and height = may_default get_height src ~opt:height in
  _render_alpha ~src bm ~src_x ~src_y ~dest_x ~dest_y ~width ~height ~threshold

external _draw_pixbuf :
  src:pixbuf -> [>`drawable] obj -> gc -> src_x:int -> src_y:int ->
  dest_x:int -> dest_y:int -> width:int -> height:int ->
  dither:Tags.rgb_dither -> x_dither:int -> y_dither:int -> unit
  = "ml_gdk_pixbuf_render_to_drawable_bc"
    "ml_gdk_pixbuf_render_to_drawable"
let draw_pixbuf dw gc ?(dest_x=0) ?(dest_y=0)
    ?width ?height ?(dither=`NONE) ?(x_dither=0) ?(y_dither=0)
    ?(src_x=0) ?(src_y=0) src =
  let width = may_default get_width src ~opt:width
  and height = may_default get_height src ~opt:height in
  _draw_pixbuf dw gc ~src ~src_x ~src_y ~dest_x ~dest_y ~width ~height
    ~dither ~x_dither ~y_dither

let render_to_drawable dw ?(gc=Gdk.GC.create dw) =
  draw_pixbuf dw gc

external _render_to_drawable_alpha :
  src:pixbuf -> [>`drawable] obj -> src_x:int -> src_y:int ->
  dest_x:int -> dest_y:int -> width:int -> height:int ->
  alpha:alpha_mode -> threshold:int ->
  dither:Tags.rgb_dither -> x_dither:int -> y_dither:int -> unit
  = "ml_gdk_pixbuf_render_to_drawable_alpha_bc"
    "ml_gdk_pixbuf_render_to_drawable_alpha"
let render_to_drawable_alpha dw ?(dest_x=0) ?(dest_y=0) ?width ?height
    ?(alpha=`FULL) ?(threshold=128)
    ?(dither=`NONE) ?(x_dither=0) ?(y_dither=0) ?(src_x=0) ?(src_y=0) src =
  let width = may_default get_width src ~opt:width
  and height = may_default get_height src ~opt:height in
  _render_to_drawable_alpha ~src dw ~src_x ~src_y ~dest_x ~dest_y ~width
    ~height ~dither ~x_dither ~y_dither ~alpha ~threshold

external _create_pixmap : pixbuf -> threshold:int -> pixmap * bitmap option
  = "ml_gdk_pixbuf_render_pixmap_and_mask"
let create_pixmap ?(threshold=128) pb = _create_pixmap pb ~threshold

(* Transform *)

external _add_alpha : pixbuf -> subst:bool -> r:int -> g:int -> b:int -> pixbuf
  = "ml_gdk_pixbuf_add_alpha"
let add_alpha ?transparent pb =
  match transparent with None -> _add_alpha pb ~subst:false ~r:0 ~g:0 ~b:0
  | Some (r, g, b) -> _add_alpha pb ~subst:true ~r ~g ~b

external fill : pixbuf -> int32 -> unit = "ml_gdk_pixbuf_fill"
external _saturate_and_pixelate : pixbuf -> dest:pixbuf -> saturation:float -> pixelate:bool -> unit
    = "ml_gdk_pixbuf_saturate_and_pixelate"
let saturate_and_pixelate ~dest ~saturation ~pixelate src =
  _saturate_and_pixelate src ~dest ~saturation ~pixelate

external _copy_area :
  src:pixbuf -> src_x:int -> src_y:int -> width:int -> height:int ->
  dest:pixbuf -> dest_x:int -> dest_y:int -> unit
  = "ml_gdk_pixbuf_copy_area_bc" "ml_gdk_pixbuf_copy_area"
let copy_area ~dest ?(dest_x=0) ?(dest_y=0) ?width ?height
    ?(src_x=0) ?(src_y=0) src =
  let mw = min (get_width src - src_x) (get_width dest - dest_x)
  and mh = min (get_height src - src_y) (get_height dest - dest_y) in
  let width = match width with Some w -> w | None -> mw
  and height = match height with Some h -> h | None -> mh in
  if src_x < 0 || src_y < 0 || dest_x < 0 || dest_y < 0
  || width <= 0 || height <= 0 || width > mw || height > mh
  then invalid_arg "GdkPixbuf.copy_area";
  _copy_area ~src ~src_x ~src_y ~width ~height ~dest ~dest_x ~dest_y

let get_size sz sc ~ssrc ~sdest ~dest ~ofs =
  match sz, sc with
    None, None    -> (sdest - dest, (float dest +. ofs) /. float ssrc)
  | None, Some sc -> (truncate(float ssrc *. sc -. ofs), sc)
  | Some sz, None -> (sz, (float sz +. ofs) /. float ssrc)
  | Some sz, Some sc -> (sz, sc)

external _scale :
  src:pixbuf -> dest:pixbuf -> dest_x:int -> dest_y:int -> width:int ->
  height:int -> ofs_x:float -> ofs_y:float -> scale_x:float ->
  scale_y:float -> interp:interpolation -> unit
  = "ml_gdk_pixbuf_scale_bc" "ml_gdk_pixbuf_scale"
let scale ~dest ?(dest_x=0) ?(dest_y=0) ?width ?height ?(ofs_x=0.) ?(ofs_y=0.)
    ?scale_x ?scale_y ?(interp=`BILINEAR) src =
  let width, scale_x =
    get_size width scale_x ~ssrc:(get_width src)
      ~sdest:(get_width dest) ~dest:dest_x ~ofs:ofs_x
  and height, scale_y =
    get_size height scale_y ~ssrc:(get_height src)
      ~sdest:(get_height dest) ~dest:dest_y ~ofs:ofs_y
  in
  _scale ~src ~dest ~dest_x ~dest_y ~width ~height ~ofs_x ~ofs_y ~scale_x
    ~scale_y ~interp

external _composite :
  src:pixbuf -> dest:pixbuf -> dest_x:int -> dest_y:int -> width:int ->
  height:int -> ofs_x:float -> ofs_y:float -> scale_x:float ->
  scale_y:float -> interp:interpolation -> alpha:int -> unit
  = "ml_gdk_pixbuf_composite_bc" "ml_gdk_pixbuf_composite"
let composite ~dest ~alpha ?(dest_x=0) ?(dest_y=0) ?width ?height
    ?(ofs_x=0.) ?(ofs_y=0.) ?scale_x ?scale_y ?(interp=`BILINEAR) src =
  let width, scale_x =
    get_size width scale_x ~ssrc:(get_width src)
      ~sdest:(get_width dest) ~dest:dest_x ~ofs:ofs_x
  and height, scale_y =
    get_size height scale_y ~ssrc:(get_height src)
      ~sdest:(get_height dest) ~dest:dest_y ~ofs:ofs_y
  in
  _composite ~src ~dest ~dest_x ~dest_y ~width ~height ~ofs_x ~ofs_y ~scale_x
    ~scale_y ~interp ~alpha

(* Saving *)

external save : filename:string -> typ:string -> ?options:(string * string) list -> pixbuf -> unit
    = "ml_gdk_pixbuf_save"

external save_to_callback : 
  pixbuf -> typ:string -> ?options:(string * string) list -> 
  (string -> unit) -> unit = "ml_gdk_pixbuf_save_to_callback"

let save_to_buffer pb ~typ ?options buffer =
  save_to_callback pb ~typ ?options (Buffer.add_string buffer)
