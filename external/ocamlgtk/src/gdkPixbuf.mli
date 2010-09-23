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

(* $Id: gdkPixbuf.mli 1347 2007-06-20 07:40:34Z guesdon $ *)

(** Object for manipulating image data 
    @gtkdoc gdk-pixbuf index *)

(** {3 The GdkPixbuf Structure} *)

(* Types *)

(** @gtkdoc gdk-pixbuf gdk-pixbuf-gdk-pixbuf *)
type pixbuf = [`pixbuf] Gobject.obj
type colorspace = [ `RGB]
type alpha_mode = [ `BILEVEL | `FULL]
type interpolation = [ `BILINEAR | `HYPER | `NEAREST | `TILES]

type gdkpixbuferror =
  | ERROR_CORRUPT_IMAGE
  | ERROR_INSUFFICIENT_MEMORY
  | ERROR_BAD_OPTION
  | ERROR_UNKNOWN_TYPE
  | ERROR_UNSUPPORTED_OPERATION
  | ERROR_FAILED
exception GdkPixbufError of gdkpixbuferror * string

external set_marshal_use_rle : bool -> unit = "ml_gdk_pixbuf_set_marshal_use_rle"

(** {3 Creation} *)

(** @gtkdoc gdk-pixbuf gdk-pixbuf-creating*)
val create :
  width:int -> height:int ->
  ?bits:int -> ?colorspace:colorspace -> ?has_alpha:bool -> unit -> pixbuf

val cast : 'a Gobject.obj -> pixbuf
external copy : pixbuf -> pixbuf = "ml_gdk_pixbuf_copy"
external subpixbuf : pixbuf -> src_x:int -> src_y:int -> width:int -> height:int -> pixbuf 
  = "ml_gdk_pixbuf_new_subpixbuf"

(** @gtkdoc gdk-pixbuf gdk-pixbuf-file-loading *)
external from_file : string -> pixbuf = "ml_gdk_pixbuf_new_from_file"

(** @since GTK 2.4 *)
external get_file_info : string -> string * int * int = "ml_gdk_pixbuf_get_file_info"

(** @since GTK 2.4
    @gtkdoc gdk-pixbuf gdk-pixbuf-file-loading *)
external from_file_at_size : string -> width:int -> height:int -> pixbuf 
  = "ml_gdk_pixbuf_new_from_file_at_size"

external from_xpm_data : string array -> pixbuf
  = "ml_gdk_pixbuf_new_from_xpm_data"
val from_data :
  width:int -> height:int ->
  ?bits:int -> ?rowstride:int -> ?has_alpha:bool -> Gpointer.region -> pixbuf

(** @gtkdoc gdk gdk-Pixbufs *)
val get_from_drawable :
  dest:pixbuf ->
  ?dest_x:int -> ?dest_y:int ->
  ?width:int ->  ?height:int ->
  ?src_x:int -> ?src_y:int ->
  ?colormap:Gdk.colormap -> [>`drawable] Gobject.obj -> unit

(** {3 Accessors} *)

external get_n_channels : pixbuf -> int = "ml_gdk_pixbuf_get_n_channels"
external get_has_alpha : pixbuf -> bool = "ml_gdk_pixbuf_get_has_alpha"
external get_bits_per_sample : pixbuf -> int
  = "ml_gdk_pixbuf_get_bits_per_sample"
external get_width : pixbuf -> int = "ml_gdk_pixbuf_get_width"
external get_height : pixbuf -> int = "ml_gdk_pixbuf_get_height"
external get_rowstride : pixbuf -> int = "ml_gdk_pixbuf_get_rowstride"
val get_pixels : pixbuf -> Gpointer.region

(** {3 Rendering} *)

(** @gtkdoc gdk gdk-Drawing-Primitives *)
val draw_pixbuf :
  [>`drawable] Gobject.obj ->
  Gdk.gc ->
  ?dest_x:int ->
  ?dest_y:int ->
  ?width:int ->
  ?height:int ->
  ?dither:Gdk.Tags.rgb_dither ->
  ?x_dither:int ->
  ?y_dither:int -> ?src_x:int -> ?src_y:int -> pixbuf -> unit


(** @gtkdoc gdk gdk-Pixbufs
    @deprecated use {!GdkPixbuf.draw_pixbuf} *)
val render_to_drawable :
  [>`drawable] Gobject.obj ->
  ?gc:Gdk.gc ->
  ?dest_x:int ->
  ?dest_y:int ->
  ?width:int ->
  ?height:int ->
  ?dither:Gdk.Tags.rgb_dither ->
  ?x_dither:int ->
  ?y_dither:int -> ?src_x:int -> ?src_y:int -> pixbuf -> unit

(** @gtkdoc gdk gdk-Pixbufs *)
val render_alpha :
  Gdk.bitmap ->
  ?dest_x:int ->
  ?dest_y:int ->
  ?width:int ->
  ?height:int -> ?threshold:int -> ?src_x:int -> ?src_y:int -> pixbuf -> unit

(** @gtkdoc gdk gdk-Pixbufs
    @deprecated use {!GdkPixbuf.draw_pixbuf} *)
val render_to_drawable_alpha :
  [>`drawable] Gobject.obj ->
  ?dest_x:int ->
  ?dest_y:int ->
  ?width:int ->
  ?height:int ->
  ?alpha:alpha_mode ->
  ?threshold:int ->
  ?dither:Gdk.Tags.rgb_dither ->
  ?x_dither:int ->
  ?y_dither:int -> ?src_x:int -> ?src_y:int -> pixbuf -> unit

(** @gtkdoc gdk gdk-Pixbufs *)
val create_pixmap : ?threshold:int -> pixbuf -> Gdk.pixmap * Gdk.bitmap option

(** {3 Transform} *)

(** @gtkdoc gdk-pixbuf gdk-pixbuf-util *)
val add_alpha : ?transparent:int * int * int -> pixbuf -> pixbuf

(** @gtkdoc gdk-pixbuf gdk-pixbuf-util *)
val fill : pixbuf -> int32 -> unit

(** @gtkdoc gdk-pixbuf gdk-pixbuf-util *)
val saturate_and_pixelate : dest:pixbuf -> saturation:float -> pixelate:bool -> pixbuf -> unit

(** @gtkdoc gdk-pixbuf gdk-pixbuf-util *)
val copy_area :
  dest:pixbuf ->
  ?dest_x:int ->
  ?dest_y:int ->
  ?width:int -> ?height:int -> ?src_x:int -> ?src_y:int -> pixbuf -> unit

(** @gtkdoc gdk-pixbuf gdk-pixbuf-scaling *)
val scale :
  dest:pixbuf ->
  ?dest_x:int ->
  ?dest_y:int ->
  ?width:int ->
  ?height:int ->
  ?ofs_x:float ->
  ?ofs_y:float ->
  ?scale_x:float -> ?scale_y:float -> ?interp:interpolation -> pixbuf -> unit

(** @gtkdoc gdk-pixbuf gdk-pixbuf-scaling *)
val composite :
  dest:pixbuf ->
  alpha:int ->
  ?dest_x:int ->
  ?dest_y:int ->
  ?width:int ->
  ?height:int ->
  ?ofs_x:float ->
  ?ofs_y:float ->
  ?scale_x:float -> ?scale_y:float -> ?interp:interpolation -> pixbuf -> unit

(** {3 Saving} *)

(** @gtkdoc gdk-pixbuf gdk-pixbuf-file-saving *)
external save : filename:string -> typ:string -> ?options:(string * string) list -> pixbuf -> unit
    = "ml_gdk_pixbuf_save"

(** @since GTK 2.4 *)
external save_to_callback : 
  pixbuf -> typ:string -> ?options:(string * string) list -> 
  (string -> unit) -> unit = "ml_gdk_pixbuf_save_to_callback"

(** @since GTK 2.4 *)
val save_to_buffer :
  pixbuf -> typ:string -> ?options:(string * string) list -> 
  Buffer.t -> unit
