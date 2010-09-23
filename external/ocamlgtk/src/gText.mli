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

(* $Id: gText.mli 1415 2008-09-04 15:59:15Z ben_99_9 $ *)

open Gtk
open GObj

(** The text widget and associated objects
   @gtkdoc gtk TextWidget *)

type mark = [`INSERT | `SEL_BOUND | `NAME of string | `MARK of text_mark]

class child_anchor : text_child_anchor ->
object
  method as_childanchor : text_child_anchor
  method deleted : bool
  method get_oid : int
  method widgets : widget list
end
val child_anchor : unit -> child_anchor

(** {3 GtkTextTag} *)

type tag_property = [
  | `BACKGROUND of string
  | `BACKGROUND_FULL_HEIGHT of bool
  | `BACKGROUND_FULL_HEIGHT_SET of bool
  | `BACKGROUND_GDK of Gdk.color
  | `BACKGROUND_SET of bool
  | `BACKGROUND_STIPPLE of Gdk.bitmap
  | `BACKGROUND_STIPPLE_SET of bool
  | `DIRECTION of Tags.text_direction
  | `EDITABLE of bool
  | `EDITABLE_SET of bool
  | `FAMILY of string
  | `FAMILY_SET of bool
  | `FONT of string
  | `FONT_DESC of Pango.font_description
  | `FOREGROUND of string
  | `FOREGROUND_GDK of Gdk.color
  | `FOREGROUND_SET of bool
  | `FOREGROUND_STIPPLE of Gdk.bitmap
  | `FOREGROUND_STIPPLE_SET of bool
  | `INDENT of int
  | `INDENT_SET of bool
  | `INVISIBLE of bool
  | `INVISIBLE_SET of bool
  | `JUSTIFICATION of Tags.justification
  | `JUSTIFICATION_SET of bool
  | `LANGUAGE of string
  | `LANGUAGE_SET of bool
  | `LEFT_MARGIN of int
  | `LEFT_MARGIN_SET of bool
  | `PIXELS_ABOVE_LINES of int
  | `PIXELS_ABOVE_LINES_SET of bool
  | `PIXELS_BELOW_LINES of int
  | `PIXELS_BELOW_LINES_SET of bool
  | `PIXELS_INSIDE_WRAP of int
  | `PIXELS_INSIDE_WRAP_SET of bool
  | `RIGHT_MARGIN of int
  | `RIGHT_MARGIN_SET of bool
  | `RISE of int
  | `RISE_SET of bool
  | `SCALE of Pango.Tags.scale
  | `SCALE_SET of bool
  | `SIZE of int
  | `SIZE_POINTS of float
  | `SIZE_SET of bool
  | `STRETCH of Pango.Tags.stretch
  | `STRETCH_SET of bool
  | `STRIKETHROUGH of bool
  | `STRIKETHROUGH_SET of bool
  | `STYLE of Pango.Tags.style
  | `STYLE_SET of bool
  | `TABS_SET of bool
  | `UNDERLINE of Pango.Tags.underline
  | `UNDERLINE_SET of bool
  | `VARIANT of Pango.Tags.variant
  | `VARIANT_SET of bool
  | `WEIGHT of Pango.Tags.weight
  | `WEIGHT_SET of bool
  | `WRAP_MODE of Tags.wrap_mode
  | `WRAP_MODE_SET of bool
]

(** @gtkdoc gtk GtkTextTag *)
class tag_signals : ([> `texttag] as 'b) obj ->
object ('a)
  method after : 'a
  method event :
    callback:(origin:unit Gobject.obj -> GdkEvent.any -> text_iter -> bool) ->
    GtkSignal.id
end

(** A tag that can be applied to text in a {!GText.buffer}
   @gtkdoc gtk GtkTextTag *)
class tag : text_tag ->
object
  method as_tag : text_tag
  method connect : tag_signals
  method event : 'a obj -> GdkEvent.any -> text_iter -> bool
  method get_oid : int
  method priority : int
  method set_priority : int -> unit
  method set_properties : tag_property list -> unit
  method set_property : tag_property -> unit
  method get_property : ([`texttag],'a) Gobject.property -> 'a
end

(** @gtkdoc gtk GtkTextTag *)
val tag : ?name:string -> unit -> tag

(** {3 Text buffer iterator} *)

type contents =
  [ `CHAR of Glib.unichar
  | `PIXBUF of GdkPixbuf.pixbuf
  | `CHILD of child_anchor
  | `UNKNOWN ]

(** Movement functions returning an iter are truly functional i.e. the
   returned iter shares nothing with the originale one.  

   If you need to move some iter in an imperative way use
   [#nocopy#...].  
*)

(** @gtkdoc gtk gtk-GtkTextIter *)
class nocopy_iter :  text_iter -> 
object
  method backward_char : bool
  method backward_chars : int -> bool
  method backward_cursor_position : bool
  method backward_cursor_positions : int -> bool
  method backward_find_char : ?limit:iter -> (Glib.unichar -> bool) -> bool
  method backward_line : bool
  method backward_lines : int -> bool
  method backward_sentence_start : bool
  method backward_sentence_starts : int -> bool
  method backward_to_tag_toggle : tag option -> bool
  method backward_word_start : bool
  method backward_word_starts : int -> bool
  method forward_char : bool
  method forward_chars : int -> bool
  method forward_cursor_position : bool
  method forward_cursor_positions : int -> bool
  method forward_find_char : ?limit:iter -> (Glib.unichar -> bool) -> bool
  method forward_line : bool
  method forward_lines : int -> bool
  method forward_sentence_end : bool
  method forward_sentence_ends : int -> bool
  method forward_to_end : unit
  method forward_to_tag_toggle : tag option -> bool
  method forward_word_end : bool
  method forward_word_ends : int -> bool
  method forward_to_line_end : bool
  method set_line : int -> unit
  method set_line_index : int -> unit
  method set_line_offset : int -> unit
  method set_offset : int -> unit
  method set_visible_line_index : int -> unit
  method set_visible_line_offset : int -> unit
end 

(** @gtkdoc gtk gtk-GtkTextIter *)
and iter : text_iter ->
object ('self)
  method as_iter : text_iter
  method copy : iter
  method nocopy : nocopy_iter
  method backward_char : iter
  method backward_chars : int -> iter
  method backward_cursor_position : iter
  method backward_cursor_positions : int -> iter
  method backward_find_char : ?limit:iter -> (Glib.unichar -> bool) -> iter
  method backward_line : iter
  method backward_lines : int -> iter
  method backward_search : ?flags:Gtk.Tags.text_search_flag list ->
    ?limit:iter -> string -> (iter * iter) option
  method backward_sentence_start : iter
  method backward_sentence_starts : int -> iter
  method backward_to_tag_toggle : tag option -> iter
  method backward_word_start : iter
  method backward_word_starts : int -> iter
  method begins_tag : tag option -> bool
  method buffer : text_buffer
  method bytes_in_line : int
  method can_insert : default:bool -> bool
  method char : Glib.unichar
  method chars_in_line : int
  method compare : iter -> int
  method contents : contents
  method editable : default:bool -> bool
  method ends_line : bool
  method ends_sentence : bool
  method ends_tag : tag option -> bool
  method ends_word : bool
  method equal : iter -> bool
  method forward_char : iter
  method forward_chars : int -> iter
  method forward_cursor_position : iter
  method forward_cursor_positions : int -> iter
  method forward_find_char : ?limit:iter -> (Glib.unichar -> bool) -> iter
  method forward_line : iter
  method forward_lines : int -> iter
  method forward_search : ?flags:Gtk.Tags.text_search_flag list ->
    ?limit:iter -> string -> (iter * iter) option
  method forward_sentence_end : iter
  method forward_sentence_ends : int -> iter
  method forward_to_end : iter
  method forward_to_line_end : iter
  method forward_to_tag_toggle : tag option -> iter
  method forward_word_end : iter
  method forward_word_ends : int -> iter
  method get_slice : stop:iter -> string
  method get_text : stop:iter -> string
  method get_toggled_tags : bool -> tag list
  method get_visible_slice : stop:iter -> string
  method get_visible_text : stop:iter -> string
  method has_tag : tag -> bool
  method in_range : start:iter -> stop:iter -> bool
  method inside_sentence : bool
  method inside_word : bool
  method is_cursor_position : bool
  method is_end : bool
  method is_start : bool
  method language : string
  method line : int
  method line_index : int
  method line_offset : int
  method marks : text_mark list
  method offset : int
  method set_line : int -> iter
  method set_line_index : int -> iter
  method set_line_offset : int -> iter
  method set_offset : int -> iter
  method set_visible_line_index : int -> iter
  method set_visible_line_offset : int -> iter
  method starts_line : bool
  method starts_sentence : bool
  method starts_word : bool
  method tags : tag list
  method toggles_tag : tag option -> bool
  method visible_line_index : int
  method visible_line_offset : int
end

(** @gtkdoc gtk gtk-GtkTextIter *)
val as_iter : iter -> text_iter

(** {3 GtkTextTagTable} *)

(** @gtkdoc gtk GtkTextTagTable *)
class tag_table_signals : ([> `texttagtable] as 'b) obj ->
object ('a)
  method after : 'a
  method tag_added : callback:(text_tag -> unit) -> GtkSignal.id
  method tag_changed : callback:(text_tag -> size:bool -> unit) -> GtkSignal.id
  method tag_removed : callback:(text_tag -> unit) -> GtkSignal.id
end

(** @gtkdoc gtk GtkTextTagTable *)
class tag_table_skel : [> `texttagtable] obj ->
object
  method get_oid : int
  method as_tag_table : text_tag_table
  method add : text_tag -> unit
  method remove : text_tag -> unit
  method lookup : string -> text_tag option
  method size : int
end

(** Collection of tags that can be used together
   @gtkdoc gtk GtkTextTagTable *)
class tag_table : [> `texttagtable] obj ->
object
  inherit tag_table_skel 
  method connect : tag_table_signals
end

(** @gtkdoc gtk GtkTextTagTable *)
val tag_table : unit -> tag_table

(** {3 GtkTextBuffer} *)

(** @gtkdoc gtk GtkTextBuffer *)
(*
class buffer_signals : [> `textbuffer] obj ->
object ('a)  
  method apply_tag :
    callback:(tag -> start:iter -> stop:iter -> unit) -> GtkSignal.id
  method begin_user_action : callback:(unit -> unit) -> GtkSignal.id
  method changed : callback:(unit -> unit) -> GtkSignal.id
  method delete_range :
    callback:(start:iter -> stop:iter -> unit) -> GtkSignal.id
  method end_user_action : callback:(unit -> unit) -> GtkSignal.id
  method insert_child_anchor :
    callback:(iter -> text_child_anchor -> unit) -> GtkSignal.id
  method insert_pixbuf :
    callback:(iter -> GdkPixbuf.pixbuf -> unit) -> GtkSignal.id
  method insert_text : callback:(iter -> string -> unit) -> GtkSignal.id
  method mark_deleted : callback:(text_mark -> unit) -> GtkSignal.id
  method mark_set : callback:(iter -> text_mark -> unit) -> GtkSignal.id
  method modified_changed : callback:(unit -> unit) -> GtkSignal.id
  method remove_tag :
    callback:(tag -> start:iter -> stop:iter -> unit) -> GtkSignal.id
  method after : 'a
end
*)

class type buffer_signals_skel_type = 
  object
    method apply_tag :
      callback:(tag -> start:iter -> stop:iter -> unit) -> GtkSignal.id
    method begin_user_action : callback:(unit -> unit) -> GtkSignal.id
    method changed : callback:(unit -> unit) -> GtkSignal.id
    method delete_range :
      callback:(start:iter -> stop:iter -> unit) -> GtkSignal.id
    method end_user_action : callback:(unit -> unit) -> GtkSignal.id
    method insert_child_anchor :
      callback:(iter -> Gtk.text_child_anchor -> unit) -> GtkSignal.id
    method insert_pixbuf :
      callback:(iter -> GdkPixbuf.pixbuf -> unit) -> GtkSignal.id
    method insert_text : callback:(iter -> string -> unit) -> GtkSignal.id
    method mark_deleted : callback:(Gtk.text_mark -> unit) -> GtkSignal.id
    method mark_set :
      callback:(iter -> Gtk.text_mark -> unit) -> GtkSignal.id
    method modified_changed : callback:(unit -> unit) -> GtkSignal.id
    method remove_tag :
      callback:(tag -> start:iter -> stop:iter -> unit) -> GtkSignal.id
  end
class virtual buffer_signals_skel :
object
  inherit buffer_signals_skel_type
  method private virtual connect :
    'a. ([> `textbuffer ], 'a) GtkSignal.t -> callback:'a -> GtkSignal.id
end

class type ['b] buffer_signals_type = 
object ('a)
  inherit buffer_signals_skel_type
  method after : 'a
  method private connect :
    'c. ('b, 'c) GtkSignal.t -> callback:'c -> GtkSignal.id
end

class buffer_signals : ([> `textbuffer ] as 'b) Gtk.obj -> ['b] buffer_signals_type

exception No_such_mark of string

type position =
    [ `OFFSET of int | `LINE of int
    | `LINECHAR of int * int | `LINEBYTE of int * int
    | `START | `END | `ITER of iter | mark ]

(** Stores attributed text for display in a {!GText.view}
   @gtkdoc gtk GtkTextBuffer *)
class buffer_skel : [> `textbuffer] obj ->
object
  method as_buffer : text_buffer
  method add_selection_clipboard : GData.clipboard -> unit
  method apply_tag : tag -> start:iter -> stop:iter -> unit
  method apply_tag_by_name : string -> start:iter -> stop:iter -> unit
  method begin_user_action : unit -> unit
  method bounds : iter * iter
  method char_count : int
  method copy_clipboard : GData.clipboard -> unit
  method create_child_anchor : iter -> child_anchor
  method create_mark :
    ?name:string -> ?left_gravity:bool -> iter -> text_mark
  (** @param left_gravity default value is [true] *)
  method create_tag : ?name:string -> tag_property list -> tag
  method cut_clipboard : ?default_editable:bool -> GData.clipboard -> unit
   (** @param default_editable default value is [true] *)
  method delete : start:iter -> stop:iter -> unit
  method delete_interactive :
    start:iter ->
    stop:iter -> ?default_editable:bool -> unit -> bool
  (** @param default_editable default value is [true] *)
  method delete_mark : mark -> unit
  method delete_selection :
    ?interactive:bool -> ?default_editable:bool -> unit -> bool
  (** @param interactive default value is [true]
      @param default_editable default value is [true] *)
  method end_iter : iter
  method end_user_action : unit -> unit
  method get_iter : position -> iter
  method get_iter_at_char : ?line:int -> int -> iter
  method get_iter_at_byte : line:int -> int -> iter
  method get_iter_at_mark : mark -> iter
  method get_mark : mark -> text_mark
  method get_oid : int
  method get_text :
    ?start:iter -> ?stop:iter -> ?slice:bool -> ?visible:bool -> unit -> string
  (** @param slice default value is [false] 
      @param visible default value is [false] *)
  method insert :
    ?iter:iter -> ?tag_names:string list -> ?tags:tag list 
    -> string -> unit
  method insert_child_anchor : iter -> child_anchor -> unit
  method insert_interactive :
    ?iter:iter -> ?default_editable:bool -> string -> bool
  (** @param default_editable default value is [true] *)
  method insert_pixbuf : iter:iter -> pixbuf:GdkPixbuf.pixbuf -> unit
  method insert_range :
    iter:iter -> start:iter -> stop:iter -> unit
  method insert_range_interactive :
    iter:iter -> start:iter -> stop:iter ->
    ?default_editable:bool -> unit -> bool
  (** @param default_editable default value is [true] *)
  method line_count : int
  method modified : bool
  method move_mark : mark -> where:iter -> unit
  method paste_clipboard : 
    ?iter:iter -> ?default_editable:bool -> GData.clipboard -> unit
  (** @param default_editable default value is [true] *)
  method place_cursor : where:iter -> unit
  method select_range : iter -> iter -> unit (** @since GTK 2.4 *)
  method remove_all_tags : start:iter -> stop:iter -> unit
  method remove_selection_clipboard : GData.clipboard -> unit
  method remove_tag : tag -> start:iter -> stop:iter -> unit
  method remove_tag_by_name : string -> start:iter -> stop:iter -> unit
  method selection_bounds : iter * iter
  method set_modified : bool -> unit
  method set_text : string -> unit
  method start_iter : iter
  method tag_table : text_tag_table
  method has_selection : bool (**  since Gtk 2.10 *)
  method cursor_position : int (**  since Gtk 2.10 *)
end

class buffer : [> `textbuffer] obj ->
object
  inherit buffer_skel
  method connect : buffer_signals
end

(** @gtkdoc gtk GtkTextBuffer *)
val buffer : ?tag_table:tag_table -> ?text:string -> unit -> buffer

(** {3 GtkTextView} *)

(** @gtkdoc gtk GtkTextView *)
class view_signals : [> text_view] obj ->
object ('a)
  method after : 'a
  method copy_clipboard : callback:(unit -> unit) -> GtkSignal.id
  method cut_clipboard : callback:(unit -> unit) -> GtkSignal.id
  method delete_from_cursor :
    callback:(Tags.delete_type -> int -> unit) -> GtkSignal.id
  method destroy : callback:(unit -> unit) -> GtkSignal.id
  method insert_at_cursor : callback:(string -> unit) -> GtkSignal.id
  method move_cursor :
    callback:(Tags.movement_step -> int -> extend:bool -> unit) -> GtkSignal.id
  method move_focus :
    callback:(Tags.direction_type -> unit) -> GtkSignal.id
  method page_horizontally :
    callback:(int -> extend:bool -> unit) -> GtkSignal.id
  method paste_clipboard : callback:(unit -> unit) -> GtkSignal.id
  method populate_popup :
    callback:(menu obj -> unit) -> GtkSignal.id
  method set_anchor : callback:(unit -> unit) -> GtkSignal.id
  method set_scroll_adjustments :
    callback:(GData.adjustment option -> GData.adjustment option -> unit)
    -> GtkSignal.id
  method toggle_overwrite : callback:(unit -> unit) -> GtkSignal.id
end

(** Widget that displays a {!GText.buffer}
   @gtkdoc gtk GtkTextView *)
class view_skel : ([> text_view] as 'a) obj ->
object
  inherit GObj.widget
  val obj : 'a obj
  method as_view : text_view obj
  method accepts_tab : bool
  method add_child_at_anchor : GObj.widget -> child_anchor -> unit
  method add_child_in_window :
    child:GObj.widget ->
    which_window:Tags.text_window_type -> x:int -> y:int -> unit
  method backward_display_line : iter -> bool
  method backward_display_line_start : iter -> bool
  method buffer : buffer
  method buffer_to_window_coords :
    tag:Tags.text_window_type -> x:int -> y:int -> int * int
  method cursor_visible : bool
  method editable : bool
  method event : GObj.event_ops
  method forward_display_line : iter -> bool
  method forward_display_line_end : iter -> bool
  method get_border_window_size : [ `BOTTOM | `LEFT | `RIGHT | `TOP] -> int
  method get_iter_at_location : x:int -> y:int -> iter
  method get_iter_location : iter -> Gdk.Rectangle.t
  method get_line_at_y : int -> iter * int
  method get_line_yrange : iter -> int * int
  method get_window : Tags.text_window_type -> Gdk.window option
  method get_window_type : Gdk.window -> Tags.text_window_type
  method indent : int
  method justification : Tags.justification
  method left_margin : int
  method misc : GObj.misc_ops
  method move_child : child:GObj.widget -> x:int -> y:int -> unit
  method move_mark_onscreen : mark -> bool
  method move_visually : iter -> int -> bool
  method pixels_above_lines : int
  method pixels_below_lines : int
  method pixels_inside_wrap : int
  method place_cursor_onscreen : unit -> bool
  method right_margin : int
  method scroll_mark_onscreen : mark -> unit
  method scroll_to_iter :
    ?within_margin:float ->
    ?use_align:bool ->
    ?xalign:float -> ?yalign:float -> iter -> bool
  (** @param use_align default value is [false] *)
  method scroll_to_mark :
    ?within_margin:float ->
    ?use_align:bool -> ?xalign:float -> ?yalign:float -> mark -> unit
  (** @param use_align default value is [false] *)
  method set_accepts_tab : bool -> unit
  method set_border_window_size :
    typ:[ `BOTTOM | `LEFT | `RIGHT | `TOP] -> size:int -> unit
  method set_buffer : buffer -> unit
  method set_cursor_visible : bool -> unit
  method set_editable : bool -> unit
  method set_indent : int -> unit
  method set_justification : Tags.justification -> unit
  method set_left_margin : int -> unit
  method set_pixels_above_lines : int -> unit
  method set_pixels_below_lines : int -> unit
  method set_pixels_inside_wrap : int -> unit
  method set_right_margin : int -> unit
  method set_wrap_mode : Tags.wrap_mode -> unit
  method starts_display_line : iter -> bool
  method visible_rect : Gdk.Rectangle.t
  method window_to_buffer_coords :
    tag:Tags.text_window_type -> x:int -> y:int -> int * int
  method wrap_mode : Tags.wrap_mode
end

class view : ([> text_view] as 'a) obj ->
object
  inherit view_skel
  val obj : 'a obj
  method connect : view_signals
end

(** @gtkdoc gtk GtkTextView *)
val view :
  ?buffer:buffer ->
  ?editable:bool ->
  ?cursor_visible:bool ->
  ?justification:Tags.justification ->
  ?wrap_mode:Tags.wrap_mode ->
  ?accepts_tab:bool ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) -> ?show:bool -> unit -> view
