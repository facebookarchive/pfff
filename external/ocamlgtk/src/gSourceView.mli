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

(** {2 GtkSourceView interface} *)

open Gtk
open GText


(** {2 GtkSourceTag} *)

type source_tag_property = [
  | `BACKGROUND of Gdk.color
  | `BOLD of bool
  | `FOREGROUND of Gdk.color
  | `ITALIC of bool
  | `STRIKETHROUGH of bool
  | `UNDERLINE of bool
]

class source_tag_style :
  GtkSourceView_types.source_tag_style obj ->
  object
    method as_source_tag_style : GtkSourceView_types.source_tag_style Gtk.obj
    method copy : source_tag_style

    method background : Gdk.color
    method bold : bool
    method foreground : Gdk.color
    method italic : bool
    method strikethrough : bool
    method underline : bool
    method use_background : bool
    method use_foreground : bool

    method set_background : Gdk.color -> unit
    method set_background_by_name : string -> unit
    method set_bold : bool -> unit
    method set_foreground : Gdk.color -> unit
    method set_foreground_by_name : string -> unit
    method set_italic : bool -> unit
    method set_strikethrough : bool -> unit
    method set_underline : bool -> unit
    method set_use_background : bool -> unit
    method set_use_foreground : bool -> unit
  end

val source_tag_style :
  ?background:Gdk.color ->
  ?background_by_name:string ->
  ?bold:bool ->
  ?foreground:Gdk.color ->
  ?foreground_by_name:string ->
  ?italic:bool ->
  ?strikethrough:bool -> ?underline:bool -> unit -> source_tag_style

type source_tag_id = string

class source_tag :
  GtkSourceView_types.source_tag obj ->
  object
    inherit GText.tag
    method as_source_tag : GtkSourceView_types.source_tag obj
    method id : source_tag_id

    method style : source_tag_style
    method set_style : source_tag_style -> unit

    method set_source_properties : source_tag_property list -> unit
    method set_source_property : source_tag_property -> unit
  end

val syntax_tag :
  id:string ->
  name:string -> pat_start:string -> pat_end:string -> source_tag
val pattern_tag : id:string -> name:string -> pat:string -> source_tag
val keyword_list_tag :
  id:string ->
  name:string ->
  keywords:string list ->
  ?case_sensitive:bool ->
  ?match_empty_string_at_beginning:bool ->
  ?match_empty_string_at_end:bool ->
  ?beginning_regex:string -> ?end_regex:string -> unit -> source_tag
val block_comment_tag :
  id:string ->
  name:string -> pat_start:string -> pat_end:string -> source_tag
val line_comment_tag :
  id:string -> name:string -> pat_start:string -> source_tag
val string_tag :
  id:string ->
  name:string ->
  pat_start:string -> pat_end:string -> end_at_line_end:bool -> source_tag


(** {2 GtkSourceTagTable} *)

class source_tag_table_signals:
  ([> GtkSourceView_types.source_tag_table] as 'b) obj ->
  object('a)
    inherit GText.tag_table_signals
    method changed : callback:(unit -> unit) -> GtkSignal.id
  end

class source_tag_table:
  GtkSourceView_types.source_tag_table obj ->
  object
    inherit GText.tag_table_skel
    method as_source_tag_table : [`sourcetagtable] obj
    method connect: source_tag_table_signals
    method misc: GObj.gobject_ops
    method remove_source_tags : unit -> unit
    method add_tags : source_tag list -> unit
  end

val source_tag_table : unit -> source_tag_table

(** {2 GtkSourceStyleScheme} *)

class source_style_scheme :
  GtkSourceView_types.source_style_scheme obj ->
  object
    method as_source_style_scheme :
      GtkSourceView_types.source_style_scheme obj
    method get_name : string

    (** @raise Not_found if the specified tag does not exist. *)
    method get_tag_style : string -> source_tag_style
  end
val default_style_scheme : unit -> source_style_scheme


(** {2 GtkSourceLanguage} *)

class source_language_signals:
  ([> GtkSourceView_types.source_language ] as 'b) obj ->
  object ('a)
    inherit ['b] GObj.gobject_signals
    method tag_style_changed: callback:(string -> unit) -> GtkSignal.id
  end

class source_language:
  GtkSourceView_types.source_language obj ->
  object
    method as_source_language: GtkSourceView_types.source_language obj
    method connect: source_language_signals
    method get_escape_char: Glib.unichar
    method get_name: string
    method get_section: string
    method get_style_scheme: source_style_scheme
    method set_style_scheme: source_style_scheme -> unit
    method get_tags: source_tag list
    method get_tag_default_style: source_tag_id -> source_tag_style
    method get_tag_style: source_tag_id -> source_tag_style
    method set_tag_style: source_tag_id -> source_tag_style -> unit
    method misc: GObj.gobject_ops
  end

(** {2 GtkSourceLanguagesManager} *)

class source_languages_manager:
  GtkSourceView_types.source_languages_manager obj ->
  object
    method get_oid: int
    method get_available_languages: source_language list
    method as_source_languages_manager:
      GtkSourceView_types.source_languages_manager obj
    method get_language_from_mime_type: string -> source_language option
    method lang_files_dirs: string list
  end

val source_languages_manager:
(*   ?lang_files_dirs:string list -> *)
  unit ->
    source_languages_manager

val source_language_from_file:
  ?languages_manager:source_languages_manager -> string ->
    source_language option

(** {2 GtkSourceMarker} *)

class source_marker :
  GtkSourceView_types.source_marker Gtk.obj ->
object
  method as_source_marker : GtkSourceView_types.source_marker Gtk.obj
  method get_buffer : source_buffer
  method get_line : int
  method get_name : string
  method get_type : string
  method next : source_marker
  method prev : source_marker
  method set_type : string -> unit
end


(** {2 GtkSourceBuffer} *)

and source_buffer_signals:
  (GtkSourceView_types.source_buffer as 'b) obj ->
object ('a)
  inherit ['b] GText.buffer_signals_type
  method changed : callback:(unit -> unit) -> GtkSignal.id
  method can_redo: callback:(bool -> unit) -> GtkSignal.id
  method can_undo: callback:(bool -> unit) -> GtkSignal.id
  method highlight_updated:
    callback:(Gtk.text_iter -> Gtk.text_iter -> unit) -> GtkSignal.id
  method marker_updated: callback:(Gtk.text_iter -> unit) -> GtkSignal.id

end

and source_buffer:
  GtkSourceView_types.source_buffer obj ->
object
  inherit GText.buffer_skel
  method as_source_buffer: GtkSourceView_types.source_buffer obj
  method connect: source_buffer_signals
  method misc: GObj.gobject_ops
  method check_brackets: bool
  method set_check_brackets: bool -> unit
  method set_bracket_match_style: source_tag_style -> unit
  method highlight: bool
  method set_highlight: bool -> unit
  method max_undo_levels: int
  method set_max_undo_levels: int -> unit
  method language: source_language option
  method set_language: source_language -> unit
  method escape_char: Glib.unichar
  method set_escape_char: Glib.unichar -> unit
  method can_undo: bool
  method can_redo: bool
  method undo: unit -> unit
  method redo: unit -> unit
  method begin_not_undoable_action: unit -> unit
  method end_not_undoable_action: unit -> unit
  method create_marker: ?name:string -> ?typ:string -> GText.iter -> source_marker
  method move_marker: source_marker -> GText.iter -> unit
  method delete_marker: source_marker -> unit

  (** @raise Not_found if the marker does not exist. *)
  method get_marker: string -> source_marker
  method get_markers_in_region:
    start:GText.iter -> stop:GText.iter -> source_marker list
  method get_first_marker: source_marker option
  method get_last_marker: source_marker option
  method get_iter_at_marker: source_marker -> GText.iter
  method get_next_marker: GText.iter -> source_marker option
  method get_prev_marker: GText.iter -> source_marker option

  method source_tag_table : source_tag_table

end

val source_buffer:
  ?language:source_language ->
  ?tag_table:source_tag_table ->
  ?text:string ->
  ?check_brackets:bool ->
  ?escape_char:int ->
  ?highlight:bool ->
  ?max_undo_levels:int ->
  unit ->
    source_buffer

(** {2 GtkSourceView} *)

class source_view_signals:
  ([> GtkSourceView_types.source_view ] as 'b) obj ->
  object ('a)
    inherit GText.view_signals
    method redo: callback:(unit -> unit) -> GtkSignal.id
    method undo: callback:(unit -> unit) -> GtkSignal.id
  end

class source_view:
  GtkSourceView_types.source_view obj ->
  object
    inherit GText.view_skel
    val obj: GtkSourceView_types.source_view obj
    method connect: source_view_signals
    method source_buffer: source_buffer
    method set_show_line_numbers: bool -> unit
    method show_line_numbers: bool
    method set_show_line_markers: bool -> unit
    method show_line_markers: bool
    method set_highlight_current_line: bool -> unit
    method highlight_current_line: bool
    method set_tabs_width: int -> unit
    method tabs_width: int
    method set_auto_indent: bool -> unit
    method auto_indent: bool
    method set_insert_spaces_instead_of_tabs: bool -> unit
    method insert_spaces_instead_of_tabs: bool
    method set_show_margin: bool -> unit
    method show_margin: bool
    method set_margin: int -> unit
    method margin: int
    method set_marker_pixbuf: string -> GdkPixbuf.pixbuf -> unit
    method marker_pixbuf: string -> GdkPixbuf.pixbuf
    method set_smart_home_end: bool -> unit
    method smart_home_end: bool
    method set_cursor_color: Gdk.color -> unit
    method set_cursor_color_by_name: string -> unit
  end

val source_view:
  ?source_buffer:source_buffer ->
  ?auto_indent:bool ->
  ?highlight_current_line:bool ->
  ?insert_spaces_instead_of_tabs:bool ->
  ?margin:int ->
  ?show_line_markers:bool ->
  ?show_line_numbers:bool ->
  ?show_margin:bool ->
  ?smart_home_end:bool ->
  ?tabs_width:int ->
  ?editable:bool ->
  ?cursor_visible:bool ->
  ?justification:GtkEnums.justification ->
  ?wrap_mode:GtkEnums.wrap_mode ->
  ?accepts_tab:bool ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) ->
  ?show:bool ->
  unit ->
    source_view

(** {2 Misc} *)

val find_matching_bracket: GText.iter -> GText.iter option

val iter_forward_search :
  GText.iter ->
  SourceViewEnums.source_search_flag list ->
  start:< as_iter : Gtk.text_iter; .. > ->
  stop:< as_iter : Gtk.text_iter; .. > ->
  ?limit:< as_iter : Gtk.text_iter; .. > ->
  string -> (GText.iter * GText.iter) option

val iter_backward_search :
  GText.iter ->
  SourceViewEnums.source_search_flag list ->
  start:< as_iter : Gtk.text_iter; .. > ->
  stop:< as_iter : Gtk.text_iter; .. > ->
  ?limit:< as_iter : Gtk.text_iter; .. > ->
  string -> (GText.iter * GText.iter) option
