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

(** {2 GtkSourceStyleScheme} *)

class source_style_scheme :
  GtkSourceView2_types.source_style_scheme obj ->
  object
    method as_source_style_scheme :
      GtkSourceView2_types.source_style_scheme obj
    method name : string
    method description : string
  end


(** {2 GtkSourceStyleSchemeManager} *)

class source_style_scheme_manager :
  GtkSourceView2_types.source_style_scheme_manager obj ->
  object
    method search_path: string list
    method set_search_path: string list -> unit
    method append_search_path: string -> unit
    method prepend_search_path: string -> unit
    method style_scheme_ids: string list
    method style_scheme: string -> source_style_scheme option
    method force_rescan: unit -> unit
  end

val source_style_scheme_manager : default:bool -> source_style_scheme_manager


(** {2 GtkSourceLanguage} *)

class source_language:
  GtkSourceView2_types.source_language obj ->
  object
    method as_source_language: GtkSourceView2_types.source_language obj
    method misc: GObj.gobject_ops

    method hidden: bool
    method id: string
    method name: string
    method section: string

    method metadata: string -> string option
    method mime_types: string list
    method globs: string list
    method style_name: string -> string option
    method style_ids: string list
  end

(** {2 GtkSourceLanguageManager} *)

class source_language_manager:
  GtkSourceView2_types.source_language_manager obj ->
  object
    method get_oid: int
    method as_source_language_manager:
      GtkSourceView2_types.source_language_manager obj

    method set_search_path : string list -> unit
    method search_path : string list
    method language_ids : string list
    method language : string -> source_language option
    method guess_language:
      ?filename:string -> ?content_type:string -> unit -> source_language option
  end

val source_language_manager : default:bool -> source_language_manager


(** {2 GtkSourceMark} *)

class source_mark: ((GtkSourceView2_types.source_mark obj) as 'a) ->
object
  method as_source_mark : 'a
  method coerce: GText.mark
  method category: string option
  method next: ?category:string -> unit -> source_mark option
  method prev: ?category:string -> unit -> source_mark option
end

val source_mark : ?category:string -> unit -> source_mark


(** {2 GtkSourceBuffer} *)

class source_buffer_signals:
  (GtkSourceView2_types.source_buffer as 'b) obj ->
object ('a)
  inherit ['b] GText.buffer_signals_type
  method changed : callback:(unit -> unit) -> GtkSignal.id
  method highlight_updated:
    callback:(Gtk.text_iter -> Gtk.text_iter -> unit) -> GtkSignal.id
  method source_mark_updated: callback:(GtkSourceView2_types.source_mark obj -> unit) -> GtkSignal.id

end

and source_buffer: GtkSourceView2_types.source_buffer obj ->
object
  inherit GText.buffer_skel
  val obj: GtkSourceView2_types.source_buffer obj
  method as_source_buffer: GtkSourceView2_types.source_buffer obj
  method connect: source_buffer_signals
  method misc: GObj.gobject_ops

  method highlight_syntax: bool
  method set_highlight_syntax: bool -> unit
  method language: source_language option
  method set_language: source_language option -> unit
  method highlight_matching_brackets: bool
  method set_highlight_matching_brackets: bool -> unit
  method style_scheme: source_style_scheme option
  method set_style_scheme: source_style_scheme option -> unit
  method max_undo_levels: int
  method set_max_undo_levels: int -> unit
  method undo: unit -> unit
  method redo: unit -> unit
  method can_undo: bool
  method can_redo: bool
  method begin_not_undoable_action: unit -> unit
  method end_not_undoable_action: unit -> unit

  method create_source_mark: ?name:string -> ?category:string -> GText.iter
    -> source_mark
  method source_marks_at_line: ?category:string -> int
    -> source_mark list
  method source_marks_at_iter: ?category:string -> GText.iter
    -> source_mark list
  method remove_source_marks :
    ?category:string -> start:GText.iter -> stop:GText.iter -> unit -> unit

  method forward_iter_to_source_mark: ?category:string -> GText.iter -> bool
  method backward_iter_to_source_mark: ?category:string -> GText.iter -> bool

  method ensure_highlight: start:GText.iter -> stop:GText.iter -> unit

end

val source_buffer:
  ?language:source_language ->
  ?style_scheme:source_style_scheme ->
  ?tag_table:GText.tag_table ->
  ?text:string ->
  ?highlight_matching_brackets:bool ->
  ?highlight_syntax:bool ->
  ?max_undo_levels:int ->
  unit -> source_buffer

(** {2 GtkSourceView} *)

class source_view_signals:
  ([> GtkSourceView2_types.source_view ] as 'b) obj ->
  object ('a)
    inherit GText.view_signals
    method redo: callback:(unit -> unit) -> GtkSignal.id
    method undo: callback:(unit -> unit) -> GtkSignal.id
  end

class source_view:
  GtkSourceView2_types.source_view obj ->
object
  inherit GText.view_skel
  inherit OgtkSourceView2Props.source_view_props
  val obj: GtkSourceView2_types.source_view obj
  method connect: source_view_signals
  method source_buffer: source_buffer
  method set_show_line_numbers: bool -> unit
  method show_line_numbers: bool
  method set_highlight_current_line: bool -> unit
  method highlight_current_line: bool
  method set_tab_width: int -> unit
  method tab_width: int
  method set_auto_indent: bool -> unit
  method auto_indent: bool
  method set_insert_spaces_instead_of_tabs: bool -> unit
  method insert_spaces_instead_of_tabs: bool
  method set_cursor_color: Gdk.color -> unit
  method set_cursor_color_by_name: string -> unit

  method draw_spaces:
    SourceView2Enums.source_draw_spaces_flags list
  method set_draw_spaces:
    SourceView2Enums.source_draw_spaces_flags list -> unit

  method get_mark_category_priority:
    category:string -> int
  method set_mark_category_priority:
    category:string -> int -> unit
  method get_mark_category_pixbuf:
    category:string -> GdkPixbuf.pixbuf option
  method set_mark_category_pixbuf:
    category:string -> GdkPixbuf.pixbuf option -> unit
  method get_mark_category_background:
    category:string -> Gdk.color option
  method set_mark_category_background:
    category:string -> Gdk.color option -> unit
end

val source_view :
  ?source_buffer:source_buffer ->
  ?draw_spaces:SourceView2Enums.source_draw_spaces_flags list ->
  ?auto_indent:bool ->
  ?highlight_current_line:bool ->
  ?indent_on_tab:bool ->
  ?indent_width:int ->
  ?insert_spaces_instead_of_tabs:bool ->
  ?right_margin_position:int ->
  ?show_line_marks:bool ->
  ?show_line_numbers:bool ->
  ?show_right_margin:bool ->
  ?smart_home_end:SourceView2Enums.source_smart_home_end_type ->
  ?tab_width:int ->
  ?editable:bool ->
  ?cursor_visible:bool ->
  ?justification:GtkEnums.justification ->
  ?wrap_mode:GtkEnums.wrap_mode ->
  ?accepts_tab:bool ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) -> ?show:bool -> unit -> source_view


(** {2 Misc} *)

val iter_forward_search :
  GText.iter ->
  SourceView2Enums.source_search_flag list ->
  start:< as_iter : Gtk.text_iter; .. > ->
  stop:< as_iter : Gtk.text_iter; .. > ->
  ?limit:< as_iter : Gtk.text_iter; .. > ->
  string -> (GText.iter * GText.iter) option

val iter_backward_search :
  GText.iter ->
  SourceView2Enums.source_search_flag list ->
  start:< as_iter : Gtk.text_iter; .. > ->
  stop:< as_iter : Gtk.text_iter; .. > ->
  ?limit:< as_iter : Gtk.text_iter; .. > ->
  string -> (GText.iter * GText.iter) option
