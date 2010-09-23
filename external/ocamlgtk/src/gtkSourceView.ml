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

open GtkSourceView_types
open Gaux
open Gobject
open Gtk
open Tags
open GtkSourceViewProps
open GtkBase

(* external gslist_of_string_list: string list -> 'a obj =
  "ml_gslist_of_string_list" *)

external _gtk_source_tag_style_init: unit -> unit = "ml_gtk_source_tag_style_init"
external _gtk_source_tag_init: unit -> unit = "ml_gtk_source_tag_init"
external _gtk_source_tag_table_init: unit -> unit = "ml_gtk_source_tag_table_init"
external _gtk_source_style_scheme_init: unit -> unit = "ml_gtk_source_style_scheme_init"
external _gtk_source_language_init: unit -> unit = "ml_gtk_source_language_init"
external _gtk_source_languages_manager_init: unit -> unit =
  "ml_gtk_source_languages_manager_init"
external _gtk_source_marker_init: unit -> unit = "ml_gtk_source_marker_init"
external _gtk_source_buffer_init: unit -> unit = "ml_gtk_source_buffer_init"
external _gtk_source_view_init: unit -> unit = "ml_gtk_source_view_init"

let () =
  _gtk_source_style_scheme_init ();
  _gtk_source_language_init ();
  _gtk_source_languages_manager_init ();
  _gtk_source_tag_style_init ();
  _gtk_source_tag_init ();
  _gtk_source_tag_table_init ();
  _gtk_source_marker_init ();
  _gtk_source_buffer_init ();
  _gtk_source_view_init ()

module SourceStyleScheme =
struct
   include SourceStyleScheme
  external get_default: unit -> [>`sourcestylescheme] obj =
    "ml_gtk_source_style_scheme_get_default"
  external get_tag_style:
      source_style_scheme obj -> string -> source_tag_style obj option =
	"ml_gtk_source_style_scheme_get_tag_style"
  external get_name: source_style_scheme obj -> string =
    "ml_gtk_source_style_scheme_get_name"
end

module SourceLanguage =
struct
  include SourceLanguage
  external new_from_file: string -> [>`sourcelanguagesmanager] obj -> source_language obj option =
    "ml__gtk_source_language_new_from_file"
  external get_name: [>`sourcelanguage] obj -> string =
    "ml_gtk_source_language_get_name"
  external get_section: [>`sourcelanguage] obj -> string =
    "ml_gtk_source_language_get_section"
  external get_tags: [>`sourcelanguage] obj -> source_tag obj list =
    "ml_gtk_source_language_get_tags"
  external get_escape_char: [>`sourcelanguage] obj -> Glib.unichar =
    "ml_gtk_source_language_get_escape_char"
(*   external get_mime_types: [>`sourcelanguage] obj -> string list *)
(*   external set_mime_types: [>`sourcelanguage] obj -> string list -> unit *)
  external get_style_scheme: [>`sourcelanguage] obj -> source_style_scheme obj =
    "ml_gtk_source_language_get_style_scheme"
  external set_style_scheme: [>`sourcelanguage] obj -> source_style_scheme obj -> unit =
    "ml_gtk_source_language_set_style_scheme"
  external get_tag_style: [>`sourcelanguage] obj -> string -> source_tag_style obj =
    "ml_gtk_source_language_get_tag_style"
  external set_tag_style: [>`sourcelanguage] obj -> string -> source_tag_style obj -> unit =
    "ml_gtk_source_language_set_tag_style"
  external get_tag_default_style: [>`sourcelanguage] obj -> string -> source_tag_style obj =
    "ml_gtk_source_language_get_tag_default_style"
end

module SourceLanguagesManager =
struct
  include SourceLanguagesManager
  external new_: unit -> source_languages_manager obj =
    "ml_gtk_source_languages_manager_new"
   external get_available_languages:
    [>`sourcelanguagesmanager] obj -> source_language obj list
    = "ml_gtk_source_languages_manager_get_available_languages"
  external get_language_from_mime_type:
    [>`sourcelanguagesmanager] obj -> string -> source_language obj option
    = "ml_gtk_source_languages_manager_get_language_from_mime_type"
  external get_lang_files_dirs:
    [>`sourcelanguagesmanager] obj -> string list
    = "ml_gtk_source_languages_manager_get_lang_files_dirs"
(*   external set_lang_files_dirs:
    [>`sourcelanguagesmanager] obj -> string list -> unit
    = "ml_gtk_source_languages_manager_set_lang_files_dirs" *)
end

module SourceTagStyle =
struct
  include SourceTagStyle
  external new_ : unit -> source_tag_style obj = "ml_gtk_source_tag_style_new"
  external copy : source_tag_style obj -> source_tag_style obj = "ml_gtk_source_tag_style_copy"

  external get_background : source_tag_style obj -> Gdk.color =
    "ml_gtk_source_tag_style_get_background"
  external get_bold : source_tag_style obj -> bool =
    "ml_gtk_source_tag_style_get_bold"
  external get_foreground : source_tag_style obj -> Gdk.color =
    "ml_gtk_source_tag_style_get_foreground"
  external get_italic : source_tag_style obj -> bool =
    "ml_gtk_source_tag_style_get_italic"
  external get_strikethrough : source_tag_style obj -> bool =
    "ml_gtk_source_tag_style_get_strikethrough"
  external get_underline : source_tag_style obj -> bool =
    "ml_gtk_source_tag_style_get_underline"
  external get_use_background : source_tag_style obj -> bool =
    "ml_gtk_source_tag_style_get_use_background"
  external get_use_foreground : source_tag_style obj -> bool =
    "ml_gtk_source_tag_style_get_use_foreground"

  external set_background : source_tag_style obj -> Gdk.color -> unit =
    "ml_gtk_source_tag_style_set_background"
  external set_bold : source_tag_style obj -> bool -> unit =
    "ml_gtk_source_tag_style_set_bold"
  external set_foreground : source_tag_style obj -> Gdk.color -> unit =
    "ml_gtk_source_tag_style_set_foreground"
  external set_italic : source_tag_style obj -> bool -> unit =
    "ml_gtk_source_tag_style_set_italic"
  external set_strikethrough : source_tag_style obj -> bool -> unit =
    "ml_gtk_source_tag_style_set_strikethrough"
  external set_underline : source_tag_style obj -> bool -> unit =
    "ml_gtk_source_tag_style_set_underline"
  external set_use_background : source_tag_style obj -> bool -> unit =
    "ml_gtk_source_tag_style_set_use_background"
  external set_use_foreground : source_tag_style obj -> bool -> unit =
    "ml_gtk_source_tag_style_set_use_foreground"end

module SourceTag =
struct
  include SourceTag
  external syntax_tag :
      id: string -> name: string -> pat_start: string -> pat_end: string -> source_tag obj =
	"ml_gtk_syntax_tag_new"

  external pattern_tag :
      id: string -> name: string -> pat: string -> source_tag obj =
	"ml_gtk_pattern_tag_new"

  external keyword_list_tag :
   id: string -> name: string -> keywords: string list ->
   case_sensitive: bool -> match_empty_string_at_beginning: bool ->
   match_empty_string_at_end: bool ->
   beginning_regex: string option -> end_regex: string option ->
   source_tag obj =
   "ml_gtk_keyword_list_tag_new_bc" "ml_gtk_keyword_list_tag_new"

   let block_comment_tag = syntax_tag

   external line_comment_tag :
   id: string -> name: string -> pat_start: string -> source_tag obj =
     "ml_gtk_line_comment_tag_new"

   external string_tag :
   id: string -> name: string -> pat_start: string -> pat_end: string ->
   end_at_line_end: bool -> source_tag obj =
     "ml_gtk_string_tag_new"

   external get_style : [> `sourcetag] obj -> [`sourcetagstyle] obj option =
     "ml_gtk_source_tag_get_style"
   external set_style : [> `sourcetag] obj -> [`sourcetagstyle] obj -> unit =
     "ml_gtk_source_tag_set_style"

end

module SourceTagTable =
struct
  include SourceTagTable
  external new_ : unit -> source_tag_table obj = "ml_gtk_source_tag_table_new"
  external add_tags : [> `sourcetagtable] obj -> [> `sourcetag] obj list -> unit =
    "ml_gtk_source_tag_table_add_tags"
  external remove_source_tags : [> `sourcetagtable] obj -> unit =
    "ml_gtk_source_tag_table_remove_source_tags"
end

module SourceMarker =
struct
  include SourceMarker
  external set_type : source_marker obj -> string -> unit =
    "ml_gtk_source_marker_set_marker_type"
  external get_type : source_marker obj -> string =
    "ml_gtk_source_marker_get_marker_type"
  external get_line : source_marker obj -> int =
    "ml_gtk_source_marker_get_line"
  external get_name : source_marker obj -> string =
    "ml_gtk_source_marker_get_name"
  external get_buffer : source_marker obj -> source_buffer obj =
    "ml_gtk_source_marker_get_buffer"
  external next : source_marker obj -> source_marker obj =
    "ml_gtk_source_marker_next"
  external prev : source_marker obj -> source_marker obj =
    "ml_gtk_source_marker_prev"
end

module SourceBuffer =
struct
  include SourceBuffer
   external new_: [`sourcetagtable] obj -> source_buffer obj = "ml_gtk_source_buffer_new"
   external new_with_langage: [>`sourcelanguage] obj -> source_buffer obj =
    "ml_gtk_source_buffer_new_with_language"
  external can_undo: [>`sourcebuffer] obj -> bool =
    "ml_gtk_source_buffer_can_undo"
  external can_redo: [>`sourcebuffer] obj -> bool =
    "ml_gtk_source_buffer_can_redo"
  external undo: [>`sourcebuffer] obj -> unit = "ml_gtk_source_buffer_undo"
  external redo: [>`sourcebuffer] obj -> unit = "ml_gtk_source_buffer_redo"
  external begin_not_undoable_action: [>`sourcebuffer] obj -> unit =
    "ml_gtk_source_buffer_begin_not_undoable_action"
  external end_not_undoable_action: [>`sourcebuffer] obj -> unit =
    "ml_gtk_source_buffer_end_not_undoable_action"
  external set_bracket_match_style: [>`sourcebuffer] obj -> source_tag_style obj -> unit =
    "ml_gtk_source_buffer_set_bracket_match_style"
  external create_marker:
      [>`sourcebuffer] obj -> string option -> string option -> Gtk.text_iter ->
      source_marker obj =
      "ml_gtk_source_buffer_create_marker"
  external move_marker:
      [>`sourcebuffer] obj -> source_marker obj -> Gtk.text_iter -> unit =
      "ml_gtk_source_buffer_move_marker"
  external delete_marker:
      [>`sourcebuffer] obj -> source_marker obj -> unit =
      "ml_gtk_source_buffer_delete_marker"
  external get_marker:
      [>`sourcebuffer] obj -> string -> source_marker obj option =
      "ml_gtk_source_buffer_get_marker"
  external get_markers_in_region:
      [>`sourcebuffer] obj -> Gtk.text_iter -> Gtk.text_iter ->  source_marker obj list =
      "ml_gtk_source_buffer_get_markers_in_region"
  external get_iter_at_marker:
      [>`sourcebuffer] obj -> source_marker obj -> Gtk.text_iter =
      "ml_gtk_source_buffer_get_iter_at_marker"
  external get_first_marker:
      [>`sourcebuffer] obj -> source_marker obj option =
      "ml_gtk_source_buffer_get_first_marker"
  external get_last_marker:
      [>`sourcebuffer] obj -> source_marker obj option =
      "ml_gtk_source_buffer_get_last_marker"
 external get_next_marker:
      [>`sourcebuffer] obj -> Gtk.text_iter -> source_marker obj option =
      "ml_gtk_source_buffer_get_next_marker"
 external get_prev_marker:
      [>`sourcebuffer] obj -> Gtk.text_iter -> source_marker obj option =
      "ml_gtk_source_buffer_get_prev_marker"
end

module SourceView =
struct
  include SourceView
  external new_: unit -> source_view obj = "ml_gtk_source_view_new"
  external new_with_buffer: [>`sourcebuffer] obj -> source_view obj =
    "ml_gtk_source_view_new_with_buffer"
  external set_marker_pixbuf:  [>`sourceview] obj -> string -> GdkPixbuf.pixbuf -> unit =
    "ml_gtk_source_view_set_marker_pixbuf"
  external get_marker_pixbuf:  [>`sourceview] obj -> string -> GdkPixbuf.pixbuf =
    "ml_gtk_source_view_get_marker_pixbuf"
  external set_cursor_color:   [>`sourceview] obj -> Gdk.color -> unit =
    "ml_gtk_modify_cursor_color"
end

module SourceViewMisc =
struct
  external find_matching_bracket: text_iter -> bool =
    "ml_gtk_source_iter_find_matching_bracket"
  external iter_backward_search:
       Gtk.text_iter -> string -> SourceViewEnums.source_search_flag list ->
	start: Gtk.text_iter -> stop: Gtk.text_iter -> Gtk.text_iter option ->
	(Gtk.text_iter * Gtk.text_iter) option =
    "ml_gtk_source_iter_backward_search_bc" "ml_gtk_source_iter_backward_search"
  external iter_forward_search:
      Gtk.text_iter -> string -> SourceViewEnums.source_search_flag list ->
	start: Gtk.text_iter -> stop: Gtk.text_iter -> Gtk.text_iter option ->
	(Gtk.text_iter * Gtk.text_iter) option =
    "ml_gtk_source_iter_forward_search_bc" "ml_gtk_source_iter_forward_search"
end
