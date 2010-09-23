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

(* $Id: gtkFile.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

external _gtkfile_init : unit -> unit = "ml_gtkfile_init"
let () = _gtkfile_init ()

module FileFilter = struct
  external create : unit -> Gtk.file_filter Gtk.obj
      = "ml_gtk_file_filter_new"
  external set_name : [> Gtk.file_filter] Gtk.obj -> string -> unit
      = "ml_gtk_file_filter_set_name"
  external get_name : [> Gtk.file_filter] Gtk.obj -> string
      = "ml_gtk_file_filter_get_name"
  external add_mime_type : [> Gtk.file_filter] Gtk.obj -> string -> unit
      = "ml_gtk_file_filter_add_mime_type"
  external add_pattern : [> Gtk.file_filter] Gtk.obj -> string -> unit
      = "ml_gtk_file_filter_add_pattern"
  external add_custom : [> Gtk.file_filter] Gtk.obj -> 
    GtkEnums.file_filter_flags list ->
    callback:((GtkEnums.file_filter_flags * string) list -> bool) -> unit
      = "ml_gtk_file_filter_add_custom"
end

module FileChooser = struct
  include GtkFileProps.FileChooser

  type error =
    | ERROR_NONEXISTENT
    | ERROR_BAD_FILENAME
  exception Error of error * string
  let () = Callback.register_exception 
      "gtk_file_chooser_error" (Error (ERROR_NONEXISTENT, ""))

  external set_current_name : [> Gtk.file_chooser] Gtk.obj -> string -> unit
      = "ml_gtk_file_chooser_set_current_name"

  external get_filename : [> Gtk.file_chooser] Gtk.obj -> string option
      = "ml_gtk_file_chooser_get_filename"
  external set_filename : [> Gtk.file_chooser] Gtk.obj -> string -> bool
      = "ml_gtk_file_chooser_set_filename"
  external select_filename : [> Gtk.file_chooser] Gtk.obj -> string -> bool
      = "ml_gtk_file_chooser_select_filename"
  external unselect_filename : [> Gtk.file_chooser] Gtk.obj -> string -> unit
      = "ml_gtk_file_chooser_unselect_filename"
  external select_all : [> Gtk.file_chooser] Gtk.obj -> unit
      = "ml_gtk_file_chooser_select_all"
  external unselect_all : [> Gtk.file_chooser] Gtk.obj -> unit
      = "ml_gtk_file_chooser_unselect_all"
  external get_filenames : [> Gtk.file_chooser] Gtk.obj -> string list
      = "ml_gtk_file_chooser_get_filenames"
  external get_current_folder : [> Gtk.file_chooser] Gtk.obj -> string option
      = "ml_gtk_file_chooser_get_current_folder"
  external set_current_folder : [> Gtk.file_chooser] Gtk.obj -> string -> bool
      = "ml_gtk_file_chooser_set_current_folder"

  external get_uri : [> Gtk.file_chooser] Gtk.obj -> string option
      = "ml_gtk_file_chooser_get_uri"
  external set_uri : [> Gtk.file_chooser] Gtk.obj -> string -> bool
      = "ml_gtk_file_chooser_set_uri"
  external select_uri : [> Gtk.file_chooser] Gtk.obj -> string -> bool
      = "ml_gtk_file_chooser_select_uri"
  external unselect_uri : [> Gtk.file_chooser] Gtk.obj -> string -> unit
      = "ml_gtk_file_chooser_unselect_uri"
  external get_uris : [> Gtk.file_chooser] Gtk.obj -> string list
      = "ml_gtk_file_chooser_get_uris"
  external get_current_folder_uri : [> Gtk.file_chooser] Gtk.obj -> string
      = "ml_gtk_file_chooser_get_current_folder_uri"
  external set_current_folder_uri : [> Gtk.file_chooser] Gtk.obj -> string -> bool
      = "ml_gtk_file_chooser_set_current_folder_uri"

  external get_preview_filename : [> Gtk.file_chooser] Gtk.obj -> string option
      = "ml_gtk_file_chooser_get_preview_filename"
  external get_preview_uri : [> Gtk.file_chooser] Gtk.obj -> string option
      = "ml_gtk_file_chooser_get_preview_uri"

  external add_filter : [> Gtk.file_chooser] Gtk.obj -> Gtk.file_filter Gtk.obj -> unit
      = "ml_gtk_file_chooser_add_filter"
  external remove_filter : [> Gtk.file_chooser] Gtk.obj -> Gtk.file_filter Gtk.obj -> unit
      = "ml_gtk_file_chooser_remove_filter"
  external list_filters : [> Gtk.file_chooser] Gtk.obj -> Gtk.file_filter Gtk.obj list
      = "ml_gtk_file_chooser_list_filters"

  external add_shortcut_folder : [> Gtk.file_chooser] Gtk.obj -> string -> unit
      = "ml_gtk_file_chooser_add_shortcut_folder"
  external remove_shortcut_folder : [> Gtk.file_chooser] Gtk.obj -> string -> unit
      = "ml_gtk_file_chooser_remove_shortcut_folder"
  external list_shortcut_folders : [> Gtk.file_chooser] Gtk.obj -> string list
      = "ml_gtk_file_chooser_list_shortcut_folders"
  external add_shortcut_folder_uri : [> Gtk.file_chooser] Gtk.obj -> string -> unit
      = "ml_gtk_file_chooser_add_shortcut_folder_uri"
  external remove_shortcut_folder_uri : [> Gtk.file_chooser] Gtk.obj -> string -> unit
      = "ml_gtk_file_chooser_remove_shortcut_folder_uri"
  external list_shortcut_folder_uris : [> Gtk.file_chooser] Gtk.obj -> string list
      = "ml_gtk_file_chooser_list_shortcut_folder_uris"

  let dialog_create pl : [Gtk.dialog|Gtk.file_chooser] Gtk.obj = GtkObject.make "GtkFileChooserDialog" pl
  let widget_create pl : [Gtk.widget|Gtk.file_chooser] Gtk.obj = GtkObject.make "GtkFileChooserWidget" pl
end

module FileChooserButton = GtkFileProps.FileChooserButton
