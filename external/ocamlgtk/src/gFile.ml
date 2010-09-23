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

open GtkFile
  
class filter obj = object
   inherit GObj.gtkobj obj
   method as_file_filter = (obj :> Gtk.file_filter Gtk.obj)
   method set_name = FileFilter.set_name obj 
   method name = FileFilter.get_name obj
   method add_mime_type = FileFilter.add_mime_type obj
   method add_pattern = FileFilter.add_pattern obj
   method add_custom = FileFilter.add_custom obj
end

let filter ?name ?(patterns=[]) ?(mime_types=[]) () =
   let w = FileFilter.create () in
   Gaux.may (FileFilter.set_name w) name ;
   List.iter (FileFilter.add_pattern w) patterns ;
   List.iter (FileFilter.add_mime_type w) mime_types ;
   new filter w

class type chooser_signals = object
  method current_folder_changed : callback:(unit -> unit) -> GtkSignal.id
  method file_activated : callback:(unit -> unit) -> GtkSignal.id
  method selection_changed : callback:(unit -> unit) -> GtkSignal.id
  method update_preview : callback:(unit -> unit) -> GtkSignal.id
  method confirm_overwrite : 
    callback:(unit -> GtkEnums.file_chooser_confirmation) -> GtkSignal.id
end

class type chooser =
  object
    method set_action : GtkEnums.file_chooser_action -> unit
    method action : GtkEnums.file_chooser_action
    method set_local_only : bool -> unit
    method local_only : bool
    method set_select_multiple : bool -> unit
    method select_multiple : bool
    method set_current_name : string -> unit
    method show_hidden : bool
    method set_show_hidden : bool -> unit

    method set_filename : string -> bool
    method filename : string option
    method select_filename : string -> bool
    method unselect_filename : string -> unit
    method get_filenames : string list
    method set_current_folder : string -> bool
    method current_folder : string option

    method set_uri : string -> bool
    method uri : string option
    method select_uri : string -> bool
    method unselect_uri : string -> unit
    method get_uris : string list
    method set_current_folder_uri : string -> bool
    method current_folder_uri : string

    method select_all : unit
    method unselect_all : unit

    method set_preview_widget : GObj.widget -> unit
    method preview_widget : GObj.widget
    method set_preview_widget_active : bool -> unit
    method preview_widget_active : bool
    method preview_filename : string option
    method preview_uri : string option
    method set_use_preview_label : bool -> unit
    method use_preview_label : bool

    method set_extra_widget : GObj.widget -> unit
    method extra_widget : GObj.widget

    method add_filter : filter -> unit
    method remove_filter : filter -> unit
    method list_filters : filter list
    method set_filter : filter -> unit
    method filter : filter

    method add_shortcut_folder : string -> unit
    method remove_shortcut_folder : string -> unit
    method list_shortcut_folders : string list
    method add_shortcut_folder_uri : string -> unit
    method remove_shortcut_folder_uri : string -> unit
    method list_shortcut_folder_uris : string list
      
    method do_overwrite_confirmation : bool
    method set_do_overwrite_confirmation : bool -> unit

  end

class virtual chooser_impl = object (self)
  val virtual obj : 'a Gtk.obj
  inherit OgtkFileProps.file_chooser_props

  method set_current_name = FileChooser.set_current_name obj

  method set_filename = FileChooser.set_filename obj
  method filename = FileChooser.get_filename obj
  method select_filename = FileChooser.select_filename obj
  method unselect_filename = FileChooser.unselect_filename obj
  method select_all = FileChooser.select_all obj
  method unselect_all = FileChooser.unselect_all obj
  method get_filenames = FileChooser.get_filenames obj
  method set_current_folder = FileChooser.set_current_folder obj
  method current_folder = FileChooser.get_current_folder obj

  method set_uri = FileChooser.set_uri obj
  method uri = FileChooser.get_uri obj
  method select_uri = FileChooser.select_uri obj
  method unselect_uri = FileChooser.unselect_uri obj
  method get_uris = FileChooser.get_uris obj
  method set_current_folder_uri = FileChooser.set_current_folder_uri obj
  method current_folder_uri = FileChooser.get_current_folder_uri obj

  method preview_filename = FileChooser.get_preview_filename obj
  method preview_uri = FileChooser.get_preview_uri obj

  method add_filter (f : filter) = FileChooser.add_filter obj f#as_file_filter
  method remove_filter (f : filter) = FileChooser.remove_filter obj f#as_file_filter
  method list_filters = List.map (new filter) (FileChooser.list_filters obj )
  method set_filter (f : filter) = Gobject.set FileChooser.P.filter obj f#as_file_filter
  method filter = new filter (Gobject.get FileChooser.P.filter obj)
      
  method add_shortcut_folder = FileChooser.add_shortcut_folder obj
  method remove_shortcut_folder = FileChooser.remove_shortcut_folder obj
  method list_shortcut_folders = FileChooser.list_shortcut_folders obj
  method add_shortcut_folder_uri = FileChooser.add_shortcut_folder_uri obj
  method remove_shortcut_folder_uri = FileChooser.remove_shortcut_folder_uri obj
  method list_shortcut_folder_uris = FileChooser.list_shortcut_folder_uris obj
end

class chooser_widget_signals obj = object
  inherit GObj.widget_signals_impl obj
  inherit OgtkFileProps.file_chooser_sigs
end

class chooser_widget obj = object
  inherit [_] GObj.widget_impl obj
  inherit chooser_impl
  method event = new GObj.event_ops obj
  method connect = new chooser_widget_signals obj
end

let may_cons = Gobject.Property.may_cons 

let chooser_widget ~action ?backend ?packing ?show () =
  let w = FileChooser.widget_create 
      (may_cons 
	 FileChooser.P.file_system_backend backend
	 [ Gobject.param FileChooser.P.action action ]) in
  let o = new chooser_widget w in
  GObj.pack_return o ?packing ?show

class chooser_button_signals obj = object
  inherit GContainer.container_signals_impl obj
  inherit OgtkFileProps.file_chooser_sigs
end

class chooser_button obj = object
  method private obj = obj
  inherit GPack.box_skel obj
  inherit chooser_impl
  inherit OgtkFileProps.file_chooser_button_props
  method connect = new chooser_button_signals obj
end

let chooser_button ~action ?title ?width_chars ?backend =
  GContainer.pack_container
    (Gobject.param FileChooser.P.action action ::
     (may_cons FileChooser.P.file_system_backend backend (
      may_cons FileChooserButton.P.title title (
      may_cons FileChooserButton.P.width_chars width_chars []))))
    ~create:(fun pl -> new chooser_button (FileChooserButton.create pl))
