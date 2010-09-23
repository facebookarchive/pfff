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

(** The new file chooser widget *)

(** {3 GtkFileFilter} *)

(** @since GTK 2.4
    @gtkdoc gtk gtk-gtkfilefilter *)
class filter :
  ([> Gtk.file_filter ] as 'a) Gtk.obj ->
  object
    inherit GObj.gtkobj
    val obj : 'a Gtk.obj
    method as_file_filter : Gtk.file_filter Gtk.obj
    method add_mime_type : string -> unit
    method add_pattern : string -> unit
    method add_custom : GtkEnums.file_filter_flags list -> 
      callback:((GtkEnums.file_filter_flags * string) list -> bool) -> unit
    method name : string
    method set_name : string -> unit
  end

(** @since GTK 2.4
    @gtkdoc gtk gtk-gtkfilefilter *)
val filter : 
  ?name:string -> 
  ?patterns:string list ->
  ?mime_types:string list -> unit -> filter

(** {3 GtkFileChooser} *)

(** @since GTK 2.4
    @gtkdoc gtk GtkFileChooser *)
class type chooser_signals =
  object
    method current_folder_changed : callback:(unit -> unit) -> GtkSignal.id
    method selection_changed : callback:(unit -> unit) -> GtkSignal.id
    method update_preview : callback:(unit -> unit) -> GtkSignal.id
    method file_activated : callback:(unit -> unit) -> GtkSignal.id
    method confirm_overwrite : 
      callback:(unit -> GtkEnums.file_chooser_confirmation) -> GtkSignal.id (**  since Gtk 2.8 *)
  end

(** @since GTK 2.4
    @gtkdoc gtk GtkFileChooser *)
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

    method add_shortcut_folder : string -> unit (** @raise GtkFile.FileChooser.Error if operation fails *)
    method remove_shortcut_folder : string -> unit (** @raise GtkFile.FileChooser.Error if operation fails *)
    method list_shortcut_folders : string list
    method add_shortcut_folder_uri : string -> unit (** @raise GtkFile.FileChooser.Error if operation fails *)
    method remove_shortcut_folder_uri : string -> unit (** @raise GtkFile.FileChooser.Error if operation fails *)
    method list_shortcut_folder_uris : string list

    method do_overwrite_confirmation : bool (** since Gtk 2.8 *)
    method set_do_overwrite_confirmation : bool -> unit (** since Gtk 2.8 *)

  end

(** @since GTK 2.4
    @gtkdoc gtk GtkFileChooserWidget *)
class chooser_widget_signals :
  ([> Gtk.widget|Gtk.file_chooser] as 'a) Gtk.obj ->
  object
    inherit GObj.widget_signals
    inherit chooser_signals
  end

(** @since GTK 2.4
    @gtkdoc gtk GtkFileChooserWidget *)
class chooser_widget :
  ([> Gtk.widget|Gtk.file_chooser] as 'a) Gtk.obj ->
  object
    inherit GObj.widget
    inherit chooser
    val obj : 'a Gtk.obj
    method event : GObj.event_ops
    method connect : chooser_widget_signals
  end

(** @since GTK 2.4
    @gtkdoc gtk GtkFileChooserWidget *)
val chooser_widget : 
  action:GtkEnums.file_chooser_action ->
  ?backend:string ->
  ?packing:(GObj.widget -> unit) -> 
  ?show:bool ->
  unit ->
  chooser_widget

(** @since GTK 2.6
    @gtkdoc gtk GtkFileChooserButton *)
class chooser_button_signals :
  ([> Gtk.container | `filechooser] as 'a) Gtk.obj ->
  object
    inherit GContainer.container_signals
    inherit chooser_signals
  end

(** @since GTK 2.6
    @gtkdoc gtk GtkFileChooserButton *)
class chooser_button : 
  ([> Gtk.file_chooser_button] as 'a) Gtk.obj ->
  object
    inherit GPack.box_skel
    inherit chooser
    val obj : 'a Gtk.obj
    method connect : chooser_button_signals

    method title : string
    method set_title : string -> unit
    method width_chars : int
    method set_width_chars : int -> unit
  end

val chooser_button :
  action:GtkEnums.file_chooser_action ->
  ?title:string ->
  ?width_chars:int ->
  ?backend:string ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) -> 
  ?show:bool ->
  unit ->
  chooser_button

(**/**)

class virtual chooser_impl :
  object
    val virtual obj : [> Gtk.file_chooser] Gtk.obj
    inherit chooser
  end

