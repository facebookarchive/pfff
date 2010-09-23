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

(* $Id: gEdit.mli 1426 2008-10-07 18:41:02Z ben_99_9 $ *)

open Gtk
open GObj

(** Editable Widgets *)

(** {3 GtkEditable} *)

(** @gtkdoc gtk GtkEditable *)
class editable_signals : [> editable] obj ->
  object
    inherit GObj.widget_signals
    method changed : callback:(unit -> unit) -> GtkSignal.id
    method delete_text :
      callback:(start:int -> stop:int -> unit) -> GtkSignal.id
    method insert_text :
      callback:(string -> pos:int ref -> unit) -> GtkSignal.id
  end

(** Interface for text-editing widgets
   @gtkdoc gtk GtkEditable *)
class editable : ([> Gtk.editable] as 'a) obj ->
  object
    inherit ['a] GObj.widget_impl
    method copy_clipboard : unit -> unit
    method cut_clipboard : unit -> unit
    method delete_selection : unit -> unit
    method delete_text : start:int -> stop:int -> unit
    method editable : bool
    method get_chars : start:int -> stop:int -> string
    method insert_text : string -> pos:int -> int
    method paste_clipboard : unit -> unit
    method position : int
    method select_region : start:int -> stop:int -> unit
    method selection : (int * int) option
    method set_position : int -> unit
    method set_editable : bool -> unit
  end

(** {3 GtkEntry & GtkEntryCompletion} *)

(** @since GTK 2.4
    @gtkdoc gtk GtkEntryCompletion *)
class entry_completion_signals :
  [> `entrycompletion ] Gtk.obj ->
  object ('a)
    method after : 'a
    method action_activated : callback:(int -> unit) -> GtkSignal.id
    method match_selected :
      callback:(GTree.model_filter -> Gtk.tree_iter -> bool) -> GtkSignal.id
  end

(** @since GTK 2.4
    @gtkdoc gtk GtkEntryCompletion *)
class entry_completion :
  ([> `entrycompletion|`celllayout] as 'a) Gtk.obj ->
  object
    inherit GTree.cell_layout
    val obj : 'a Gtk.obj
    method as_entry_completion : Gtk.entry_completion
    method misc : GObj.gobject_ops
    method connect : entry_completion_signals

    method minimum_key_length : int
    method set_minimum_key_length : int -> unit
    method model : GTree.model
    method set_model : GTree.model -> unit

    method get_entry : GObj.widget option
    method complete : unit -> unit
    method insert_action_text : int -> string -> unit
    method insert_action_markup : int -> string -> unit
    method delete_action : int -> unit

    method set_match_func : (string -> Gtk.tree_iter -> bool) -> unit
    method set_text_column : string GTree.column -> unit
  end

(** @gtkdoc gtk GtkEntry *)
class entry_signals : [> Gtk.entry] obj ->
  object
    inherit editable_signals
    method activate : callback:(unit -> unit) -> GtkSignal.id
    method copy_clipboard : callback:(unit -> unit) -> GtkSignal.id
    method cut_clipboard : callback:(unit -> unit) -> GtkSignal.id
    method delete_from_cursor :
      callback:(Gtk.Tags.delete_type -> int -> unit) -> GtkSignal.id
    method insert_at_cursor : callback:(string -> unit) -> GtkSignal.id
    method move_cursor :
      callback:(Gtk.Tags.movement_step -> int -> extend:bool -> unit) ->
      GtkSignal.id
    method paste_clipboard : callback:(unit -> unit) -> GtkSignal.id
    method populate_popup : callback:(GMenu.menu -> unit) -> GtkSignal.id
    method toggle_overwrite : callback:(unit -> unit) -> GtkSignal.id
  end

(** A single line text entry field
   @gtkdoc gtk GtkEntry *)
class entry : ([> Gtk.entry] as 'a) obj ->
  object
    inherit editable
    inherit ['a] GObj.objvar
    method connect : entry_signals
    method event : event_ops
    method append_text : string -> unit
    method prepend_text : string -> unit
    method scroll_offset : int
    method text : string
    method text_length : int
    method set_activates_default : bool -> unit
    method set_editable : bool -> unit
    method set_has_frame : bool -> unit
    method set_invisible_char : int -> unit
    method set_max_length : int -> unit
    method set_text : string -> unit
    method set_visibility : bool -> unit
    method set_width_chars : int -> unit
    method set_xalign : float -> unit
    method activates_default : bool
    method editable : bool
    method has_frame : bool
    method invisible_char : int
    method max_length : int
    method visibility : bool
    method width_chars : int
    method xalign : float

    method set_completion : entry_completion -> unit (** @since GTK 2.4 *)
    method get_completion : entry_completion option  (** @since GTK 2.4 *)
  end

(** @gtkdoc gtk GtkEntry *)
val entry :
  ?text:string ->
  ?visibility:bool ->
  ?max_length:int ->
  ?activates_default:bool ->
  ?editable:bool ->
  ?has_frame:bool ->
  ?width_chars:int ->
  ?xalign:float ->
  ?width:int -> ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> entry

(** @since GTK 2.4
    @gtkdoc gtk GtkEntryCompletion *)
val entry_completion :
  ?model:#GTree.model ->
  ?minimum_key_length:int ->
  ?entry:entry -> unit -> entry_completion

(** {4 GtkSpinButton} *)

(** @gtkdoc gtk GtkSpinButton *)
class spin_button_signals : [> Gtk.spin_button] obj ->
  object
    inherit entry_signals
    method change_value :
      callback:(Gtk.Tags.scroll_type -> unit) -> GtkSignal.id
    method input : callback:(unit -> int) -> GtkSignal.id
    method output : callback:(unit -> bool) -> GtkSignal.id
    method value_changed : callback:(unit -> unit) -> GtkSignal.id

    method wrapped : callback:(unit -> unit) -> GtkSignal.id  
      (** @since GTK 2.10 *)
  end

(** Retrieve an integer or floating-point number from the user
   @gtkdoc gtk GtkSpinButton *)
class spin_button : Gtk.spin_button obj ->
  object
    inherit GObj.widget
    val obj : Gtk.spin_button obj
    method connect : spin_button_signals
    method event : GObj.event_ops
    method spin : Tags.spin_type -> unit
    method update : unit
    method value_as_int : int
    method set_adjustment : GData.adjustment -> unit
    method set_digits : int -> unit
    method set_numeric : bool -> unit
    method set_rate : float -> unit
    method set_snap_to_ticks : bool -> unit
    method set_update_policy : [`ALWAYS|`IF_VALID] -> unit
    method set_value : float -> unit
    method set_wrap : bool -> unit
    method adjustment : GData.adjustment
    method digits : int
    method numeric : bool
    method rate : float
    method snap_to_ticks : bool
    method update_policy : [`ALWAYS|`IF_VALID]
    method value : float
    method wrap : bool
  end

(** @gtkdoc gtk GtkSpinButton *)
val spin_button :
  ?adjustment:GData.adjustment ->
  ?rate:float ->
  ?digits:int ->
  ?numeric:bool ->
  ?snap_to_ticks:bool ->
  ?update_policy:[`ALWAYS|`IF_VALID] ->
  ?value:float ->
  ?wrap:bool ->
  ?width:int -> ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> spin_button

(** {3 GtkCombo} *)

(** A text entry field with a dropdown list
   @gtkdoc gtk GtkCombo *)
class combo : Gtk.combo obj ->
  object
    inherit GObj.widget
    val obj : Gtk.combo obj
    method disable_activate : unit -> unit
    method entry : entry
    method list : GList.liste
    method set_item_string : GList.list_item -> string -> unit
    method set_popdown_strings : string list -> unit
    method set_allow_empty : bool -> unit
    method set_case_sensitive : bool -> unit
    method set_enable_arrow_keys : bool -> unit
    method set_value_in_list : bool -> unit
    method allow_empty : bool
    method case_sensitive : bool
    method enable_arrow_keys : bool
    method value_in_list : bool
  end

(** @gtkdoc gtk GtkCombo *)
val combo :
  ?popdown_strings:string list ->
  ?allow_empty:bool ->
  ?case_sensitive:bool ->
  ?enable_arrow_keys:bool ->
  ?value_in_list:bool ->
  ?border_width:int -> ?width:int -> ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> combo

(** {3 GtkComboBox} *)

(** @since GTK 2.4 
    @gtkdoc gtk GtkComboBox *)
class combo_box_signals : [> Gtk.combo_box] Gtk.obj ->
  object
    inherit GContainer.container_signals
    method changed : callback:(unit -> unit) -> GtkSignal.id
  end

(** @since GTK 2.4
    @gtkdoc gtk GtkComboBox *)
class combo_box :
  ([> Gtk.combo_box] as 'a) Gtk.obj ->
  object
    inherit GContainer.bin
    inherit GTree.cell_layout
    val obj : 'a Gtk.obj
    method event : GObj.event_ops
    method active : int
    method active_iter : Gtk.tree_iter option
    method connect : combo_box_signals
    method model : GTree.model
    method set_active : int -> unit
    method set_active_iter : Gtk.tree_iter option -> unit				   
    method set_column_span_column : int GTree.column -> unit
    method set_model : GTree.model -> unit
    method set_row_span_column : int GTree.column -> unit
    method set_wrap_width : int -> unit
    method wrap_width : int
    method add_tearoffs : bool (** @since GTK 2.6 *)
    method set_add_tearoffs : bool -> unit (** @since GTK 2.6 *)
    method focus_on_click : bool (** @since GTK 2.6 *)
    method set_focus_on_click : bool -> unit (** @since GTK 2.6 *)
    method has_frame : bool (** @since GTK 2.6 *)
    method set_has_frame : bool -> unit (** @since GTK 2.6 *)
    method set_row_separator_func : (GTree.model -> Gtk.tree_iter -> bool) option -> unit (** @since GTK 2.6 *)
  end

(** @since GTK 2.4
    @gtkdoc gtk GtkComboBox *)
val combo_box :
  ?model:#GTree.model ->
  ?active:int ->
  ?add_tearoffs:bool ->
  ?focus_on_click:bool ->
  ?has_frame:bool ->
  ?wrap_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) ->
  ?show:bool ->
  unit -> combo_box

(** @since GTK 2.4
    @gtkdoc gtk GtkComboBoxEntry *)
class combo_box_entry : 
  ([> Gtk.combo_box_entry] as 'a) Gtk.obj ->
    object
      inherit combo_box
      val obj : 'a Gtk.obj
      method text_column : string GTree.column
      method set_text_column : string GTree.column -> unit
      method entry : entry
    end

(** @since GTK 2.4
    @gtkdoc gtk GtkComboBoxEntry *)
val combo_box_entry :
  ?model:#GTree.model ->
  ?text_column:string GTree.column ->
  ?active:int ->
  ?add_tearoffs:bool ->
  ?focus_on_click:bool ->
  ?has_frame:bool ->
  ?wrap_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) ->
  ?show:bool ->
  unit -> combo_box_entry

(** {4 Convenience API for text-only ComboBoxes} *)

type 'a text_combo = 'a * (GTree.list_store * string GTree.column)
  constraint 'a = #combo_box

val text_combo_add        : 'a text_combo -> string -> unit
val text_combo_get_active : 'a text_combo -> string option

(** A convenience function for creating simple {!GEdit.combo_box}. 
    Creates a simple {!GTree.list_store} with a single text column, 
    adds [strings] in it, creates a {!GTree.cell_renderer_text} and 
    connects it with the model.
    @since GTK 2.4
    @gtkdoc gtk GtkComboBox *)
val combo_box_text :
  ?strings:string list ->
  ?use_markup:bool ->
  ?active:int ->
  ?add_tearoffs:bool ->
  ?focus_on_click:bool ->
  ?has_frame:bool ->
  ?wrap_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) ->
  ?show:bool ->
  unit -> combo_box text_combo

(** A convenience function. See {!GEdit.combo_box_text}
    @since GTK 2.4
    @gtkdoc gtk GtkComboBoxEntry *)
val combo_box_entry_text :
  ?strings:string list ->
  ?active:int ->
  ?add_tearoffs:bool ->
  ?focus_on_click:bool ->
  ?has_frame:bool ->
  ?wrap_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(GObj.widget -> unit) ->
  ?show:bool ->
  unit -> combo_box_entry text_combo
