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

(* $Id: gPack.mli 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gtk
open GObj
open GContainer

(** Several container widgets *)

(** {3 Boxes} *)

(** @gtkdoc gtk GtkBox *)
class box_skel : ([> box] as 'a) obj ->
  object
    inherit GContainer.container
    val obj : 'a obj
    method pack :
      ?from:Tags.pack_type ->
      ?expand:bool -> ?fill:bool -> ?padding:int -> widget -> unit
   (** @param from default value is [`START]
       @param expand default vaue is [false]
       @param fill default value is [true], ignored if [expand] is [false] *)
    method reorder_child : widget -> pos:int -> unit
    method set_child_packing :
      ?from:Tags.pack_type ->
      ?expand:bool -> ?fill:bool -> ?padding:int -> widget -> unit
    method set_homogeneous : bool -> unit
    method homogeneous : bool
    method set_spacing : int -> unit
    method spacing : int
  end

(** A base class for box containers
   @gtkdoc gtk GtkBox *)
class box : ([> Gtk.box] as 'a) obj ->
  object
    inherit box_skel
    val obj : 'a obj
    method connect : GContainer.container_signals
  end

(** @gtkdoc gtk GtkBox 
    @param homogeneous default value is [false] *)
val box :
  Tags.orientation ->
  ?homogeneous:bool ->
  ?spacing:int ->
  ?border_width:int ->
  ?width:int ->
  ?height:int -> ?packing:(widget -> unit) -> ?show:bool -> unit -> box

(** @gtkdoc gtk GtkVBox
    @param homogeneous default value is [false] *)
val vbox :
  ?homogeneous:bool ->
  ?spacing:int ->
  ?border_width:int ->
  ?width:int ->
  ?height:int -> ?packing:(widget -> unit) -> ?show:bool -> unit -> box

(** @gtkdoc gtk GtkHVBox
    @param homogeneous default value is [false] *)
val hbox :
  ?homogeneous:bool ->
  ?spacing:int ->
  ?border_width:int ->
  ?width:int ->
  ?height:int -> ?packing:(widget -> unit) -> ?show:bool -> unit -> box

(** @gtkdoc gtk GtkButtonBox *)
class button_box : ([> Gtk.button_box] as 'a) obj ->
  object
    inherit box
    val obj : 'a obj
    method set_child_ipadding : ?x:int -> ?y:int -> unit -> unit (** @deprecated . *)
    method set_child_size : ?width:int -> ?height:int -> unit -> unit (** @deprecated . *)
    method set_layout : Gtk.Tags.button_box_style -> unit
    method layout : Gtk.Tags.button_box_style
    method get_child_secondary : widget -> bool (** @since GTK 2.4 *)
    method set_child_secondary : widget -> bool -> unit (** @since GTK 2.4 *)
  end

(** @gtkdoc gtk GtkButtonBox *)
val button_box :
  Tags.orientation ->
  ?spacing:int ->
  ?child_width:int ->
  ?child_height:int ->
  ?child_ipadx:int ->
  ?child_ipady:int ->
  ?layout:GtkPack.BBox.bbox_style ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> button_box

(** {3 GtkTable} *)

(** Pack widgets in regular patterns
   @gtkdoc gtk GtkTable *)
class table :
  Gtk.table obj ->
  object
    inherit GContainer.container_full
    val obj : Gtk.table obj
    method attach :
      left:int ->
      top:int ->
      ?right:int ->
      ?bottom:int ->
      ?expand:Tags.expand_type ->
      ?fill:Tags.expand_type ->
      ?shrink:Tags.expand_type ->
      ?xpadding:int -> ?ypadding:int -> widget -> unit
    (** @param left column number to attach the left side of the widget to
        @param top  row number to attach the top of the widget to
        @param right default value is [left+1]
        @param bottom default value is [top+1]
        @param expand default value is [`NONE]
        @param fill default value is [`BOTH]
        @param shrink default value is [`NONE] *)
    method col_spacings : int
    method columns : int
    method homogeneous : bool
    method row_spacings : int
    method rows : int
    method set_col_spacing : int -> int -> unit
    method set_col_spacings : int -> unit
    method set_columns : int -> unit
    method set_homogeneous : bool -> unit
    method set_row_spacing : int -> int -> unit
    method set_row_spacings : int -> unit
    method set_rows : int -> unit
  end

(** @gtkdoc gtk GtkTable *)
val table :
  ?columns:int ->
  ?rows:int ->
  ?homogeneous:bool ->
  ?row_spacings:int ->
  ?col_spacings:int ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> table

(** {3 GtkFixed} *)

(** A container which allows you to position widgets at fixed coordinates
   @gtkdoc gtk GtkFixed *)
class fixed :
  Gtk.fixed obj ->
  object
    inherit GContainer.container_full
    val obj : Gtk.fixed obj
    method event : event_ops
    method move : widget -> x:int -> y:int -> unit
    method put : widget -> x:int -> y:int -> unit
    method set_has_window : bool -> unit
    method has_window : bool
  end

(** @gtkdoc gtk GtkFixed *)
val fixed :
  ?has_window: bool ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> fixed

(** {3 GtkLayout} *)

(** Infinite scrollable area containing child widgets and/or custom drawing
   @gtkdoc gtk GtkLayout *)
class layout :
  'a obj ->
  object
    inherit GContainer.container_full
    constraint 'a = [> Gtk.layout]
    val obj : 'a obj
    method event : event_ops
    method bin_window : Gdk.window
    method freeze : unit -> unit
    method hadjustment : GData.adjustment
    method height : int
    method move : widget -> x:int -> y:int -> unit
    method put : widget -> x:int -> y:int -> unit
    method set_hadjustment : GData.adjustment -> unit
    method set_height : int -> unit
    method set_vadjustment : GData.adjustment -> unit
    method set_width : int -> unit
    method thaw : unit -> unit
    method vadjustment : GData.adjustment
    method width : int
  end

(** @gtkdoc gtk GtkLayout *)
val layout :
  ?hadjustment:GData.adjustment ->
  ?vadjustment:GData.adjustment ->
  ?layout_width:int ->
  ?layout_height:int ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> layout

(** {3 GtkNotebook} *)

(** @gtkdoc gtk GtkNotebook *)
class notebook_signals : [> Gtk.notebook] obj ->
  object
    inherit GContainer.container_signals
    method switch_page : callback:(int -> unit) -> GtkSignal.id
  end

(** A tabbed notebook container
   @gtkdoc gtk GtkNotebook *)
class notebook : Gtk.notebook obj ->
  object
    inherit GContainer.container
    val obj : Gtk.notebook obj
    method event : event_ops
    method append_page :
      ?tab_label:widget -> ?menu_label:widget -> widget -> int
    method connect : notebook_signals
    method current_page : int
    method get_menu_label : widget -> widget
    method get_nth_page : int -> widget
    method get_tab_label : widget -> widget
    method goto_page : int -> unit
    method insert_page :
      ?tab_label:widget -> ?menu_label:widget -> ?pos:int -> widget -> int
    method next_page : unit -> unit
    method page_num : widget -> int
    method prepend_page :
      ?tab_label:widget -> ?menu_label:widget -> widget -> int
    method previous_page : unit -> unit
    method remove_page : int -> unit
    method set_enable_popup : bool -> unit
    method set_homogeneous_tabs : bool -> unit
    method set_page :
      ?tab_label:widget -> ?menu_label:widget -> widget -> unit
    method set_scrollable : bool -> unit
    method set_show_border : bool -> unit
    method set_show_tabs : bool -> unit
    method set_tab_border : int -> unit
    method set_tab_hborder : int -> unit
    method set_tab_vborder : int -> unit
    method set_tab_pos : Tags.position -> unit
    method enable_popup : bool
    method homogeneous_tabs : bool
    method scrollable : bool
    method show_border : bool
    method show_tabs : bool
    method tab_hborder : int
    method tab_pos : GtkEnums.position_type
    method tab_vborder : int
  end

(** @gtkdoc gtk GtkNotebook *)
val notebook :
  ?enable_popup:bool ->
  ?homogeneous_tabs:bool ->
  ?scrollable:bool ->
  ?show_border:bool ->
  ?show_tabs:bool ->
  ?tab_border:int ->
  ?tab_pos:Tags.position ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> notebook

(** {3 GtkPaned} *)

(** Base class for widgets with two adjustable panes
   @gtkdoc gtk GtkPaned *)
class paned :
  Gtk.paned obj ->
  object
    inherit GContainer.container_full
    val obj : Gtk.paned obj
    method event : event_ops
    method add1 : widget -> unit
    method add2 : widget -> unit
    method pack1 : ?resize:bool -> ?shrink:bool -> widget -> unit
    (** @param resize default value is [false]
        @param shrink default value is [false] *)
    method pack2 : ?resize:bool -> ?shrink:bool -> widget -> unit
    (** @param resize default value is [false]
        @param shrink default value is [false] *)
    method child1 : widget
    method child2 : widget
    method set_position : int -> unit
    method position : int
    method max_position : int (** @since GTK 2.4 *)
    method min_position : int (** @since GTK 2.4 *)
  end

(** @gtkdoc gtk GtkPaned *)
val paned :
  Tags.orientation ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> paned
