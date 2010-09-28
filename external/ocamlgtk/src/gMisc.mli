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

(* $Id: gMisc.mli 1527 2010-09-09 08:02:22Z garrigue $ *)

open Gtk
open GObj
open GContainer

(** Miscellaneous widgets *)

(** @gtkdoc gtk GtkSeparator
    @gtkdoc gtk GtkHSeparator
    @gtkdoc gtk GtkVSeparator *)
val separator :
  Tags.orientation ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> widget_full

(** {3 Statusbar} *)

class statusbar_context :
  Gtk.statusbar obj -> Gtk.statusbar_context ->
  object
    val context : Gtk.statusbar_context
    val obj : Gtk.statusbar obj
    method context : Gtk.statusbar_context
    method flash : ?delay:int -> string -> unit
    (** @param delay default value is [1000] ms *)
    method pop : unit -> unit
    method push : string -> statusbar_message
    method remove : statusbar_message -> unit
  end

(** Report messages of minor importance to the user
    @gtkdoc gtk GtkStatusbar *)
class statusbar : Gtk.statusbar obj ->
  object
    inherit GPack.box
    val obj : Gtk.statusbar obj
    method new_context : name:string -> statusbar_context
    method has_resize_grip : bool
    method set_has_resize_grip : bool -> unit
  end

(** @gtkdoc gtk GtkStatusbar *)
val statusbar :
  ?has_resize_grip:bool ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> statusbar

(** {3 Status icon} *)

(** @gtkdoc gtk GtkStatusIcon *)
class status_icon_signals : Gtk.status_icon Gobject.obj ->
  object
    method activate : callback:(unit -> unit) -> GtkSignal.id
    method popup_menu : callback:(int -> int -> unit) -> GtkSignal.id
    method size_changed : callback:(int -> unit) -> GtkSignal.id
  end

(** Display an icon in the system tray.
  @gtkdoc gtk GtkStatusIcon *)
class status_icon : Gtk.gtk_status_icon ->
  object
    val obj : Gtk.status_icon Gobject.obj
    method connect : status_icon_signals
    method blinking : bool
    method get_icon_name : string
    method get_pixbuf : GdkPixbuf.pixbuf
    method screen : Gdk.screen
    method get_size : int
    method get_stock : string
    method visible : bool
    method is_embedded : bool
    method set_blinking : bool -> unit
    method set_from_file : string -> unit
    method set_from_icon_name : string -> unit
    method set_from_pixbuf : GdkPixbuf.pixbuf -> unit
    method set_from_stock : string -> unit
    method set_screen : Gdk.screen -> unit
    method set_tooltip : string -> unit
    method set_visible : bool -> unit
  end

val status_icon :
  ?screen:Gdk.screen -> ?visible:bool -> ?blinking:bool -> unit -> status_icon
val status_icon_from_pixbuf :
  ?screen:Gdk.screen -> ?visible:bool -> ?blinking:bool -> GdkPixbuf.pixbuf -> status_icon
val status_icon_from_file :
  ?screen:Gdk.screen -> ?visible:bool -> ?blinking:bool -> string -> status_icon
val status_icon_from_stock :
  ?screen:Gdk.screen -> ?visible:bool -> ?blinking:bool -> string -> status_icon
val status_icon_from_icon_name :
  ?screen:Gdk.screen -> ?visible:bool -> ?blinking:bool -> string -> status_icon


(** {3 Calendar} *)

(** @gtkdoc gtk GtkCalendar *)
class calendar_signals : 'a obj ->
  object
    inherit GObj.widget_signals
    constraint 'a = [> calendar]
    val obj : 'a obj
    method day_selected : callback:(unit -> unit) -> GtkSignal.id
    method day_selected_double_click :
      callback:(unit -> unit) -> GtkSignal.id
    method month_changed : callback:(unit -> unit) -> GtkSignal.id
    method next_month : callback:(unit -> unit) -> GtkSignal.id
    method next_year : callback:(unit -> unit) -> GtkSignal.id
    method prev_month : callback:(unit -> unit) -> GtkSignal.id
    method prev_year : callback:(unit -> unit) -> GtkSignal.id
  end

(** Display a calendar and/or allow the user to select a date
    @gtkdoc gtk GtkCalendar *)
class calendar : Gtk.calendar obj ->
  object
    inherit GObj.widget
    val obj : Gtk.calendar obj
    method day : int
    method month : int
    method year : int
    method set_day : int -> unit
    method set_month : int -> unit
    method set_year : int -> unit
    method event : event_ops
    method clear_marks : unit
    method connect : calendar_signals
    method date : int * int * int
    method display_options : Tags.calendar_display_options list -> unit
    method freeze : unit -> unit
    method mark_day : int -> unit
    method select_day : int -> unit
    method select_month : month:int -> year:int -> unit
    method thaw : unit -> unit
    method unmark_day : int -> unit
    method is_day_marked : int -> bool
    method num_marked_dates : int
  end

(** @gtkdoc gtk GtkCalendar *)
val calendar :
  ?options:Tags.calendar_display_options list ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> calendar

(** {3 Drawing Area} *)

(** A widget for custom user interface elements
    @gtkdoc gtk GtkDrawingArea *)
class drawing_area : ([> Gtk.drawing_area] as 'a) obj ->
  object
    inherit GObj.widget_full
    val obj : 'a obj
    method event : event_ops
    method set_size : width:int -> height:int -> unit
  end

(** @gtkdoc gtk GtkDrawingArea *)
val drawing_area :
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> drawing_area

(** {3 Curve} *)

(** Allows direct editing of a curve
    @gtkdoc gtk GtkCurve *)
class curve : Gtk.curve obj ->
  object
    inherit drawing_area
    val obj : Gtk.curve obj
    method reset : unit -> unit
    method set_gamma : int -> unit
    method set_vector : float array -> unit
    method get_vector : int -> float array
    method curve_type : GtkEnums.curve_type
    method max_x : float
    method max_y : float
    method min_x : float
    method min_y : float
    method set_curve_type : GtkEnums.curve_type -> unit
    method set_max_x : float -> unit
    method set_max_y : float -> unit
    method set_min_x : float -> unit
    method set_min_y : float -> unit
  end

(** @gtkdoc gtk GtkCurve *)
val curve :
  ?width:int -> ?height:int ->
  ?curve_type:GtkEnums.curve_type ->
  ?max_x:float -> ?max_y:float ->
  ?min_x:float -> ?min_y:float ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> curve


(** {3 Misc. Widgets} *)

(** A base class for widgets with alignments and padding
    @gtkdoc gtk GtkMisc *)
class misc : ([> Gtk.misc] as 'a) obj ->
  object
    inherit GObj.widget
    val obj : 'a obj
    method set_xalign : float -> unit
    method set_yalign : float -> unit
    method set_xpad : int -> unit
    method set_ypad : int -> unit
    method xalign : float
    method yalign : float
    method xpad : int
    method ypad : int
  end

(** Produces an arrow pointing in one of the four cardinal directions
    @gtkdoc gtk GtkArrow *)
class arrow : ([> Gtk.arrow] as 'a) obj ->
  object
    inherit misc
    val obj : 'a obj
    method set_kind : Tags.arrow_type -> unit
    method set_shadow : Tags.shadow_type -> unit
    method kind : Tags.arrow_type
    method shadow : Tags.shadow_type
  end

(** @gtkdoc gtk GtkArrow
    @param kind default value is [`RIGHT]
    @param shadow default value is [`OUT] *)
val arrow :
  ?kind:Tags.arrow_type ->
  ?shadow:Tags.shadow_type ->
  ?xalign:float ->
  ?yalign:float ->
  ?xpad:int ->
  ?ypad:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> arrow

type image_type =
  [ `EMPTY | `PIXMAP | `IMAGE | `PIXBUF | `STOCK | `ICON_SET | `ANIMATION ]

(** A widget displaying an image
    @gtkdoc gtk GtkImage *)
class image : 'a obj ->
  object
    inherit misc
    constraint 'a = [> Gtk.image]
    val obj : 'a obj
    method clear : unit -> unit (** since Gtk 2.8 *)
    method storage_type : image_type
    method set_image : Gdk.image -> unit
    method set_pixmap : GDraw.pixmap -> unit
    method set_mask : Gdk.bitmap option -> unit
    method set_file : string -> unit
    method set_pixbuf : GdkPixbuf.pixbuf -> unit
    method set_stock : GtkStock.id -> unit
    method set_icon_set : icon_set -> unit
    method set_icon_size : Tags.icon_size -> unit
    method set_pixel_size : int -> unit
    method image : Gdk.image
    method pixmap : GDraw.pixmap
    method mask : Gdk.bitmap option
    method pixbuf : GdkPixbuf.pixbuf
    method pixel_size : int
    method stock : GtkStock.id
    method icon_set : icon_set
    method icon_size : Tags.icon_size
  end

(** @gtkdoc gtk GtkImage *)
val image :
  ?file:string ->
  ?image:Gdk.image ->
  ?pixbuf:GdkPixbuf.pixbuf ->
  ?pixel_size:int ->
  ?pixmap:Gdk.pixmap ->
  ?mask:Gdk.bitmap ->
  ?stock:GtkStock.id ->
  ?icon_set:icon_set ->
  ?icon_size:Tags.icon_size ->
  ?xalign:float ->
  ?yalign:float ->
  ?xpad:int ->
  ?ypad:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> image

(* Use an image as a pixmap... *)
val pixmap :
  #GDraw.pixmap ->
  ?xalign:float ->
  ?yalign:float ->
  ?xpad:int ->
  ?ypad:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> image

(** {4 Labels} *)

(** @gtkdoc gtk GtkLabel *)
class label_skel : 'a obj ->
  object
    inherit misc
    constraint 'a = [> Gtk.label]
    val obj : 'a obj
    method cursor_position : int
    method selection_bound : int
    method selection_bounds : (int * int) option
    method select_region : int -> int -> unit
    method set_justify : Tags.justification -> unit
    method set_label : string -> unit
    method set_line_wrap : bool -> unit
    method set_mnemonic_widget : widget option -> unit
    method set_pattern : string -> unit
    method set_selectable : bool -> unit
    method set_text : string -> unit
    method set_use_markup : bool -> unit
    method set_use_underline : bool -> unit
    method justify : Tags.justification
    method label : string
    method line_wrap : bool
    method mnemonic_keyval : int
    method mnemonic_widget : widget option
    method selectable : bool
    method text : string
    method use_markup : bool
    method use_underline : bool

    method angle : float (** @since GTK 2.6 *)
    method set_angle : float -> unit (** @since GTK 2.6 *)
    method max_width_chars : int (** @since GTK 2.6 *)
    method set_max_width_chars : int -> unit (** @since GTK 2.6 *)
    method single_line_mode : bool (** @since GTK 2.6 *)
    method set_single_line_mode : bool -> unit (** @since GTK 2.6 *)
    method width_chars : int (** @since GTK 2.6 *)
    method set_width_chars : int -> unit (** @since GTK 2.6 *)
    method ellipsize : PangoEnums.ellipsize_mode (** @since GTK 2.6 *)
    method set_ellipsize : PangoEnums.ellipsize_mode -> unit (** @since GTK 2.6 *)
  end

(** A widget that displays a small to medium amount of text
   @gtkdoc gtk GtkLabel *)
class label : Gtk.label obj ->
  object
    inherit label_skel
    val obj : Gtk.label obj
    method connect : widget_signals
  end

(** @gtkdoc gtk GtkLabel
    @param markup overrides [text] if both are present
    @param use_underline default value is [false]
    @param justify default value is [`LEFT]
    @param selectable default value is [false]
    @param line_wrap default values is [false] *)
val label :
  ?text:string ->
  ?markup:string ->     (* overrides ~text if present *)
  ?use_underline:bool ->
  ?mnemonic_widget:#widget ->
  ?justify:Tags.justification ->
  ?line_wrap:bool ->
  ?pattern:string ->
  ?selectable:bool ->
  ?ellipsize:PangoEnums.ellipsize_mode ->
  ?xalign:float ->
  ?yalign:float ->
  ?xpad:int ->
  ?ypad:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> label
val label_cast : < as_widget : 'a obj ; .. > -> label

(** {4 Tips query} *)

(** @gtkdoc gtk GtkTipsQuery
    @deprecated . *)
class tips_query_signals : Gtk.tips_query obj ->
  object
    inherit GObj.widget_signals
    method start_query : callback:(unit -> unit) -> GtkSignal.id
    method stop_query : callback:(unit -> unit) -> GtkSignal.id
    method widget_entered :
      callback:(widget option -> text:string -> privat:string -> unit) ->
      GtkSignal.id
    method widget_selected :
      callback:(widget option -> text:string -> privat:string ->
                GdkEvent.Button.t -> bool) ->
      GtkSignal.id
  end

(** Displays help about widgets in the user interface
    @gtkdoc gtk GtkTipsQuery
    @deprecated . *)
class tips_query : Gtk.tips_query obj ->
  object
    inherit label_skel
    val obj : Gtk.tips_query obj
    method connect : tips_query_signals
    method start : unit -> unit
    method stop : unit -> unit
    method set_caller : widget option -> unit
    method set_emit_always : bool -> unit
    method set_label_inactive : string -> unit
    method set_label_no_tip : string -> unit
    method caller : widget option
    method emit_always : bool
    method label_inactive : string
    method label_no_tip : string
  end

(** @gtkdoc gtk GtkTipsQuery
    @deprecated . *)
val tips_query :
  ?caller:#widget ->
  ?emit_always:bool ->
  ?label_inactive:string ->
  ?label_no_tip:string ->
  ?xalign:float ->
  ?yalign:float ->
  ?xpad:int ->
  ?ypad:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> tips_query

(** {3 Color and font selection} *)

(** A widget used to select a color
    @gtkdoc gtk GtkColorSelection *)
class color_selection : Gtk.color_selection obj ->
  object
    inherit GObj.widget_full
    val obj : Gtk.color_selection obj
    method alpha : int
    method color : Gdk.color
    method set_alpha : int -> unit
    method set_border_width : int -> unit
    method set_color : Gdk.color -> unit
    method set_has_opacity_control : bool -> unit
    method set_has_palette : bool -> unit
    method has_opacity_control : bool
    method has_palette : bool
  end

(** @gtkdoc gtk GtkColorSelection *)
val color_selection :
  ?alpha:int ->
  ?color:Gdk.color ->
  ?has_opacity_control:bool ->
  ?has_palette:bool ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> color_selection

(** A widget for selecting fonts.
    @gtkdoc gtk GtkFontSelection *)
class font_selection : Gtk.font_selection obj ->
  object
    inherit GObj.widget_full
    val obj : Gtk.font_selection obj
    method event : event_ops
    method font_name : string
    method preview_text : string
    method set_border_width : int -> unit
    method set_font_name : string -> unit
    method set_preview_text : string -> unit
  end

(** @gtkdoc gtk GtkFontSelection *)
val font_selection :
  ?font_name:string ->
  ?preview_text:string ->
  ?border_width:int ->
  ?width:int ->
  ?height:int ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> font_selection
