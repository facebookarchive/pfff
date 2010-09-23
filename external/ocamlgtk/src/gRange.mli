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

(* $Id: gRange.mli 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gtk
open GObj

(** Scrollbars, progressbars, etc. *)

(** {3 GtkProgressBar} *)

(** A widget which indicates progress visually
   @gtkdoc gtk GtkProgress
   @gtkdoc gtk GtkProgressBar *)
class progress_bar : Gtk.progress_bar obj ->
  object
    inherit GObj.widget_full
    val obj : Gtk.progress_bar Gtk.obj
    method adjustment : GData.adjustment
    method event : GObj.event_ops
    method pulse : unit -> unit
    method set_adjustment : GData.adjustment -> unit
    method set_fraction : float -> unit
    method set_orientation : Tags.progress_bar_orientation -> unit
    method set_pulse_step : float -> unit
    method set_text : string -> unit
    method fraction : float
    method orientation : Tags.progress_bar_orientation
    method pulse_step : float
    method text : string
    method ellipsize : PangoEnums.ellipsize_mode (** @since GTK 2.6 *)
    method set_ellipsize : PangoEnums.ellipsize_mode -> unit (** @since GTK 2.6 *)
  end

(** @gtkdoc gtk GtkProgress
    @gtkdoc gtk GtkProgressBar
    @param orientation default value is [`LEFT_TO_RIGHT]
    @param pulse_step default value is [0.1] *)
val progress_bar :
  ?orientation:Tags.progress_bar_orientation ->
  ?pulse_step:float ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> progress_bar

(** {3 GtkRange} *)

(** @gtkdoc gtk GtkRange *)
class range_signals : [> Gtk.range] obj ->
  object
    inherit GObj.widget_signals
    method adjust_bounds : callback:(float -> unit) -> GtkSignal.id
    method move_slider : callback:(Tags.scroll_type -> unit) -> GtkSignal.id
    method value_changed : callback:(unit -> unit) -> GtkSignal.id
  end

(** Base class for widgets which visualize an adjustment
   @gtkdoc gtk GtkRange *)
class range : ([> Gtk.range] as 'a) obj ->
  object
    inherit GObj.widget
    val obj : 'a obj
    method connect : range_signals
    method event : GObj.event_ops
    method set_adjustment : GData.adjustment -> unit
    method set_inverted : bool -> unit
    method set_update_policy : Tags.update_type -> unit
    method adjustment : GData.adjustment
    method inverted : bool
    method update_policy : Tags.update_type
  end

(** A slider widget for selecting a value from a range
   @gtkdoc gtk GtkScale
   @gtkdoc gtk GtkHScale
   @gtkdoc gtk GtkVScale *)
class scale : Gtk.scale obj ->
  object
    inherit range
    val obj : Gtk.scale obj
    method set_digits : int -> unit
    method set_draw_value : bool -> unit
    method set_value_pos : Tags.position -> unit
    method digits : int
    method draw_value : bool
    method value_pos : Tags.position
  end

(** @gtkdoc gtk GtkScale
    @gtkdoc gtk GtkHScale
    @gtkdoc gtk GtkVScale 
    @param digits default value is [1]
    @param draw_value default value is [false]
    @param value_pos default value is [`LEFT]
    @param inverted default value is [false]
    @param update_policy default value is [`CONTINUOUS] *)
val scale :
  Tags.orientation ->
  ?adjustment:GData.adjustment ->
  ?digits:int ->
  ?draw_value:bool ->
  ?value_pos:Tags.position ->
  ?inverted:bool ->
  ?update_policy:Tags.update_type ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> scale

(** @gtkdoc gtk GtkScrollbar
    @gtkdoc gtk GtkHScrollbar
    @gtkdoc gtk GtkVScrollbar
    @param inverted default value is [false]
    @param update_policy default value is [`CONTINUOUS] *)
val scrollbar :
  Tags.orientation ->
  ?adjustment:GData.adjustment ->
  ?inverted:bool ->
  ?update_policy:Tags.update_type ->
  ?packing:(widget -> unit) -> ?show:bool -> unit -> range

(** {3 GtkRuler} *)

(** @gtkdoc gtk GtkRuler
    @gtkdoc gtk GtkHRuler
    @gtkdoc gtk GtkVRuler *)
class ruler :
  ([> Gtk.ruler] as 'a) Gtk.obj ->
  object
    inherit GObj.widget_full
    val obj : 'a Gtk.obj
    method event : GObj.event_ops
    method set_metric : Tags.metric_type -> unit
    method set_lower : float -> unit
    method set_max_size : float -> unit
    method set_metric : Gtk.Tags.metric_type -> unit
    method set_position : float -> unit
    method set_upper : float -> unit
    method lower : float
    method max_size : float
    method position : float
    method upper : float
  end

(** @gtkdoc gtk GtkRuler
    @gtkdoc gtk GtkHRuler
    @gtkdoc gtk GtkVRuler
    @param metric default value is [`PIXELS] *)
val ruler :
  Tags.orientation ->
  ?metric:Tags.metric_type ->
  ?lower:float ->
  ?upper:float ->
  ?max_size:float ->
  ?position:float ->
  ?packing:(GObj.widget -> unit) -> ?show:bool -> unit -> ruler
