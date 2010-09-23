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

(* $Id: gData.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gaux
open Gobject
open Gtk
open GtkBase
open GtkData
open GObj

class adjustment_signals obj = object (self)
  inherit gtkobj_signals_impl obj
  method changed = self#connect Adjustment.S.changed
  method value_changed = self#connect Adjustment.S.value_changed
end

class adjustment obj = object
  inherit gtkobj obj
  method as_adjustment : Gtk.adjustment obj = obj
  method connect = new adjustment_signals obj
  method set_value = Adjustment.set_value obj
  method clamp_page = Adjustment.clamp_page obj
  method lower = Adjustment.get_lower obj
  method upper = Adjustment.get_upper obj
  method value = Adjustment.get_value obj
  method step_increment = Adjustment.get_step_increment obj
  method page_increment = Adjustment.get_page_increment obj
  method page_size = Adjustment.get_page_size obj
  method set_bounds = Adjustment.set_bounds obj
end

let adjustment ?(value=0.) ?(lower=0.) ?(upper=100.)
    ?(step_incr=1.) ?(page_incr=10.) ?(page_size=10.) () =
  let w =
    Adjustment.create ~value ~lower ~upper ~step_incr ~page_incr ~page_size in
  new adjustment w

let as_adjustment (adj : adjustment) = adj#as_adjustment

let wrap_adjustment w = new adjustment (unsafe_cast w)
let unwrap_adjustment w = unsafe_cast w#as_adjustment
let conv_adjustment_option =
  { kind = `OBJECT;
    proj = (function `OBJECT c -> may_map ~f:wrap_adjustment c
           | _ -> failwith "GObj.get_object");
    inj = (fun c -> `OBJECT (may_map ~f:unwrap_adjustment c)) }
let conv_adjustment =
  { kind = `OBJECT;
    proj = (function `OBJECT (Some c) -> wrap_adjustment c
           | `OBJECT None -> raise Gpointer.Null
           | _ -> failwith "GObj.get_object");
    inj = (fun c -> `OBJECT (Some (unwrap_adjustment c))) }

class tooltips obj = object
  inherit gtkobj (obj : Gtk.tooltips obj)
  method as_tooltips = obj
  method connect = new gtkobj_signals_impl obj
  method enable () = Tooltips.enable obj
  method disable () = Tooltips.disable obj
  method set_tip ?text ?privat w =
    Tooltips.set_tip obj (as_widget w) ?text ?privat
  method set_delay = Tooltips.set_delay obj
end

let tooltips ?delay () =
  let tt = Tooltips.create () in
  may delay ~f:(Tooltips.set_delay tt);
  new tooltips tt

class clipboard_skel clip = object (self)
  method as_clipboard = Lazy.force clip
  method clear () = self#call_clear; Clipboard.clear self#as_clipboard
  method set_text = self#call_clear; Clipboard.set_text self#as_clipboard
  method text = Clipboard.wait_for_text self#as_clipboard
  method set_image = self#call_clear; Clipboard.set_image self#as_clipboard
  method image = Clipboard.wait_for_image self#as_clipboard
  method targets = Clipboard.wait_for_targets self#as_clipboard
  method get_contents ~target =
    new GObj.selection_data
      (Clipboard.wait_for_contents self#as_clipboard ~target)
  method private call_clear = ()
end

(* Additions by SooHyoung Oh *)

let default_get_cb context ~info ~time  = ()

class clipboard ~selection = object (self)
  inherit clipboard_skel (lazy (GtkBase.Clipboard.get selection))
  val mutable widget = None
  val mutable get_cb = default_get_cb
  val mutable clear_cb = None

  method private call_get context ~info ~time =
    get_cb context ~info ~time
  method private call_clear =
    match clear_cb with
      None -> ()
    | Some cb ->
        get_cb <- default_get_cb; clear_cb <- None; cb ()

  method private init_widget =
    match widget with Some w -> w
    | None ->
        let w = new GObj.widget (GtkBin.Invisible.create []) in
        widget <- Some w;
        ignore (w#misc#connect#selection_get ~callback:self#call_get);
        ignore ((new GObj.event_signals w#as_widget)#selection_clear
                  ~callback:(fun _ -> self#call_clear; true));
        w

  method set_contents ~targets ~get:get_func ~clear:clear_func =
    let widget = self#init_widget in
    self#call_clear;
    get_cb <- get_func;
    clear_cb <- Some clear_func;
    widget#misc#grab_selection selection;
    widget#misc#clear_selection_targets selection;
    List.iter
      (fun target -> widget#misc#add_selection_target ~target selection)
      targets
end

let clipboard selection = new clipboard ~selection

let as_clipboard clip = clip#as_clipboard
