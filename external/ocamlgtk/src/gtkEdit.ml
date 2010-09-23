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

(* $Id: gtkEdit.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open StdLabels
open Gaux
open Gobject
open Gtk
open Tags
open GtkEditProps
open GtkBase

external _gtkedit_init : unit -> unit = "ml_gtkedit_init"
let () = _gtkedit_init ()

module Editable = struct
  include Editable
  let marshal_insert f argv =
    match List.tl (Closure.get_args argv) with
    | `STRING _ :: `INT len :: `POINTER(Some p) :: _ ->
        (* XXX These two accesses are implementation-dependent *)
        let s = Gpointer.peek_string (Closure.get_pointer argv ~pos:1) ~len
        and pos = ref (Gpointer.peek_int p) in
        (f s ~pos : unit); Gpointer.poke_int p !pos
    | _ -> invalid_arg "GtkEdit.Editable.marshal_insert"
  let () = Internal.marshal_insert := marshal_insert
end

module Entry = Entry

module SpinButton = struct
  include SpinButton
  let get_value_as_int w = truncate (floor (get P.value w +. 0.5))
end

(*
module Text = struct
  let cast w : text obj = Object.try_cast w "GtkText"
  external create : [>`adjustment] optobj -> [>`adjustment] optobj -> text obj
      = "ml_gtk_text_new"
  let create ?hadjustment ?vadjustment () =
    create (Gpointer.optboxed hadjustment) (Gpointer.optboxed vadjustment)
  external set_word_wrap : [>`text] obj -> bool -> unit
      = "ml_gtk_text_set_word_wrap"
  external set_line_wrap : [>`text] obj -> bool -> unit
      = "ml_gtk_text_set_line_wrap"
  external set_adjustment :
      [>`text] obj -> ?horizontal:[>`adjustment] obj ->
      ?vertical:[>`adjustment] obj -> unit -> unit
      = "ml_gtk_text_set_adjustments"
  external get_hadjustment : [>`text] obj -> adjustment obj
      = "ml_gtk_text_get_hadj"
  external get_vadjustment : [>`text] obj -> adjustment obj
      = "ml_gtk_text_get_vadj"
  external set_point : [>`text] obj -> int -> unit
      = "ml_gtk_text_set_point"
  external get_point : [>`text] obj -> int = "ml_gtk_text_get_point"
  external get_length : [>`text] obj -> int = "ml_gtk_text_get_length"
  external freeze : [>`text] obj -> unit = "ml_gtk_text_freeze"
  external thaw : [>`text] obj -> unit = "ml_gtk_text_thaw"
  external insert :
      [>`text] obj -> ?font:Gdk.font -> ?foreground:Gdk.Color.t ->
      ?background:Gdk.Color.t -> string -> unit
      = "ml_gtk_text_insert"
  let set ?hadjustment ?vadjustment ?word_wrap w =
    if hadjustment <> None || vadjustment <> None then
      set_adjustment w ?horizontal: hadjustment ?vertical: vadjustment ();
    may word_wrap ~f:(set_word_wrap w)
end
*)

module Combo = struct
  include Combo
  external entry : [>`combo] obj -> entry obj = "ml_gtk_combo_entry"
  external list : [>`combo] obj -> liste obj = "ml_gtk_combo_list"
  let set_popdown_strings combo strings =
    GtkList.Liste.clear_items (list combo) ~start:0 ~stop:(-1);
    List.iter strings ~f:
      begin fun s ->
	let li = GtkList.ListItem.create_with_label s in
	Widget.show li;
	Container.add (list combo) li
      end
  external disable_activate : [>`combo] obj -> unit
      = "ml_gtk_combo_disable_activate"
  external set_item_string : [>`combo] obj -> [>`item] obj -> string -> unit
      = "ml_gtk_combo_set_item_string"
end

module ComboBox = GtkEditProps.ComboBox

module ComboBoxEntry = GtkEditProps.ComboBoxEntry

module EntryCompletion = GtkEditProps.EntryCompletion
