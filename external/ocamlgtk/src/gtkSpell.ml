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

type error = BACKEND
exception Error of error * string
external _init : unit -> unit = "ml_gtkspell_init"
let () =  _init () ; Callback.register_exception "gtkspell_error" (Error (BACKEND, ""))

(* unsafe! lifecycle of the GtkSpell struct is tied to the GtkTextView *)
type t

external _new_attach : Gtk.text_view Gtk.obj -> string option -> unit = "ml_gtkspell_new_attach"
external _is_attached : Gtk.text_view Gtk.obj -> bool = "ml_gtkspell_is_attached"
external _get_from_text_view : Gtk.text_view Gtk.obj -> t option = "ml_gtkspell_get_from_text_view"
external _detach : t -> unit = "ml_gtkspell_detach"
external _set_language : t -> string option -> unit = "ml_gtkspell_set_language"
external _recheck_all : t -> unit = "ml_gtkspell_recheck_all"

let attach ?lang view = _new_attach view#as_view lang

let is_attached view = _is_attached view#as_view

let detach view = 
  match _get_from_text_view view#as_view with
  | None -> ()
  | Some s -> _detach s

let recheck_all view = 
  match _get_from_text_view view#as_view with
  | None -> ()
  | Some s -> _recheck_all s

let set_language view lang =
  match _get_from_text_view view#as_view with
  | None -> ()
  | Some s -> _set_language s lang
