(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** WARNING: EXPERIMENTAL *)

type canceller (*VVV ? *)
type ('a, 'b) t
val lwt_arr : ('a -> 'b Lwt.t) -> ('a, 'b) t
val arr : ('a -> 'b) -> ('a, 'b) t
val (>>>) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
val (>>>|) : ('a, 'b) t -> ('b -> 'c Lwt.t) -> ('a, 'c) t
val run : ('a, unit) t -> 'a -> canceller
val cancel : canceller -> unit

(** Behaves as the first element of the list to terminate *)
val first : ('a, 'b) t list -> ('a, 'b) t

(*  val loop : ('a, 'b) t -> ('b, 'a) t -> ('a, 'c) t *)

val click : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> ('a, Dom_html.mouseEvent Js.t) t
val dblclick : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> ('a, Dom_html.mouseEvent Js.t) t
val mousedown : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> ('a, Dom_html.mouseEvent Js.t) t
val mouseup : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> ('a, Dom_html.mouseEvent Js.t) t
val mouseover : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> ('a, Dom_html.mouseEvent Js.t) t
val mousemove : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> ('a, Dom_html.mouseEvent Js.t) t
val mouseout : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> ('a, Dom_html.mouseEvent Js.t) t
val keypress : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> ('a, Dom_html.keyboardEvent Js.t) t
val keydown : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> ('a, Dom_html.keyboardEvent Js.t) t
val keyup : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> ('a, Dom_html.keyboardEvent Js.t) t
(* val mousewheel : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> ('a, Dom_html.mousewheelEvent Js.t) t *)

val clicks : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> (Dom_html.mouseEvent Js.t, 'a) t -> ('b, 'c) t
val dblclicks : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> (Dom_html.mouseEvent Js.t, 'a) t -> ('b, 'c) t
val mousedowns : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> (Dom_html.mouseEvent Js.t, 'a) t -> ('b, 'c) t
val mouseups : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> (Dom_html.mouseEvent Js.t, 'a) t -> ('b, 'c) t
val mouseovers : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> (Dom_html.mouseEvent Js.t, 'a) t -> ('b, 'c) t
val mousemoves : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> (Dom_html.mouseEvent Js.t, 'a) t -> ('b, 'c) t
val mouseouts : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> (Dom_html.mouseEvent Js.t, 'a) t -> ('b, 'c) t
val keypresses : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> (Dom_html.keyboardEvent Js.t, 'a) t -> ('b, 'c) t
val keydowns : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> (Dom_html.keyboardEvent Js.t, 'a) t -> ('b, 'c) t
val keyups : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> (Dom_html.keyboardEvent Js.t, 'a) t -> ('b, 'c) t
(* val mousewheels : ?use_capture : bool ->  ?keep_default : bool ->
  #Dom_html.eventTarget Js.t -> (Dom_html.mousewheelEvent Js.t, 'a) t -> ('b, 'c) t
*)
