(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
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

(** Javascript events *)

module Typ :
  sig
    type 'a typ
    val click : Dom_html.mouseEvent Js.t typ
    val dblclick : Dom_html.mouseEvent Js.t typ
    val mousedown : Dom_html.mouseEvent Js.t typ
    val mouseup : Dom_html.mouseEvent Js.t typ
    val mouseover : Dom_html.mouseEvent Js.t typ
    val mousemove : Dom_html.mouseEvent Js.t typ
    val mouseout : Dom_html.mouseEvent Js.t typ
    val keypress : Dom_html.keyboardEvent Js.t typ
    val keydown : Dom_html.keyboardEvent Js.t typ
    val keyup : Dom_html.keyboardEvent Js.t typ
    val mousewheel : Dom_html.mousewheelEvent Js.t typ
    val _DOMMouseScroll : Dom_html.mouseScrollEvent Js.t typ
  end


type listener


val listen :
     ?capture:bool
  -> (#Dom_html.eventTarget as 'a) Js.t
  -> (#Dom_html.event as 'b) Js.t Typ.typ
  -> ('a Js.t -> 'b Js.t -> unit)
  -> listener


val stop_listen : listener -> unit
