(* Graph viewer
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

type command =
    Move_to of float * float
  | Curve_to of float * float * float * float * float * float

type color = float * float * float

type ('color, 'font, 'text) element =
    Path of command array * 'color option * 'color option
  | Polygon of (float * float) array * 'color option * 'color option
  | Ellipse of float * float * float * float * 'color option * 'color option
  | Text of float * float * 'text * 'font * 'color option * 'color option

(****)

val rectangle :
  float * float * float * float -> 'color option -> 'color option ->
  ('color, 'font, 'text) element

(****)

type ('color, 'font, 'text) t
type cairo_t = (float * float * float, string * float, string) t

val make : unit -> ('color, 'font, 'text) t
val add : ('color, 'font, 'text) t -> ('color, 'font, 'text) element -> unit
val get : ('color, 'font, 'text) t -> ('color, 'font, 'text) element array
