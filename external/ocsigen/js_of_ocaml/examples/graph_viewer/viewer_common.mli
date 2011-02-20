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

module F (M : sig
  type color
  type font
  type text
  val white : color

  type ctx

  val save : ctx -> unit
  val restore : ctx -> unit

  val scale : ctx -> sx:float -> sy:float -> unit
  val translate : ctx -> tx:float -> ty:float -> unit

  val begin_path : ctx -> unit
  val close_path : ctx -> unit
  val move_to : ctx -> x:float -> y:float -> unit
  val line_to : ctx -> x:float -> y:float -> unit
  val curve_to :
    ctx ->
    x1:float -> y1:float -> x2:float -> y2:float -> x3:float -> y3:float ->
    unit
  val arc :
    ctx ->
    xc:float -> yc:float -> radius:float -> angle1:float -> angle2:float ->
    unit
  val rectangle :
    ctx -> x:float -> y:float -> width:float -> height:float -> unit

  val fill : ctx -> color -> unit
  val stroke : ctx -> color -> unit
  val clip : ctx -> unit

  val draw_text :
    ctx -> float -> float -> text ->
    font -> color option -> color option -> unit

  type window
  type drawable
  type pixmap
  val get_drawable : window -> drawable
  val make_pixmap : window -> int -> int -> pixmap
  val drawable_of_pixmap : pixmap -> drawable
  val get_context : pixmap -> ctx
  val put_pixmap :
    dst:drawable ->
    x:int -> y:int -> xsrc:int -> ysrc:int -> width:int -> height:int ->
    pixmap -> unit

  (****)

  type rectangle = {x : int; y : int; width : int; height: int}

  val compute_extents :
    ctx ->
    (color, font, text) Scene.element array ->
    (float * float * float * float) array
end) : sig

  type pixmap

  val make_pixmap : unit -> pixmap
  val invalidate_pixmap : pixmap -> unit

  type st =
    { mutable bboxes : (float * float * float * float) array;
      scene : (M.color, M.font, M.text) Scene.element array;
      mutable zoom_factor : float;
      st_x : float; st_y : float; st_width : float; st_height : float;
      st_pixmap : pixmap }

  val redraw :
    st -> float -> float -> float ->
    M.window -> M.rectangle -> int -> int -> int -> int -> unit

end
