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

open Scene

let pi = 4. *. atan 1.

(****)

module F (M : sig
  type font
  type color
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
end) = struct

open M

let empty_rectangle = {x = 0; y = 0; width = 0; height = 0}
let rectangle_is_empty r = r.width = 0 || r.height = 0

(****)

type pixmap =
  { mutable pixmap : M.pixmap option;
    mutable p_width : int; mutable p_height : int;
    mutable valid_rect : rectangle }

let make_pixmap () =
  { pixmap = None; p_width = 0; p_height = 0;
    valid_rect = empty_rectangle }

let invalidate_pixmap p = p.valid_rect <- empty_rectangle

let grow_pixmap pm window width height =
  let width = max width pm.p_width in
  let height = max height pm.p_height in
  if width > pm.p_width || height > pm.p_height then begin
    let old_p = pm.pixmap in
    let p = M.make_pixmap window width height in
    let r = pm.valid_rect in
    begin match old_p with
      Some old_p ->
        put_pixmap ~dst:(drawable_of_pixmap p)
          ~x:0 ~y:0 ~xsrc:0 ~ysrc:0 ~width:r.width ~height:r.height old_p
    | None ->
        ()
    end;
    pm.pixmap <- Some p;
    pm.p_width <- width;
    pm.p_height <- height
  end

let get_pixmap pm = match pm.pixmap with Some p -> p | None -> assert false

(****)

type st =
  { mutable bboxes : (float * float * float * float) array;
    scene : (color, font, text) Scene.element array;
    mutable zoom_factor : float;
    st_x : float; st_y : float; st_width : float; st_height : float;
    st_pixmap : pixmap }

(****)

let perform_draw ctx fill stroke =
  begin match fill with
    Some c -> M.fill ctx c
  | None   -> ()
  end;
  begin match stroke with
    Some c -> M.stroke ctx c
  | None   -> ()
  end

let draw_element ctx e =
  begin_path ctx;
  match e with
    Path (cmd, fill, stroke) ->
      Array.iter
        (fun c ->
           match c with
             Move_to (x, y) ->
               move_to ctx x y
           | Curve_to (x1, y1, x2, y2, x3, y3) ->
               curve_to ctx x1 y1 x2 y2 x3 y3)
        cmd;
      perform_draw ctx fill stroke
  | Ellipse (cx, cy, rx, ry, fill, stroke) ->
      save ctx;
      translate ctx cx cy;
      scale ctx rx ry;
      arc ctx 0. 0. 1. 0. (2. *. pi);
      restore ctx;
      perform_draw ctx fill stroke
  | Polygon (points, fill, stroke) ->
      Array.iteri
        (fun i (x, y) ->
           if i = 0 then move_to ctx x y else line_to ctx x y)
        points;
      close_path ctx;
      perform_draw ctx fill stroke
  | Text (x, y, txt, font, fill, stroke) ->
      draw_text ctx x y txt font fill stroke

let intersects
      ((x1, y1, x2, y2) : float * float * float * float) (x3, y3, x4, y4) =
  x1 <= x4 && y1 <= y4 && x3 <= x2 && y3 <= y2

let compute_scale st range =
  st.zoom_factor ** range#adjustment#value /. st.zoom_factor

let redraw st scale x y x' y' w h =
(*
Format.eprintf "REDRAW %d %d %d %d@." x' y' w h;
*)
  let ctx = get_context (get_pixmap st.st_pixmap) in
  save ctx;
  if Array.length st.bboxes = 0 && Array.length st.scene > 0 then
    st.bboxes <- compute_extents ctx st.scene;
  begin_path ctx;
  rectangle ctx (float x') (float y') (float w) (float h);
  M.fill ctx M.white;
  clip ctx;
  let x = float x /. scale in
  let y = float y /. scale in
  M.scale ctx scale scale;
  translate ctx (-. st.st_x -. x) (-. st.st_y -. y);
  let bbox =
    let x = st.st_x +. x +. float x' /. scale in
    let y = st.st_y +. y +. float y' /. scale in
    (x, y,
     x +. float st.st_pixmap.p_width /. scale,
     y +. float st.st_pixmap.p_height /. scale)
  in
  for i = 0 to Array.length st.scene - 1 do
    let box = st.bboxes.(i) in
    let e = st.scene.(i) in
    if intersects box bbox then draw_element ctx e
  done;
  restore ctx

let redraw st scale x0 y0 window a x y width height =
  let pm = st.st_pixmap in
  grow_pixmap pm window a.width a.height;
  let round x = truncate (x *. scale +. 0.5) in
  let x0 = round x0 in
  let x0' = round ((float a.width /. scale -. st.st_width) /. 2.) in
  let x0 = if x0' > 0 then - x0' else x0 in
  let y0 = round y0 in
  let y0' = round ((float a.height /. scale -. st.st_height) /. 2.) in
  let y0 = if y0' > 0 then - y0' else y0 in
  let dx = pm.valid_rect.x - x0  in
  let dy = pm.valid_rect.y - y0  in
(*
Firebug.console##log_6 (dx, pm.valid_rect.width, a.width,
               dy, pm.valid_rect.height, a.height);
*)
  if
    (dx > 0 && pm.valid_rect.width + dx < a.width) ||
    (dy > 0 && pm.valid_rect.height + dy < a.height)
  then begin
    pm.valid_rect <- empty_rectangle
  end else if not (rectangle_is_empty pm.valid_rect) then begin
(*XXX FIX: should redraw up to four rectangles here *)
(*XXX FIX: does not change pm.valid_rect when it is large enough already and valid *)
    let p = get_pixmap pm in
    let r = pm.valid_rect in
(*
Format.eprintf "Translation: %d %d@." dx dy;
*)
    if (dx <> 0 || dy <> 0) then
      put_pixmap ~dst:(drawable_of_pixmap p) ~x:dx ~y:dy
        ~xsrc:0 ~ysrc:0 ~width:r.width ~height:r.height p;
    let offset p l d m = (* 0 <= p; 0 <= l; p + l <= m *)
      if p + d + l <= 0 then
        (0, 0)
      else if p + d < 0 then
        (0, l + p + d)
      else if p + d >= m then
        (m, 0)
      else if p + d + l > m then
        (p + d, m - p - d)
      else
        (p + d, l)
    in
    let (x, width) = offset 0 r.width dx pm.p_width in
    let (y, height) = offset 0 r.height dy pm.p_height in
    if height > 0 then begin
      if x > 0 then begin
        assert (x + width >= a.width);
        redraw st scale x0 y0 0 y x height
      end else begin
        assert (x = 0);
        if a.width > width then
          redraw st scale x0 y0 width y (a.width - width) height
      end
    end;
    if y > 0 then begin
      assert (y + height >= a.height);
      redraw st scale x0 y0 0 0 a.width y;
    end else begin
      assert (y = 0);
      if a.height > height then
        redraw st scale x0 y0 0 height a.width (a.height - height)
    end;
    pm.valid_rect <- { x = x0; y = y0; width = a.width; height = a.height }
  end;
  let r = pm.valid_rect in
  if
    x < 0 || y < 0 ||
    x + width > r.width || y + height > r.height
  then begin
    redraw st scale x0 y0 0 0 a.width a.height;
    pm.valid_rect <- {x = x0; y = y0; width = a.width; height = a.height };
  end;
  put_pixmap
    ~dst:(get_drawable window) ~x ~y ~xsrc:x ~ysrc:y ~width ~height
    (get_pixmap pm)
end
