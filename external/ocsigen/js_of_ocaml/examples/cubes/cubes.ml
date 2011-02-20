(* Js_of_ocaml example
 * http://www.ocsigen.org/js_of_ocaml/
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

let n = 12

let h = 20.
let w = floor (h *. sqrt 3. /. 2. +. 0.5)

(****)

let _ = Random.self_init ()

let create_cubes v =
  Array.init n (fun _ -> Array.init n (fun _ -> Array.make n v))

let get a i j k =
  i < 0 || j < 0 || k < 0 ||
  (i < n && j < n && k < n && a.(i).(j).(k))

let update a =
  let i = Random.int n in
  let j = Random.int n in
  let k = Random.int n in
  if a.(i).(j).(k) then begin
    if
      not (get a (i + 1) j k || get a i (j + 1) k || get a i j (k + 1))
    then begin
      a.(i).(j).(k) <- false;
      true
    end else
      false
  end else begin
    if get a (i - 1) j k & get a i (j - 1) k && get a i j (k - 1) then begin
      a.(i).(j).(k) <- true;
      true
    end else
      false
  end

(****)

module Html = Dom_html

let top = Js.string "#a8a8f6"
let left = Js.string "#d9d9d9"
let right = Js.string "#767676"
let edge = Js.string "#000000"

let on_cube c i j k f =
  let x = float (i - k + n - 1) *. w in
  let y = float (n - 1 - j) *. h +. float (i + k) *. h /. 2. in
  c##save ();
  c##translate (x, y);
  f c;
  c##restore ()

let draw_top c =
  c##fillStyle <- top;
  c##beginPath ();
  c##moveTo (w, 0.);
  c##lineTo (2. *. w, h /. 2.);
  c##lineTo (w, h);
  c##lineTo (0., h /. 2.);
  c##fill ()

let top_edges c =
  c##beginPath ();
  c##moveTo (0., h /. 2.);
  c##lineTo (w, 0.);
  c##lineTo (2. *. w, h /. 2.);
  c##stroke ()

let draw_right c =
  c##fillStyle <- right;
  c##beginPath ();
  c##moveTo (w, h);
  c##lineTo (w, 2. *. h);
  c##lineTo (2. *. w, 1.5 *. h);
  c##lineTo (2. *. w, h /. 2.);
  c##fill ()

let right_edges c =
  c##beginPath ();
  c##moveTo (w, 2. *. h);
  c##lineTo (w, h);
  c##lineTo (2. *. w, h /. 2.);
  c##stroke ()

let draw_left c =
  c##fillStyle <- left;
  c##beginPath ();
  c##moveTo (w, h);
  c##lineTo (w, 2. *. h);
  c##lineTo (0., 1.5 *. h);
  c##lineTo (0., h /. 2.);
  c##fill ()

let left_edges c =
  c##beginPath ();
  c##moveTo (w, h);
  c##lineTo (0., h /. 2.);
  c##lineTo (0., 1.5 *. h);
  c##stroke ()

let remaining_edges c =
  c##beginPath ();
  c##moveTo (0., float n *. 1.5 *. h);
  c##lineTo (float n *. w, float n *. 2. *. h);
  c##lineTo (float n *. 2. *. w, float n *. 1.5 *. h);
  c##lineTo (float n *. 2. *. w, float n *. 0.5 *. h);
  c##stroke ()

let tile c a (top, right, left) =
  for i = 0 to n - 1 do
    let j = ref (n - 1) in
    for k = 0 to n - 1 do
      while (!j >= 0 && not a.(i).(!j).(k)) do decr j done;
      on_cube c i !j k top
    done
  done;
  for j = 0 to n - 1 do
    let i = ref (n - 1) in
    for k = 0 to n - 1 do
      while (!i >= 0 && not a.(!i).(j).(k)) do decr i done;
      on_cube c !i j k right
    done
  done;
  for i = 0 to n - 1 do
    let k = ref (n - 1) in
    for j = 0 to n - 1 do
      while (!k >= 0 && not a.(i).(j).(!k)) do decr k done;
      on_cube c i j !k left
    done
  done

let create_canvas () =
  let d = Html.window##document in
  let c = Html.createCanvas d in
  c##width <- n * 2 * truncate w + 1;
  c##height <- n * 2 * truncate h + 1;
  c

let redraw ctx canvas a =
  let c = canvas##getContext (Html._2d_) in
  c##setTransform (1., 0., 0., 1., 0., 0.);
  c##clearRect (0., 0., float canvas##width, float canvas##height);
  c##setTransform (1., 0., 0., 1., 0.5, 0.5);
  c##globalCompositeOperation <- Js.string "lighter";
  tile c a (draw_top, draw_right, draw_left);
  c##globalCompositeOperation <- Js.string "source-over";
  tile c a (top_edges, right_edges, left_edges);
  remaining_edges c;
  ctx##drawImage_fromCanvas (canvas, 0., 0.)

let (>>=) = Lwt.bind

let rec loop c c' a =
  Lwt_js.sleep 0.2 >>= fun () ->
  let need_redraw = ref false in
  for i = 0 to 99 do
    need_redraw := update a || !need_redraw
  done;
  if !need_redraw then redraw c c' a;
  loop c c' a

let start _ =
  let c = create_canvas () in
  let c' = create_canvas () in
  Dom.appendChild Html.window##document##body c;
  let c = c##getContext (Html._2d_) in
  c##globalCompositeOperation <- Js.string "copy";
  let a = create_cubes true in
  redraw c c' a;
  ignore (loop c c' a);
  Js._false

let _ =
Html.window##onload <- Html.handler start
