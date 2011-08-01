
(*
open Common
*)
module T = Treemap

let pr2 s = 
  Firebug.console##log(Js.string s)
let spf = Printf.sprintf

let draw_line ctx (color, size, (x1, y1), (x2, y2)) =
  ctx##strokeStyle <- (Js.string color);
  ctx##lineWidth <- float size;
  ctx##beginPath();
  ctx##moveTo(float x1, float y1);
  ctx##lineTo(float x2, float y2);
  ctx##stroke()

let draw_line2 ctx (color, size, (x1, y1), (x2, y2)) =
  ctx##strokeStyle <- (Js.string color);
  ctx##lineWidth <- size;
  ctx##beginPath();
  ctx##moveTo(x1, y1);
  ctx##lineTo(x2, y2);
  ctx##stroke()

let draw_canvas () =
  pr2 "draw_canvas";
  let width = 300 in
  let height = 300 in
  let canvas = Dom_html.createCanvas Dom_html.document in
  let ctx = canvas##getContext (Dom_html._2d_) in
  canvas##width <- width; 
  canvas##height <- height;
  ctx##lineCap <- Js.string "round";

  Dom.appendChild Dom_html.document##body canvas;
  draw_line ctx ("#ffaa33", 12, (10, 10), (200, 100))


let draw_treemap_rendering (rects: Treemap.treemap_rendering) =
  pr2 "draw_treemap_rendering";
  pr2 (spf "# rects = %d " (List.length rects));

  let width = 600 in
  let height = 400 in
  let canvas = Dom_html.createCanvas Dom_html.document in
  let ctx = canvas##getContext (Dom_html._2d_) in
  canvas##width <- width; 
  canvas##height <- height;
  Dom.appendChild Dom_html.document##body canvas;

  (*draw_line ctx ("#ffaa33", 12, (10, 10), (200, 100));*)

  ctx##setTransform (1.,0.,0.,1.,0.,0.);
  ctx##scale (float_of_int width /. T.xy_ratio,
              float_of_int height);

  draw_line2 ctx ("#ffaa33", 0.001, (0., 0.), (1.6, 1.0));

  ()
