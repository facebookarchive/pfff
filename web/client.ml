(*
let foo () =
  let slider = jsnew Goog.Ui.slider(Js.null) in
  ()
*)

let draw ctx (color, size, (x1, y1), (x2, y2)) =
  ctx##strokeStyle <- (Js.string color);
  ctx##lineWidth <- float size;
  ctx##beginPath();
  ctx##moveTo(float x1, float y1);
  ctx##lineTo(float x2, float y2);
  ctx##stroke()


let width = 300
let height = 300

let draw_canvas () =
  let canvas = Dom_html.createCanvas Dom_html.document in
  let ctx = canvas##getContext (Dom_html._2d_) in
  canvas##width <- width; 
  canvas##height <- height;
  ctx##lineCap <- Js.string "round";

  Dom.appendChild Dom_html.document##body canvas;
  draw ctx ("#ffaa33", 12, (10, 10), (200, 100))


