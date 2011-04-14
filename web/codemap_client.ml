
module Html = Dom_html

module Shared = Codemap_shared

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ccs s = 
  Goog.Ui.ControlContent.string (Js.string s)

let get_element_by_id s = 
  Js.Opt.get (Dom_html.document##getElementById(Js.string s))
    (fun _ -> assert false)

let js = 
  Js.string

let add_item s (m : Goog.Ui.menu Js.t) = 
  m##addItem(Goog.Tools.Union.i1 jsnew 
                Goog.Ui.menuItem(ccs s, Js.null, Js.null))


let draw ctx (color, size, (x1, y1), (x2, y2)) =
  ctx##strokeStyle <- (Js.string color);
  ctx##lineWidth <- float size;
  ctx##beginPath();
  ctx##moveTo(float x1, float y1);
  ctx##lineTo(float x2, float y2);
  ctx##stroke();
  ()

(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

let mk_gui () =

  (*-------------------------------------------------------------------*)
  (* Menu *)
  (*-------------------------------------------------------------------*)

  let menu = jsnew Goog.Ui.menu(Js.null, Js.null) in
  add_item "File" menu; 
  add_item "Misc" menu; 
  let div = Dom_html.createDiv Dom_html.document in
  Dom.appendChild Dom_html.document##body div;
  menu##render(Js.some div);

  (*-------------------------------------------------------------------*)
  (* toolbar *)
  (*-------------------------------------------------------------------*)


  (*-------------------------------------------------------------------*)
  (* main view *)
  (*-------------------------------------------------------------------*)

  let canvas = Dom_html.createCanvas Dom_html.document in
  let ctx = canvas##getContext (Dom_html._2d_) in
  canvas##width <- Shared.width; 
  canvas##height <- Shared.height;
  Dom.appendChild Dom_html.document##body canvas;

  (*-------------------------------------------------------------------*)
  (* End *)
  (*-------------------------------------------------------------------*)

  ctx##lineCap <- Js.string "round";
  draw ctx ("#ffaa33", 12, (10, 10), (200, 100));
  ()

let onload () = 
  mk_gui ()
