open Common

let pr2 s = 
  Firebug.console##log(Js.string s)

(* from jflo slides *)
let unopt x =
  Js.Opt.get x (fun () -> raise Not_found)
let retrieve id =
  unopt (Dom_html.document##getElementById (Js.string id))

(* src: http://answers.oreilly.com/topic/1929-how-to-use-the-canvas-and-draw-elements-in-html5/ 
 * in the canvas pocket reference they mention another technique p46
 * using instead getBoundlingClientRect
*)
let get_position elt ev =
  let x = 
    ev##clientX + Dom_html.document##body##scrollLeft +
      Dom_html.document##documentElement##scrollLeft 
      - elt##offsetLeft in
  let y = 
    ev##clientY + Dom_html.document##body##scrollTop +
      Dom_html.document##documentElement##scrollTop 
      - elt##offsetTop in
  x, y
