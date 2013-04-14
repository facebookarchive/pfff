
open Common

let pr2 s = 
  Firebug.console##log(Js.string s)

(* from jflo slides *)
let unopt x =
  Js.Opt.get x (fun () -> raise Not_found)
let retrieve id =
  unopt (Dom_html.document##getElementById (Js.string id))
