open Goog

module D = Dom_html

let dialog = jsnew Ui.dialog(Js.null, Js.null, Js.null)

let button = D.createButton Dom_html.document
let _ = 
  button##onclick <- D.handler (fun _ -> 
    dialog##setVisible(Js._true); Js.bool true)
let _ = Dom.appendChild(D.document##body) button;
  Dom.appendChild button (D.document##createTextNode(Js.string "Dialog"))

let _ =
  dialog##setVisible(Js._false);
  dialog##setContent(Js.string "<br />OClosure : <br />A simple dialog box");
