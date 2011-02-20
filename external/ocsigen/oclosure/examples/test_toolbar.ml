open Goog

module D = Dom_html
let d = D.document

let t = jsnew Ui.toolbar(Js.null, Js.null, Js.null)
let toolbar = Js.Opt.get (d##getElementById (Js.string "toolbar"))
    (fun () -> assert false)

let button = jsnew Ui.toolbarButton(Ui.ControlContent.string (Js.string "Clik me!"), Js.null, Js.null)
let button2 = jsnew Ui.toolbarButton(Ui.ControlContent.string (Js.string "Disabled"), Js.null, Js.null)

let _ = 
  ignore (Events.listen 
    (Tools.Union.i1 button) 
    (Js.string "action") 
    (Js.wrap_callback (fun () -> D.window##alert(Js.string "Bravo!"))) Js.null);
  Ui.Component.addChild t button (Js.some Js._true);
  Ui.Component.addChild t button2 (Js.some Js._true);
  let c = Js.Opt.get (t##getChildAt(1)) (fun _ -> assert false) in
  (Js.Unsafe.coerce c)##setEnabled(Js._false);
  t##render(Js.some toolbar)
