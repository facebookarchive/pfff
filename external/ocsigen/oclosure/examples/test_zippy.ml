module D = Dom_html
let d = D.document

let get_el s = Js.Opt.get (d##getElementById (Js.string s))
    (fun () -> assert false)

let header = Js.Opt.get (d##getElementById (Js.string "header"))
    (fun () -> assert false)

let z = jsnew Goog.Ui.animatedZippy (
  Js.some (Goog.Tools.Union.i1 header), 
  Js.some (Goog.Tools.Union.i2 (Js.string "content")),
  Js.null)

let z2 = jsnew Goog.Ui.animatedZippy (
  Js.some (Goog.Tools.Union.i1 (get_el "header2")),
  Js.some (Goog.Tools.Union.i2 (Js.string "content2")),
  Js.null)

let _ = Goog.Events.listen 
    (Goog.Tools.Union.i1 z)
    (Js.string "hover")
    (Js.wrap_callback (fun () -> z##toggle()))
    Js.null

let js_not b = Js.bool (not (Js.to_bool b))

let _ = Goog.Events.listen
    (Goog.Tools.Union.i1 z)
    (Js.string "toggle")
    (Js.wrap_callback (fun () -> z2##setExpanded(js_not z##isExpanded())))
    Js.null

