let d = Dom_html.document

let getElById i = Js.Opt.get (d##getElementById(Js.string i)) 
    (fun () -> assert false)

let c1 = jsnew Goog.Ui.bidiInput(Js.null)
let c2 = jsnew Goog.Ui.bidiInput(Js.null)
let c3 = jsnew Goog.Ui.bidiInput(Js.null)
let c4 = jsnew Goog.Ui.bidiInput(Js.null)

let _ =    
  c1##decorate(getElById "c1");
  c2##render(Js.some (getElById "c2"));
  c3##decorate(getElById "c3");
  c4##decorate(getElById "c4")
