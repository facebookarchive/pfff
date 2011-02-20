open Goog

module D = Dom_html
let document = D.document

let show_prompt prompt =
  prompt##setVisible(Js._true)
    
let handler o = D.window##alert(Js.string o)
let handler s = 
  let res = Js.Opt.get s (fun () -> Js.string "faux") in
  if res = (Js.string "faux") then
    D.window##alert(Js.string "Afraid to answer?")
  else 
    D.window##alert(res);
  Js._true

let prompt = 
  jsnew Ui.prompt(Js.string "Goog.ui.Prompt", Js.string "Question", 
			 Js.wrap_callback handler)

let button = D.createButton document 
let _ = 
  button##onclick <- D.handler (fun _ -> show_prompt prompt; Js._true);
  Dom.appendChild(document##body) (D.createBr document);
  Dom.appendChild(document##body) button;
  Dom.appendChild button (document##createTextNode(Js.string "Prompt"))

let _ =
  prompt##setDefaultValue(Js.string "Default text message");
  prompt##setRows(3);
  prompt##setContent(Js.string "Question");  (** Fonction de la classe mère **)

