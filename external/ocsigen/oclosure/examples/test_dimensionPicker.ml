let get_el s = Js.Opt.get (Dom_html.document##getElementById (Js.string s))
    (fun _ -> assert false)

let p1 = jsnew Goog.Ui.dimensionPicker(Js.null, Js.null)

let _ =
  p1##render(Js.some (get_el "p1"));
  Goog.Events.listen
    (Goog.Tools.Union.i1 p1) 
    (Js.string "action")
    (Js.wrap_callback (fun _ ->
      let size = p1##getValue() in
      Goog.Gdom.setTextContent (get_el "p1_value")
        (Js.string ((string_of_int size##width)^" x "^(string_of_int size##height)));
      ))
    Js.null
  
let p2 = jsnew Goog.Ui.dimensionPicker(Js.null, Js.null) in
p2##decorate(get_el "decorateTarget")
