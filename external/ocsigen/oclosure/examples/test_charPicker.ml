let get_el s = Js.Opt.get (Dom_html.document##getElementById (Js.string s)) 
  (fun _ -> assert false) 

let selectionAction picker =
  let selected = get_el "p1_value" in
  selected##innerHTML <- Js.Opt.get picker##getSelectedChar()
      (fun _ -> Js.string "none")

let _ =
  let picker = jsnew Goog.Ui.charPicker(
    jsnew Goog.I18n.charPickerData(),
    Js.array (Array.map Js.string [|"a"; "b"; "c"|]), 
    Js.some 10, 
    Js.some 1, Js.null, Js.null, Js.null) in
  let el = get_el "char-picker" in
  picker##decorate(el);
  Goog.Events.listen
    (Goog.Tools.Union.i1 picker)
    (Js.string "action")
    (Js.wrap_callback (fun () -> selectionAction picker))
    Js.null
