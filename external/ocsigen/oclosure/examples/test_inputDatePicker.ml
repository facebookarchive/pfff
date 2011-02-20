let get_el s = Js.Opt.get (Dom_html.document##getElementById(Js.string s))
  (fun () -> assert false)


let _PATTERN = Js.string "MM'/'dd'/'yyyy"
let formatter = jsnew Goog.I18n.dateTimeFormat(Goog.Tools.Union.i1 _PATTERN)
let parse = jsnew Goog.I18n.dateTimeParse(Goog.Tools.Union.i1 _PATTERN)

let idp1 = jsnew Goog.Ui.inputDatePicker(formatter, parse, Js.null, Js.null)

let idp2 = jsnew Goog.Ui.inputDatePicker(formatter, parse, Js.null, Js.null)

let fieldLabelInput = jsnew Goog.Ui.labelInput(
  Js.some (Js.string "MM/DD/YYYY"), Js.null)

let idp3 = jsnew Goog.Ui.inputDatePicker(formatter, parse, Js.null, Js.null)

let _ = 
  idp1##decorate(get_el "date-field1");
  idp2##render(Js.some (get_el "date-field2"));
  fieldLabelInput##render(Js.some (get_el "date-container"));
  idp3##decorate(fieldLabelInput##getElement())
  
