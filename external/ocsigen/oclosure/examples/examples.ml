let to_s = Js.string
let ccs s = Goog.Ui.ControlContent.string (to_s s)

let d = Dom_html.window##document

let get_el s = Js.Opt.get (d##getElementById (to_s s)) (fun _ -> assert false)

let body = get_el "body"

let add_item s (m : Goog.Ui.menu Js.t) = m##addItem(Goog.Tools.Union.i1 jsnew Goog.Ui.menuItem(ccs s, Js.null, Js.null))

let toolbar = jsnew Goog.Ui.toolbar(Js.null, Js.null, Js.null)
let code_button = jsnew Goog.Ui.toolbarToggleButton(ccs "Code", Js.null, Js.null)
let _ = code_button##setTooltip(to_s "Show/hide Ocaml code for the example")

(*let fill_examples_menu m = 
  let dh = Unix.opendir "." in
  let rec aux () = 
    try begin
      let s = Unix.readdir dh in
      if Str.string_match (Str.regexp "test_\(.*\)\.ml") s 0 then begin
	m##addItem(
	  Goog.Tools.Union.i1 jsnew Goog.Ui.menuItem(ccs (Str.matched_group 1 s)));
	aux ()
      end
      else aux ()
    end
    with End_of_file -> () in
  aux ()*)
  

let menu = jsnew Goog.Ui.menu(Js.null, Js.null)
(*let _ = fill_examples_menu menu*)
let _ = 
  add_item "advancedTooltip" menu; 
  add_item "autoCompleteBasic" menu; 
  add_item "bidiInput" menu;
  add_item "bubble" menu;
  add_item "charPicker" menu;
  add_item "charCounter" menu;
  add_item "checkbox" menu


let menu_b = jsnew Goog.Ui.toolbarMenuButton(
  ccs "Show example", Js.some menu, Js.null, Js.null)
let toolbar_div = Dom_html.createDiv d
let _ = 
(*  Goog.Ui.Component.addChild toolbar menu_b (Js.some Js._true);*)
  Goog.Ui.Component.addChild toolbar code_button (Js.some Js._true);
  toolbar##render(Js.some toolbar_div);
  Dom.appendChild body toolbar_div
