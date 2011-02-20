
module D = Dom_html
let d = D.document

let get_el s = Js.Opt.get (d##getElementById (Js.string s)) (fun _ -> assert false)

let _ =
  let el = get_el "menu1" 
  and array = [|"Gmail";"Finance";"Bugs";"Apps";"Apps UI";"Apps UX";"Gmail Bugs";
  "Reference";"DO";"READ";"misc" |]
  and menu = jsnew Goog.Ui.filteredMenu(Js.null,Js.null) in
    menu##setFilterLabel(Js.some (Js.string "Create / search")); 
    menu##setAllowMultiple(Js._true);
    Array.iter (
      fun a -> 
	let tsmi = jsnew Goog.Ui.triStateMenuItem (Goog.Ui.ControlContent.string (Js.string a),Js.null,Js.null) in
	let item = Goog.Tools.Union.i1 (tsmi) in
	  tsmi##setCheckedState(Goog.Ui.TriStateMenuItem.State.PARTIALLY_CHECKED);
	  menu##addItem(item)) array;
    
    menu##render(Js.some el);
    menu##setPosition(Goog.Tools.Union.i1 10,Js.some 10)
