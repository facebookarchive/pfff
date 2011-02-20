open Js

(*let add_item c i = Goog.Ui.Component.addChild c i Js.null*)
let add_item (c : Goog.Ui.comboBox t) i = c##addItem(i)
let get_el s = Js.Opt.get (Dom_html.document##getElementById(Js.string s))
    (fun _ -> assert false)

let el = get_el "combo1"
let cb = jsnew Goog.Ui.comboBox(Js.null)
let _ = 
  cb##setUseDropdownArrow(Js._true);
  cb##setDefaultText(Js.string "Select a folder...");
  add_item cb (jsnew Goog.Ui.comboBoxItem(Js.string "Inbox"));
  add_item cb (jsnew Goog.Ui.comboBoxItem(Js.string "Bills & statements"));
  add_item cb (jsnew Goog.Ui.comboBoxItem(Js.string "Cal alumni"));
  add_item cb (jsnew Goog.Ui.comboBoxItem(Js.string "Calendar Stuff"));
  add_item cb (jsnew Goog.Ui.comboBoxItem(Js.string "Design"));
  add_item cb (jsnew Goog.Ui.comboBoxItem(Js.string "Music"));
  add_item cb (jsnew Goog.Ui.comboBoxItem(Js.string "Netflix"));
  add_item cb (jsnew Goog.Ui.comboBoxItem(Js.string "Personal"));
  add_item cb (jsnew Goog.Ui.comboBoxItem(Js.string "Photos"));
  add_item cb (jsnew Goog.Ui.comboBoxItem(Js.string "Programming"));
  add_item cb (jsnew Goog.Ui.menuSeparator(Js.null))

let newfolder = jsnew Goog.Ui.comboBoxItem(Js.string "New Folder...")
let _ =
  newfolder##setSticky(Js._true);
  add_item cb newfolder;
  cb##render(Js.some el);
  Goog.Events.listen 
    (Goog.Tools.Union.i1 cb) 
    (Js.string "change") 
    (Js.wrap_callback (fun () -> (get_el "v")##innerHTML <- cb##getValue()))
    Js.null

let el2 = get_el "combo2"
let cb2 = jsnew Goog.Ui.comboBox(Js.null)
let _ =
  cb2##setDefaultText(Js.string "Select a color...");
  add_item cb2 (jsnew Goog.Ui.comboBoxItem(Js.string "Red"));
  add_item cb2 (jsnew Goog.Ui.comboBoxItem(Js.string "Maroon"));
  add_item cb2 (jsnew Goog.Ui.comboBoxItem(Js.string "Green"));
  add_item cb2 (jsnew Goog.Ui.comboBoxItem(Js.string "Blue"));
  add_item cb2 (jsnew Goog.Ui.comboBoxItem(Js.string "Royal Blue"));
  add_item cb2 (jsnew Goog.Ui.comboBoxItem(Js.string "Yellow"));
  add_item cb2 (jsnew Goog.Ui.comboBoxItem(Js.string "Magenta"));
  add_item cb2 (jsnew Goog.Ui.comboBoxItem(Js.string "Mouve"));
  add_item cb2 (jsnew Goog.Ui.comboBoxItem(Js.string "Grey"))

let caption = jsnew Goog.Ui.comboBoxItem(Js.string "Select a color...")
let _ =
  caption##setSticky(Js._true);
  caption##setEnabled(Js._false);
  cb2##addItemAt(caption, 0);
  cb2##render(Js.some el2);
  ignore(  Goog.Events.listen
    (Goog.Tools.Union.i1 cb2)
    (Js.string "change")
    (Js.wrap_callback (fun () -> (get_el "v")##innerHTML <- cb2##getValue()))
    Js.null);

