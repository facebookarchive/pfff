module D = Dom_html
module GUCS = Goog.Ui.Component.State  

let to_s = Js.string
let ccs s = Goog.Ui.ControlContent.string (Js.string s)

let get_el s = Js.Opt.get (D.document##getElementById (Js.string s))
    (fun _ -> D.window##alert (Js.string (s^": element not found!")); assert false)
let get x : Goog.Ui.button Js.t = Js.Unsafe.coerce (Js.Opt.get x (fun _ -> assert false))

let js_string_of_bool_t b = to_s (string_of_bool (Js.to_bool b))

let jsnot b = Js.bool (not(Js.to_bool b))

let disabledButton =
  jsnew Goog.Ui.customButton(ccs"Disabled Button",
			     Js.some(jsnew Goog.Ui.css3ButtonRenderer()),
			     Js.null)
let customButtons = [
  jsnew Goog.Ui.customButton(ccs"Button",
			     Js.some(jsnew Goog.Ui.css3ButtonRenderer()),
			     Js.null);
  jsnew Goog.Ui.customButton(ccs"Another Button",
			     Js.some(jsnew  Goog.Ui.css3ButtonRenderer()),
			     Js.null);
  disabledButton;
  jsnew Goog.Ui.customButton(ccs"Yet Another Button",
			     Js.some(jsnew Goog.Ui.css3ButtonRenderer()),
			     Js.null)
    ]

let f (b:Goog.Ui.customButton Js.t) =
  b##render(Js.some (get_el "cb1"));
  ignore(Goog.Events.listen 
    (Goog.Tools.Union.i1 b) 
    Goog.Ui.Component.EventType._ACTION
    (Js.wrap_callback (fun () -> 
      let newCaption = D.window##prompt(
	to_s"Enter new caption for button:", to_s"Where am I ?") in
      b##setCaption(newCaption)))
    Js.null)

let _ = 
  disabledButton##setEnabled(Js._false);
  List.iter f customButtons;      
let cb2 = ref [] in
  let decoratedButtons = List.map get_el
      ["foo"; "bar"; "fee"; "btn1"; "btn2"; "btn3"; "btn4"; "btn5"; "btn6"] in
  List.iter 
    (fun e -> 
      let button = get (Goog.Ui.decorate e) in 
      button##setDispatchTransitionEvents(
        GUCS.state_pre_of_state GUCS.ALL, Js._true);
      cb2 := !cb2@[button]) decoratedButtons;

  let toggleEnableElem = get_el "toggleEnable" in
  let toggleEnable = get (Goog.Ui.decorate toggleEnableElem) in 
  toggleEnable##setDispatchTransitionEvents(
    GUCS.state_pre_of_state GUCS.ALL, Js._true);

  ignore(Goog.Events.listen
    (Goog.Tools.Union.i1 toggleEnable)
    Goog.Ui.Component.EventType._ACTION
    (Js.wrap_callback (fun () -> 
      (List.nth !cb2 1)##setEnabled(toggleEnable##isChecked())))
    Js.null);

  let hideShowElem = get_el "hideShow" in
  let hideShow = get (Goog.Ui.decorate hideShowElem) in
  hideShow##setDispatchTransitionEvents(
    GUCS.state_pre_of_state GUCS.ALL, Js._true);
  ignore(Goog.Events.listen
    (Goog.Tools.Union.i1 hideShow)
    Goog.Ui.Component.EventType._ACTION
    (Js.wrap_callback (fun () ->
      ignore((List.nth !cb2 1)##setVisible(hideShow##isChecked(), Js.null)))
    )
    Js.null)

