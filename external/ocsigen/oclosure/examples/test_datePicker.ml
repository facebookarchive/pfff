open Goog.I18n

let alert s = 
  let dialog = jsnew Goog.Ui.dialog(Js.null, Js.null, Js.null) in
  dialog##setContent(s);
  dialog##setVisible(Js._true)

let get_el s = Js.Opt.get (Dom_html.document##getElementById(Js.string s))
    (fun _ -> assert false)

let get x = Js.Opt.get x (fun _ -> assert false)
let get_input s = 
  get (Dom_html.CoerceTo.input (get_el s))

let empty_date d = d##toIsoString(Js.some Js._true, Js.null) == (Js.string "")

let dp_iso_8601 = jsnew Goog.Ui.datePicker(Js.null, Js.null)
let _ =
  dp_iso_8601##render(Js.some (get_el "widget_iso_8601"));

  Goog.Events.listen
    (Goog.Tools.Union.i1 dp_iso_8601)
    (Js.string "change")
    (Js.wrap_callback (fun () ->
      alert (
	if (dp_iso_8601##getDate() != Js.null) 
	then (get dp_iso_8601##getDate())##toIsoString(Js.some Js._true, Js.null)
	else Js.string "none")))
    Js.null

let dp_custom = jsnew Goog.Ui.datePicker(Js.some (Goog.Tools.Union.i1
  (jsnew Goog.Date.date(Js.some 2006, Js.some 0, Js.some 1))), Js.null)
let _ =
  dp_custom##render(Js.some(get_el "widget_custom"));

  Goog.Events.listen
    (Goog.Tools.Union.i1 dp_custom)
    (Js.string "change")
    (Js.wrap_callback (fun () -> 
      (get_el "label_custom")##innerHTML <-
	if (dp_custom##getDate() != Js.null) 
	then (get dp_custom##getDate())##toIsoString(Js.some Js._true, Js.null)
	else Js.string "none"))
    Js.null

let dp_en_US = jsnew Goog.Ui.datePicker(Js.null, Js.some (dateTimeSymbols_en_US))
let _ = 
  dp_en_US##render(Js.some(get_el "widget_en_US"));
  Goog.Events.listen
    (Goog.Tools.Union.i1 dp_en_US)
    (Js.string "change")
    (Js.wrap_callback (fun () -> 
      (get_el "label_en_US")##innerHTML <-
	if (dp_en_US##getDate() != Js.null)
	then (get dp_en_US##getDate())##toIsoString(Js.some Js._true, Js.null)
	else Js.string "none"))
    Js.null

let dp_fr = jsnew Goog.Ui.datePicker(Js.null, Js.some (dateTimeSymbols_fr))
let _ = 
  dp_fr##render(Js.some(get_el "widget_fr"));
  ignore (Goog.Events.listen
    (Goog.Tools.Union.i1 dp_fr)
    (Js.string "change")
    (Js.wrap_callback (fun () ->
      (get_el "label_fr")##innerHTML <- 
	if(dp_fr##getDate() != Js.null)
	then (get dp_fr##getDate())##toIsoString(Js.some Js._true, Js.null) 
	else (Js.string "none")))
    Js.null);
  (get_el "label_fr")##innerHTML <-
    (get dp_fr##getDate())##toIsoString(Js.some Js._true, Js.null)
      

let dp_ml = jsnew Goog.Ui.datePicker(Js.null, Js.some (dateTimeSymbols_ml))
let _ =
  dp_ml##render(Js.some(get_el "widget_ml"));
  ignore(Goog.Events.listen
    (Goog.Tools.Union.i1 dp_ml) 
    (Js.string "change")
    (Js.wrap_callback (fun () ->
      (get_el "label_ml")##innerHTML <-
	if (dp_ml##getDate() != Js.null) 
	then (get dp_ml##getDate())##toIsoString(Js.some Js._true, Js.null) 
	else (Js.string "none")))
    Js.null);
  (get_el "label_ml")##innerHTML <- 
    (get dp_ml##getDate())##toIsoString(Js.some Js._true, Js.null)

let _ = 
  let i = get_input "ck-0" in
  i##onclick <- Dom_html.handler
      (fun _ -> dp_custom##setShowFixedNumWeeks(i##checked); Js._true)
  let i = get_input "ck-1" in
  i##onclick <- Dom_html.handler
      (fun _ -> dp_custom##setShowOtherMonths(i##checked); Js._true)
  let i = get_input "ck-2" in
  i##onclick <- Dom_html.handler
      (fun _ -> dp_custom##setExtraWeekAtEnd(i##checked); Js._true)
  let i = get_input "ck-3" in
  i##onclick <- Dom_html.handler
      (fun _ -> dp_custom##setShowWeekNum(i##checked); Js._true)
  let i = get_input "ck-4" in
  i##onclick <- Dom_html.handler
      (fun _ -> dp_custom##setShowWeekdayNames(i##checked); Js._true)
  let i = get_input "ck-5" in
  i##onclick <- Dom_html.handler
      (fun _ -> dp_custom##setAllowNone(i##checked); Js._true)
  let i = get_input "ck-6" in
  i##onclick <- Dom_html.handler
      (fun _ -> dp_custom##setShowToday(i##checked); Js._true)
  let i = get_input "ck-7" in
  i##onclick <- Dom_html.handler
      (fun _ -> dp_custom##setUseNarrowWeekdayNames(i##checked); Js._true)
  let i = get_input "ck-8" in
  i##onclick <- Dom_html.handler
      (fun _ -> dp_custom##setUseSimpleNavigationMenu(i##checked); Js._true)
