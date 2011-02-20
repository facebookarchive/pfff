let get_el s = Js.Opt.get (Dom_html.document##getElementById(Js.string s))
    (fun _ -> Dom_html.window##alert (Js.string s); assert false)

let sel = get_el "sel"

let cp = jsnew Goog.Ui.colorPicker (Js.null, Js.null)
let _ = 
  cp##setSize(7);
  cp##setColors(Goog.Ui.ColorPicker._SIMPLE_GRID_COLORS);
  ignore(Goog.Events.listen
    (Goog.Tools.Union.i1 cp)
    Goog.Ui.ColorPicker.EventType._CHANGE
    (Js.wrap_callback(fun () -> sel##innerHTML <- cp##getSelectedColor()))
    Js.null);
  cp##render(Js.some (get_el "colorPicker"))
