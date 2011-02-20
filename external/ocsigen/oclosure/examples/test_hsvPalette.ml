let p = jsnew Goog.Ui.hsvPalette(Js.null, Js.null, Js.null)
let pSmall = jsnew Goog.Ui.hsvPalette(Js.null, Js.null, 
				    Js.some (Js.string"goog-hsv-palette-sm"))

(*let linkColors () = p##other##setColor(p##getColor())*)

let _ =
  p##render(Js.null);
  pSmall##render(Js.null);
  
  ignore(Goog.Events.listen
    (Goog.Tools.Union.i1 p)
    Goog.Ui.Component.EventType._ACTION
    (Js.wrap_callback (fun () -> pSmall##setColor(p##getColor())))
    Js.null);
  ignore(Goog.Events.listen
    (Goog.Tools.Union.i1 pSmall)
    Goog.Ui.Component.EventType._ACTION
    (Js.wrap_callback (fun () -> p##setColor(pSmall##getColor())))
    Js.null);

    
