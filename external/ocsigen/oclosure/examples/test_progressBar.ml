(**
 * OClosure Project - 2010
 * Class goog.ui.ProgressBar
 *
 * This creates a progress bar object.
 *
 * @author : Oran Charles
 * @version 0.2
 * @see 'goog.ui.Component'
 *)
open Goog

module D = Dom_html
let d = D.document

let div1 = Js.Opt.get (d##getElementById (Js.string "d"))
  (fun () -> assert false)
let div2 = Js.Opt.get (d##getElementById (Js.string "pb2"))
  (fun () -> assert false)
let out = Js.Opt.get (d##getElementById (Js.string "out"))
  (fun () -> assert false)

let pB1 = jsnew Ui.progressBar ()
let pB2 = jsnew Ui.progressBar ()

let last = ref 0.
let delta = ref 1.

let handler o = 
  if (!last > 100.) or (!last < 0.) then
    delta := -. (!delta);
  last := !last +. !delta;
  pB1##setValue(!last);
  pB2##setValue(!last);
  Js._true

let timer = jsnew Timer.timer (Js.some 20)

let _ =
  pB1##setOrientation (Ui.ProgressBar.Orientation._VERTICAL);
  pB1##render(Js.some div1);

  pB2##setOrientation (Ui.ProgressBar.Orientation._HORIZONTAL);
  pB2##decorate(div2);

  timer##addEventListener (Js.string "tick",Js.wrap_callback handler, Js.null);
  timer##start ();

  let dom = jsnew Gdom.domHelper() in
  pB1##addEventListener(
  Ui.Component.EventType._CHANGE,
  Js.wrap_callback (fun() -> dom##setTextContent(out,Js.string (string_of_float (Js.to_float (pB1##getValue()))^"%"));Js._true),
  Js.null)
    

    
