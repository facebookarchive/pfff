(**
   * OClosure Project - 2010
   * Class goog.ui.Charcounter
   *
   * CharCounter widget. Counts the number of characters in a input field or a
   * text box and displays the number of additional characters that may be
   * entered before the maximum length is reached.
   *
   * @author : Oran Charles
   * @version 0.2
   * @see 'Js'
*)
open Goog
module D = Dom_html
let d = D.document

let _ =
  let input1 = Js.Opt.get (d##getElementById (Js.string "input1"))
    (fun () -> assert false)
  and counter1 = Js.Opt.get (d##getElementById (Js.string "counter1"))
    (fun () -> assert false)
  in
    ignore(jsnew Ui.charCounter (input1,counter1,160,Js.null));

  let input2 = Js.Opt.get (d##getElementById (Js.string "input2"))
    (fun () -> assert false)
  and counter2 = Js.Opt.get (d##getElementById (Js.string "counter2"))
    (fun () -> assert false)
  in
  ignore (jsnew Ui.charCounter (input2,counter2,160,Js.some Ui.CharCounter.Display.INCREMENTAL));

  let input3 = Js.Opt.get (d##getElementById (Js.string "input3"))
    (fun () -> assert false)
  and counter3 = Js.Opt.get (d##getElementById (Js.string "counter3"))
    (fun () -> assert false)
  and set10 = Js.Opt.get (d##getElementById (Js.string "set1"))
    (fun () -> assert false)
  and set20 = Js.Opt.get (d##getElementById (Js.string "set2"))
    (fun () -> assert false)
  in
  let test = jsnew Ui.charCounter (input3,counter3,10, Js.null) 
  in
    set10##onclick <- D.handler (fun _ -> test##setMaxLength(10); Js.bool true);
    set20##onclick <- D.handler (fun _ -> test##setMaxLength(20); Js.bool true);

  let input4 = Js.Opt.get (d##getElementById (Js.string "input4"))
    (fun () -> assert false)
  and counter4 = Js.Opt.get (d##getElementById (Js.string "counter4"))
    (fun () -> assert false)
  in
    ignore(jsnew Ui.charCounter (input4,counter4,255,Js.null))
