(**
   * OClosure Project - 2010
   * Class goog.ui.AutoComplete
   *
   * This is the central manager class for an AutoComplete instance.
   * 
   * @author : Oran Charles
   * @version 0.2
   * @see 'goog.events.EventTarget'
   * @see 'goog.events.Event'
*)
open Goog
module D = Dom_html
let d = D.document

let _ =
  let array = [|"OCaml";"O'Browser";"Mon Colonel";"O'Closure";
	      "Caml";"Ocsigen";"Javascript";"Binding";"Closure";
	      "Pikachu";"Haskell";|] 
  in
  let div1 = Js.Opt.get (d##getElementById (Js.string "input1"))
    (fun () -> assert false)
  and div2 = Js.Opt.get (d##getElementById (Js.string "input2"))
    (fun () -> assert false)
  in
    ignore(jsnew Ui.AutoComplete.basic (Js.array array,div1,Js.some (Js.bool false),Js.null));
    jsnew Ui.AutoComplete.basic (Js.array array,div2,Js.some (Js.bool true),Js.null)
