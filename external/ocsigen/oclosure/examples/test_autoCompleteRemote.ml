(**
   * OClosure Project - 2010
   * Class goog.ui.AutoComplete.Remote
   *
   * Factory class for building a remote autocomplete widget that autocompletes an inputbox or text area from a data array provided via ajax.
   * 
   * @author : Oran Charles
   * @version 0.2
   * @see 'goog.ui.AutoComplete'
*)
open Goog
module D = Dom_html
let d = D.document

let _ =
  let input = Js.Opt.get (d##getElementById (Js.string "txtInput"))
    (fun () -> assert false) 
  in
    jsnew Ui.AutoComplete.remote (Js.string "data.js",input,Js.null,Js.null) 
