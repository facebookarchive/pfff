(** 
    * OClosure Project - 2010
    * Class goog.ui.Ratings
    *
    * A UI Control used for rating things, i.e. videos on Google Video.
    *
    * @author : Oran Charles
    * @version 0.2
    * @see 'goog.ui.Component'
*)
open Goog
open Js

module D = Dom_html
let d = D.document

let _ =
  let rw1 = jsnew Ui.ratings () 
  and text =  Js.Opt.get (d##getElementById (Js.string "lab")) 
    (fun () -> assert false)
  and dom = jsnew Gdom.domHelper() 
  and test1 = Js.Opt.get (d##getElementById (Js.string "test1"))
      (fun () -> assert false) in

    ignore(Events.listen
      (Tools.Union.i1 rw1)
      Ui.Ratings.Display._CHANGE
      (Js.wrap_callback (fun () -> dom##setTextContent(text,rw1##getValue())))
      Js.null);

    ignore(Events.listen 
      (Tools.Union.i1 rw1)
      Ui.Ratings.Display._HIGHLIGHT_CHANGE
      (Js.wrap_callback (fun () -> dom##setTextContent(text,rw1##getHighlightedValue())))
      Js.null);
    rw1##decorate(test1);

  let rw2 = jsnew Ui.ratings ()
  and test2 = Js.Opt.get (d##getElementById (Js.string "test2"))
      (fun () -> assert false) 
  and array = Js.array [|"1";"2";"3";"4";"5";"6";"7";"8";"9";"10" |] in
    rw2##setRatings(array);
    rw2##render(Js.some test2);
    rw2##setSelectedIndex(3)
    
