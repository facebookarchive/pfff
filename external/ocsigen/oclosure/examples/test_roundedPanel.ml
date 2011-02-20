open Goog
open Tools

let d = Dom_html.document

let getElement i = Js.Opt.get (d##getElementById(Js.string i)) 
    (fun () -> assert false)

let borderWidth = 1
let borderColor = Js.string "#000"
let radius = 20
let backgroundColor = Js.string "#EEE"
let corners = 15


    
(**
Decorates roundedPanel node through a RoundedPanel instance.
*)
let decorateRoundedPanel () =
  (*if (rp) {
   rp.dispose();
   rp = null;
   }*)

(* Set the dimensions of the panel and decorate roundedPanel. *)
  let roundedPanelNode = getElement "roundedPanel" in
  let rp = Ui.RoundedPanel.create radius
                                  borderWidth
                                  borderColor
                                  (Js.some backgroundColor)
                                  (Js.some corners) Js.null in
   rp##decorate(roundedPanelNode)


(**
   Sets event handlers on the 'input' and 'select' elements containing
   values needed to create the rounded panel.
*)
let _ = decorateRoundedPanel();


