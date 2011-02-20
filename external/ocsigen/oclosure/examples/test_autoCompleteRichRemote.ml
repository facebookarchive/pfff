(**
   * OClosure Project - 2010
   * Class goog.ui.AutoComplete
   *
   * 
   * @author : Oran Charles
   * @version 0.2
   * @see 'goog.events.EventTarget'
   * @see 'goog.events.Event'
*)
open Goog
module D = Dom_html
let d = D.document

(*let wikipedia = Js.Opt.get (Dom_html.CoerceTo.iframe (Js.Opt.get (d##getElementById (Js.string "wikipedia"))
  (fun () -> assert false))) (fun () -> assert false)

let makeRichRow_ item itemType itemClassName =
  item##_type <- itemType;
  
  item##render <- (fun node token -> 
    let dom = jsnew Gdom.domHelper() in
    let dom_ = dom##getDomHelper(node) in
    let typeNode = dom_##createTextNode(itemType) in
      dom_##appendChild(typeNode,dom_##createTextNode(itemType));

    let nameNode = dom_##createDom(Js.string "span",Js.null, Js.null) in
      dom_##appendChild(nameNode,dom_##createTextNode(item##name));

     dom_##appendChild(node,typeNode);
     dom_##appendChild(node,nameNode));
  
  item##select <- (fun target ->
		     target##value <- item##name;
		     wikipedia##src <- item##url
		  );
  item

let apple item = makeRichRow_ item (Js.string "Apple") (Js.string "apple")

let citrus item = makeRichRow_ item (Js.string "Citrus") (Js.string "citrus")

let berry item = makeRichRow_ item (Js.string "Berry") (Js.string "berry")

let _ =
  let input = Js.Opt.get (d##getElementById (Js.string "txtInput"))
    (fun () -> assert false) in
  let ac = jsnew Ui.AutoComplete.richRemote(Js.string "autocompleterichremotedata.js",input,Js.null,Js.null) in
    apple Js.array_empty

  (*let setFilter () =
    let checkbox = Js.Opt.get (Dom_html.CoerceTo.input (Js.Opt.get (d##getElementById (Js.string "berryAllergy"))
  (fun () -> assert false))) (fun () -> assert false) in
      if (Js.to_bool checkbox##checked) then
	D.window##alert(Js.string "Bravo!")
      else
	ac##setRowFilter((fun () -> ()))
  in
    D.window##onload <- D.handler ( fun _ -> setFilter(); Js._true)*)*)

		     
		    

      
