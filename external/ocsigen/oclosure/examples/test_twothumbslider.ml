(**
   OClosure Project - 2010

   Class goog.ui.TwoThumbSlider
   
   @author Charles Oran
   @version 0.2
*)
open Goog

module D = Dom_html
let d = D.document

let dom = jsnew Gdom.domHelper()

let (+.) a b = (Js.to_float a) +. (Js.to_float b)

let string_of_float_t n = string_of_float (Js.to_float n)

let _ =
  let el = Js.Opt.get (d##getElementById (Js.string "s1"))
    (fun () -> assert false) 
  and s = jsnew Ui.twoThumbSlider(Js.null) in
    s##decorate(el);
    s##addEventListener(
      Ui.Component.EventType._CHANGE,
      Js.wrap_callback (fun () -> 
	 let out =  Js.Opt.get (d##getElementById (Js.string "out1"))
	   (fun () -> assert false) 
	 and extent = s##getValue() +. s##getExtent() in
	 let res = "start : "^(string_of_float_t s##getValue())^" end : "^(string_of_float (extent)) in
	   dom##setTextContent(out,Js.string res); Js._true),
      Js.null)
  
let _ =
  let s2 = jsnew Ui.twoThumbSlider(Js.null) in
    s2##setOrientation(Ui.SliderBase.Orientation._VERTICAL);
    s2##createDom();
    let el = s2##getElement() in
      el##style##width <- Js.string "20px";
      el##style##height <- Js.string "200px";
      s2##render(Js.some d##body);
      s2##setStep(Js.null);
      s2##addEventListener(
	Ui.Component.EventType._CHANGE,
	Js.wrap_callback (fun () -> 
	   let out2 =  Js.Opt.get (d##getElementById (Js.string "out2"))
	     (fun () -> assert false) 
	   and extent = s2##getValue() +. s2##getExtent() in
	   let res = "start : "^(string_of_float_t s2##getValue())^" end : "^(string_of_float (extent)) in
	     dom##setTextContent(out2,Js.string res); Js._true),
	Js.null)
