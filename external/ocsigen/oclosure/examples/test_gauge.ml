(** 
    OClosure Project - 2010
    
    Class goog.ui.Gauge

    @author : Oran Charles
    @version 0.2
    @see 'goog.ui.Component'
*)
open Goog
module D = Dom_html
let d = D.document

let _ =
  let basic = Js.Opt.get (d##getElementById (Js.string "basic"))
    (fun () -> assert false)
  and gauge = jsnew Ui.gauge(120,120,Js.null) in
    gauge##setValue(33,Js.null);
    gauge##render(Js.some basic);

  let colors = Js.Opt.get (d##getElementById (Js.string "colors"))
    (fun () -> assert false)
  and gauge2 = jsnew Ui.gauge(200, 200,Js.null) in
      gauge2##addBackgroundColor(50, 60, Ui.Gauge._RED);
      gauge2##addBackgroundColor(35, 50, Ui.Gauge._YELLOW);
      gauge2##addBackgroundColor(15, 25, Ui.Gauge._GREEN);
      gauge2##setMinimum(15);
      gauge2##setMaximum(60);
      gauge2##setTicks(3, 6);
      gauge2##setValue(40,Js.null);
      gauge2##setTitleBottom(Js.string "RPM");
      gauge2##render(Js.some colors);

   let interactive = Js.Opt.get (d##getElementById (Js.string "interactive"))
    (fun () -> assert false)
   and interactiveGauge = jsnew Ui.gauge(300, 200,Js.null) in
      interactiveGauge##addBackgroundColor(0, 30, Ui.Gauge._RED);
      interactiveGauge##addBackgroundColor(75, 90, Ui.Gauge._YELLOW);
      interactiveGauge##addBackgroundColor(90, 100, Ui.Gauge._RED);
      interactiveGauge##setTitleTop(Js.string "CPU Utilization");
      interactiveGauge##setTicks(5, 2);
      let array = Js.array [| "Idle"; "20%"; "40%"; "60%"; "80%"; "Argh"|] in
      interactiveGauge##setMajorTickLabels(array);
      interactiveGauge##render(Js.some interactive);

      let set_Value () = 
	let sv = (Js.Opt.get (D.CoerceTo.input (Js.Opt.get (d##getElementById (Js.string "v1"))
    (fun () -> assert false))) (fun () -> assert false))##value in 
	  interactiveGauge##setValue(int_of_string (Js.to_string sv), Js.some (Js.string ((Js.to_string sv)^"%")))
	  
      in
	set_Value ();
	let button = Js.Opt.get (d##getElementById (Js.string "v1"))
    (fun () -> assert false) in
	button##onkeyup <- D.handler (fun _ -> set_Value();Js._true)  

