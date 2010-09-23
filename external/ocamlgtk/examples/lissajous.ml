(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: lissajous.ml 1347 2007-06-20 07:40:34Z guesdon $ *)
(* Lissajous 図形 *)

open GMain


let main () =
  let window = GWindow.window ~border_width: 10 () in
  window#event#connect#delete
     ~callback:(fun _ -> prerr_endline "Delete event occured"; true);
  window#connect#destroy ~callback:Main.quit;
  let vbx = GPack.vbox ~packing:window#add () in
  let quit = GButton.button ~label:"Quit" ~packing:vbx#add () in
  quit#connect#clicked ~callback:window#destroy;
  let area = GMisc.drawing_area ~width:200 ~height:200 ~packing:vbx#add () in
  let drawing = area#misc#realize (); new GDraw.drawable (area#misc#window) in
  let m_pi = acos (-1.) in
  let c = ref 0. in
  let expose_event _ =
    drawing#set_foreground `WHITE;
    drawing#rectangle ~filled:true ~x:0 ~y:0 ~width:200 ~height:200 ();
    drawing#set_foreground `BLACK;
(*    drawing#line x:0 y:0 x:150 y:150; 
      drawing#polygon filled:true [10,100; 35,35; 100,10; 10, 100];
*)
    let n = 200 in
    let r = 100. in
    let a = 3 in let b = 5 in 
    for i=0 to n do
      let theta0 = 2.*.m_pi*.(float (i-1))/. (float n) in
      let x0 = 100 + (truncate (r*.sin ((float a)*.theta0))) in
      let y0 = 100 - (truncate (r*.cos ((float b)*.(theta0+. !c)))) in
      let theta1 = 2.*.m_pi*.(float i)/.(float n) in
      let x1 = 100 + (truncate (r*.sin((float a)*.theta1))) in
      let y1 = 100 - (truncate (r*.cos((float b)*.(theta1+. !c)))) in
      drawing#line ~x:x0 ~y:y0 ~x:x1 ~y:y1
    done;  
    false
  in 
  area#event#connect#expose ~callback:expose_event;
  let timeout _ = c := !c +. 0.01*.m_pi;
                  expose_event ();
		  true in 
  Timeout.add ~ms:500 ~callback:timeout;
  window#show ();
  Main.main ()

let _ = Printexc.print main()
