(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: radiobuttons.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open GMain

let main () =

  let window = GWindow.window ~title: "radio buttons" ~border_width: 0 () in
  window#connect#destroy ~callback:Main.quit;

  let box1 = GPack.vbox ~packing: window#add () in

  let box2 = GPack.vbox ~spacing:10 ~border_width: 10 ~packing: box1#add () in

  let button1 = GButton.radio_button ~label:"button1" ~packing: box2#add () in
  button1#connect#clicked ~callback:(fun () -> prerr_endline "button1");

  let button2 = GButton.radio_button ~group:button1#group ~label:"button2"
      ~active:true ~packing: box2#add () in
  button2#connect#clicked ~callback:(fun () -> prerr_endline "button2");

  let button3 = GButton.radio_button
      ~group:button1#group ~label:"button3" ~packing: box2#add () in
  button3#connect#clicked ~callback:(fun () -> prerr_endline "button3");

  let separator =
    GMisc.separator `HORIZONTAL ~packing: box1#pack () in

  let box3 = GPack.vbox ~spacing: 10 ~border_width: 10
      ~packing: box1#pack () in

  let button = GButton.button ~label: "close" ~packing: box3#add () in
  button#connect#clicked ~callback:Main.quit;
  button#grab_default ();

  window#show ();

  Main.main ()

let _ = main ()
