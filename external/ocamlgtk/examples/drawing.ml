(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: drawing.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open GMain

let window = GWindow.window ()
let area = GMisc.drawing_area ~packing:window#add ()

let w = area#misc#realize (); area#misc#window
let drawing = new GDraw.drawable w

let redraw _ =
  drawing#polygon ~filled:true
    [ 10,100; 35,35; 100,10; 165,35; 190,100;
      165,165; 100,190; 35,165; 10,100 ];
  false

let _ =
  window#connect#destroy ~callback:Main.quit;
  area#event#connect#expose ~callback:redraw;
  window#show ();
  Main.main ()
