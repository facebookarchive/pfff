(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: hello.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open GMain

let window = GWindow.window ~border_width: 10 ()

let button = GButton.button ~label:"Hello World" ~packing: window#add ()

let main () =
  window#event#connect#delete 
    ~callback:(fun _ -> prerr_endline "Delete event occured"; true);
  window#connect#destroy ~callback:Main.quit;
  button#connect#clicked ~callback:(fun () -> prerr_endline "Hello World");
  button#connect#clicked ~callback:window#destroy;
  window#show ();
  Main.main ()

let _ = Printexc.print main ()
