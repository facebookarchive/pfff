(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: socket.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open GMain

let main () =
  let w = GWindow.window ~title:"Socket example" () in
  w#connect#destroy ~callback:Main.quit;
  let vbox = GPack.vbox ~packing:w#add () in
  let label = GMisc.label ~packing:vbox#pack () in
  w#show ();
  let socket = GWindow.socket ~packing:vbox#add ~height:40 () in
  label#set_text ("XID to plug into this socket: 0x" ^ 
                  Int32.format "%x" socket#xwindow);
  Main.main ()

let _ = main ()
