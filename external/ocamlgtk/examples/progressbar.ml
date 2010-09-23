(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: progressbar.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open GMain

let main () =

  let window = GWindow.window ~border_width: 10 () in
  window#connect#destroy ~callback:Main.quit;

  let table = GPack.table ~rows:3 ~columns:2 ~packing: window#add () in
  
  GMisc.label ~text:"Progress Bar Example" ()
    ~packing:(table#attach ~left:0 ~right:2 ~top:0 ~expand:`X ~shrink:`BOTH);
  
  let pbar =
    GRange.progress_bar ~pulse_step:0.01 ()
      ~packing:(table#attach ~left:0 ~right:2 ~top:1
                  ~expand:`BOTH ~fill:`X ~shrink:`BOTH) in

  let ptimer = Timeout.add ~ms:50 ~callback:(fun () -> pbar#pulse(); true) in

  let button = GButton.button ~label:"Reset" ()
      ~packing:(table#attach ~left:0 ~top:2
                  ~expand:`NONE ~fill:`X ~shrink:`BOTH) in
  button#connect#clicked ~callback:(fun () -> pbar#set_fraction 0.);

  let button = GButton.button ~label:"Cancel" ()
      ~packing:(table#attach ~left:1 ~top:2
                  ~expand:`NONE ~fill:`X ~shrink:`BOTH) in
  button#connect#clicked ~callback:Main.quit;

  window#show ();
  Main.main ()

let _ = main ()
