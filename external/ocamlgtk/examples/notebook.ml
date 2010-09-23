(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: notebook.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open GMain

let main () =
  let window = GWindow.window ~title:"Notebook" ~border_width:10 () in
  window#connect#destroy ~callback:Main.quit;
  let notebook = GPack.notebook ~packing:window#add () in
  let button = GButton.button ~label:"Page 1" 
    ~packing:(fun w -> ignore (notebook#append_page w)) () in
  button#connect#clicked ~callback:
    (fun () -> prerr_endline "Hello again - cool button 1 was pressed");

  let button = GButton.button ~label:"Page 2" 
   ~packing:(fun w -> ignore (notebook#append_page w))
    () in
  button#connect#clicked ~callback:
    (fun () -> prerr_endline "Hello again - cool button 2 was pressed");
  notebook#connect#switch_page 
    ~callback:(fun i -> prerr_endline ("Page switch to " ^ string_of_int i));
  button#connect#clicked ~callback:
    (fun () -> prerr_endline "Coucou");
  window#show ();
  Main.main ()

let _ = main ()

(* Local Variables: *)
(* compile-command: "ocamlc -I ../src -w s lablgtk.cma gtkInit.cmo notebook.ml" *)
(* End: *)
