(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: link_button.ml 1385 2007-09-27 02:12:42Z garrigue $ *)

open GMain

let main () =

  let window = GWindow.window ~title: "Link button" ~border_width: 0 () in

  let box = GPack.vbox ~packing: window#add () in

  let button = GButton.link_button 
    "http://HELLO.ORG" 
    ~label:"BYE" ~packing:box#add () 
  in
  button#set_uri "GHHHHH";
  Format.printf "Got:%a@." GUtil.print_widget button;
  GtkButton.LinkButton.set_uri_hook 
    (fun _ s -> Format.printf "Got url '%s'@." s;   button#set_uri "AGAIN");
  window#connect#destroy GMain.quit;
  window#show ();
  Main.main ()

let _ = main ()
