(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: $ *)
open GMain

let main () =

  let assistant = GAssistant.assistant () in

  let box = GPack.vbox () 
  in
  ignore (assistant#append_page box#as_widget);
  assistant#set_page_complete box#as_widget true;
  prerr_endline "Complete";
  assistant#set_page_type box#as_widget `SUMMARY;
  let button = GButton.link_button 
    "http://HELLO.ORG" 
    ~label:"BYE" ~packing:box#add () 
  in
  button#set_uri "GHHHHH";
  Format.printf "Got:%a@." GUtil.print_widget button;
  GtkButton.LinkButton.set_uri_hook 
    (fun _ s -> Format.printf "Got url '%s'@." s;   button#set_uri "AGAIN");
  assistant#connect#close GMain.quit;
  assistant#show ();
  Main.main ()

let _ = main ()

