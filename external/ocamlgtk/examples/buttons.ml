(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: buttons.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open GMain

let xpm_label_box ~(window : #GContainer.container)
    ~file ~text ?packing ?(show=true) () =
  if not (Sys.file_exists file) then failwith (file ^ " does not exist");
  let box = GPack.hbox ~border_width: 2 ?packing ~show:false () in
  let pixmap = GDraw.pixmap_from_xpm ~file ~window () in
  GMisc.pixmap pixmap ~packing:(box#pack ~padding:3) ();
  GMisc.label ~text ~packing:(box#pack ~padding:3) ();
  if show then box#misc#show ();
  new GObj.widget_full box#as_widget

let main () =
  let window = GWindow.window ~title:"Pixmap'd Buttons!" ~border_width:10 () in
  window#connect#destroy ~callback:Main.quit;
  let hbox = GPack.hbox ~packing:window#add () in
  let button = GButton.button ~packing:(hbox#pack ~padding:5) () in
  button#connect#clicked ~callback:
    (fun () -> prerr_endline "Hello again - cool button was pressed");
  xpm_label_box ~window ~file:"test.xpm" ~text:"cool button"
    ~packing:button#add ();
  let button = GButton.button ~use_mnemonic:true ~label:"_Coucou" ~packing:(hbox#pack ~padding:5) () in
  button#connect#clicked ~callback:
    (fun () -> prerr_endline "Coucou");
  let button = GButton.button ~stock:`HOME ~packing:(hbox#pack ~padding:5) () in
  button#connect#clicked ~callback:
    (fun () -> prerr_endline "Stock buttons look nice");
  window#show ();
  Main.main ()

let _ = main ()
