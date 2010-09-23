(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: combo.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open GMain

let make_arrow_label combo ~label ~string =
  let item = GList.list_item () in (* no packing here, it blocks GTK *)
  let hbox = GPack.hbox ~spacing:3 ~packing:item#add () in
  GMisc.arrow ~kind:`RIGHT ~shadow:`OUT ~packing:hbox#pack ();
  GMisc.label ~text:label ~packing:hbox#pack ();
  combo#set_item_string item string;
  combo#list#add item;
  item

let main () =
  let window = GWindow.window ~border_width:10 () in
  window#connect#destroy ~callback:Main.quit;
  let combo = GEdit.combo ~packing:window#add () in
  make_arrow_label combo ~label:"First item" ~string:"1st item";
  make_arrow_label combo ~label:"Second item" ~string:"2nd item";
  window#show ();
  Main.main ()
  
let _ = main ()
