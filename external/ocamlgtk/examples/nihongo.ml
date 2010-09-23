(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: nihongo.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

(* これを実行する前にLC_ALL=ja_JP.EUCなどと指定しなければならない *)

open GMain

let window = GWindow.window ()
let box = GPack.vbox ~packing: window#add ()
let text = GText.view ~packing: box#add ()
let button = GButton.button ~label: "終了" ~packing: box#add ()
let label = GMisc.label ~text:"これには影響しない" ~packing: box#add ()

let _ =
  window#connect#destroy ~callback:Main.quit;
  text#buffer#insert "こんにちは";
  text#misc#set_size_chars ~width:20 ~height:5 ();
  let style = button#misc#style#copy in
  button#misc#set_style style;
  style#set_bg [`NORMAL,`NAME "green"; `PRELIGHT,`NAME "red"];
  button#connect#clicked ~callback:Main.quit

let _ =
  window#show ();
  Main.main ()
