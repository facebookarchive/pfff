(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: clist.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open StdLabels
open GMain

let main () =
  let window = GWindow.window ~title:"CList example" ~width:300 ~height:150 () in
  window#connect#destroy ~callback:Main.quit;

  let vbox = GPack.vbox ~border_width:5 ~packing:window#add () in

  let hbox = GPack.hbox ~packing:vbox#add () in
  let sb =
    GRange.scrollbar `VERTICAL ~packing:(hbox#pack ~from:`END) () in
  let clist =
    GList.clist ~titles:["Ingredients";"Amount"] ~shadow_type:`OUT
      ~packing:hbox#add ~vadjustment:sb#adjustment () in
  clist#connect#select_row ~callback:
    begin fun ~row ~column ~event ->
      let text = clist#cell_text row column in
      Printf.printf "You selected row %d. More specifically you clicked in column %d, and the text in this cell is %s\n\n" row column text;
      flush stdout
    end;

  let hbox = GPack.hbox ~packing:vbox#pack () in

  let button_add = GButton.button ~label:"Add List" ~packing:hbox#add () in
  button_add#connect#clicked ~callback:
    begin fun () ->
      List.iter ~f:(fun t -> ignore (clist#append t))
	[ ["Milk"; "3 Oz"];
	  ["Water"; "6 l"];
	  ["Carrots"; "2"];
	  ["Snakes"; "55"] ]
    end;

  let button_clear = GButton.button ~label:"Clear List" ~packing:hbox#add () in
  button_clear#connect#clicked ~callback:clist#clear;

  let button_hide_show =
    GButton.button ~label:"Hide/Show titles" ~packing:hbox#add () in
  let flag = ref false in
  button_hide_show#connect#clicked ~callback:
    begin fun () ->
      clist#set_titles_show !flag;
      flag := not !flag
    end;

  window#show ();
  Main.main ()

let _ = main ()
