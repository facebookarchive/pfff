(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

let start = try
    int_of_string Sys.argv.(1)
  with _ -> 
    prerr_endline "Usage : unicode_table <from> <to>"; 
    exit 1 
;;

let stop = try
   int_of_string Sys.argv.(2)
  with _ -> 
    prerr_endline "Usage : unicode_table <from> <to>"; 
    exit 1 
;;


GtkMain.Main.init ();;
     
let main () = 
  let w = GWindow.window 
	    ~width:640 ~height:480 ~title:"2)view_with_buffer" ()
  in
  let sw = GBin.scrolled_window ~packing:(w#add) () in
  let b = GText.buffer () in
  b#set_text (Printf.sprintf "Unicode characters from %d to %d Click to continue\n" start stop);
  let font = Pango.Font.from_string "Sans 15" in
  let tv = GText.view ~buffer:b ~packing:(sw#add) () in
  let _ = tv#misc#modify_font font in
  ignore (tv#event#connect#button_release 
	    ~callback:
	    (fun _ -> 
	       for i=start to stop do 
		 let c = Printf.sprintf "%d:%s:\n" i (Glib.Utf8.from_unichar i) in
		 b#insert c
	       done;false));
  w#show ();;


main () ;; 

GMain.Main.main ();;
