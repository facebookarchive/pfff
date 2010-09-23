(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

let file_name = 
  try
    Sys.argv.(1)
  with _ -> 
    prerr_endline "Usage : unicode_viewer <file_name>"; 
    exit 1 
;;

GtkMain.Main.init ();;

let f_to_string n = 
  let ic = open_in_bin n in 
  let s = ref "" in
    try while true do
      s:= !s ^ (input_line ic) ^ "\n"
    done;
      !s
    with End_of_file -> close_in ic ; !s
     
let main () = 
  let w = GWindow.window 
	    ~width:640 ~height:480 ~title:"Unicode Viewer" ()
  in
  let sw = GBin.scrolled_window ~packing:(w#add) () in
  let b = GText.buffer () in
  let s = f_to_string file_name in
  b#set_text s;
  let tv = GText.view ~buffer:b ~packing:(sw#add) () in
  w#show ();;


main () ;; 

GMain.Main.main ();;
