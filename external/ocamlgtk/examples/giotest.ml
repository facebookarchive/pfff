(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

open StdLabels
module Unix = UnixLabels
open GMain

let l = Main.init ()

let fd = Unix.stdin (* Unix.openfile "giotest.ml" [Unix.O_RDONLY] 0 *)
let ch = Io.channel_of_descr fd
let w = GWindow.window ~width:300 ~height:200 ()
let buffer = GText.buffer ()
let text = GText.view ~buffer ~packing:w#add ()

let () =
  prerr_endline "Input some text on <stdin>";
  Io.add_watch ch ~prio:0 ~cond:[`IN; `HUP; `ERR] ~callback:
    begin fun c -> 
      if List.mem `IN c then begin
	let buf = " " in
	(* On Windows, you must use Io.read *)
	let len = Glib.Io.read ch ~buf ~pos:0 ~len:1 in
	len = 1 && (buffer#insert buf; true) end
      else if List.mem `HUP c then begin
	prerr_endline "got `HUP, exiting in 5s" ;
	Timeout.add 5000 (fun () -> Main.quit () ; false) ;
	false end
      else assert false
    end ;
  w#connect#destroy quit;
  w#show ();
  main ()
