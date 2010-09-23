(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

open Printf
open GMain

let print_msg msg = printf "%s" msg; flush stdout
let flush_stdout () = flush stdout

open Unix

let clear_func name () =
  printf "(%s) Cleared\n" name;
  flush_stdout ()

let get_func name (context: GObj.selection_context) ~info ~time =
  printf "(%s) selection_handle: target[%s]\n" name context#target;
  flush_stdout ();
  let tm = Unix.localtime (Unix.gettimeofday ()) in
  let data = Printf.sprintf "(%s) %d/%d/%d %d::%d::%d"
    name
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec in
  context#return data

let clicked name () =
  clipboard#set_contents ~targets:["STRING"] ~get:(get_func name)
	~clear:(clear_func name)

let main () =
  let name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  let window = GWindow.window ~title:"Clipboard Puts" ~border_width:10 () in
  window#connect#destroy ~callback:GMain.quit;

  let button =
    GButton.toggle_button ~label:"Claim Selection" ~packing:window#add () in
  button#connect#clicked ~callback:(clicked name);

  window#show ();
  GMain.main ()

let _ = Printexc.print main ()
