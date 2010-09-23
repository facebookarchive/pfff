(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

open GMain

(* On Windows, the channel will be set to non blocking mode. 
   The argument given to [callback] may no be UTF-8 encoded.
   The redirection stops as soon as [callbacks] return [false]
   or an error occured *)
let channel_redirector channel callback = 
  let cout,cin = Unix.pipe () in
  Unix.dup2 cin channel ;
  let channel = Io.channel_of_descr cout in
  let len = 80 in
  let buf = String.create len in
  Io.add_watch channel ~prio:0 ~cond:[`IN; `HUP; `ERR] ~callback:
    begin fun cond -> 
      try if List.mem `IN cond then begin
	(* On Windows, you must use Io.read *)
	let len = Io.read channel ~buf ~pos:0 ~len in
	len >= 1 && (callback (String.sub buf 0 len)) 
      end
      else false
      with e -> callback 
       ("Channel redirector got an exception: " ^ (Printexc.to_string e)); 
       false
     end

let () = 
  let _l = Main.init () in
  let w = GWindow.window ~width:300 ~height:200 () in
  let notebook = GPack.notebook ~packing:w#add () in
  let redirect channel name = 
    let buffer = GText.buffer () in
    let sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC 
             () 
    in
    let label = GMisc.label ~markup:name () in
    let _ = notebook#prepend_page ~tab_label:label#coerce sw#coerce in
    let _text = GText.view ~buffer ~editable:false ~packing:sw#add () in
    channel_redirector channel (fun c -> buffer#insert c; true )
  in	
  redirect Unix.stdout "Std Out";
  redirect Unix.stderr "Std Error";
  let _ = 
  Timeout.add 500 (fun () -> try
		       Pervasives.print_endline "Hello print_endline";
		       true
		       with e -> prerr_endline (Printexc.to_string e); false)
  ,Timeout.add 500 (fun () -> 
		      Printf.printf "Hello printf\n%!";
		      true)  
  ,Timeout.add 500 (fun () -> 
		      Format.printf "Hello format@.";
		      true),
  Timeout.add 5000 (fun () ->
		       Pervasives.prerr_endline "Hello prerr_endline";
		       true)
  in
  let _ = w#connect#destroy quit in
  w#show ();
  main ()
