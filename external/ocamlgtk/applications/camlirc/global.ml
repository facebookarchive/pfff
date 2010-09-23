(**************************************************************************)
(*     Lablgtk - Camlirc                                                  *)
(*                                                                        *)
(*    * You are free to do anything you want with this code as long       *)
(*      as it is for personal use.                                        *)
(*                                                                        *)
(*    * Redistribution can only be "as is".  Binary distribution          *)
(*      and bug fixes are allowed, but you cannot extensively             *)
(*      modify the code without asking the authors.                       *)
(*                                                                        *)
(*    The authors may choose to remove any of the above                   *)
(*    restrictions on a per request basis.                                *)
(*                                                                        *)
(*    Authors:                                                            *)
(*      Nobuaki Yoshida  <nyoshi@dd.iij4u.or.jp>                          *)
(*      Jacques Garrigue <garrigue@kurims.kyoto-u.ac.jp>                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: global.ml 1354 2007-07-20 04:18:38Z garrigue $ *)
open Message_utils

class global_signals =
object
  inherit GUtil.ml_signals []
end 

let mem_space = Str.regexp "[ \t]+"

let ctcp_status label who ctcp msg =
  label#set_text (who^":"^(Ctcp.get_ctcp_string ctcp)^
		  (match msg with None -> "" | Some msg -> ":"^msg))

class global_view ~(handler:Message_handler.irc_message_handler) 
    ?packing ?show () =
  let vb = GPack.vbox ?packing ?show ()
  in
  let hb = GPack.hbox ~packing:vb#pack ()
  and adj = GData.adjustment ()
  in
  let view = GBroken.text ~vadjustment:adj ~packing:(hb#pack ~expand:true) ()
  and sb = GRange.scrollbar `VERTICAL ~adjustment:adj ~packing:hb#pack ()
  and h = handler
  and status_frame = GBin.frame ~shadow_type:`IN ~packing:vb#pack ()
  in
  let status_l = GMisc.label ~text:"" ~packing:status_frame#add 
      ~xalign:0.0 ()
  in
  let message_text nick channel message =
    Printf.sprintf "<%s:%s> %s" channel nick message
  and server_message_text message = "***  "^message
  and my_message_text channel message =
    print_text view (Printf.sprintf ">%s:%s< %s" 
		       channel ((handler#server)#nick ()) message)
  in 
  let m_check m =
    match m with 
      (Some (f, _, _) , Message.MSG_PRIVATE, Some [chan; mes]) ->
	begin
	  try match Ctcp.check_ctcp mes with 
	    None -> print_text view (message_text f chan mes)
	  | Some (ctcp, arg) -> ctcp_status status_l f ctcp arg
	  with Ctcp.Unknown_ctcp _ -> ()
	end
    | (Some (n, _, _), Message.MSG_PART, Some [c; m]) ->
	begin
	  print_text view ("***  "^n^" has left "^c^" ("^m^")"); 
	  ()
	end
    | (Some (n, _, _), Message.MSG_QUIT, Some [m]) ->
	begin
	  print_text view ("***  "^n^" has left IRC. ("^m^")"); 
	  ()
	end
    | (Some (n, _, _), Message.MSG_NOTICE, Some [c; m]) ->
	begin 
	  try 
	    match Ctcp.check_ctcp m with 
	      None -> ()
	    | Some(ctcp, arg) ->
		print_text view
		  (">"^n^"< CTCP_"^(Ctcp.get_ctcp_string ctcp)^":"
		   ^(begin
		     match arg with 
		       Some arg -> arg
		     | None -> ""
		   end))
	  with Ctcp.Unknown_ctcp _ -> ()
	end
    | _ -> ()
  and r_check r =
    match r with 
      Reply.Connection _ -> ()
    | Reply.Command (f,cr,arg)  ->
	begin
	  match (f,cr,arg) with
	    (_, Reply.RPL_WHOISUSER, Some [_;nick;uname;host;_;realname]) ->
	      print_text view (nick^" is "^
			       nick^"!"^uname^"@"^host^"("^realname^")")
	  | (_, Reply.RPL_WHOISSERVER, Some [_;nick;server;info]) ->
	      print_text view ("on via server "^server^"("^info^")")
	  | (_, Reply.RPL_WHOISOPERATOR, Some [_;nick;info]) ->
	      print_text view (nick^" "^info)
	  | (_, Reply.RPL_WHOISIDLE, Some [_;nick;idletime;info]) ->
	      print_text view (nick^" "^idletime^" "^info)
	  | (_, Reply.RPL_WHOISCHANNELS, Some [_;nick;info]) ->
	      print_text view ("channels:"^info)
	  | (_, Reply.RPL_WHOWASUSER, Some [_;nick;uname;host;realname]) ->
	      print_text view (nick^" was "^
			       nick^"!"^uname^"@"^host^"("^realname^")")
	  | (_, Reply.RPL_ENDOFWHOWAS, _ ) -> ()
	  | (_, Reply.RPL_ENDOFWHOIS, _ ) -> ()
	  | _ -> ()
	end
    | Reply.Error _ -> ()
  in      
  object (self)
    inherit GObj.widget hb#as_widget
    val view = view
    val mutable messigid = None
    val mutable repsigid = None
    method channelname = (handler#server)#nick ()
    method my_message = my_message_text
    initializer
      h#connect#connected
	~callback:
	(fun () ->
	  messigid <- Some (h#connect#message ~callback:m_check);
	  repsigid <- Some (h#connect#reply ~callback:r_check));
      h#connect#disconnected 
	~callback:
	(fun () -> 
	  begin
	    begin
	      match messigid with 
		Some messigid -> 
		  begin 
		    h#message_signal#disconnect messigid;
		    ()
		  end
	      | None -> ()
	    end;
	    begin
	      match repsigid with 
		Some repsigid -> 
		  begin 
		    h#reply_signal#disconnect repsigid;
		    ()
		  end
	      | None -> ()
	    end;
	    view#delete_text ~start:0 ~stop:view#length;
	  end);
      ()
  end


