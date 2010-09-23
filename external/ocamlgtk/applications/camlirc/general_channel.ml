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

(* $Id: general_channel.ml 1354 2007-07-20 04:18:38Z garrigue $ *)
open Message_utils

class general_channel_signals ~(part:string GUtil.signal) 
    ~(privmsg:(string * string) GUtil.signal) 
: I_channel.i_channel_signals =
object
  inherit GUtil.ml_signals [part#disconnect; privmsg#disconnect]
  method part = part#connect ~after
  method privmsg = privmsg#connect ~after
end 

let mem_space = Str.regexp "[ \t]+"

class general_channel ~(handler:Message_handler.irc_message_handler) 
    ?packing ?show () : I_channel.i_channel =
  let server = handler#server
  in
  let vb = GPack.vbox ?packing ?show ()
  and adj = GData.adjustment ()
  and channel_name = server#nick ()
  in
  let topic_hb = GPack.hbox ~packing:vb#pack ()
  in
  let label = GMisc.label ~xalign:0.0 ~text:(server#server ())
      ~packing:topic_hb#pack ()
  and hb = GPack.hbox ~packing:(vb#pack ~expand:true) () 
  in
  let view = GBroken.text ~vadjustment:adj ~packing:(hb#pack ~expand:true) ()
  and sb = GRange.scrollbar `VERTICAL ~adjustment:adj ~packing:hb#pack ()
  and members = GList.clist ~columns:1 ~titles:["nickname"] 
      ~width:100 ~packing:hb#pack ()
  and h = handler
  in
  let message_text nick message =
    let	s = Printf.sprintf "<%s> %s" nick message
    in 
    s
  and server_message_text message = "***  "^message
  and my_message_text message =
    print_text view (Printf.sprintf ">%s< %s" (server#nick ()) message)
  in 
  let privmsg_signal = new GUtil.signal ()
  in
  let m_check m =
    match m with 
      (Some (f, _, _) , Message.MSG_PRIVATE, Some [chan; mes]) ->
	begin
	  try
	    match Ctcp.check_ctcp mes with
	      None -> privmsg_signal#call (f, chan)
	    | Some (ctcp, arg) ->
		begin
		  match ctcp with
		    Ctcp.CTCP_VERSION -> 
		      Ctcp.send_ctcp_reply ~handler ~to_nick:f ~ctcp 
			~message:Constants.id ()
		  | Ctcp.CTCP_PING -> 
		      Ctcp.send_ctcp_reply ~handler ~to_nick:f ~ctcp
			~message:(Ctcp.get_current_time ()) ()
		  | Ctcp.CTCP_TIME -> 
		      Ctcp.send_ctcp_reply ~handler ~to_nick:f ~ctcp
			~message:(Ctcp.get_current_formatted_time ()) ()
		  | Ctcp.CTCP_ACTION -> ()
		end
	  with Ctcp.Unknown_ctcp _ -> ()
	end
    | (Some (f, _, _), Message.MSG_NOTICE, Some [chan; mes]) ->
	begin 
	  try 
	    match Ctcp.check_ctcp mes with 
	      None -> privmsg_signal # call (f, chan)
	    | Some(ctcp, arg) ->
		print_text view
		  (">"^f^"< CTCP_"^(Ctcp.get_ctcp_string ctcp)^":"
		   ^(begin
		     match arg with 
		       Some arg -> arg
		     | None -> ""
		   end))
	  with Ctcp.Unknown_ctcp _ -> ()
	end
    | (None, Message.MSG_NOTICE, Some [chan; mes]) ->
	begin
	  print_text view (">"^chan^"<"^mes)
	end
    | _ -> ()
  and r_check r =
    match r with 
      Reply.Connection (f, cr, arg) ->
	begin
	  match (f, cr, arg) with 
	    (_, Reply.RPL_MYINFO, Some [_;s]) -> 
	      print_text view (server_message_text s); ()
	  | (_, Reply.RPL_YOURHOST, Some [_;s]) -> 
	      print_text view (server_message_text s); ()
	  | (_, Reply.RPL_CREATED, Some [_;s]) -> 
	      print_text view (server_message_text s); ()
	  | (_, Reply.RPL_WELCOME, Some [_;s]) -> 
	      begin
		handler#emit_init_complete_signal ();
		print_text view (server_message_text s); ()
	      end
	  | (_, Reply.RPL_BOUNCE, Some [_;s]) -> 
	      print_text view (server_message_text s); ()
	  | _ -> ()
	end
    | Reply.Error (f, er, arg) ->
	begin
	  match (f, er, arg) with
	    (_, Reply.ERR_NICKNAMEINUSE, Some _) ->
	      Message_utils.nick_config ~handler:h
	  |  (_, Reply.ERR_PASSWDMISMATCH, Some _) ->
	      Message_utils.passwd_config ~handler:h
	  | _ -> ()
	end
    | Reply.Command (f, cr, arg) ->
	begin
	  match (f, cr, arg) with
	    (_, Reply.RPL_LUSERCLIENT, Some [_;m]) ->
	      print_text view (server_message_text m); ()
	  | (_, Reply.RPL_LUSEROP, Some [_;n;m]) ->
	      print_text view (server_message_text (n^" "^m)); ()
	  | (_, Reply.RPL_LUSERUNKNOWN, Some [_;n;m]) ->
	      print_text view (server_message_text (n^" "^m)); ()
	  | (_, Reply.RPL_LUSERCHANNELS, Some [_;n;m]) ->
	      print_text view (server_message_text (n^" "^m)); ()
	  | (_, Reply.RPL_LUSERME, Some [_;m]) ->
	      print_text view (server_message_text m); ()
	  | (_, Reply.RPL_MOTDSTART, Some [_;m]) ->
	      print_text view (server_message_text m); ()
	  | (_, Reply.RPL_MOTD, Some [_;m]) ->
	      print_text view (server_message_text m); ()
	  | (_, Reply.RPL_ENDOFMOTD, Some [_;m]) -> ()
	  | _ -> ()
	end
  in      
  object (self)
    inherit GObj.widget vb#as_widget
    val view = view
    val channelname = channel_name 
    val part = new GUtil.signal ()
    val privmsg = privmsg_signal
    val repsigid = h#connect#reply ~callback:r_check
    val messigid = h#connect#message ~callback:m_check
    method part = part
    method part_command () = ()
    method topic_command () = ()
    method ctcp_command () = ()
    method connect = new general_channel_signals ~part ~privmsg
    method channelname = server#nick ()
    method my_message = my_message_text
    method initialize () = ()
    initializer
      h#connect#disconnected 
	~callback:
	(fun () -> 
	  begin
	    h#reply_signal#disconnect repsigid;
	    h#message_signal#disconnect messigid;
	    ()
	  end);
      ()
  end

class channel_factory : I_channel.i_channel_factory =
  object
    method module_name = "General"
    method new_channel_object
	~handler ~channel_name ~server ?packing ?show () = 
      new general_channel ~handler ?packing ?show ()
  end

