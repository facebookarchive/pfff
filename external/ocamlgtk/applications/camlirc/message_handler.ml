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

(* $Id: message_handler.ml 1354 2007-07-20 04:18:38Z garrigue $ *)
open Unix
open Str

exception Server_not_configured

class irc_message_signals 
    ~(message: Message.message GUtil.signal)
    ~(reply: Reply.reply GUtil.signal)
    ~(connected: unit GUtil.signal) ~(disconnected: unit GUtil.signal) 
    ~(init_complete: unit GUtil.signal) =
  object
    inherit GUtil.ml_signals [connected#disconnect;disconnected#disconnect;
			      message#disconnect;reply#disconnect]
    method message = message#connect ~after
    method reply = reply#connect ~after
    method connected = connected#connect ~after
    method init_complete = init_complete#connect ~after
    method disconnected = disconnected#connect ~after
  end

exception Rest of string 

type message_data = Quit | Data of string


class irc_message_handler ?(server: Server.server_info option) () = 
  let reply_signal = new GUtil.signal ()
  and connected_signal = new GUtil.signal ()
  and disconnected_signal = new GUtil.signal ()
  and message_signal = new  GUtil.signal ()
  and init_complete_signal = new  GUtil.signal ()
  in
  let main_read_loop in_c =
    try
      while true do
	let s = input_line in_c
	in
	try
	  match (Parser.process_one_message
		   (Eucjp.ntol (String.sub 
				  s 0 
				  ((String.length s) -1)))) with
	    Parser.MSG m -> message_signal#call m
	  | Parser.REP r -> reply_signal#call r
	with 
	  Not_found -> ()
	| Reply.Unknown_Reply n -> print_int n
      done
    with
      End_of_file -> 
	begin
	  disconnected_signal#call (); Thread.exit ()
	end
    | Unix.Unix_error _ -> 
	begin
	  disconnected_signal#call ();
	  Thread.exit ()
	end
    | Sys_error _ ->
	begin
	  disconnected_signal#call (); Thread.exit ()
	end
  and main_write_loop(out_c,message_channel,disconnect_channel) =
      try
      while true do
	let data = Event.sync (Event.receive message_channel)
	in
	begin
	  match data with
	    Data m ->
	      begin
		output_string out_c (Eucjp.lton m);
		flush out_c;
		Thread.yield();
	      end
	  | Quit ->
	      begin
		Thread.exit ()
	      end

	end
      done
    with Sys_error _ -> Thread.exit ()

  in
  object (self)
    val mutable server = server
    method message_signal = message_signal
    method reply_signal = reply_signal
    method connected_signal = connected_signal
    method disconnected_signal = disconnected_signal
    method init_complete_signal = init_complete_signal
    method set_server s = server <- Some s
    method reload_server () = 
      match server with
	Some s -> server <- Some (s#self)
      |	None -> ()
    val mutable read_thread = None
    val mutable write_thread = None
    val mutable in_chan = Pervasives.stdin
    val mutable out_chan = Pervasives.stdout
    val mutable message_channel : message_data Event.channel = 
      Event.new_channel ()
    val mutable disconnect_channel : unit Event.channel = Event.new_channel ()
    val mutable connected = false
    method connect = 
      new irc_message_signals 
	~message:message_signal ~reply:reply_signal
	~connected:connected_signal ~disconnected:disconnected_signal
	~init_complete:init_complete_signal
    method server = 
      match server with 
	Some s -> s 
      | None -> raise Server_not_configured
    method send_message m =
      let
	  ms = Message.construct_message_string m
      in
      Event.sync (Event.send message_channel (Data ms)); 
      Thread.yield();
      ()
    method disconnect_write_channel () =
      Event.sync (Event.send message_channel Quit)
    method emit_init_complete_signal () = init_complete_signal#call ()
    method status_connected () = connected
    method start_connection () = 
      let (in_c, out_c) = 
	try 
	  ThreadUnix.open_connection ((self#server)#sock_addr() )
	with Server.Config_error ->
	  begin
	    self#server#make_server_addr ();
	    ThreadUnix.open_connection ((self#server)#sock_addr() )
	  end
      and _ = message_channel <- Event.new_channel ()
      and _ = disconnect_channel <- Event.new_channel ()
      in 
      let r = Thread.create main_read_loop in_c
      and w = 
	Thread.create main_write_loop
	  (out_c,message_channel,disconnect_channel)
      in
      read_thread <- Some r ; write_thread <- Some w; 
      in_chan <- in_c; out_chan <- out_c;
      connected <- true;
      connected_signal#call ()
    method kill_connection ()  =
      Unix.shutdown_connection in_chan;
      connected <- false
    method initial_sequence () =
      if not ((self#server)#passwd () = "") then
	self#send_message
	  (None, Message.MSG_PASS, Some [(self#server)#passwd ()]);
      self#send_message
	(None, Message.MSG_NICK, Some [(self#server)#nick ()]);
      self#send_message
	(None, Message.MSG_USER, Some
	   [(self#server)#username (); 
	    "0"; "*"; ":"^((self#server)#fullname ())]);
      ()
    method final_sequence () =
      let
	  ms = Message.construct_message_string
	  (None, Message.MSG_QUIT, 
	   Some [":"^((self#server)#quit_message ())])
      in
      Event.sync (Event.send message_channel (Data ms));
      Event.sync (Event.send message_channel Quit);
      ()
    initializer
      self#connect#disconnected
	~callback:
	(fun _ -> self#disconnect_write_channel ());
      ()
  end
    


