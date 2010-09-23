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

(* $Id: irc_widget.ml 1354 2007-07-20 04:18:38Z garrigue $ *)

class irc_widget_signals ~(url:string GUtil.signal) =
object
  inherit GUtil.ml_signals [url#disconnect]
  method url = url#connect ~after
end 

class irc_widget ?packing ?show () =
  let box = GPack.vbox ?packing ?show ()
  in
  let serverlist = new Server.server_info_list ~servers:[]
  in
  let _ = serverlist#load_settings ~file:Constants.config_file
  and h = new Message_handler.irc_message_handler ()
  in
  let channels = new Channelview.channels ~handler:h  ()
  and controlbox = GPack.hbox ~packing:box#pack ~spacing:4 ~border_width:2 ()
  in
  let url = new GUtil.signal ()
  in
  let channel_factory_list = 
    let text_chan = new Channel.channel_factory
    in
    List.map ~f:Cf_manager.channel_factory_manager#add_channel_factory
      [text_chan]
  in
  let control =
    new Control.irc_control ~handler:h ~channels ~servers:serverlist
      ~packing:controlbox#pack ()
  in
  let _ = serverlist#connect#changed 
      ~callback:
      (fun () ->
	let 
	    s = control#server_selection#entry#text
	in
	begin
	  control#server_selection#set_popdown_strings
	    (serverlist#server_names ());
	  control#server_selection#entry#set_text s
	end)
  in
  let config_button = 
    GButton.button ~label:"CONFIG" ~packing:controlbox#add () 
  in
  let config = new Control.config_dialog ~settings:serverlist ()
  in
  let _ = config_button#connect#clicked
      ~callback:
      (fun () -> 
	let
	    c = new Control.config_dialog ~settings:serverlist ()
	in
	c#show())
  in
  let _ = box#pack ~expand:true channels#coerce
  and entrybox =
    new Entry.message_entry ~packing:box#pack ~handler:h 
      ~channels ()
  and globalview = 
    new Global.global_view ~packing:box#add ~handler:h ()
  in
  let _ =
    begin
      entrybox#connect#message 
	~callback:
	(fun s -> 
	  begin
	    channels#send_message s;
	    try
	      begin
		globalview#my_message
		  ((channels#current_channel ())#channelname) s; ()
	      end
	    with Channelview.No_channel -> ();
	      ()
	  end)
    end
  in
  object
    method url = url
    method connect = new irc_widget_signals ~url
    initializer
      box#connect#destroy ~callback:(fun _ -> Control.disconnect h ());
      h#connect#message 
	~callback:
	(fun m ->
	  match m with 
	    (_,Message.MSG_PING, Some [s]) ->
	      h#send_message(None, Message.MSG_PONG, 
			     Some [":"^s])
	  |  _ -> ());
      ()
  end
