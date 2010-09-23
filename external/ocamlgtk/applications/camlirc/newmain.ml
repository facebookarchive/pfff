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

(* $Id: newmain.ml 1354 2007-07-20 04:18:38Z garrigue $ *)
let _ = 
  Gdk.Rgb.init ();
  GtkBase.Widget.set_default_visual (Gdk.Rgb.get_visual ());
  GtkBase.Widget.set_default_colormap (Gdk.Rgb.get_cmap ())

let win = GWindow.window ()
let box = GPack.vbox ~packing:win#add ()

let serverlist = new Server.server_info_list ~servers:[]

let channel_factory_list = 
  let text_chan = new Channel.channel_factory
  in
  List.map Cf_manager.channel_factory_manager#add_channel_factory
    [text_chan]

let _ = serverlist#load_settings ~file:Constants.config_file

let h = new Message_handler.irc_message_handler ()

let channels = new Channelview.channels ~handler:h  ()

let control =
  new Control.irc_control ~handler:h ~channels ~servers:serverlist

let menubar = GMenu.menu_bar ~packing:box#pack ()
let menu_factory = new GMenu.factory menubar

let file_menu = menu_factory#add_submenu "File"
and config_menu = menu_factory#add_submenu "Configure"
and operation_menu = menu_factory#add_submenu "Operation"
and channel_menu = menu_factory#add_submenu "Channel"
and help_menu = menu_factory#add_submenu "Help"

let _ = 
  let file_menu_factory = new GMenu.factory file_menu in
  file_menu_factory#add_item "Connect" ~callback:control#connect;
  file_menu_factory#add_item "Disconnect" ~callback:control#disconnect;
  file_menu_factory#add_separator ();
  file_menu_factory#add_item "Quit" ~callback:GMain.Main.quit;

  let config_menu_factory = new GMenu.factory config_menu in
  config_menu_factory#add_item "Server" 
    ~callback:(fun () -> 
      let
	  c = new Control.config_dialog ~settings:serverlist ()
      in
      c#show());

  let operation_menu_factory = new GMenu.factory operation_menu in
  operation_menu_factory#add_item "Join" ~callback:control#join;
  operation_menu_factory#add_item "Priv" ~callback:control#priv;
  operation_menu_factory#add_item "CTCP Message"
    ~callback:(fun () ->
      try 
	(channels#current_channel ())#ctcp_command ()
      with 
	Channelview.No_channel -> ());
  
  let channel_menu_factory = new GMenu.factory channel_menu in
  channel_menu_factory#add_item "Part" 
    ~callback:(fun () -> 
      try 
	(channels#current_channel ())#part_command ()
      with
	Channelview.No_channel -> ());
  channel_menu_factory#add_item "Topic"
    ~callback:(fun () -> 
      try 
	(channels#current_channel ())#topic_command ()
      with
	Channelview.No_channel -> ());
  let help_menu_factory = new GMenu.factory help_menu in
  help_menu_factory#add_item "About"
    ~callback:
    begin fun () ->
      let w = GWindow.dialog ~title:"About" ~modal:true ~position:`CENTER () in
      ignore (GMisc.label ~text:Constants.id ~packing:w#vbox#add 
	        ~width:250 ~height:70 ());
      let ok_b = GButton.button ~label:"OK" ~packing:w#vbox#add () in
      ignore (ok_b#connect#clicked ~callback:w#destroy);
      w#show ()
    end
    
let controlbox = GPack.hbox ~border_width:2 ~packing:box#pack ()

let _ = box#pack ~expand:true channels#coerce

and entrybox =
  new Entry.message_entry ~packing:box#pack ~handler:h 
    ~channels ()

and globalview = 
  new Global.global_view ~packing:box#add ~handler:h ()

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
let _ = 
  h#connect#message 
    ~callback:
    (fun m ->
	match m with 
	  (_,Message.MSG_PING, Some [s]) ->
	    h#send_message(None, Message.MSG_PONG, 
			   Some [":"^s])
	|  _ -> ());
  win#connect#destroy ~callback:GMain.Main.quit;
  win#show();
  GtkThread.main ()
