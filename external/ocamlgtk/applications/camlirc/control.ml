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

(* $Id: control.ml 1354 2007-07-20 04:18:38Z garrigue $ *)
class irc_control_signals =
  object
    inherit GUtil.ml_signals []
  end

let rec gen_int_list ~from_n ~to_n =
  if from_n = to_n then []
  else from_n::(gen_int_list ~from_n:(from_n+1) ~to_n)

let join h c = 
  fun () ->
    let w = GWindow.dialog ~title:"JOIN" ~modal:true ~position:`CENTER ()
    in 
    let title = GBin.frame 
	~shadow_type:`OUT ~packing:w#vbox#add ()
    in
    let entry_h = GPack.hbox ~packing:w#vbox#add ()
    and mode_h = GPack.hbox ~packing:w#vbox#add ()
    in
    let e_label = GMisc.label ~text:"Channel Name" ~packing:entry_h#add ()
    and e_entry = GEdit.entry ~packing:entry_h#add ()
    and hbox = GPack.hbox ~packing:w#vbox#add ()
    in
    let ok_b = GButton.button  ~label:"OK" ~packing:hbox#add ()
    and cancel_b = GButton.button  ~label:"cancel" ~packing:hbox#add ()
    in
    let _ = ok_b#connect#clicked 
	~callback:
	(fun () -> c#add_channel e_entry#text "Text";
	  w#destroy ();
	  ());
    and _ = e_entry#connect#activate
	~callback:
	(fun () -> c#add_channel e_entry#text "Text";
	  w#destroy ();
	  ()); 
    and _ = cancel_b#connect#clicked ~callback:(fun () -> w#destroy (); ())
    in
    w#show()

let priv h c = 
  fun () ->
    let w = GWindow.dialog ~title:"PRIV" ~modal:true ~position:`CENTER ()
    in 
    let title = GBin.frame 
	~shadow_type:`OUT ~packing:w#vbox#add ()
    in
    let hbox_entry = GPack.hbox ~packing:title#add ()
    in
    GMisc.label ~text:"Nickname:" ~packing:hbox_entry#add ();
    let entry = GEdit.entry ~packing:hbox_entry#add ()
    and hbox = GPack.hbox ~packing:w#vbox#add ()
    in
    let ok_b = GButton.button  ~label:"OK" ~packing:hbox#add ()
    and cancel_b = GButton.button  ~label:"cancel" ~packing:hbox#add ()
    in
    let _ = ok_b#connect#clicked 
	~callback:
	(fun () -> c#add_priv entry#text;
	  w#destroy ();
	  ());
    and _ = entry#connect#activate
	~callback:
	(fun () -> c#add_priv entry#text;
	  w#destroy ();
	  ()); 
    and _ = cancel_b#connect#clicked ~callback:(fun () -> w#destroy (); ())
    in
    w#show()
  
let connect h s = fun () ->
  let w = GWindow.dialog ~title:"Connect" ~modal:true ~position:`CENTER ()
  in 
  let title = GBin.frame
    ~shadow_type:`OUT ~packing:w#vbox#add () in 
  GMisc.label ~text:"Server" ~packing:title#add ();
  let server_selection = 
    GEdit.combo ~popdown_strings:(s#server_names())
      ~value_in_list:true ~packing:w#vbox#add ~allow_empty:true ()
  and ok_button =
    GButton.button ~label:"CONNECT" ~packing:w#vbox#add ()
  in
  let _ =
    ok_button#connect#clicked
      ~callback:
      (fun () ->
	try 
	  let 
	      new_s = s#get_server_setting server_selection#entry#text
	  in
	  begin
	    h#set_server new_s;
	    w#destroy ();
	    h#reload_server ();
	    h#start_connection ();
	    h#initial_sequence ()
	  end
	with 
	  Server.Config_error ->
	    let w = GWindow.dialog ~title:"ERROR" 
		~modal:true ~position:`CENTER ()
	    in
	    let _ = GMisc.label ~text:"Config error" ~packing:w#vbox#add ()
	    and ok_b = GButton.button ~label:"OK"~packing:w#vbox#add ()
	    in
	    let _ = ok_b#connect#clicked ~callback:w#destroy
	    in
	    w#show ())
  in
  w#show()

and disconnect h = fun () ->
  try
    if h#status_connected () then
      begin
	h#final_sequence ();
	h#kill_connection () 
      end
  with 
    Message_handler.Server_not_configured -> ()

class one_entry 
    ~label ?default ?visibility ?packing ?show ?max_length () =
  let hb = GPack.hbox ?packing ?show ()
  in
  let l = GMisc.label ~text:label ~xalign:0.0 ~packing:hb#pack ()
  and e = GEdit.entry ?text:default ?visibility ?max_length
      ~packing:(hb#pack ~from:`END) ()
  in
  object
    method text = e#text
    method set_text = e#set_text
  end

class server_selection_signal =
  object
    inherit GUtil.ml_signals []
  end

let new_config_dialog ~setting =
  let w = GWindow.dialog ~title:"New server" ~modal:true ~position:`CENTER ()
  in
  let title = GBin.frame ~shadow_type:`OUT ~packing:w#vbox#add ()
  in
  let _ =  GMisc.label ~text:"Enter new server configuration name." 
      ~packing:title#add ()
  in
  let entry = GEdit.entry ~packing:w#vbox#add
      ~text:(setting#setting_name ()) ()
  and ok_b = GButton.button ~label:"OK" ~packing:w#vbox#add ()
  in
  let _ = entry#connect#activate 
      ~callback:
      (fun () ->
	begin 
	  setting#set_setting_name entry#text;
	  w#destroy ()
	end)
  and _ = ok_b#connect#clicked 
      ~callback:
      (fun () ->
	begin 
	  setting#set_setting_name entry#text;
	  w#destroy ()
	end)
  in
  w#show()
   
class config_dialog ~(settings : Server.server_info_list) () =
  let w = GWindow.dialog ~title:"server configuration" 
      ~modal:true ~position:`CENTER ()
  in
  let title = GBin.frame ~shadow_type:`OUT ~packing:w#vbox#add ()
  in
  let config_name = GEdit.combo ~packing:w#vbox#add 
      ~popdown_strings:(settings#server_names ()) ()
  and config_hbox = GPack.hbox ~packing:w#vbox#add ()
  in
  let config_box = GPack.vbox ~packing:config_hbox#add ()
  in
  let _ = settings#connect#changed 
      ~callback:(fun () ->  
	config_name#set_popdown_strings (settings#server_names ()))
  and _ = config_name#entry#set_editable false
  in
  let server_entry = new one_entry ~label:"Server:" ~packing:config_box#add ()
  and port_entry = new one_entry ~label:"Port:" 
      ~packing:config_box#add ~default:"6667" ()
  and passwd_entry = new one_entry ~label:"Password:" ~visibility:false 
      ~packing:config_box#add ()
  and nick_entry = new one_entry ~label:"Nickname:" 
      ~packing:config_box#add ~max_length:9 ()
  and name_entry = new one_entry ~label:"Username:" ~packing:config_box#add ()
  and f_name_entry = new one_entry ~label:"Name:" ~packing:config_box#add ()
  and part_msg_entry = new one_entry ~label:"Part Message:" 
      ~packing:config_box#add ()
  and quit_msg_entry = new one_entry ~label:"Quit Message:" 
      ~packing:config_box#add ()
(*  and auto_connect_toggle = 
    GButton.check_button ~label:"Auto Connect" ~packing:config_box#add () *)
  in
  let channel_box = GPack.vbox ~packing:config_hbox#add ()
  in
  let _ = GMisc.label ~text:"Channels" ~packing:channel_box#pack ()
  and chan_l = GList.clist ~selection_mode:`BROWSE 
      ~columns:1 ~titles_show:false ~titles:["channel"]
      ~packing:(channel_box#pack ~expand:true) ()
  and chan_e_box = GPack.hbox ~packing:channel_box#pack ()
  in
  let chan_e = GEdit.entry ~packing:chan_e_box#pack ()
  and chan_a_b = GButton.button ~label:"add" ~packing:chan_e_box#pack ()
  and chan_e_b = GButton.button ~label:"delete" ~packing:chan_e_box#pack ()
  and selected_cell : int option ref = ref None
  in
  let _ = chan_a_b#misc#set_sensitive false
  and _ = chan_e_b#misc#set_sensitive false
  and _ = chan_l#connect#select_row 
      ~callback:(fun ~row ~column ~event -> 
	begin
	  chan_e_b#misc#set_sensitive true;
	  selected_cell := Some row
	end)
  and _ = chan_a_b#connect#clicked
      ~callback:
      (fun () ->
	begin
	  chan_l#append [chan_e#text];
	  chan_e#set_text "";
	  chan_a_b#misc#set_sensitive false
	end)
  and _ = chan_e_b#connect#clicked
      ~callback:
      (fun () -> 
	begin
	  match !selected_cell with
	    Some row -> chan_l#remove ~row
	  | None -> ()
	end;
	selected_cell := None;
	chan_l#unselect_all ();
	chan_e_b#misc#set_sensitive false)
  in
  let _ = chan_e#connect#activate
      ~callback:
      (fun () -> 
	begin
	  chan_l#append [chan_e#text];
	  chan_e#set_text "";
	  chan_a_b#misc#set_sensitive false
	end)
  and _ = chan_e#connect#changed
      ~callback:(fun () -> 
	chan_a_b#misc#set_sensitive 
	  (if chan_e#text_length = 0 then false else true))
  in
  let buttons = GPack.hbox ~packing:w#vbox#add ~homogeneous:true ()
  in
  let new_b = GButton.button  ~label:"New" ~packing:buttons#pack ()
  and finish_b = GButton.button  ~label:"Quit" ~packing:buttons#pack ()
  and delete_b = GButton.button  ~label:"Delete" ~packing:buttons#pack ()
  and cancel_b = GButton.button  ~label:"Cancel" ~packing:buttons#pack ()
  in
  let set_server_entry s_info = 
    begin
      s_info#set_server server_entry#text;
      s_info#set_nick nick_entry#text;
      s_info#set_port (int_of_string port_entry#text);
      s_info#set_passwd passwd_entry#text;
      s_info#set_username name_entry#text;
      s_info#set_fullname f_name_entry#text;
      s_info#set_part_message part_msg_entry#text;
      s_info#set_quit_message quit_msg_entry#text;
      s_info#set_channel_list
	(List.map (fun n -> chan_l#cell_text n 0, "Text")
	   (gen_int_list ~from_n:0 ~to_n:chan_l#rows))
    end
  and get_server_entry s_info =
    begin
      server_entry#set_text (s_info#server ());
      nick_entry#set_text (s_info#nick ());
      port_entry#set_text (string_of_int (s_info#port ()));
      passwd_entry#set_text (s_info#passwd ());
      name_entry#set_text (s_info#username ());
      f_name_entry#set_text (s_info#fullname ());
      part_msg_entry#set_text (s_info#part_message ());
      quit_msg_entry#set_text (s_info#quit_message ());
      chan_l#clear ();
      List.map (fun (s,m)-> chan_l#append [s]) (s_info#channel_list ())
    end
  in
  object (self)
    val mutable current_setting = None
    method servers = settings
    method show () = w#show()
    method read_setting s = settings#get_server_setting s
    initializer
      current_setting <-
	begin
	  try Some (self#read_setting config_name#entry#text)
	  with Server.Config_error -> None
	end;
      config_name#entry#connect#activate
	~callback:(fun () ->
	  begin
	    match current_setting with 
	      Some s -> set_server_entry s
	    | None -> () 
	  end;
	  let s = config_name#entry#text
	  in
	  try
	    let sc =  self#read_setting s 
	    in
	    begin
	      get_server_entry sc;
	      current_setting <- Some sc
	    end
	  with 
	    Server.Config_error -> 
	      try
		let sc = new Server.server_info 
		    ~setting_name:config_name#entry#text ()
		in
		begin
		  set_server_entry sc;
		  settings#add_server sc;
		  current_setting <- Some sc
		end
	      with Server.Config_error -> ());
      config_name#entry#connect#changed
	~callback:(fun () ->
	  begin
	    match current_setting with 
	      Some s -> 
		if ((s#setting_name()) = config_name#entry#text) then
		  begin
		    set_server_entry s
		  end
	    | None -> () 
	  end;
	  let s = config_name#entry#text
	  in
	  try
	    let sc =  self#read_setting s 
	    in
	    begin
	      get_server_entry sc;
	      current_setting <- Some sc
	    end
	  with Server.Config_error -> ());
      new_b#connect#clicked
	~callback:(fun () ->
	  begin
	    match current_setting with 
	      Some s -> 
		set_server_entry s;
		settings#replace_server s;
	    | None -> () 
	  end;
	  try
	    let s = new Server.server_info 
		~setting_name:"New" ()
	    in
	    let
		_ = current_setting <- Some s;
	    in
	    let _ = s#connect#name_changed 
		~callback:(fun (from_sn,to_sn) -> 
		  settings#change_setting_name ~from_sn ~to_sn);
	    in		
	    begin
	      new_config_dialog ~setting:s;
	      get_server_entry s;
	      settings#add_server s;
	    end
	  with Server.Config_error -> ());
      delete_b#connect#clicked ~callback:
	(fun () -> 
	  settings#delete_server config_name#entry#text;
	  ());
      cancel_b#connect#clicked ~callback:
	(fun () -> 
	  w#destroy ();
	  ());
      finish_b#connect#clicked ~callback:
	(fun () -> 
	  begin
	    match current_setting with 
	      Some s -> 
		set_server_entry s
	    | None -> () 
	  end;
	  settings#save_settings ~file:Constants.config_file;
	  w#destroy ();
	  ());
      begin
      try
	let 
	    s = settings#get_server_setting config_name#entry#text
	in
	get_server_entry s;
	current_setting <- Some s;
	()
      with Server.Config_error -> ()
      end;
      ()
  end

class irc_control ~(handler:Message_handler.irc_message_handler)
    ~(channels:Channelview.channels) ~(servers:Server.server_info_list) =
  object
    method connect =  connect handler servers 
    method disconnect = disconnect handler
    method join = join handler channels
    method priv = priv handler channels
  end
