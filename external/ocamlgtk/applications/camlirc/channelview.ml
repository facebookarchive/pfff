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

(* $Id: channelview.ml 1354 2007-07-20 04:18:38Z garrigue $ *)
exception No_channel

class channels_signals ~(channel_changed: string GUtil.signal) =
  object
    inherit GUtil.ml_signals [channel_changed#disconnect]
    method channel_changed = channel_changed#connect ~after
	
  end

class channels ~(handler:Message_handler.irc_message_handler)
    ?packing ?show ?(width = 640) ?(height = 400)() =
  let note = GPack.notebook ?packing ?show ~width ~height ()
  and channel_changed = new GUtil.signal ()
  in
  let add_channel_page channel_name cv =
    let tab_label = GMisc.label ~text:channel_name ()
    and menu_label = GMisc.label ~text:channel_name ()
    in
    note#append_page ~tab_label:tab_label#coerce
      ~menu_label:menu_label#coerce cv#coerce
   in
   object (self)
     inherit GObj.widget note#as_widget
     val note = note
     val handler = handler
     val mutable current_channel = None
     val mutable channel_list = []
     val mutable notify_channel_list = []
     val mutable requested_channel_list = []
     val mutable init_comp_id = None
     val connect = new channels_signals ~channel_changed
     val channel_changed = channel_changed 
     method private cv_part_callback cv =
       (fun _ -> 
	 let
	     id = note#page_num cv#coerce
	 in
	 if id >= 0 then 
	   begin
	     note#remove_page id;
	     channel_list <- 
	       List.filter (fun (_,n) -> not (n = id)) channel_list
	   end)
     method add_channel channel_name channel_mode =
       begin
	 try
	   let 
	       (s, n) = 
	     List.find (fun (s,_) -> s#channelname = channel_name) 
	       channel_list
	   in
	   self#set_channel s
	 with 
	   Not_found ->
	     try 
	       begin
	       (* check server *)
		 handler#server;
		 handler#send_message(None, Message.MSG_JOIN,
				      Some [channel_name]);
		 requested_channel_list 
		 <- (channel_name, channel_mode)::requested_channel_list 
	       end
	     with
	       Message_handler.Server_not_configured -> ()
       end
     method add_priv priv_name =
       begin
	 try
	   let 
	       (s, n) = 
	     List.find (fun (s,_) -> s#channelname = priv_name) 
	       channel_list
	   in
	   self#set_channel s
	 with 
	   Not_found -> 
	     try 
	       begin
		 let 
		     cv = 
		   (Cf_manager.channel_factory_manager#get_constructor "Text")
		     ~handler ~channel_name:priv_name 
		     ~server:(handler#server) ()
		 in
		 add_channel_page priv_name cv;
		 cv#initialize ();
		 (handler#server)#add_channel_list priv_name "General";
		 channel_list <- 
		   (cv, note#page_num cv#coerce) :: channel_list;
		 self#set_channel cv
	       end
	     with
	       Message_handler.Server_not_configured -> ()
       end
     method set_channel c = 
       try
	 let
	     (_, n) = List.find (fun (s, _) -> 
	       s#channelname = c#channelname) channel_list
	 in
	 current_channel <- Some c;
	 note#goto_page n;
	 channel_changed#call c#channelname
       with
	 Not_found -> raise No_channel
     method current_channel () = 
       match current_channel with
	 Some c -> c | None -> raise No_channel
     method send_message s = 
       (self#current_channel ())#my_message s;

       handler#send_message(None, Message.MSG_PRIVATE,
			    Some [(self#current_channel ())#channelname;
				  " :"^s])
     initializer
       handler#connect#message 
	 ~callback:
	 (fun m -> 
	   begin
	     match m with 
	       (Some (n, _, _), Message.MSG_JOIN, Some [c]) ->
		 let
		     c_mode = 
		   try
		     List.assoc c requested_channel_list
		   with 
		     Not_found -> "Text"
		 in
		 let 
		     cv = 
		   (Cf_manager.channel_factory_manager#get_constructor c_mode)
		     ~handler ~channel_name:c ~server:(handler#server) ()
		 in
		 if n = (handler#server)#nick () then
		   begin
		     add_channel_page c cv;
		     cv#initialize ();
		     (handler#server)#add_channel_list c c_mode;
		     cv#connect#part ~callback:(self#cv_part_callback cv);
		     channel_list <- 
		       (cv, note#page_num cv#coerce) :: channel_list;
		     self#set_channel cv;
		     requested_channel_list <-
		       List.filter (fun (s,_) -> not (s = c)) 
			 requested_channel_list
		   end
	     | (Some (n, _, _), Message.MSG_NICK, Some [new_n]) ->
		 if n = (handler#server#nick ()) then 
		   handler#server#set_nick new_n
	     | _ -> ()
	   end);
       handler#connect#connected
	 ~callback:(fun () ->
	   let general = new General_channel.general_channel ~handler ()
	   in 
	   general#connect#privmsg
	     ~callback:(fun (ch, m) ->
	       if List.exists 
		   (fun s -> s = ch) notify_channel_list then
		 begin
		   let w = GWindow.dialog 
		       ~title:"message" ~modal:true ()
		   in
		   let _ = GMisc.label ~text:ch ~packing:w#vbox#add ()
		   and pb = GButton.button ~label:"OK" ~packing:w#vbox#add ()
		   in
		   begin 
		     pb#connect#clicked 
		       ~callback:(fun () -> w#destroy ());
		     w#show ();
		     ()
		   end;
		 end);
	   add_channel_page "*Console*" general;
	   channel_list <-  [(general, note#page_num general#coerce)];
	   current_channel <- Some general );
       init_comp_id <- 
	 Some (handler#connect#init_complete
		 ~callback:(fun () -> List.map 
		     (fun (channel, mode) -> (* print_string channel; *)
		       Message_utils.send_join ~handler ~channel;
		       requested_channel_list 
		       <- (channel, mode):: requested_channel_list)
		     ((handler#server)#channel_list ());
		   ()));
       note#connect#switch_page
	~callback:
	(fun n -> 
	  try 
	    let
		(s, _) = List.find (fun (_, id) -> n = id) channel_list
	    in
	    current_channel <- Some s;
	    channel_changed#call s#channelname
	  with 
	    Not_found -> current_channel <- None);
      handler#connect#disconnected
	~callback:
	(fun () -> 
	  List.map (fun (cv, _) -> 
	    let id = note#page_num cv#coerce
	    in if id >= 0 then note#remove_page id) channel_list;
	  channel_list <- [];
	());
      ()
  end
      
