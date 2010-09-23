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

(* $Id: channel.ml 1354 2007-07-20 04:18:38Z garrigue $ *)
open Message_utils

class channel_signals ~(part:string GUtil.signal) 
    ~(privmsg : (string * string) GUtil.signal)
    : I_channel.i_channel_signals =
object
  inherit GUtil.ml_signals [part#disconnect; privmsg#disconnect]
  method part = part#connect ~after
  method privmsg = privmsg#connect ~after
end 

type channel_flags = 
    {mutable a : bool; mutable i : bool; mutable m : bool; mutable n : bool; 
     mutable q : bool; mutable p : bool; mutable s : bool; mutable r : bool;
     mutable t : bool }

let mem_space = Str.regexp "[ \t]+"

let rec take_n n l =
  if n = 0 then l
  else 
    match l with 
      [] -> []
    | (_::ltl) -> take_n (n-1) ltl

let check_channel_flags s cf =
  let v = ref false 
  and ms = ref ""
  and uparams = ref 0
  and oparams = ref 0
  in 
  for i = 0 to (String.length s)-1 do
    (match s.[i] with 
      'a' -> cf.a  <- !v | 'i' -> cf.i  <- !v | 'm' -> cf.m <- !v
    | 'n' -> cf.n  <- !v | 'q' -> cf.q  <- !v | 'p' -> cf.p <- !v
    | 's' -> cf.s  <- !v | 'r' -> cf.r  <- !v | 't' -> cf.t <- !v 
    | 'o' -> uparams := !uparams + 1 | 'v' -> oparams := !oparams + 1
    | 'k' -> oparams := !oparams + 1 | 'l' -> oparams := !oparams + 1
    | 'b' -> oparams := !oparams + 1 | 'e' -> oparams := !oparams + 1
    | 'I' -> oparams := !oparams + 1 | '-' -> v := false | '+' -> v := true 
    | _ -> ())
  done;
  (!uparams, !oparams)

let rec check_channel_string ml cf o =
  match ml with 
    [] -> o
  | (m::mtl) -> 
      let (newun, newon) = check_channel_flags m cf
      in
      check_channel_string 
	(take_n (newun+newon) mtl) cf ((newun > 0) || o)

let message_text nick message =
   Printf.sprintf "<%s> %s" nick message
and my_message_text nick message =
  Printf.sprintf ">%s< %s" nick message

let mode_string t = List.fold_left (fun s t -> s^t^" ") "" t

let channel_flag_string cf =
  String.concat ""
    (List.map2 (fun flag str -> if flag then str else "")
       [cf.a;cf.i;cf.m;cf.n;cf.q;cf.p;cf.s;cf.r;cf.t]
       ["a";"i";"m";"n";"q";"p";"s";"r";"t"])

let set_topic label channel_name t = label#set_text (channel_name^" :"^t)
and set_mode label t = label#set_text (" ["^t^"]")

class channel ~(handler:Message_handler.irc_message_handler) 
    ~(channel_name: string) ~(server:Server.server_info) 
    ?packing ?show () : I_channel.i_channel =
  let vb = GPack.vbox ?packing ?show ()
  and adj = GData.adjustment ()
  in
  let topic_hb = GPack.hbox ~packing:vb#pack ~border_width:2 ()
  in
  let label = GMisc.label ~xalign:0.0 ~text:channel_name 
      ~packing:topic_hb#pack ()
  and mode_label = GMisc.label ~packing:topic_hb#pack ()
  and control_hb =
    GPack.button_box `HORIZONTAL ~packing:(topic_hb#pack ~from:`END) 
      ~spacing:4 ()
  in
  let hb = GPack.hbox ~packing:(vb#pack ~expand:true) ()
  in
  let view = GBroken.text ~vadjustment:adj ~packing:(hb#pack ~expand:true) ()
  and sb = GRange.scrollbar `VERTICAL ~adjustment:adj ~packing:hb#pack ()
  and members = new Members.members ~width:100 ~packing:hb#pack ()
  and h = handler
  in
  let names_buffer : string list ref = ref []
  in
  let cf = { a = false; i = false; m = false; n = false;
	     q = false; p = false; s = false; r = false; t = false }
  in
  let colormap = view#misc#colormap
  in
  let red   = Gdk.Color.alloc ~colormap (`RGB(0xffff,0,0))
  and green = Gdk.Color.alloc ~colormap (`RGB(0,0xffff,0))
  and blue  = Gdk.Color.alloc ~colormap (`RGB(0,0,0xffff))
  in
  let part =  new GUtil.signal ()
  in
  let m_check m =
    match m with
      (Some (name,_,_) , Message.MSG_PRIVATE, Some [c;m]) -> 
	if c = channel_name then
	  print_text ~u_foreground:(`COLOR red) ~emit:(server#auto_url_open())
	    view (message_text name m)
	else if name = channel_name then
	  print_text ~u_foreground:(`COLOR red) ~emit:(server#auto_url_open())
	    view (message_text name m)
    | (None , Message.MSG_PRIVATE, Some [c;m]) -> 
	if c = channel_name then
	  print_text ~u_foreground:(`COLOR red) view 
	    (message_text channel_name m)
    | (Some (name,_,_), Message.MSG_TOPIC, Some [c;t]) ->
	if c = channel_name then 
	  begin 
	    set_topic label channel_name t;
	    print_text ~foreground:(`COLOR green) view 
	      ("Topic set by "^name^": "^t)
	  end
    | (Some (n, _, _), Message.MSG_JOIN, Some [c]) ->
	if c = channel_name then 
	  (members#append n; 
	   print_text ~foreground:(`COLOR red)
	     view ("***  "^n^" has joined "^c); 
	   ())
    | (Some (n, _, _), Message.MSG_PART, Some [c; m]) ->
	begin
	  if c = channel_name then 
	    begin
	      (members#remove n;
	       print_text ~foreground:(`COLOR red)
		 view ("***  "^n^" has left "^c^" ("^m^")"); ());
	      if n = server#nick () then
		part#call ((handler#server)#part_message ());
	    end
	end
    | (Some (n, _, _), Message.MSG_QUIT, Some [m]) ->
	if members#check n then 
	  begin
	    members#remove n;
	    print_text ~foreground:(`COLOR red)
	      view ("***  "^n^" has left IRC. ("^m^")"); ()
	  end
    | (Some (n, _, _), Message.MSG_NICK, Some [new_n]) ->
	begin
	  print_text ~foreground:(`COLOR blue)
	    view ("***  "^n^" is now known as "^new_n^"."); 
	  members#change n new_n
	end
    | (Some (n, _, _), Message.MSG_MODE, Some (c::t)) ->
	if c = channel_name then 
	  begin
	    print_text ~foreground:(`COLOR blue) view 
	      ("New mode set by "^n^":"^(mode_string t));
	    let
		need_names = check_channel_string t cf false
	    in
	    set_mode mode_label (channel_flag_string cf);
	    if need_names then 
	      handler#send_message (None, Message.MSG_NAMES, Some [c]);
	  end
    | _ -> ()
  and r_check r =
    match r with 
      Reply.Command (f, cr, arg)  ->
	begin
	  match (f,cr,arg) with 
	    (_, Reply.RPL_TOPIC, Some [_;c;t]) ->  
	      if c = channel_name then 
		begin
		  set_topic label channel_name t; 
		  print_text ~foreground:(`COLOR green) view 
		    ("Topic for this channel: "^t)
		end
	  | (_, Reply.RPL_NAMREPLY, Some [_;_;c;t]) -> 
	      if c = channel_name then 
		names_buffer := (Str.split mem_space t)@(!names_buffer)
	  | (_, Reply.RPL_ENDOFNAMES, Some (_::c::_)) ->
	      if c = channel_name then
		begin
		  members#clear ();
		  List.map (fun s -> members#append s) (!names_buffer);
		  names_buffer := []
		end
	  | (_, Reply.RPL_CHANNELMODEIS, Some (_::c::t)) -> 
	      if c = channel_name 
	      then 
		begin
		  check_channel_string t cf false;
		  set_mode mode_label (channel_flag_string cf)
		end
	  | _ -> ()
	end
    | _ -> ()
  in      
  object (self)
    inherit GObj.widget vb#as_widget
    val view = view
    val channelname = channel_name
    val part = part
    val privmsg = new GUtil.signal ()
    method part = part
    method part_command () =
      h#send_message (None, Message.MSG_PART, 
		      Some [self#channelname; 
			    ":"^((handler#server)#part_message ())]);
      self#part#call ((handler#server)#part_message ())
    method topic_command () = 
      Message_utils.topic_dialog ~handler:h ~channelname
    method ctcp_command () =
      Ctcp.send_ctcp_dialog ~handler:h ~members
    method connect = new channel_signals ~part ~privmsg
    method channelname = channelname
    method initialize () = ()
    method my_message = 
      (fun x -> print_text ~u_foreground:(`COLOR red) view 
	  (my_message_text (server#nick ()) x))
    initializer
      h#connect#message ~callback:m_check;
      h#connect#reply ~callback:r_check;
      members#connect#selected 
	~callback:
	(fun nick -> Message_utils.send_whois ~handler:h ~nick);
      handler#send_message (None, Message.MSG_MODE, Some [channelname]);
      ()
  end

class channel_factory : I_channel.i_channel_factory =
  object
    method module_name = "Text"
    method new_channel_object 
	~handler ~channel_name ~server ?packing ?show () = 
      new channel ~handler ~channel_name ~server ?packing ?show ()
  end
