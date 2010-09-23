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

(* $Id: message_utils.ml 1354 2007-07-20 04:18:38Z garrigue $ *)
exception Msg_format_error

class url_signals ~(url:string GUtil.signal) =
object
  inherit GUtil.ml_signals [url#disconnect]
  method url = url#connect ~after
end 

class url_emitter () =
  let url = new GUtil.signal ()
  in
  object
    method url = url
    method connect = new url_signals ~url
  end

let url_emit = new url_emitter ()

let privmsg arg =
  match arg with 
    (frm, [target; message]) ->
      begin
	match frm with
	  None -> (None, target, message)
	| Some (nick, _, _) -> (Some nick, target, message)
      end
  | _ -> raise Msg_format_error

let http_regexp = Str.regexp "http:/[a-zA-Z0-9./&=~?-_]*"

let print_text_sub ?font ?foreground ?background 
    ?u_foreground ?u_background ?(emit=false) view s =
  match s with 
    Str.Text s -> 
      begin
	view#insert ?font ?foreground ?background s
      end
  | Str.Delim s -> 
      begin
	if emit then 
	  begin
	    url_emit#url#call s
	  end;
	view#insert ?font ?foreground:u_foreground 
	  ?background:u_background s;
      end

let print_text ?font ?foreground ?background 
    ?u_foreground ?u_background ?emit view s =
  let t = Unix.localtime (Unix.time ())
  and slist =  Str.full_split http_regexp s
  in
  view#insert ?font ?foreground ?background 
    (Printf.sprintf "%02d:%02d " t.Unix.tm_hour t.Unix.tm_min);
  List.map (print_text_sub ?font ?foreground ?background 
	      ?u_foreground ?u_background ?emit view) slist;
  view#insert ?font ?foreground ?background "\n"

let one_config ~title ~text ~default ?visibility ?max_length addfun =
  let w = GWindow.dialog ~title ~modal:true ~position:`CENTER ()
  in
  let heading = GBin.frame ~shadow_type:`OUT ~packing:w#vbox#add ()
  in GMisc.label ~text ~packing:w#vbox#add ();
  let e = GEdit.entry 
      ~text:default ?visibility ?max_length ~packing:w#vbox#add ()
  and ok_b = GButton.button  ~label:"OK" ~packing:w#vbox#add ()
  in 
  let _ = ok_b#connect#clicked
	~callback: (fun () -> addfun e#text; w#destroy (); ())
  and _ = e#connect#activate
      ~callback: (fun () -> addfun e#text; w#destroy (); ())
  in
  w#show()

let nick_config ~handler =
 one_config 
    ~title:"Your nickname is already used by someone else." 
    ~text:"Nickname:"
    ~default:((handler#server)#nick ())
    ~max_length:9
    (fun s -> (handler#server)#set_nick s;
      handler#send_message(None, Message.MSG_NICK, 
				 Some [(handler#server)#nick ()]))

let passwd_config ~handler =
 one_config 
    ~title:"Password is required." 
    ~text:"Password"
    ~default:((handler#server)#passwd ())
    ~visibility:false
    (fun s -> (handler#server)#set_passwd s)


let topic_dialog ~handler ~channelname =
  one_config
    ~title:"Enter new topic for this channel."
    ~text:"Topic"
    ~default:""
    (fun s -> 
      if not (s = "")
      then handler#send_message(None, Message.MSG_TOPIC,
				Some [channelname; ":"^s]))

let send_notice ~handler ~to_nick ~message =
  handler#send_message (None, Message.MSG_NOTICE,
			Some [to_nick; ":"^message])

let send_join ~handler ~channel =
  handler#send_message (None, Message.MSG_JOIN, Some [":"^channel])

let send_whois ~handler ~nick =
  handler#send_message (None, Message.MSG_WHOIS, Some [nick])

let send_simple_message h m t = h#send_message (None, m, ":"^t)

