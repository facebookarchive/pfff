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

(* $Id: ctcp.ml 1354 2007-07-20 04:18:38Z garrigue $ *)  
exception Unknown_ctcp of string

open Str

type ctcp_message =
    CTCP_VERSION
  | CTCP_PING
  | CTCP_ACTION
  | CTCP_TIME

let get_ctcp_message s =
  match s with
    "VERSION" -> CTCP_VERSION
  | "PING" -> CTCP_PING
  | "ACTION" -> CTCP_ACTION
  | "TIME" -> CTCP_TIME
  | s -> raise (Unknown_ctcp s)

let get_ctcp_string s =
  match s with
    CTCP_VERSION -> "VERSION"
  | CTCP_PING -> "PING"
  | CTCP_ACTION -> "ACTION"
  | CTCP_TIME -> "TIME"

let get_current_time () =
  let
      t = Unix.localtime (Unix.time ())
  in
  string_of_int 
    (t.Unix.tm_sec + t.Unix.tm_min * 60 + 
       t.Unix.tm_hour * 3600 + t.Unix.tm_yday * 86400)

let ctime_day_of_the_week d = 
  let
      a = [|"Sun";"Mon";"Tue";"Wed";"Thu";"Fri";"Sat"|]
  in a.(d)

let ctime_month d = 
  let
      a = [|"Jan";"Feb";"Mar";"Apr";"May";"Jun";
	    "Jul";"Aug";"Sep";"Oct";"Nov";"Dec"|]
  in a.(d)

let ctime tm =
  Printf.sprintf "%s %s %d %02d:%02d:%02d %d"
    (ctime_day_of_the_week tm.Unix.tm_wday) 
    (ctime_month tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec (tm.Unix.tm_year + 1900)

let get_current_formatted_time () = ctime (Unix.localtime (Unix.time ()))

let ctcp_regexp = regexp "\001\\([A-Z]+\\)\\(\\|[ \t]+\\(.+\\)\\)\001"
and p_pos = 1
and arg_pos = 3
    
let check_ctcp m =
  if string_match ctcp_regexp m 0
  then Some (get_ctcp_message (matched_group p_pos m),
	     try Some (matched_group arg_pos m)
	     with Not_found -> None)
  else None


let send_ctcp ~handler ~ctcp_str ~to_nick ~message =
  handler#send_message(None, Message.MSG_PRIVATE,
		       Some [to_nick; 
			     ":\001"^ctcp_str^
			     (match message with None -> "" 
			     | Some m -> " "^m)^"\001"])

let send_ctcp_reply ~handler ~to_nick ~ctcp ?message () =
  Message_utils.send_notice ~handler ~to_nick 
    ~message:("\001"^(get_ctcp_string ctcp)^
	      (match message with None -> "" | Some m -> " "^m)^"\001")

let member_list ~members ~packing =
  let 
      a = GEdit.combo ~popdown_strings:(members#member_list) ~packing ()
  in 
  a#entry
  
let message_entry ~members ~packing = GEdit.entry ~packing ()

let ctcp_strings = 
  ["VERSION", [];
   "PING", [];
   "TIME", [];
   "ACTION", [message_entry]]

let send_ctcp_dialog ~handler ~members =
  let
      w = GWindow.dialog ~title:"CTCP" ~modal:true ~position:`CENTER ()
  in
  let title = GBin.frame 
      ~shadow_type:`OUT ~packing:w#vbox#add ()
  in
  let ctcp_name = GEdit.combo
      ~popdown_strings:(List.map (fun (s,_)-> s) ctcp_strings)
      ~packing:w#vbox#add ()
  and ctcp_to = GEdit.combo
      ~popdown_strings:(members#member_list) ~packing:w#vbox#add ()
  in
  let ok_b = GButton.button  ~label:"OK" ~packing:w#vbox#add ()
  in
  let _ = ok_b#connect#clicked 
      ~callback:
      (fun () ->
	let  args = List.assoc (ctcp_name#entry#text) ctcp_strings
	in
	match args with
	  [] -> 
	    begin
	      send_ctcp 
		~handler ~ctcp_str:ctcp_name#entry#text 
		~to_nick:ctcp_to#entry#text
		~message:None;
	      w#destroy ()
	    end
	| _ ->
	    let edits = 
	      List.map (fun f -> f ~members ~packing:w#vbox#add) args
	      in
	      let ok2_b = GButton.button  
		  ~label:"OK" ~packing:w#vbox#add ()	
	      in
	      let _ = ok2_b#connect#clicked
		  ~callback:
		  (fun () -> 
		    send_ctcp 
		      ~handler ~ctcp_str:ctcp_name#entry#text 
		      ~to_nick:ctcp_to#entry#text
		      ~message:(Some 
				  (List.fold_left (fun s k -> s^k) ""
				     (List.map (fun e -> e#text) edits)));
		    w#destroy ())
	      in
	      ())
  in
  w#show ()
