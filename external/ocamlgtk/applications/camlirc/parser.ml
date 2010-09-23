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

(* $Id: parser.ml 1354 2007-07-20 04:18:38Z garrigue $ *)
open Str

exception Illegal_format

let prefix_regexp = regexp "^:\\([^ \t]+\\)[ \t]+\\(.+\\)$"
and prefix_loc = 1
and rest_loc = 2

let message_regexp = regexp "^\\([A-Z]+\\)\\($\\|[ \t]+\\(.+\\)$\\)"
and message_id_loc = 1
and message_arg_loc = 3

let reply_regexp = regexp "^\\([0-9][0-9][0-9]\\)\\($\\|[ \t]+\\(.+\\)$\\)"
and reply_id_loc = 1
and reply_arg_loc = 3

type m = 
    MSG of Message.message
  | REP of Reply.reply

let process_one_message s =
  let (prefix, np_s) = 
    if string_match prefix_regexp s 0
    then (Some (Prefix.parse_prefix (matched_group prefix_loc s)),
	  (matched_group rest_loc s))
    else (None, s)
  in
  if string_match message_regexp np_s 0
  then 
    begin
      let
	  id = Message.get_message_id (matched_group message_id_loc np_s)
      in
      MSG (prefix, 
	   id,
	   try Some (IrcArg.process_arg (matched_group message_arg_loc np_s))
	   with Not_found -> None)
    end
  else if string_match reply_regexp np_s 0
  then 
    begin
      let
	  id = int_of_string (matched_group reply_id_loc np_s)
      and arg = 
	try Some (IrcArg.process_arg 
		    (matched_group reply_arg_loc np_s))
	with Not_found -> None
      in
      REP 
      (match Reply.check_reply_type id with
	Reply.Type_connection ->
	  Reply.Connection (prefix, Reply.get_connection_reply id, arg)
      |	Reply.Type_command ->
	  Reply.Command (prefix, Reply.get_command_reply id, arg)
      |	Reply.Type_error ->
	  Reply.Error (prefix, Reply.get_error_reply id, arg))
    end
  else raise Illegal_format
