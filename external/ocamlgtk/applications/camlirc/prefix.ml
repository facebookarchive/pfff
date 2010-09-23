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

(* $Id: prefix.ml 1354 2007-07-20 04:18:38Z garrigue $ *)
open Str

type prefix = string * string option * string option
let prefix_regexp =
  regexp "^\\([^@!]+\\)\\(\\|\\(!\\([^@!]+\\)\\|\\)@\\([^@!]+\\)\\)$"
and nick_loc = 1
and user_loc = 4
and host_loc = 5

let parse_prefix s =
  let result = string_match prefix_regexp s 0
  in
  let nick = matched_group nick_loc s
  and user = 
    try Some (matched_group user_loc s) with Not_found -> None
  and host = 
    try Some (matched_group host_loc s) with Not_found -> None
  in
  (nick, user, host)
    
let to_string (nick, user, host) =
  nick^
  (match user with Some s -> "!"^s | None -> "")^
  (match host with Some s -> "@"^s | None -> "")
