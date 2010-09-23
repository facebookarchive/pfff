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

(* $Id: message.ml 1354 2007-07-20 04:18:38Z garrigue $ *)
exception Unknown_message

type irc_message =
   MSG_DNS
 | MSG_HASH
 | MSG_DIE
 | MSG_CLOSE
 | MSG_RESTART
 | MSG_REHASH
 | MSG_SERVSET
 | MSG_SERVLIST
 | MSG_SQUERY
 | MSG_NOTE
 | MSG_ISON
 | MSG_USERHOST
 | MSG_SERVICE
 | MSG_RECONECT
 | MSG_KICK
 | MSG_UMODE
 | MSG_MODE
 | MSG_MOTD
 | MSG_LUSERS
 | MSG_PART
 | MSG_NJOIN
 | MSG_JOIN
 | MSG_NOTICE
 | MSG_TRACE
 | MSG_ADMIN
 | MSG_NAMES
 | MSG_TIME
 | MSG_WALLOPS
 | MSG_PASS
 | MSG_OPER
 | MSG_PONG
 | MSG_PING
 | MSG_CONNECT
 | MSG_AWAY
 | MSG_ERROR
 | MSG_HELP
 | MSG_USERS
 | MSG_STATS
 | MSG_SUMMON
 | MSG_LINKS
 | MSG_INFO
 | MSG_KILL
 | MSG_SQUIT
 | MSG_QUIT
 | MSG_VERSION
 | MSG_INVITE
 | MSG_TOPIC
 | MSG_LIST
 | MSG_SERVER
 | MSG_NICK
 | MSG_USER
 | MSG_WHOWAS
 | MSG_WHOIS
 | MSG_WHO
 | MSG_PRIVATE

type message = Prefix.prefix option * irc_message * IrcArg.arg option

let get_message_id = function 
   "PRIVMSG"	->	MSG_PRIVATE
 | "UMODE"	->	MSG_UMODE
 | "MODE"	->	MSG_MODE
 | "PONG"	->	MSG_PONG
 | "PING"	->	MSG_PING
 | "WHOWAS"	->	MSG_WHOWAS
 | "WHOIS"	->	MSG_WHOIS
 | "WHO"	->	MSG_WHO
 | "DNS"	->	MSG_DNS
 | "CONNECT"	->	MSG_CONNECT
 | "HAZH"	->	MSG_HASH
 | "DIE"	->	MSG_DIE
 | "CLOSE"	->	MSG_CLOSE
 | "RESTART"	->	MSG_RESTART
 | "REHASH"	->	MSG_REHASH
 | "SERVSET"	->	MSG_SERVSET
 | "SERVLIST"	->	MSG_SERVLIST
 | "SQUERY"	->	MSG_SQUERY
 | "NOTE"	->	MSG_NOTE
 | "ISON"	->	MSG_ISON
 | "USERHOST"	->	MSG_USERHOST
 | "SERVICE"	->	MSG_SERVICE
 | "RECONNECT"	->	MSG_RECONECT
 | "KICK"	->	MSG_KICK
 | "MOTD"	->	MSG_MOTD
 | "LUSERS"	->	MSG_LUSERS
 | "PART"	->	MSG_PART
 | "NJOIN"	->	MSG_NJOIN
 | "JOIN"	->	MSG_JOIN
 | "NOTICE"	->	MSG_NOTICE
 | "TRACE"	->	MSG_TRACE
 | "ADMIN"	->	MSG_ADMIN
 | "NAMES"	->	MSG_NAMES
 | "TIME"	->	MSG_TIME
 | "WALLOPS"	->	MSG_WALLOPS
 | "PASS"	->	MSG_PASS
 | "OPER"	->	MSG_OPER
 | "AWAY"	->	MSG_AWAY
 | "ERROR"	->	MSG_ERROR
 | "HELP"	->	MSG_HELP
 | "USERS"	->	MSG_USERS
 | "STATS"	->	MSG_STATS
 | "SUMMON"	->	MSG_SUMMON
 | "LINKS"	->	MSG_LINKS
 | "INFO"	->	MSG_INFO
 | "KILL"	->	MSG_KILL
 | "SQUIT"	->	MSG_SQUIT
 | "QUIT"	->	MSG_QUIT
 | "VERSION"	->	MSG_VERSION
 | "INVITE"	->	MSG_INVITE
 | "TOPIC"	->	MSG_TOPIC
 | "LIST"	->	MSG_LIST
 | "SERVER"	->	MSG_SERVER
 | "NICK"	->	MSG_NICK
 | "USER"	->	MSG_USER
 | _	->	raise Unknown_message

let to_string = function 
    MSG_PRIVATE	->	"PRIVMSG"
  | MSG_UMODE	->	"UMODE"
  | MSG_MODE	->	"MODE"
  | MSG_PONG	->	"PONG"
  | MSG_PING	->	"PING"
  | MSG_QUIT	->	"QUIT"
  | MSG_NICK	->	"NICK"
  | MSG_WHOWAS	->	"WHOWAS"
  | MSG_WHOIS	->	"WHOIS"
  | MSG_WHO	->	"WHO"
  | MSG_DNS	->	"DNS"
  | MSG_HASH	->	"HAZH"
  | MSG_DIE	->	"DIE"
  | MSG_CLOSE	->	"CLOSE"
  | MSG_RESTART	->	"RESTART"
  | MSG_REHASH	->	"REHASH"
  | MSG_SERVSET	->	"SERVSET"
  | MSG_SERVLIST	->	"SERVLIST"
  | MSG_SQUERY	->	"SQUERY"
  | MSG_NOTE	->	"NOTE"
  | MSG_ISON	->	"ISON"
  | MSG_USERHOST	->	"USERHOST"
  | MSG_SERVICE	->	"SERVICE"
  | MSG_RECONECT	->	"RECONNECT"
  | MSG_KICK	->	"KICK"
  | MSG_MOTD	->	"MOTD"
  | MSG_LUSERS	->	"LUSERS"
  | MSG_PART	->	"PART"
  | MSG_NJOIN	->	"NJOIN"
  | MSG_JOIN	->	"JOIN"
  | MSG_NOTICE	->	"NOTICE"
  | MSG_TRACE	->	"TRACE"
  | MSG_ADMIN	->	"ADMIN"
  | MSG_NAMES	->	"NAMES"
  | MSG_TIME	->	"TIME"
  | MSG_WALLOPS	->	"WALLOPS"
  | MSG_PASS	->	"PASS"
  | MSG_OPER	->	"OPER"
  | MSG_CONNECT	->	"CONNECT"
  | MSG_AWAY	->	"AWAY"
  | MSG_ERROR	->	"ERROR"
  | MSG_HELP	->	"HELP"
  | MSG_USERS	->	"USERS"
  | MSG_STATS	->	"STATS"
  | MSG_SUMMON	->	"SUMMON"
  | MSG_LINKS	->	"LINKS"
  | MSG_INFO	->	"INFO"
  | MSG_KILL	->	"KILL"
  | MSG_SQUIT	->	"SQUIT"
  | MSG_VERSION	->	"VERSION"
  | MSG_INVITE	->	"INVITE"
  | MSG_TOPIC	->	"TOPIC"
  | MSG_LIST	->	"LIST"
  | MSG_SERVER	->	"SERVER"
  | MSG_USER	->	"USER"


let construct_message_string (prefix, message, arg) =
  (match prefix with
    Some p -> ":"^(Prefix.to_string p)^" "
  | None -> "")^
  (to_string message)^" "^
  (match arg with
    Some a -> IrcArg.to_string a
  | None -> "")^"\n"

