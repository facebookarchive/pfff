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

(* $Id: reply.ml 1354 2007-07-20 04:18:38Z garrigue $ *)
exception Unknown_Reply of int

type command_reply =
   RPL_TRYAGAIN
 | RPL_TRACEEND
 | RPL_TRACELOG
 | RPL_ADMINEMAIL
 | RPL_ADMINLOC2
 | RPL_ADMINLOC1
 | RPL_ADMINME
 | RPL_LUSERME
 | RPL_LUSERCHANNELS
 | RPL_LUSERUNKNOWN
 | RPL_LUSEROP
 | RPL_LUSERCLIENT
 | RPL_STATSDLINE
 | RPL_STATSDEBUG
 | RPL_STATSDEFINE
 | RPL_STATSBLINE
 | RPL_STATSPING
 | RPL_STATSSLINE
 | RPL_STATSHLINE
 | RPL_STATSOLINE
 | RPL_STATSUPTIME
 | RPL_STATSLLINE
 | RPL_STATSVLINE
 | RPL_SERVLISTEND
 | RPL_SERVLIST
 | RPL_SERVICE
 | RPL_ENDOFSERVICES
 | RPL_SERVICEINFO
 | RPL_UMODEIS
 | RPL_ENDOFSTATS
 | RPL_STATSYLINE
 | RPL_STATSQLINE
 | RPL_STATSKLINE
 | RPL_STATSILINE
 | RPL_STATSNLINE
 | RPL_STATSCLINE
 | RPL_STATSCOMMANDS
 | RPL_STATSLINKINFO
 | RPL_TRACERECONNECT
 | RPL_TRACECLASS
 | RPL_TRACENEWTYPE
 | RPL_TRACESERVICE
 | RPL_TRACESERVER
 | RPL_TRACEUSER
 | RPL_TRACEOPERATOR
 | RPL_TRACEUNKNOWN
 | RPL_TRACEHANDSHAKE
 | RPL_TRACECONNECTING
 | RPL_TRACELINK
 | RPL_NOUSERS
 | RPL_ENDOFUSERS
 | RPL_USERS
 | RPL_USERSSTART
 | RPL_TIME
 | RPL_NOTOPERANYMORE
 | RPL_MYPORTIS
 | RPL_YOURESERVICE
 | RPL_REHASHING
 | RPL_YOUREOPER
 | RPL_ENDOFMOTD
 | RPL_MOTDSTART
 | RPL_ENDOFINFO
 | RPL_INFOSTART
 | RPL_MOTD
 | RPL_INFO
 | RPL_ENDOFBANLIST
 | RPL_BANLIST
 | RPL_ENDOFLINKS
 | RPL_LINKS
 | RPL_CLOSEEND
 | RPL_CLOSING
 | RPL_KILLDONE
 | RPL_ENDOFNAMES
 | RPL_NAMREPLY
 | RPL_ENDOFWHO
 | RPL_WHOREPLY
 | RPL_VERSION
 | RPL_SUMMONING
 | RPL_INVITING
 | RPL_TOPIC
 | RPL_NOTOPIC
 | RPL_CHANNELMODEIS
 | RPL_LISTEND
 | RPL_LIST
 | RPL_LISTSTART
 | RPL_WHOISCHANNELS
 | RPL_ENDOFWHOIS
 | RPL_WHOISIDLE
 | RPL_WHOISCHANOP
 | RPL_ENDOFWHOWAS
 | RPL_WHOWASUSER
 | RPL_WHOISOPERATOR
 | RPL_WHOISSERVER
 | RPL_WHOISUSER
 | RPL_NOWAWAY
 | RPL_UNAWAY
 | RPL_TEXT
 | RPL_ISON
 | RPL_USERHOST
 | RPL_AWAY
 | RPL_NONE

and error_reply = 
 | ERR_USERSDONTMATCH
 | ERR_UMODEUNKNOWNFLAG
 | ERR_NOSERVICEHOST
 | ERR_NOOPERHOST
 | ERR_RESTRICTED
 | ERR_CANTKILLSERVER
 | ERR_CHANOPRIVSNEEDED
 | ERR_NOPRIVILEGES
 | ERR_NOCHANMODES
 | ERR_BADCHANMASK
 | ERR_BADCHANNELKEY
 | ERR_BANNEDFROMCHAN
 | ERR_INVITEONLYCHAN
 | ERR_UNKNOWNMODE
 | ERR_CHANNELISFULL
 | ERR_KEYSET
 | ERR_YOUWILLBEBANNED
 | ERR_YOUREBANNEDCREEP
 | ERR_PASSWDMISMATCH
 | ERR_NOPERMFORHOST
 | ERR_ALREADYREGISTRED
 | ERR_NEEDMOREPARAMS
 | ERR_NOTREGISTERED
 | ERR_USERSDISABLED
 | ERR_SUMMONDISABLED
 | ERR_NOLOGIN
 | ERR_USERONCHANNEL
 | ERR_NOTONCHANNEL
 | ERR_USERNOTINCHANNEL
 | ERR_UNAVAILRESOURCE
 | ERR_NICKCOLLISION
 | ERR_SERVICECONFUSED
 | ERR_SERVICENAMEINUSE
 | ERR_NICKNAMEINUSE
 | ERR_ERRONEUSNICKNAME
 | ERR_NONICKNAMEGIVEN
 | ERR_FILEERROR
 | ERR_NOADMININFO
 | ERR_NOMOTD
 | ERR_UNKNOWNCOMMAND
 | ERR_TOOMANYMATCHES
 | ERR_BADMASK
 | ERR_WILDTOPLEVEL
 | ERR_NOTOPLEVEL
 | ERR_NOTEXTTOSEND
 | ERR_NORECIPIENT
 | ERR_NOORIGIN
 | ERR_NOSUCHSERVICE
 | ERR_TOOMANYTARGETS
 | ERR_WASNOSUCHNICK
 | ERR_TOOMANYCHANNELS
 | ERR_CANNOTSENDTOCHAN
 | ERR_NOSUCHCHANNEL
 | ERR_NOSUCHSERVER
 | ERR_NOSUCHNICK

and connection_reply =
    RPL_BOUNCE
 |  RPL_MYINFO
 |  RPL_CREATED
 |  RPL_YOURHOST
 |  RPL_WELCOME

type irc_reply =
    Connection_reply of connection_reply
  | Command_reply of command_reply
  | Error_reply of error_reply

and reply_type =
    Type_connection
  | Type_command
  | Type_error

and reply =
    Connection of  Prefix.prefix option * connection_reply * IrcArg.arg option
  | Command of  Prefix.prefix option * command_reply * IrcArg.arg option
  | Error of  Prefix.prefix option * error_reply * IrcArg.arg option
let get_command_reply n =
match n with
   263	->	 RPL_TRYAGAIN
 | 319	->	 RPL_WHOISCHANNELS
 | 318	->	 RPL_ENDOFWHOIS
 | 317	->	 RPL_WHOISIDLE
 | 316	->	 RPL_WHOISCHANOP
 | 369	->	 RPL_ENDOFWHOWAS
 | 314	->	 RPL_WHOWASUSER
 | 313	->	 RPL_WHOISOPERATOR
 | 312	->	 RPL_WHOISSERVER
 | 311	->	 RPL_WHOISUSER
 | 262	->	 RPL_TRACEEND
 | 261	->	 RPL_TRACELOG
 | 259	->	 RPL_ADMINEMAIL
 | 258	->	 RPL_ADMINLOC2
 | 257	->	 RPL_ADMINLOC1
 | 256	->	 RPL_ADMINME
 | 255	->	 RPL_LUSERME
 | 254	->	 RPL_LUSERCHANNELS
 | 253	->	 RPL_LUSERUNKNOWN
 | 252	->	 RPL_LUSEROP
 | 251	->	 RPL_LUSERCLIENT
 | 250	->	 RPL_STATSDLINE
 | 249	->	 RPL_STATSDEBUG
 | 248	->	 RPL_STATSDEFINE
 | 247	->	 RPL_STATSBLINE
 | 246	->	 RPL_STATSPING
 | 245	->	 RPL_STATSSLINE
 | 244	->	 RPL_STATSHLINE
 | 243	->	 RPL_STATSOLINE
 | 242	->	 RPL_STATSUPTIME
 | 241	->	 RPL_STATSLLINE
 | 240	->	 RPL_STATSVLINE
 | 235	->	 RPL_SERVLISTEND
 | 234	->	 RPL_SERVLIST
 | 233	->	 RPL_SERVICE
 | 232	->	 RPL_ENDOFSERVICES
 | 231	->	 RPL_SERVICEINFO
 | 221	->	 RPL_UMODEIS
 | 219	->	 RPL_ENDOFSTATS
 | 218	->	 RPL_STATSYLINE
 | 217	->	 RPL_STATSQLINE
 | 216	->	 RPL_STATSKLINE
 | 215	->	 RPL_STATSILINE
 | 214	->	 RPL_STATSNLINE
 | 213	->	 RPL_STATSCLINE
 | 212	->	 RPL_STATSCOMMANDS
 | 211	->	 RPL_STATSLINKINFO
 | 210	->	 RPL_TRACERECONNECT
 | 209	->	 RPL_TRACECLASS
 | 208	->	 RPL_TRACENEWTYPE
 | 207	->	 RPL_TRACESERVICE
 | 206	->	 RPL_TRACESERVER
 | 205	->	 RPL_TRACEUSER
 | 204	->	 RPL_TRACEOPERATOR
 | 203	->	 RPL_TRACEUNKNOWN
 | 202	->	 RPL_TRACEHANDSHAKE
 | 201	->	 RPL_TRACECONNECTING
 | 200	->	 RPL_TRACELINK
 | 395	->	 RPL_NOUSERS
 | 394	->	 RPL_ENDOFUSERS
 | 393	->	 RPL_USERS
 | 392	->	 RPL_USERSSTART
 | 391	->	 RPL_TIME
 | 385	->	 RPL_NOTOPERANYMORE
 | 384	->	 RPL_MYPORTIS
 | 383	->	 RPL_YOURESERVICE
 | 382	->	 RPL_REHASHING
 | 381	->	 RPL_YOUREOPER
 | 376	->	 RPL_ENDOFMOTD
 | 375	->	 RPL_MOTDSTART
 | 374	->	 RPL_ENDOFINFO
 | 373	->	 RPL_INFOSTART
 | 372	->	 RPL_MOTD
 | 371	->	 RPL_INFO
 | 368	->	 RPL_ENDOFBANLIST
 | 367	->	 RPL_BANLIST
 | 365	->	 RPL_ENDOFLINKS
 | 364	->	 RPL_LINKS
 | 363	->	 RPL_CLOSEEND
 | 362	->	 RPL_CLOSING
 | 361	->	 RPL_KILLDONE
 | 366	->	 RPL_ENDOFNAMES
 | 353	->	 RPL_NAMREPLY
 | 315	->	 RPL_ENDOFWHO
 | 352	->	 RPL_WHOREPLY
 | 351	->	 RPL_VERSION
 | 342	->	 RPL_SUMMONING
 | 341	->	 RPL_INVITING
 | 332	->	 RPL_TOPIC
 | 331	->	 RPL_NOTOPIC
 | 324	->	 RPL_CHANNELMODEIS
 | 323	->	 RPL_LISTEND
 | 322	->	 RPL_LIST
 | 321	->	 RPL_LISTSTART
 | 306	->	 RPL_NOWAWAY
 | 305	->	 RPL_UNAWAY
 | 304	->	 RPL_TEXT
 | 303	->	 RPL_ISON
 | 302	->	 RPL_USERHOST
 | 301	->	 RPL_AWAY
 | 300	->	 RPL_NONE
 | _ -> raise (Unknown_Reply n)

and get_error_reply n =
match n with
  502	->	 ERR_USERSDONTMATCH
 | 501	->	 ERR_UMODEUNKNOWNFLAG
 | 492	->	 ERR_NOSERVICEHOST
 | 491	->	 ERR_NOOPERHOST
 | 484	->	 ERR_RESTRICTED
 | 483	->	 ERR_CANTKILLSERVER
 | 482	->	 ERR_CHANOPRIVSNEEDED
 | 481	->	 ERR_NOPRIVILEGES
 | 477	->	 ERR_NOCHANMODES
 | 476	->	 ERR_BADCHANMASK
 | 475	->	 ERR_BADCHANNELKEY
 | 474	->	 ERR_BANNEDFROMCHAN
 | 473	->	 ERR_INVITEONLYCHAN
 | 472	->	 ERR_UNKNOWNMODE
 | 471	->	 ERR_CHANNELISFULL
 | 467	->	 ERR_KEYSET
 | 466	->	 ERR_YOUWILLBEBANNED
 | 465	->	 ERR_YOUREBANNEDCREEP
 | 464	->	 ERR_PASSWDMISMATCH
 | 463	->	 ERR_NOPERMFORHOST
 | 462	->	 ERR_ALREADYREGISTRED
 | 461	->	 ERR_NEEDMOREPARAMS
 | 451	->	 ERR_NOTREGISTERED
 | 446	->	 ERR_USERSDISABLED
 | 445	->	 ERR_SUMMONDISABLED
 | 444	->	 ERR_NOLOGIN
 | 443	->	 ERR_USERONCHANNEL
 | 442	->	 ERR_NOTONCHANNEL
 | 441	->	 ERR_USERNOTINCHANNEL
 | 437	->	 ERR_UNAVAILRESOURCE
 | 436	->	 ERR_NICKCOLLISION
 | 435	->	 ERR_SERVICECONFUSED
 | 434	->	 ERR_SERVICENAMEINUSE
 | 433	->	 ERR_NICKNAMEINUSE
 | 432	->	 ERR_ERRONEUSNICKNAME
 | 431	->	 ERR_NONICKNAMEGIVEN
 | 424	->	 ERR_FILEERROR
 | 423	->	 ERR_NOADMININFO
 | 422	->	 ERR_NOMOTD
 | 421	->	 ERR_UNKNOWNCOMMAND
 | 416	->	 ERR_TOOMANYMATCHES
 | 415	->	 ERR_BADMASK
 | 414	->	 ERR_WILDTOPLEVEL
 | 413	->	 ERR_NOTOPLEVEL
 | 412	->	 ERR_NOTEXTTOSEND
 | 411	->	 ERR_NORECIPIENT
 | 409	->	 ERR_NOORIGIN
 | 408	->	 ERR_NOSUCHSERVICE
 | 407	->	 ERR_TOOMANYTARGETS
 | 406	->	 ERR_WASNOSUCHNICK
 | 405	->	 ERR_TOOMANYCHANNELS
 | 404	->	 ERR_CANNOTSENDTOCHAN
 | 403	->	 ERR_NOSUCHCHANNEL
 | 402	->	 ERR_NOSUCHSERVER
 | 401	->	 ERR_NOSUCHNICK
 | _ -> raise (Unknown_Reply n)

and get_connection_reply n =
  match n with
    5	-> RPL_BOUNCE
  | 4	-> RPL_MYINFO
  | 3	-> RPL_CREATED
  | 2	-> RPL_YOURHOST
  | 1	-> RPL_WELCOME
  | _	->	raise (Unknown_Reply n)


let get_reply_id n =
  if 200 <= n & n <= 399 then Command_reply (get_command_reply n)
  else if 400 <= n & n <= 599 then Error_reply (get_error_reply n)
  else if n < 100 then Connection_reply (get_connection_reply n)
  else raise (Unknown_Reply n)

and check_reply_type n = 
  if n < 100 then Type_connection
  else if 200 <= n & n <= 399 then Type_command
  else if 400 <= n & n <= 599 then Type_error
  else raise (Unknown_Reply n)
      
let get_reply_id_from_string = function 
    "319"	->	Command_reply RPL_WHOISCHANNELS
  | "394"	->	Command_reply RPL_ENDOFUSERS
  | "393"	->	Command_reply RPL_USERS
  | "392"	->	Command_reply RPL_USERSSTART
  | "318"	->	Command_reply RPL_ENDOFWHOIS
  | "317"	->	Command_reply RPL_WHOISIDLE
  | "316"	->	Command_reply RPL_WHOISCHANOP
  | "369"	->	Command_reply RPL_ENDOFWHOWAS
  | "314"	->	Command_reply RPL_WHOWASUSER
  | "313"	->	Command_reply RPL_WHOISOPERATOR
  | "312"	->	Command_reply RPL_WHOISSERVER
  | "311"	->	Command_reply RPL_WHOISUSER
  | "263"	->	Command_reply RPL_TRYAGAIN
  | "262"	->	Command_reply RPL_TRACEEND
  | "261"	->	Command_reply RPL_TRACELOG
  | "259"	->	Command_reply RPL_ADMINEMAIL
  | "258"	->	Command_reply RPL_ADMINLOC2
  | "257"	->	Command_reply RPL_ADMINLOC1
  | "256"	->	Command_reply RPL_ADMINME
  | "255"	->	Command_reply RPL_LUSERME
  | "254"	->	Command_reply RPL_LUSERCHANNELS
  | "253"	->	Command_reply RPL_LUSERUNKNOWN
  | "252"	->	Command_reply RPL_LUSEROP
  | "251"	->	Command_reply RPL_LUSERCLIENT
  | "250"	->	Command_reply RPL_STATSDLINE
  | "249"	->	Command_reply RPL_STATSDEBUG
  | "248"	->	Command_reply RPL_STATSDEFINE
  | "247"	->	Command_reply RPL_STATSBLINE
  | "246"	->	Command_reply RPL_STATSPING
  | "245"	->	Command_reply RPL_STATSSLINE
  | "244"	->	Command_reply RPL_STATSHLINE
  | "243"	->	Command_reply RPL_STATSOLINE
  | "242"	->	Command_reply RPL_STATSUPTIME
  | "241"	->	Command_reply RPL_STATSLLINE
  | "240"	->	Command_reply RPL_STATSVLINE
  | "235"	->	Command_reply RPL_SERVLISTEND
  | "234"	->	Command_reply RPL_SERVLIST
  | "233"	->	Command_reply RPL_SERVICE
  | "232"	->	Command_reply RPL_ENDOFSERVICES
  | "231"	->	Command_reply RPL_SERVICEINFO
  | "221"	->	Command_reply RPL_UMODEIS
  | "219"	->	Command_reply RPL_ENDOFSTATS
  | "218"	->	Command_reply RPL_STATSYLINE
  | "217"	->	Command_reply RPL_STATSQLINE
  | "216"	->	Command_reply RPL_STATSKLINE
  | "215"	->	Command_reply RPL_STATSILINE
  | "214"	->	Command_reply RPL_STATSNLINE
  | "213"	->	Command_reply RPL_STATSCLINE
  | "212"	->	Command_reply RPL_STATSCOMMANDS
  | "211"	->	Command_reply RPL_STATSLINKINFO
  | "210"	->	Command_reply RPL_TRACERECONNECT
  | "209"	->	Command_reply RPL_TRACECLASS
  | "208"	->	Command_reply RPL_TRACENEWTYPE
 | "207"	->	Command_reply RPL_TRACESERVICE
 | "206"	->	Command_reply RPL_TRACESERVER
 | "205"	->	Command_reply RPL_TRACEUSER
 | "204"	->	Command_reply RPL_TRACEOPERATOR
 | "203"	->	Command_reply RPL_TRACEUNKNOWN
 | "202"	->	Command_reply RPL_TRACEHANDSHAKE
 | "201"	->	Command_reply RPL_TRACECONNECTING
 | "200"	->	Command_reply RPL_TRACELINK
 | "395"	->	Command_reply RPL_NOUSERS
 | "391"	->	Command_reply RPL_TIME
 | "385"	->	Command_reply RPL_NOTOPERANYMORE
 | "384"	->	Command_reply RPL_MYPORTIS
 | "383"	->	Command_reply RPL_YOURESERVICE
 | "382"	->	Command_reply RPL_REHASHING
 | "381"	->	Command_reply RPL_YOUREOPER
 | "376"	->	Command_reply RPL_ENDOFMOTD
 | "375"	->	Command_reply RPL_MOTDSTART
 | "374"	->	Command_reply RPL_ENDOFINFO
 | "373"	->	Command_reply RPL_INFOSTART
 | "372"	->	Command_reply RPL_MOTD
 | "371"	->	Command_reply RPL_INFO
 | "368"	->	Command_reply RPL_ENDOFBANLIST
 | "367"	->	Command_reply RPL_BANLIST
 | "365"	->	Command_reply RPL_ENDOFLINKS
 | "364"	->	Command_reply RPL_LINKS
 | "363"	->	Command_reply RPL_CLOSEEND
 | "362"	->	Command_reply RPL_CLOSING
 | "361"	->	Command_reply RPL_KILLDONE
 | "366"	->	Command_reply RPL_ENDOFNAMES
 | "353"	->	Command_reply RPL_NAMREPLY
 | "315"	->	Command_reply RPL_ENDOFWHO
 | "352"	->	Command_reply RPL_WHOREPLY
 | "351"	->	Command_reply RPL_VERSION
 | "342"	->	Command_reply RPL_SUMMONING
 | "341"	->	Command_reply RPL_INVITING
 | "332"	->	Command_reply RPL_TOPIC
 | "331"	->	Command_reply RPL_NOTOPIC
 | "324"	->	Command_reply RPL_CHANNELMODEIS
 | "323"	->	Command_reply RPL_LISTEND
 | "322"	->	Command_reply RPL_LIST
 | "321"	->	Command_reply RPL_LISTSTART
 | "306"	->	Command_reply RPL_NOWAWAY
 | "305"	->	Command_reply RPL_UNAWAY
 | "304"	->	Command_reply RPL_TEXT
 | "303"	->	Command_reply RPL_ISON
 | "302"	->	Command_reply RPL_USERHOST
 | "301"	->	Command_reply RPL_AWAY
 | "300"	->	Command_reply RPL_NONE
 | "502"	->	Error_reply ERR_USERSDONTMATCH
 | "501"	->	Error_reply ERR_UMODEUNKNOWNFLAG
 | "492"	->	Error_reply ERR_NOSERVICEHOST
 | "491"	->	Error_reply ERR_NOOPERHOST
 | "484"	->	Error_reply ERR_RESTRICTED
 | "483"	->	Error_reply ERR_CANTKILLSERVER
 | "482"	->	Error_reply ERR_CHANOPRIVSNEEDED
 | "481"	->	Error_reply ERR_NOPRIVILEGES
 | "477"	->	Error_reply ERR_NOCHANMODES
 | "476"	->	Error_reply ERR_BADCHANMASK
 | "475"	->	Error_reply ERR_BADCHANNELKEY
 | "474"	->	Error_reply ERR_BANNEDFROMCHAN
 | "473"	->	Error_reply ERR_INVITEONLYCHAN
 | "472"	->	Error_reply ERR_UNKNOWNMODE
 | "471"	->	Error_reply ERR_CHANNELISFULL
 | "467"	->	Error_reply ERR_KEYSET
 | "466"	->	Error_reply ERR_YOUWILLBEBANNED
 | "465"	->	Error_reply ERR_YOUREBANNEDCREEP
 | "464"	->	Error_reply ERR_PASSWDMISMATCH
 | "463"	->	Error_reply ERR_NOPERMFORHOST
 | "462"	->	Error_reply ERR_ALREADYREGISTRED
 | "461"	->	Error_reply ERR_NEEDMOREPARAMS
 | "451"	->	Error_reply ERR_NOTREGISTERED
 | "446"	->	Error_reply ERR_USERSDISABLED
 | "445"	->	Error_reply ERR_SUMMONDISABLED
 | "444"	->	Error_reply ERR_NOLOGIN
 | "443"	->	Error_reply ERR_USERONCHANNEL
 | "442"	->	Error_reply ERR_NOTONCHANNEL
 | "441"	->	Error_reply ERR_USERNOTINCHANNEL
 | "437"	->	Error_reply ERR_UNAVAILRESOURCE
 | "436"	->	Error_reply ERR_NICKCOLLISION
 | "435"	->	Error_reply ERR_SERVICECONFUSED
 | "434"	->	Error_reply ERR_SERVICENAMEINUSE
 | "433"	->	Error_reply ERR_NICKNAMEINUSE
 | "432"	->	Error_reply ERR_ERRONEUSNICKNAME
 | "431"	->	Error_reply ERR_NONICKNAMEGIVEN
 | "424"	->	Error_reply ERR_FILEERROR
 | "423"	->	Error_reply ERR_NOADMININFO
 | "422"	->	Error_reply ERR_NOMOTD
 | "421"	->	Error_reply ERR_UNKNOWNCOMMAND
 | "416"	->	Error_reply ERR_TOOMANYMATCHES
 | "415"	->	Error_reply ERR_BADMASK
 | "414"	->	Error_reply ERR_WILDTOPLEVEL
 | "413"	->	Error_reply ERR_NOTOPLEVEL
 | "412"	->	Error_reply ERR_NOTEXTTOSEND
 | "411"	->	Error_reply ERR_NORECIPIENT
 | "409"	->	Error_reply ERR_NOORIGIN
 | "408"	->	Error_reply ERR_NOSUCHSERVICE
 | "407"	->	Error_reply ERR_TOOMANYTARGETS
 | "406"	->	Error_reply ERR_WASNOSUCHNICK
 | "405"	->	Error_reply ERR_TOOMANYCHANNELS
 | "404"	->	Error_reply ERR_CANNOTSENDTOCHAN
 | "403"	->	Error_reply ERR_NOSUCHCHANNEL
 | "402"	->	Error_reply ERR_NOSUCHSERVER
 | "401"	->	Error_reply ERR_NOSUCHNICK
 | "005"	->	Connection_reply RPL_BOUNCE
 | "004"	->	Connection_reply RPL_MYINFO
 | "003"	->	Connection_reply RPL_CREATED
 | "002"	->	Connection_reply RPL_YOURHOST
 | "001"	->	Connection_reply RPL_WELCOME
 | x	->	raise (Unknown_Reply (int_of_string x))

let getReplyNumber r = 
  match r with
    Command_reply rs ->
      begin
	match rs with
	  RPL_TRYAGAIN	->	"263"
	|  RPL_TRACEEND	->	"262"
	|  RPL_TRACELOG	->	"261"
	|  RPL_ADMINEMAIL	->	"259"
	|  RPL_ADMINLOC2	->	"258"
	|  RPL_ADMINLOC1	->	"257"
	|  RPL_ADMINME	->	"256"
	|  RPL_LUSERME	->	"255"
	|  RPL_LUSERCHANNELS	->	"254"
	|  RPL_LUSERUNKNOWN	->	"253"
	|  RPL_LUSEROP	->	"252"
	|  RPL_LUSERCLIENT	->	"251"
	|  RPL_STATSDLINE	->	"250"
	|  RPL_STATSDEBUG	->	"249"
	|  RPL_STATSDEFINE	->	"248"
	|  RPL_STATSBLINE	->	"247"
	|  RPL_STATSPING	->	"246"
	|  RPL_STATSSLINE	->	"245"
	|  RPL_STATSHLINE	->	"244"
	|  RPL_STATSOLINE	->	"243"
	|  RPL_STATSUPTIME	->	"242"
	|  RPL_STATSLLINE	->	"241"
	|  RPL_STATSVLINE	->	"240"
	|  RPL_SERVLISTEND	->	"235"
	|  RPL_SERVLIST	->	"234"
	|  RPL_SERVICE	->	"233"
	|  RPL_ENDOFSERVICES	->	"232"
	|  RPL_SERVICEINFO	->	"231"
	|  RPL_UMODEIS	->	"221"
	|  RPL_ENDOFSTATS	->	"219"
	|  RPL_STATSYLINE	->	"218"
	|  RPL_STATSQLINE	->	"217"
	|  RPL_STATSKLINE	->	"216"
	|  RPL_STATSILINE	->	"215"
	|  RPL_STATSNLINE	->	"214"
	|  RPL_STATSCLINE	->	"213"
	|  RPL_STATSCOMMANDS	->	"212"
	|  RPL_STATSLINKINFO	->	"211"
	|  RPL_TRACERECONNECT	->	"210"
	|  RPL_TRACECLASS	->	"209"
	|  RPL_TRACENEWTYPE	->	"208"
	|  RPL_TRACESERVICE	->	"207"
	|  RPL_TRACESERVER	->	"206"
	|  RPL_TRACEUSER	->	"205"
	|  RPL_TRACEOPERATOR	->	"204"
	|  RPL_TRACEUNKNOWN	->	"203"
	|  RPL_TRACEHANDSHAKE	->	"202"
	|  RPL_TRACECONNECTING	->	"201"
	|  RPL_TRACELINK	->	"200"
	|  RPL_NOUSERS	->	"395"
	|  RPL_ENDOFUSERS	->	"394"
	|  RPL_USERS	->	"393"
	|  RPL_USERSSTART	->	"392"
	|  RPL_TIME	->	"391"
	|  RPL_NOTOPERANYMORE	->	"385"
	|  RPL_MYPORTIS	->	"384"
	|  RPL_YOURESERVICE	->	"383"
	|  RPL_REHASHING	->	"382"
	|  RPL_YOUREOPER	->	"381"
	|  RPL_ENDOFMOTD	->	"376"
	|  RPL_MOTDSTART	->	"375"
	|  RPL_ENDOFINFO	->	"374"
	|  RPL_INFOSTART	->	"373"
	|  RPL_MOTD	->	"372"
	|  RPL_INFO	->	"371"
	|  RPL_ENDOFBANLIST	->	"368"
	|  RPL_BANLIST	->	"367"
	|  RPL_ENDOFLINKS	->	"365"
	|  RPL_LINKS	->	"364"
	|  RPL_CLOSEEND	->	"363"
	|  RPL_CLOSING	->	"362"
	|  RPL_KILLDONE	->	"361"
	|  RPL_ENDOFNAMES	->	"366"
	|  RPL_NAMREPLY	->	"353"
	|  RPL_ENDOFWHO	->	"315"
	|  RPL_WHOREPLY	->	"352"
	|  RPL_VERSION	->	"351"
	|  RPL_SUMMONING	->	"342"
	|  RPL_INVITING	->	"341"
	|  RPL_TOPIC	->	"332"
	|  RPL_NOTOPIC	->	"331"
	|  RPL_CHANNELMODEIS	->	"324"
	|  RPL_LISTEND	->	"323"
	|  RPL_LIST	->	"322"
	|  RPL_LISTSTART	->	"321"
	|  RPL_WHOISCHANNELS	->	"319"
	|  RPL_ENDOFWHOIS	->	"318"
	|  RPL_WHOISIDLE	->	"317"
	|  RPL_WHOISCHANOP	->	"316"
	|  RPL_ENDOFWHOWAS	->	"369"
	|  RPL_WHOWASUSER	->	"314"
	|  RPL_WHOISOPERATOR	->	"313"
	|  RPL_WHOISSERVER	->	"312"
	|  RPL_WHOISUSER	->	"311"
	|  RPL_NOWAWAY	->	"306"
	|  RPL_UNAWAY	->	"305"
	|  RPL_TEXT	->	"304"
	|  RPL_ISON	->	"303"
	|  RPL_USERHOST	->	"302"
	|  RPL_AWAY	->	"301"
	|  RPL_NONE	->	"300"
      end
  | Error_reply rs ->
      begin
	match rs with
	  ERR_USERSDONTMATCH	->	"502"
	| ERR_UMODEUNKNOWNFLAG	->	"501"
	| ERR_NOSERVICEHOST	->	"492"
	| ERR_NOOPERHOST	->	"491"
	| ERR_RESTRICTED	->	"484"
	| ERR_CANTKILLSERVER	->	"483"
	| ERR_CHANOPRIVSNEEDED	->	"482"
	| ERR_NOPRIVILEGES	->	"481"
	| ERR_NOCHANMODES	->	"477"
	| ERR_BADCHANMASK	->	"476"
	| ERR_BADCHANNELKEY	->	"475"
	| ERR_BANNEDFROMCHAN	->	"474"
	| ERR_INVITEONLYCHAN	->	"473"
	| ERR_UNKNOWNMODE	->	"472"
	| ERR_CHANNELISFULL	->	"471"
	| ERR_KEYSET	->	"467"
	| ERR_YOUWILLBEBANNED	->	"466"
	| ERR_YOUREBANNEDCREEP	->	"465"
	| ERR_PASSWDMISMATCH	->	"464"
	| ERR_NOPERMFORHOST	->	"463"
	| ERR_ALREADYREGISTRED	->	"462"
	| ERR_NEEDMOREPARAMS	->	"461"
	| ERR_NOTREGISTERED	->	"451"
	| ERR_USERSDISABLED	->	"446"
	| ERR_SUMMONDISABLED	->	"445"
	| ERR_NOLOGIN	->	"444"
	| ERR_USERONCHANNEL	->	"443"
	| ERR_NOTONCHANNEL	->	"442"
	| ERR_USERNOTINCHANNEL	->	"441"
	| ERR_UNAVAILRESOURCE	->	"437"
	| ERR_NICKCOLLISION	->	"436"
	| ERR_SERVICECONFUSED	->	"435"
	| ERR_SERVICENAMEINUSE	->	"434"
	| ERR_NICKNAMEINUSE	->	"433"
	| ERR_ERRONEUSNICKNAME	->	"432"
	| ERR_NONICKNAMEGIVEN	->	"431"
	| ERR_FILEERROR	->	"424"
	| ERR_NOADMININFO	->	"423"
	| ERR_NOMOTD	->	"422"
	| ERR_UNKNOWNCOMMAND	->	"421"
	| ERR_TOOMANYMATCHES	->	"416"
	| ERR_BADMASK	->	"415"
	| ERR_WILDTOPLEVEL	->	"414"
	| ERR_NOTOPLEVEL	->	"413"
	| ERR_NOTEXTTOSEND	->	"412"
	| ERR_NORECIPIENT	->	"411"
	| ERR_NOORIGIN	->	"409"
	| ERR_NOSUCHSERVICE	->	"408"
	| ERR_TOOMANYTARGETS	->	"407"
	| ERR_WASNOSUCHNICK	->	"406"
	| ERR_TOOMANYCHANNELS	->	"405"
	| ERR_CANNOTSENDTOCHAN	->	"404"
	| ERR_NOSUCHCHANNEL	->	"403"
	| ERR_NOSUCHSERVER	->	"402"
	| ERR_NOSUCHNICK	->	"401"
      end
  | Connection_reply rs ->
      begin 
	match rs with
	  RPL_BOUNCE	->	"005"
	| RPL_MYINFO	->	"004"
	| RPL_CREATED	->	"003"
	| RPL_YOURHOST	->	"002"
	| RPL_WELCOME	->	"001"
      end


