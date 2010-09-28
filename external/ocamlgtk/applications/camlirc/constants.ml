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

(* $Id: constants.ml 1354 2007-07-20 04:18:38Z garrigue $ *)
open Str
open Unix

let doctype = Printf.sprintf "Caml IRC client %d.%d" 1 1
let software = "CamlIRC"
let version = "0.01"
let datestring = 
  match split (regexp " ") "$Date: 2007-07-19 21:18:38 -0700 (Thu, 19 Jul 2007) $" with
  | [_;date;time;_] ->
      date^"-"^(global_replace (regexp ":") "-" time) 
  | _ -> "" 

(* *)

let id = software^" "^version^"("^datestring^")"
and author = ""

(* getlogin doesn't work all the time.  I observe it to raise an
   exception when I log in via xdm on my Debian system, March 21, 2002.
   Using getpwuid instead.  tim@fungible.com. *)

let user_entry = getpwuid (getuid ());;

let config_file = user_entry.pw_dir^"/.camlirc.xml";;
