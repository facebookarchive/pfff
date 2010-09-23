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

(* $Id: ircArg.ml 1354 2007-07-20 04:18:38Z garrigue $ *)
open Str

type arg = string list

let sep = regexp " :"
and spl = regexp "[ \t]+"

let process_arg s =
  let
      (rest, taillist) = 
    try
      let s1 = " "^s
      in  
      let n = search_forward sep s1 0
      in 
      (string_before s1 n, [string_after s1 (n+2)])
    with Not_found -> (s,[])
  in
  try
    (split spl rest)@taillist 
  with Not_found -> taillist

let to_string sl = 
  List.fold_left (fun s r -> (s^" "^r))  "" sl
