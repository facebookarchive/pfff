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

(* $Id: eucjp.ml 1354 2007-07-20 04:18:38Z garrigue $ *)

open String
open Char

exception Conversion_error

let ntol s =
  let is_ascii = ref true
  and dest = Buffer.create 0
  and str = Stream.of_string s
  in
  try
    while true do
      let c = Stream.next str in
      match c with
	'\027' ->
	  begin
	    let c1 = Stream.next str
	    and c2 = Stream.next str
	    in
	    match (c1,c2) with
	      ('(', 'B') -> is_ascii := true
	    | ('$', 'B') -> is_ascii := false
	    | _ -> raise Conversion_error
	  end
      | _ ->
	  Buffer.add_char dest 
	    (if !is_ascii then c else (chr ((code c) + 128)))
    done; Buffer.contents dest
  with Stream.Failure -> Buffer.contents dest

let lton s =
  let is_ascii = ref true
  and dest = Buffer.create 0
  and str = Stream.of_string s
  in
  try
    while true do
      let c = Stream.next str in
      if (code c) > 127 then
	if !is_ascii then
	  begin
	    is_ascii := false;
	    Buffer.add_string dest "\027$B";
	    Buffer.add_char dest (chr ((code c) - 128))
	  end
	else Buffer.add_char dest (chr ((code c) - 128))
      else
	if !is_ascii then
	  Buffer.add_char dest c
	else 
	  begin
	    is_ascii := true;
	    Buffer.add_string dest "\027(B";
	    Buffer.add_char dest c
	  end
    done; 
    if not !is_ascii then Buffer.add_string dest "\027(B";
    Buffer.contents dest
  with Stream.Failure -> 
    if not !is_ascii then Buffer.add_string dest "\027(B";
    Buffer.contents dest
      
