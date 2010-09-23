(**************************************************************************)
(*                Lablgtk                                                 *)
(*                                                                        *)
(*    This program is free software; you can redistribute it              *)
(*    and/or modify it under the terms of the GNU Library General         *)
(*    Public License as published by the Free Software Foundation         *)
(*    version 2, with the exception described in file COPYING which       *)
(*    comes with the library.                                             *)
(*                                                                        *)
(*    This program is distributed in the hope that it will be useful,     *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU Library General Public License for more details.                *)
(*                                                                        *)
(*    You should have received a copy of the GNU Library General          *)
(*    Public License along with this program; if not, write to the        *)
(*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         *)
(*    Boston, MA 02111-1307  USA                                          *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

type unichar = int
type unistring = unichar array

module Error = struct
  type error = 
    | NO_CONVERSION
    | ILLEGAL_SEQUENCE
    | FAILED
    | PARTIAL_INPUT
    | BAD_URI
    | NOT_ABSOLUTE_PATH
  exception Error of error * string

  let raise_bad_utf8 () = 
    raise (Error (ILLEGAL_SEQUENCE, "Invalid byte sequence for UTF-8 string"))
end

open Error

let rec log64 n =
  if n = 0 then 0 else
  1 + log64 (n lsr 5)
  
let utf8_storage_len n =
  if n < 0x80 then 1 else
  log64 (n lsr 1)

(* this function is not exported, so it's OK to do a few 'unsafe' things *)
let write_unichar s ~pos (c : unichar) =
  let len = utf8_storage_len c in
  let p = !pos in
  if len = 1 then
    String.unsafe_set s p (Char.unsafe_chr c)
  else begin
    String.unsafe_set s p
      (Char.unsafe_chr (((1 lsl len - 1) lsl (8-len)) lor (c lsr ((len-1)*6))));
    for i = 1 to len-1 do
      String.unsafe_set s (p+i) 
	(Char.unsafe_chr (((c lsr ((len-1-i)*6)) land 0x3f) lor 0x80))
    done;
  end;
  pos := p + len

let from_unichar (n : unichar) =
  let s = String.create 6 and pos = ref 0 in
  write_unichar s ~pos n;
  String.sub s 0 !pos

let from_unistring (s : unistring) =
  let len = Array.length s in
  let r = String.create (len*6) in
  let pos = ref 0 in
  for i = 0 to len-1 do write_unichar r ~pos s.(i) done;
  String.sub r 0 !pos

let rec hi_bits n =
  if n land 0x80 = 0 then 0 else
  1 + hi_bits (n lsl 1)

let to_unichar s ~pos : unichar =
  let c = Char.code s.[!pos] in
  incr pos;
  let n = hi_bits c in
  if n = 0 then c else (* if string is valid then 2 <= n <= 6 *)
  let u = ref (c land (1 lsl (7-n) - 1)) in
  for i = 1 to n-1 do
    let c = Char.code s.[!pos] in
    u := !u lsl 6 + c land 0x3f ;
    incr pos
  done;
  !u

let first_char s =
  to_unichar s ~pos:(ref 0)

let validate c =
  c < 0x110000 && (c land 0x7FFFF800) <> 0xD800 &&
  (c < 0xFDD0 || c > 0xFDEF) && (c land 0xFFFE) <> 0xFFFE

let to_unichar_validated s ~pos : unichar =
  let c = Char.code s.[!pos] in
  incr pos;
  let n = hi_bits c in
  if n = 0 then c else begin
    if n = 1 || n > 6 then raise_bad_utf8 () ;
    if !pos + n > String.length s then
      raise (Error(PARTIAL_INPUT, "partial UTF-8 character"));
    let u = ref (c land (1 lsl (7-n) - 1)) in
    for i = 1 to n-1 do
      let c = Char.code s.[!pos] in
      if c lsr 6 <> 0b10 then raise_bad_utf8 () ;
      u := !u lsl 6 + c land 0x3f ;
      incr pos
    done;
    let v = !u in
    (* reject overlong sequences && invalid values *)
    if utf8_storage_len v <> n || not (validate v)
    then raise_bad_utf8 () ;
    v
  end

let rec end_of_char s ~pos =
  let c = Char.code s.[pos] in
  if (c land 0xc0) = 0x80 then end_of_char s ~pos:(pos+1) else pos

let next s ~pos =
  let c = Char.code s.[pos] in
  let n = hi_bits c in
  if n = 0 then pos + 1 else
  if n = 1 then end_of_char s ~pos:(pos+1) else pos + n

let length s =
  let len = String.length s in
  let rec loop count ~pos =
    if pos >= len then count else
    loop (count+1) ~pos:(next s ~pos)
  in loop 0 ~pos:0

let to_unistring s : unistring =
  let len = length s in
  let us = Array.create len 0 in
  let pos = ref 0 in
  for i = 0 to len - 1 do
    us.(i) <- to_unichar s ~pos
  done;
  us
