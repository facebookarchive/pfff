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

(* $Id: gpointer.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open StdLabels

(* marked pointers *)
type 'a optaddr

let optaddr : 'a option -> 'a optaddr =
  function
      None -> Obj.magic 0
    | Some x -> Obj.magic x

(* naked pointers *)
type optstring

let raw_null = snd (Obj.magic Nativeint.zero)

let optstring : string option -> optstring =
  function
      None -> raw_null
    | Some x -> Obj.magic x

(* boxed pointers *)
type boxed
let boxed_null : boxed = Obj.magic Nativeint.zero

external peek_string : ?pos:int -> ?len:int -> boxed -> string
    = "ml_string_at_pointer"
external peek_int : boxed -> int
    = "ml_int_at_pointer"
external poke_int : boxed -> int -> unit
    = "ml_set_int_at_pointer"
external peek_nativeint : boxed -> nativeint
    = "ml_long_at_pointer"
external poke_nativeint : boxed -> nativeint -> unit
    = "ml_set_long_at_pointer"

type 'a optboxed

let optboxed : 'a option -> 'a optboxed =
  function
      None -> Obj.magic boxed_null
    | Some obj -> Obj.magic obj

let may_box ~f obj : 'a optboxed =
  match obj with
    None -> Obj.magic boxed_null
  | Some obj -> Obj.magic (f obj : 'a)

(* Variant tables *)

type 'a variant_table constraint 'a = [> ]

external decode_variant : 'a variant_table -> int -> 'a
  = "ml_ml_lookup_from_c"
external encode_variant : 'a variant_table -> 'a -> int
  = "ml_ml_lookup_to_c"

let encode_flags tbl l =
  List.fold_left l ~init:0 ~f:(fun acc v -> acc lor (encode_variant tbl v))

let decode_flags tbl c =
  let l = ref [] in
  for i = 30 downto 0 do (* only 31-bits in ocaml usual integers *)
    let d = 1 lsl i in
    if c land d <> 0 then l := decode_variant tbl d :: !l
  done;
  !l

(* Exceptions *)

exception Null
let _ =  Callback.register_exception "null_pointer" Null

(* Stable pointer *)
type 'a stable
external stable_copy : 'a -> 'a stable = "ml_stable_copy"

(* Region pointers *)

type region = { data: Obj.t; path: int array; offset:int; length: int }

let length reg = reg.length

let unsafe_create_region ~path ~get_length data =
  { data = Obj.repr data; path = path; offset = 0; length = get_length data }

let sub ?(pos=0) ?len reg =
  let len = match len with Some x -> x | None -> reg.length - pos in
  if pos < 0 || pos > reg.length || pos + len > reg.length then
    invalid_arg "Gpointer.sub";
  { reg with offset = reg.offset + pos; length = len }

external unsafe_get_byte : region -> pos:int -> int
    = "ml_gpointer_get_char"
external unsafe_set_byte : region -> pos:int -> int -> unit
    = "ml_gpointer_set_char"
external unsafe_blit : src:region -> dst:region -> unit
    ="ml_gpointer_blit"

(* handle with care, if allocation not static *)
external get_addr : region -> nativeint
    = "ml_gpointer_get_addr"

let get_byte reg ~pos =
  if pos >= reg.length then invalid_arg "Gpointer.get_char";
  unsafe_get_byte reg ~pos

let set_byte reg ~pos ch =
  if pos >= reg.length then invalid_arg "Gpointer.set_char";
  unsafe_set_byte reg ~pos ch

let blit ~src ~dst =
  if src.length <> dst.length then invalid_arg "Gpointer.blit";
  unsafe_blit ~src ~dst

(* Making a region from a string is easy *)
let region_of_string =
  unsafe_create_region ~path:[||] ~get_length:String.length

let string_of_region reg =
  let s = String.create reg.length in
  let reg' = region_of_string s in
  unsafe_blit reg reg';
  s

(* Access bigarrays breaking the abstraction... dirty *)
type 'a bigarray = (int, Bigarray.int8_unsigned_elt, 'a) Bigarray.Array1.t
let bigarray_size (arr : 'a bigarray) =
  let size =
    { data = Obj.repr arr; path = [|1+4|]; offset = 0; length = 0 } in
  Nativeint.to_int (get_addr size)
let region_of_bigarray arr =
  unsafe_create_region ~path:[|1|] ~get_length:bigarray_size arr
