(* $Id: xML.mli,v 1.14 2004/12/13 14:57:45 ohl Exp $

   Copyright (C) 2004 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>

   XHTML is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   XHTML is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)

type aname = string
type attrib = string * Js.Unsafe.any
type attribs = attrib list
type event = unit -> unit
val int_attrib : 'a -> 'b -> 'a * Js.Unsafe.any
val float_attrib : 'a -> 'b -> 'a * Js.Unsafe.any
val string_attrib : 'a -> string -> 'a * Js.Unsafe.any
val space_sep_attrib : 'a -> string list -> 'a * Js.Unsafe.any
val comma_sep_attrib : 'a -> string list -> 'a * Js.Unsafe.any
val event_attrib : 'a -> (unit -> 'b) -> 'a * Js.Unsafe.any
val attrib_name : 'a * 'b -> 'a
type ename = string
type elt = Dom.node Js.t
val empty : unit -> 'a
val comment : 'a -> 'b
val pcdata : string -> elt
val encodedpcdata : string -> elt
val entity : 'a -> 'b
val cdata : string -> elt
val cdata_script : string -> elt
val cdata_style : string -> elt
val node : ?a:('a * 'b) list -> string -> #Dom.node Js.t list -> elt
val leaf : ?a:('a * 'b) list -> string -> elt
val lwt_register_event : elt -> 'a -> ('b -> 'c Lwt.t) -> 'b -> unit
val register_event : elt -> 'a -> ('b -> 'c) -> 'b -> unit
type ref_tree = Ref_tree of int option * (int * ref_tree) list
val ref_node : elt -> int
val class_name : string
