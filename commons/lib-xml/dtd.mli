(*
 * Xml Light, an small Xml parser/printer with DTD support.
 * Copyright (C) 2003 Nicolas Cannasse (ncannasse@motion-twin.com)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Dtd_types
type dtd = Dtd_types.dtd


(** {6 The DTD Functions} *)

(** Parse the named file into a Dtd data structure. Raise
	{!Xml.File_not_found} if an error occured while opening the file. 
	Raise {!Dtd.Parse_error} if parsing failed. *)
val parse_file : string -> dtd

(** Read the content of the in_channel and parse it into a Dtd data
 structure. Raise {!Dtd.Parse_error} if parsing failed. *)
val parse_in : in_channel -> dtd

(** Parse the string containing a Dtd document into a Dtd data
 structure. Raise {!Dtd.Parse_error} if parsing failed. *)
val parse_string : string -> dtd


(** Print a DTD element into a string. You can easily get a DTD
 document from a DTD data structure using for example
 [String.concat "\n" (List.map Dtd.to_string) my_dtd] *)
val to_string : dtd_item -> string


val parse_error : parse_error -> string
val check_error : check_error -> string
val prove_error : prove_error -> string

(**/**)

(* internal usage only... *)
val _raises : (string -> exn) -> unit
