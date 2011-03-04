(* Ocsigen
 * framepp.mli Copyright (C) 2005 Denis Berthod
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

val string_of_header : Ocsigen_http_frame.Http_header.http_header -> string

val string_of_method : Ocsigen_http_frame.Http_header.http_method -> string

val method_of_string : string -> Ocsigen_http_frame.Http_header.http_method

val string_of_proto : Ocsigen_http_frame.Http_header.proto -> string

val proto_of_string : string -> Ocsigen_http_frame.Http_header.proto
