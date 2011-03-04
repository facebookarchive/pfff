(* Ocsigen
 * http://www.ocsigen.org
 * Module ocsigen_parseconfig.ml
 * Copyright (C) 2005 Vincent Balat, Nataliya Guts
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

(** Config file parsing *)

(** Parse a size ("infinity" or using SI or binary units,
    e.g. 10 10B 10o 10ko 10kB 10kiB 10MiB 10TB ...).
    Raises [Failure "Ocsigen_parseconfig.parse_size"] in case of error.
*)
val parse_size : string -> int64 option

(** [parse_size_tag tag s] parses a size (same syntax as [parse_size]).
    In case of error, raises [Ocsigen_config.Config_file_error m] where [m]
    is an error message explaining that a size was expected in tag [<tag>].
*)
val parse_size_tag : string -> string -> int64 option

(** Parse a string (PCDATA) as XML content.
    Raises [Failure "Ocsigen_parseconfig.parse_string"] in case of error.
*)
val parse_string : Simplexmlparser.xml list -> string

(** [parse_string_tag tag s] parses a string (same syntax as [parse_string]).
    In case of error, raises [Ocsigen_config.Config_file_error m] where [m]
    is an error message explaining that a string was expected in tag [<tag>].
*)
val parse_string_tag : string -> Simplexmlparser.xml list -> string


(** Parses the [hostfilter] field of the configuration file, which
    is a disjunction of possible hostnames (that can themselves contain
    wildcards) *)
val parse_host_field: string option -> Ocsigen_extensions.virtual_hosts

(**/**)

val parser_config : Simplexmlparser.xml list ->
  Simplexmlparser.xml list list
val parse_server : bool -> Simplexmlparser.xml list -> unit

type socket_type =
  | IPv4 of Unix.inet_addr
  | IPv6 of Unix.inet_addr
  | All

val extract_info :
  Simplexmlparser.xml list ->
  (string option * string option) *
  ((string option * string option) option *
     (socket_type * int) list * (socket_type * int) list) * (int * int)
val parse_config :
  ?file:string ->
  unit ->
  Simplexmlparser.xml list list
