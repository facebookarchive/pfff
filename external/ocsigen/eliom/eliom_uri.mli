(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_uri
 * Copyright (C) 2007 Vincent Balat
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

(**/**)

open Lwt
open Eliom_parameters
open Eliom_services

(** Constructs a relative link (low level).
    The first parameter is the current URL,
    the second is the destination.
*)
val reconstruct_relative_url_path : 
  string list -> string list -> string list

 

val make_string_uri :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b, 
           [< `Attached of (Eliom_services.attached_service_kind,
                            [< Eliom_services.getpost]) a_s
           | `Nonattached of [< Eliom_services.getpost ] na_s ],
           [< Eliom_services.suff ], 'c, 'd, [< Eliom_services.registrable ],
           'return) Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameters.nl_params_set -> 'a -> string

val make_post_uri_components :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< `Attached of
                ([> `External ], 'c) Eliom_services.a_s &
                  ([> `External ], 'd) Eliom_services.a_s
           | `Nonattached of
               'e Eliom_services.na_s &
                 'f Eliom_services.na_s ],
           [< `WithSuffix | `WithoutSuffix ], 'g, 'h, 'i, 'j)
    Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:'k ->
  ?nl_params:Eliom_parameters.nl_params_set ->
  ?keep_get_na_params:bool ->
  'a ->
  'b ->
  string * (string * string) list * string option *
    (string * string) list

val make_string_uri_from_components : 
  string * (string * string) list * string option -> string

val make_uri_components_ :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< `Attached of ([> `External ], 'c) Eliom_services.a_s
           | `Nonattached of 'd Eliom_services.na_s ],
           [< `WithSuffix | `WithoutSuffix ], 'e, 'f, 'g, 'h)
    Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameters.nl_params_set ->
  unit -> string * (string * string) list * string option

val make_uri_components :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< `Attached of ([> `External ], 'c) Eliom_services.a_s
           | `Nonattached of 'd Eliom_services.na_s ],
           [< `WithSuffix | `WithoutSuffix ], 'e, 'f, 'g, 'h)
    Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameters.nl_params_set ->
  'a -> string * (string * string) list * string option

val make_post_uri_components_ :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
                    [< `Attached of ([> `External ], 'd) Eliom_services.a_s
                     | `Nonattached of 'f Eliom_services.na_s ],
           [< `WithSuffix | `WithoutSuffix ], 'g, 'h, 'i, 'j)
    Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:'k ->
  ?nl_params:Eliom_parameters.nl_params_set ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?keep_get_na_params:bool ->
  'a ->
  unit ->
  string * (string * string) list * string option *
    (string * string) list


val make_post_uri_components :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< `Attached of ([> `External ], 'd) Eliom_services.a_s
           | `Nonattached of 'f Eliom_services.na_s ],
           [< `WithSuffix | `WithoutSuffix ], 'g, 'h, 'i, 'j)
    Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:'k ->
  ?nl_params:Eliom_parameters.nl_params_set ->
  ?keep_get_na_params:bool ->
  'a ->
  'b ->
  string * (string * string) list * string option *
    (string * string) list

(**/**)
val make_proto_prefix :
  ?hostname:string -> ?port:int -> 
  sp:Eliom_common.server_params option ->
  bool -> string

