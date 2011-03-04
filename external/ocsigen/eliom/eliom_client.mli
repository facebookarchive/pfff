(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_client.ml
 * Copyright (C) 2010 Vincent Balat
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


(** Call a server side service and change the current page.
    If the service belongs to the same application,
    the client side program is not stopped, and only
    the content (not the container) is reloaded. *)
val change_page :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< Eliom_services.service_kind ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 
           [< Eliom_services.registrable ], 'return)
          Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:(string * string) list Ocsigen_lib.String_Table.t ->
  ?keep_get_na_params:bool -> 'a -> 'b -> unit Lwt.t

(** Call a server side service that return a Caml value. *)
val call_caml_service :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< Eliom_services.service_kind ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 
           [< Eliom_services.registrable ], 'return Eliom_parameters.caml)
          Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:(string * string) list Ocsigen_lib.String_Table.t ->
  ?keep_get_na_params:bool -> 'a -> 'b -> 'return Lwt.t


(** Stop current program and load a new page. *)
val exit_to :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< Eliom_services.service_kind ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 
           [< Eliom_services.registrable ], 'return)
          Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:(string * string) list Ocsigen_lib.String_Table.t ->
  ?keep_get_na_params:bool -> 'a -> 'b -> unit

(** Call a service returning a list of html blocks. *)
val get_subpage :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< Eliom_services.service_kind ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 
           [< Eliom_services.registrable ], 'return)
          Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:(string * string) list Ocsigen_lib.String_Table.t ->
  ?keep_get_na_params:bool -> 'a -> 'b -> 
  [< `PCDATA | Xhtmltypes.flow ] XHTML5.M.elt list Lwt.t

(** (low level) Call a server side service and return the content
    of the resulting HTTP frame as a string. *)
val call_service :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< Eliom_services.service_kind ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 
           [< Eliom_services.registrable ], 'return)
          Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:(string * string) list Ocsigen_lib.String_Table.t ->
  ?keep_get_na_params:bool -> 'a -> 'b -> string Lwt.t

(** (low level) Change the URL, without doing a request.
    As browsers do not not allow to change the URL (for security reasons),
    we write the new URL in the fragment part of the URL.
    A script must do the redirection if there is something in the fragment.
    Usually this function is only for internal use.
*)
val change_url :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< Eliom_services.service_kind ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e, 
           [< Eliom_services.registrable ], 'return)
          Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:(string * string) list Ocsigen_lib.String_Table.t ->
  ?keep_get_na_params:bool -> 'a -> 'b -> unit

(** register a function to be called on page change *)
val on_unload : (unit -> unit Lwt.t) -> unit

(**/**)

val make_a_with_onclick :
  (?a:'a -> ?onclick:XML.event -> 'c -> 'd) ->
  ('d -> string -> (unit -> unit Lwt.t) -> unit -> 'f) ->
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  ?a:'a ->
  service:('get, unit, [< Eliom_services.get_service_kind ],
           [< Eliom_services.suff ], 'gn, 'pn,
           [< Eliom_services.registrable ], 'return)
    Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameters.nl_params_set ->
  'c -> 'get -> 'd

val set_content : Eliom_services.eliom_appl_answer -> unit Lwt.t

val load_eliom_data_ :
  Eliom_client_types.eliom_data_type ->
  Dom_html.element Js.t -> unit Lwt.t

