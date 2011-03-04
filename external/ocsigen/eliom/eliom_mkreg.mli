(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_mkreg
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


(** This module defines the functor to use to creates modules
   generating functions to register services for your own types of pages.
   It is used for example in {!Eliom_output}.
 *)


open Ocsigen_extensions
open Eliom_state
open Eliom_services
open Eliom_parameters



(** {2 Creating modules to register services for one type of pages} *)
module type REGCREATE =
  sig

    type page

    type options

    type return

    val send :
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      page -> 
      Ocsigen_http_frame.result Lwt.t

    (** The following field is usually [Eliom_services.XNever]. 
        This value is recorded inside each service just after registration.
        (Use in [Eliom_output.Eliom_appl])
    *)
    val do_appl_xhr : Eliom_services.do_appl_xhr

  end


module type ELIOMREGSIG =
(* pasted from mli *)
  sig



    type page

    type options

    type return

    val send :
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      page -> 
      Ocsigen_http_frame.result Lwt.t

    val register :
      ?scope:Eliom_common.scope ->
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?state_name:string ->
      ?secure_session:bool ->
      service:('get, 'post,
               [< internal_service_kind ],
               [< suff ], 'gn, 'pn, [ `Registrable ], return) service ->
      ?error_handler:((string * exn) list -> page Lwt.t) ->
      ('get -> 'post -> page Lwt.t) ->
      unit
(** Register a service with the associated handler function.
   [register s t f] will associate the service [s] to the function [f].
   [f] is the function that creates a page, called {e service handler}.

   The handler function takes two parameters.
    - The second and third ones are respectively GET and POST parameters.

   For example if [t] is [Eliom_parameters.int "s"], then [ 'get] is [int].

   The [?scope] optional parameter is [`Global] by default, which means that the
   service will be registered in the global table and be available to any client.
   If you want to restrict the visibility of the service to a browser session,
   use [~scope:`Session].
   If you want to restrict the visibility of the service to a group of sessions,
   use [~scope:`Session_group].
   If you have a client side Eliom program running, and you want to restrict
   the visibility of the service to this instance of the program,
   use [~scope:`Client_process].

   If the same service is registered several times with different visibilities,
   Eliom will choose the service for handling a request in that order:
   [`Client_process], [`Session], [`Session_group] and finally [`Global]. It means for example
   that you can register a specialized version of a public service for a session.

    Warning: All public services created during initialization must be
    registered in the public table during initialisation, never after,

   Registering services and coservices is always done in memory as there is
   no means of marshalling closures.

    If you register new services dynamically, be aware that they will disappear
    if you stop the server. If you create dynamically new URLs,
    be very careful to re-create these URLs when you relaunch the server,
    otherwise, some external links or bookmarks may be broken!

    Some output modules (for example Redirectmod) define their own options
    for that function. Use the [?options] parameter to set them.

    The optional parameters [?charset], [?code], [?content_type] and [?headers]
    can be used to modify the HTTP answer sent by Eliom. Use this with care.

    [?state_name] is the name of the session (browser session or "tab" session),
    if you want several
    service sessions on the same site. It has no effect for scope [`Global].
    
    If [~secure_session] is false when the protocol is https, the service will be 
    registered in the unsecure session, 
    otherwise in the secure session with https, the unsecure one with http.
    (Secure session means that Eliom will ask the browser to send the cookie
    only through HTTPS). It has no effect for scope [`Global].

    Note that in the case of CSRF safe coservices, parameters
    [?state_name] and [?secure_session] must match exactly the session name
    and secure option specified while creating the CSRF safe service. 
    Otherwise, the registration will fail
    with {Eliom_services.Wrong_session_table_for_CSRF_safe_coservice}
 *)



    val register_service :
      ?scope:Eliom_common.scope ->
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?state_name:string ->
      ?secure_session:bool ->
      ?https:bool ->
      ?priority:int ->
      path:Ocsigen_lib.url_path ->
      get_params:('get, [< suff ] as 'tipo, 'gn) params_type ->
      ?error_handler:((string * exn) list -> page Lwt.t) ->
      ('get -> unit -> page Lwt.t) ->
      ('get, unit,
       [> `Attached of
          ([> `Internal of [> `Service ] ], [> `Get]) a_s ],
       'tipo, 'gn, unit,
       [> `Registrable ], return) service
(** Same as [service] followed by [register] *)

    val register_coservice :
      ?scope:Eliom_common.scope ->
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?state_name:string ->
      ?secure_session:bool ->
      ?name: string ->
      ?csrf_safe: bool ->
      ?csrf_state_name: string ->
      ?csrf_scope: Eliom_common.user_scope ->
      ?csrf_secure: bool ->
      ?max_use:int ->
      ?timeout:float ->
      ?https:bool ->
      fallback:(unit, unit,
                [ `Attached of ([ `Internal of [ `Service ] ], [`Get]) a_s ],
                [ `WithoutSuffix ] as 'tipo,
                unit, unit, [< registrable ], return)
        service ->
      get_params:
        ('get, [`WithoutSuffix], 'gn) params_type ->
      ?error_handler:((string * exn) list -> page Lwt.t) ->
      ('get -> unit -> page Lwt.t) ->
      ('get, unit,
       [> `Attached of
          ([> `Internal of [> `Coservice ] ], [> `Get]) a_s ],
       'tipo, 'gn, unit,
       [> `Registrable ], return)
        service
(** Same as [coservice] followed by [register] *)

    val register_coservice' :
      ?scope:Eliom_common.scope ->
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?state_name:string ->
      ?secure_session:bool ->
      ?name: string ->
      ?csrf_safe: bool ->
      ?csrf_state_name: string ->
      ?csrf_scope: Eliom_common.user_scope ->
      ?csrf_secure: bool ->
      ?max_use:int ->
      ?timeout:float ->
      ?https:bool ->
      get_params:
        ('get, [`WithoutSuffix] as 'tipo, 'gn) params_type ->
      ?error_handler:((string * exn) list -> page Lwt.t) ->
      ('get -> unit -> page Lwt.t) ->
      ('get, unit,
       [> `Nonattached of [> `Get] na_s ],
       'tipo, 'gn, unit, [> `Registrable ], return)
        service
(** Same as [coservice'] followed by [register] *)

    val register_post_service :
      ?scope:Eliom_common.scope ->
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?state_name:string ->
      ?secure_session:bool ->
      ?https:bool ->
      ?priority:int ->
      fallback:('get, unit,
                [ `Attached of
                    ([ `Internal of
                         ([ `Service | `Coservice ] as 'kind) ], [`Get]) a_s ],
                [< suff ] as 'tipo, 'gn,
                unit, [< `Registrable ], 'return2) (* 'return2 <> return *)
        service ->
      post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
      ?error_handler:((string * exn) list -> page Lwt.t) ->
      ('get -> 'post -> page Lwt.t) ->
      ('get, 'post, [> `Attached of
                       ([> `Internal of 'kind ], [> `Post]) a_s ],
       'tipo, 'gn, 'pn, [> `Registrable ], return)
        service
(** Same as [post_service] followed by [register] *)

    val register_post_coservice :
      ?scope:Eliom_common.scope ->
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?state_name:string ->
      ?secure_session:bool ->
      ?name: string ->
      ?csrf_safe: bool ->
      ?csrf_state_name: string ->
      ?csrf_scope: Eliom_common.user_scope ->
      ?csrf_secure: bool ->
      ?max_use:int ->
      ?timeout:float ->
      ?https:bool ->
      fallback:('get, unit ,
                [ `Attached of
                    ([ `Internal of [< `Service | `Coservice ] ], [`Get]) a_s ],
                [< suff ] as 'tipo,
                'gn, unit, [< `Registrable ], return)
        service ->
      post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
      ?error_handler:((string * exn) list -> page Lwt.t) ->
      ('get -> 'post -> page Lwt.t) ->
      ('get, 'post,
       [> `Attached of
          ([> `Internal of [> `Coservice ] ], [> `Post]) a_s ],
       'tipo, 'gn, 'pn, [> `Registrable ], return)
        service
(** Same as [post_coservice] followed by [register] *)

    val register_post_coservice' :
      ?scope:Eliom_common.scope ->
      ?options:options ->
      ?charset:string ->
      ?code: int ->
      ?content_type:string ->
      ?headers: Http_headers.t ->
      ?state_name:string ->
      ?secure_session:bool ->
      ?name: string ->
      ?csrf_safe: bool ->
      ?csrf_state_name: string ->
      ?csrf_scope: Eliom_common.user_scope ->
      ?csrf_secure: bool ->
      ?max_use:int ->
      ?timeout:float ->
      ?keep_get_na_params:bool ->
      ?https:bool ->
      post_params:('post, [ `WithoutSuffix ], 'pn) params_type ->
      ?error_handler:((string * exn) list -> page Lwt.t) ->
      (unit -> 'post -> page Lwt.t) ->
      (unit, 'post, [> `Nonattached of [> `Post] na_s ],
       [ `WithoutSuffix ], unit, 'pn,
       [> `Registrable ], return)
        service
(** Same as [post_coservice'] followed by [register] *)


  end





module MakeRegister : functor (Pages: REGCREATE) -> 
  ELIOMREGSIG with
                type page = Pages.page
              and type options = Pages.options
              and type return = Pages.return


(**/**)
val suffix_redir_uri_key : string Polytables.key
