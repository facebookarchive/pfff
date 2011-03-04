(* Ocsigen
 * http://www.ocsigen.org
 * Module eliomsessions.mli
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

(** This module contains the functions you need to get (or set)
    information about current request.
 *)

open Ocsigen_extensions


(*****************************************************************************)
(** {2 Getting information about the request} *)

(** returns the name of the user agent that did the request
   (usually the name of the browser). *)
val get_user_agent : unit -> string

(** returns the full URL as a string *)
val get_full_url : unit -> string

(** returns the internet address of the client as a string *)
val get_remote_ip : unit -> string

(** returns the internet address of the client,
   using the type [Unix.inet_addr] (defined in OCaml's standard library). *)
val get_remote_inet_addr : unit -> Unix.inet_addr

(** returns the full path of the URL as a string. *)
val get_current_full_path_string : unit -> string

(** returns the full path of the URL using the type [Ocsigen_lib.url_path] *)
val get_current_full_path : unit -> Ocsigen_lib.url_path

(** returns the full path of the URL as first sent by the browser
    (not changed by previous extensions like rewritemod) *)
val get_original_full_path_string : unit -> string

(** returns the full path of the URL as first sent by the browser
    (not changed by previous extensions like rewritemod) *)
val get_original_full_path : unit -> Ocsigen_lib.url_path

(** returns the sub path of the URL as a string.
    The sub-path is the full path without the path of the site (set in the
    configuration file).
 *)
val get_current_sub_path_string : unit -> string

(** returns the sub path of the URL using the type [Ocsigen_lib.url_path].
    The sub-path is the full path without the path of the site (set in the
    configuration file).
 *)
val get_current_sub_path : unit -> Ocsigen_lib.url_path

(** returns the hostname that has been sent by the user agent.
    For HTTP/1.0, the Host field is not mandatory in the request.
 *)
val get_header_hostname : unit -> string option

(** returns the hostname used for absolute links.
    It is either the [Host] header sent by the browser or the default hostname
    set in the configuration file, depending on server configuration
    ([<usedefaulthostname/>] option).
 *)
val get_hostname : unit -> string

(** returns the port of the server. 
    It is either the default port in the configuration file
    (if [<usedefaulthostname/>] is present is the configuration file),
    or the port in the Host header of the request (if present),
    or the port on which the request has been done (otherwise).
*)
val get_server_port : unit -> int

(** returns true if https is used, false if http. *)
val get_ssl : unit -> bool

(** returns the suffix of the current URL *)
val get_suffix : unit -> Ocsigen_lib.url_path option

(** returns the cookies sent by the browser *)
val get_cookies : ?cookie_scope:Eliom_common.cookie_scope ->
  unit -> string Ocsigen_lib.String_Table.t

(** returns an Unix timestamp associated to the request *)
val get_timeofday : unit -> float

(** returns an unique id associated to the request *)
val get_request_id : unit -> int64





(** {3 Exceptions and fallbacks} *)

(** returns a table in which you can store all the data you want during a
    request. It can also be used to send information after an action.
    Keep an eye on this information to know what
    succeeded before the current service was called
    (failed connection, timeout ...)
    The table is created at the beginning of the request.
 *)
val get_request_cache : unit -> Polytables.t

(** Remove all data from the request cache *)
val clean_request_cache : unit -> unit

(** returns [true] if the coservice called has not been found.
    In that case, the current service is the fallback.
*)
val get_link_too_old : unit -> bool

(** returns the list of names of service sessions expired for the current 
    request, for browser sessions and tab sessions. *)
val get_expired_service_sessions : 
  unit -> (Eliom_common.fullsessionname list *
                       Eliom_common.fullsessionname list)

(** returns the HTTP error code sent by the Ocsigen extension
   that tried to answer to the request before Eliom.
   It is 404 by default.
 *)
val get_previous_extension_error_code : unit -> int







(*****************************************************************************)
(** {2 Getting information about files uploaded} *)

(** Warning: The files uploaded are automatically erased by Ocsigen
   just after the request has been fulfilled.
   If you want to keep them, create a new hard link yourself during
   the service (or make a copy).
 *)

(** returns the filename used by Ocsigen for the uploaded file. *)
val get_tmp_filename : Ocsigen_lib.file_info -> string

(** returns the size of the file. *)
val get_filesize : Ocsigen_lib.file_info -> int64

(** returns the name the file had on the client when it has been sent. *)
val get_original_filename : Ocsigen_lib.file_info -> string

(** returns the root of the site. *)
val get_site_dir : unit -> Ocsigen_lib.url_path







(*****************************************************************************)
(** {2 Getting parameters (low level)} *)

(** The usual way to get parameters with Eliom is to use the second
   and third parameters of the service handlers.
   These are low level functions you may need for more advanced use.
 *)

(** returns the parameters of the URL (GET parameters)
   that concern the running service.
   For example in the case of a non-attached coservice called from
   a page with GET parameters, only the parameters of that non-attached
   coservice are returned (even if the other are still in the URL).
 *)
val get_get_params : unit -> (string * string) list

(** returns current parameters of the URL (GET parameters)
   (even those that are for subsequent services, but not previous actions) *)
val get_all_current_get_params : unit -> (string * string) list

(** returns all parameters of the URL (GET parameters)
    as sent initially by the browser *)
val get_initial_get_params : unit -> (string * string) list

(** returns the parameters of the URL (GET parameters)
   that do not concern the running service. *)
val get_other_get_params : unit -> (string * string) list

(** returns non localized parameters in the URL. *)
val get_nl_get_params : 
  unit -> (string * string) list Ocsigen_lib.String_Table.t

(** returns persistent non localized parameters in the URL. *)
val get_persistent_nl_get_params : 
  unit -> (string * string) list Ocsigen_lib.String_Table.t

(** returns non localized POST parameters. *)
val get_nl_post_params : 
  unit -> (string * string) list Ocsigen_lib.String_Table.t

(** returns the parameters in the body of the HTTP request (POST parameters)
    that concern the running service. None means that POST data where
    neither urlencoded form data or multipart data. *)
val get_post_params : unit -> (string * string) list Lwt.t option

(** returns all parameters in the body of the HTTP request (POST parameters)
   (even those that are for another service) *)
val get_all_post_params : unit -> (string * string) list option





(*****************************************************************************)
(** {2 Other low level functions} *)

(** You probably don't need these functions. *)

(** returns all the information about the request. *)
val get_ri : unit -> Ocsigen_extensions.request_info

(** returns all the information about the request and config. *)
val get_request : unit -> request

(** returns the name of the sessions to which belongs the running service
    ([None] if it is not a session service)
 *)
val get_state_name : unit -> Eliom_common.fullsessionname option


(** returns the values of the Eliom's cookies for persistent sessions
   sent by the browser. *)
val get_persistent_cookies :
  unit -> string Eliom_common.Fullsessionname_Table.t

(** returns the values of Eliom's cookies for non persistent sessions
   sent by the browser. *)
val get_data_cookies :
  unit -> string Eliom_common.Fullsessionname_Table.t


(** Returns the http error code of the request before Eliom was called *)
val get_previous_extension_error_code :unit -> int


(**/**)
(*****************************************************************************)

val get_sitedata_sp : sp:Eliom_common.server_params -> Eliom_common.sitedata
val get_sitedata : unit -> Eliom_common.sitedata

(*
(** returns the cookie expiration date for the session,
   in seconds, since the 1st of january 1970.
   must have been set just before (not saved server side).
 *)
val get_cookie_exp_date : ?state_name:string -> unit ->
  unit -> float option

(** returns the cookie expiration date for the persistent session,
    in seconds, since the 1st of january 1970.
   must have been set just before (not saved server side).
 *)
val get_persistent_cookie_exp_date : ?state_name:string ->
  unit -> unit -> float option

*)

val find_sitedata : string -> Eliom_common.sitedata

val get_si : Eliom_common.server_params -> Eliom_common.sess_info

val get_user_cookies : unit -> Ocsigen_cookies.cookieset
val get_user_tab_cookies : unit -> Ocsigen_cookies.cookieset

val get_sp_client_appl_name : unit -> string option
val get_sp_client_process_info_sp : 
  Eliom_common.server_params -> Eliom_common.client_process_info option
val get_sp_client_process_info : unit -> Eliom_common.client_process_info option

val set_site_handler : Eliom_common.sitedata ->
  (exn -> Ocsigen_http_frame.result Lwt.t) -> unit

val get_request_sp : Eliom_common.server_params -> request
val get_site_dir_sp : Eliom_common.server_params -> Ocsigen_lib.url_path
val get_hostname_sp : Eliom_common.server_params -> string
val get_full_url_sp : Eliom_common.server_params -> string

val get_other_get_params_sp : Eliom_common.server_params -> (string * string) list
val get_nl_get_params_sp : 
  Eliom_common.server_params -> (string * string) list Ocsigen_lib.String_Table.t
val get_persistent_nl_get_params_sp : 
  Eliom_common.server_params -> (string * string) list Ocsigen_lib.String_Table.t
val get_nl_post_params_sp : 
  Eliom_common.server_params -> (string * string) list Ocsigen_lib.String_Table.t

val get_original_full_path_sp : Eliom_common.server_params -> Ocsigen_lib.url_path
val get_original_full_path_string_sp : Eliom_common.server_params -> string
val get_server_port_sp : Eliom_common.server_params -> int
val get_ssl_sp : Eliom_common.server_params -> bool
val get_ri_sp : Eliom_common.server_params -> Ocsigen_extensions.request_info
val get_post_params_sp : Eliom_common.server_params -> (string * string) list Lwt.t option
val get_files_sp : Eliom_common.server_params -> (string * Ocsigen_lib.file_info) list Lwt.t option
val get_suffix_sp : Eliom_common.server_params -> Ocsigen_lib.url_path option
val get_request_cache_sp : Eliom_common.server_params -> Polytables.t
val get_request_id_sp : Eliom_common.server_params -> int64

