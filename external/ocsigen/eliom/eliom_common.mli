(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_common.mli
 * Copyright (C) 2005 Vincent Balat
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

(** Low level functions for Eliom, exceptions and types. *)

open Ocsigen_extensions


exception Eliom_404 (** Page not found *)
exception Eliom_Wrong_parameter (** Service called with wrong parameter names *)
exception Eliom_Session_expired
exception Eliom_Typing_Error of (string * exn) list
    (** The service (GET or POST) parameters do not match expected type *)


exception Eliom_site_information_not_available of string
(** That function cannot be used when the site information is not available,
    that is, outside a request or the initialisation phase of your Eliom module
    (while reading the configuration file).
    
    In particular, you cannot use the function before the configuration file
    is read for example when you are using {e static linking}.
    In that case you must
    delay the function call using {!Eliom_services.register_eliom_module}.
*)

(** Eliom is using regular (browser) cookies but can also use
    browser tab cookies (only if you are using a client side program)
*)
type cookie_scope = [ `Session | `Client_process ]
(** It is possible to define data tables or service table for one
    (browser) session, for one tab, or for one group of sessions.
*)
type user_scope = [ `Session_group | `Session | `Client_process ]
type scope = [ `Global | `Session_group | `Session | `Client_process ]

val cookie_scope_of_user_scope : [< user_scope ] -> [> cookie_scope ]
val user_scope_of_scope : [< scope ] -> [> user_scope ]

type fullsessionname = cookie_scope * string
module Fullsessionname_Table : Map.S with type key = fullsessionname


val eliom_link_too_old : bool Polytables.key
(** If present and true in request data, it means that
    the previous coservice does not exist any more *)
val eliom_service_session_expired : 
  (fullsessionname list * fullsessionname list) Polytables.key
(** If present in request data,  means that
    the service session cookies does not exist any more.
    The string lists are the list of names of expired sessions
*)




(**/**)

(*VVV Warning: raising these exceptions will NOT send cookies!
  Do not use them inside services! *)
exception Eliom_do_redirection of string 
(* Used to redirect to the suffix version of the service *)
exception Eliom_do_half_xhr_redirection of string 


(* Service kinds: *)
type att_key_serv =
  | SAtt_no (* regular service *)
  | SAtt_named of string (* named coservice *)
  | SAtt_anon of string (* anonymous coservice *)
  | SAtt_csrf_safe of (int * string option * user_scope * bool option)
      (* CSRF safe anonymous coservice *)
      (* CSRF safe service registration delayed until form/link creation *)
      (* the int is an unique id,
         the string option is the session name for delayed registration
         (if the service is registered in the global table),
         the bool option is the ?secure parameter for delayed registration
         (if the service is registered in the global table) *)

type na_key_serv =
  | SNa_no (* no na information *)
  | SNa_void_keep (* void coservice that keeps GET na parameters *)
  | SNa_void_dontkeep (* void coservice that does not keep GET na parameters *)
  | SNa_get_ of string (* named *)
  | SNa_post_ of string (* named *)
  | SNa_get' of string (* anonymous *)
  | SNa_post' of string (* anonymous *)
  | SNa_get_csrf_safe of (int * string option * user_scope * bool option)
      (* CSRF safe anonymous coservice *)
  | SNa_post_csrf_safe of (int * string option * user_scope * bool option)
      (* CSRF safe anonymous coservice *)

(* the same, for incoming requests: *)
type att_key_req =
  | RAtt_no (* no coservice information *)
  | RAtt_named of string (* named coservice *)
  | RAtt_anon of string (* anonymous coservice *)

type na_key_req =
  | RNa_no (* no na information *)
  | RNa_get_ of string (* named *)
  | RNa_post_ of string (* named *)
  | RNa_get' of string (* anonymous *)
  | RNa_post' of string (* anonymous *)





exception Eliom_duplicate_registration of string
exception Eliom_there_are_unregistered_services of
            (string list * string list list * na_key_serv list)
exception Eliom_page_erasing of string
exception Eliom_error_while_loading_site of string

val defaultpagename : string
val eliom_suffix_name : string
val eliom_suffix_internal_name : string
val eliom_nosuffix_page : string
val naservice_num : string
val naservice_name : string
val get_state_param_name : string
val post_state_param_name : string
val get_numstate_param_name : string
val post_numstate_param_name : string
val co_param_prefix : string
val na_co_param_prefix : string
val nl_param_prefix : string
val eliom_internal_nlp_prefix : string
val pnl_param_prefix : string
val npnl_param_prefix : string
val internal_form_name : string
val internal_form_bool_name : string

val datacookiename : string
val servicecookiename : string
val persistentcookiename : string
val sdatacookiename : string
val sservicecookiename : string
val spersistentcookiename : string

val persistent_cookie_table_version : string
val eliom_persistent_cookie_table : string

val inline_class_name : string
val nodisplay_class_name : string

val appl_name_cookie_name : string
val tab_cookies_param_name : string
val get_request_post_param_name : string
val full_xhr_redir_header : string
val half_xhr_redir_header : string

val default_group_name : string

type sess_info = {
  si_other_get_params : (string * string) list;
  si_all_get_params : (string * string) list;
  si_all_post_params : (string * string) list option;

  si_service_session_cookies : string Fullsessionname_Table.t;
  si_data_session_cookies : string Fullsessionname_Table.t;
  si_persistent_session_cookies : string Fullsessionname_Table.t;
  si_secure_cookie_info:
    (string Fullsessionname_Table.t *
       string Fullsessionname_Table.t *
       string Fullsessionname_Table.t) option;

  si_service_session_cookies_tab: string Fullsessionname_Table.t;
  si_data_session_cookies_tab: string Fullsessionname_Table.t;
  si_persistent_session_cookies_tab: string Fullsessionname_Table.t;
  si_secure_cookie_info_tab:
    (string Fullsessionname_Table.t *
       string Fullsessionname_Table.t *
       string Fullsessionname_Table.t) option;

  si_tab_cookies: string Ocsigen_lib.String_Table.t;

  si_nonatt_info : na_key_req;
  si_state_info: (att_key_req * att_key_req);
  si_previous_extension_error : int;

  si_na_get_params: (string * string) list Lazy.t;
  si_nl_get_params: (string * string) list Ocsigen_lib.String_Table.t;
  si_nl_post_params: (string * string) list Ocsigen_lib.String_Table.t;
  si_persistent_nl_get_params: (string * string) list Ocsigen_lib.String_Table.t Lazy.t;

  si_all_get_but_na_nl: (string * string) list Lazy.t;
  si_all_get_but_nl: (string * string) list;

  si_internal_form: bool;
}

module SessionCookies : Hashtbl.S with type key = string

(* session groups *)
type 'a sessgrp =
    (string * cookie_scope
     * (string, Ocsigen_lib.ip_address) Ocsigen_lib.leftright)
    (* The full session group is the triple
       (site_dir_string, scope, session group name).
       The scope is the scope of group members (`Session by default).
       If there is no session group, 
       we limit the number of sessions by IP address. *)
type perssessgrp (* the same triple, marshaled *)

val make_persistent_full_group_name :
  cookie_scope:cookie_scope -> string -> string option -> perssessgrp option

val getperssessgrp : perssessgrp -> 
  (string * cookie_scope * 
     (string, Ocsigen_lib.ip_address) Ocsigen_lib.leftright)

val string_of_perssessgrp : perssessgrp -> string


type 'a session_cookie = SCNo_data | SCData_session_expired | SC of 'a

type cookie_exp = 
  | CENothing (* keep current browser value *)
  | CEBrowser (* ask to remove the cookie when the browser is closed *)
  | CESome of float (* date (not duration!) *)

type timeout = TGlobal | TNone | TSome of float
type 'a one_service_cookie_info = {
  sc_value : string;
  sc_table : 'a ref;
  sc_timeout : timeout ref;
  sc_exp : float option ref;
  sc_cookie_exp : cookie_exp ref;
  sc_session_group: cookie_scope sessgrp ref (* session group *);
  mutable sc_session_group_node:string Ocsigen_cache.Dlist.node;
}
type one_data_cookie_info = {
  dc_value : string;
  dc_timeout : timeout ref;
  dc_exp : float option ref;
  dc_cookie_exp : cookie_exp ref;
  dc_session_group: cookie_scope sessgrp ref (* session group *);
  mutable dc_session_group_node:string Ocsigen_cache.Dlist.node;
}
type one_persistent_cookie_info = {
  pc_value : string;
  pc_timeout : timeout ref;
  pc_cookie_exp : cookie_exp ref;
  pc_session_group : perssessgrp option ref;
}

type 'a cookie_info1 =
    (string option * 'a one_service_cookie_info session_cookie ref)
    Fullsessionname_Table.t ref *
    (string option * one_data_cookie_info session_cookie ref) Lazy.t
    Fullsessionname_Table.t ref *
    ((string * timeout * float option *
      perssessgrp option)
     option * one_persistent_cookie_info session_cookie ref)
    Lwt.t Lazy.t Fullsessionname_Table.t ref

type 'a cookie_info =
    'a cookie_info1 (* unsecure *) * 
      'a cookie_info1 option (* secure, if https *)

type 'a servicecookiestablecontent =
    fullsessionname * 'a * float option ref * timeout ref *
      cookie_scope sessgrp ref *
      string Ocsigen_cache.Dlist.node
type 'a servicecookiestable = 
    'a servicecookiestablecontent SessionCookies.t
type datacookiestablecontent =
    fullsessionname * float option ref * timeout ref *
      cookie_scope sessgrp ref *
      string Ocsigen_cache.Dlist.node
type datacookiestable = 
    datacookiestablecontent SessionCookies.t
type page_table_key = {
  key_state : att_key_serv * att_key_serv;
  key_kind : Ocsigen_http_frame.Http_header.http_method;
}

module NAserv_Table : Map.S with type key = na_key_serv
module Serv_Table : Map.S with type key = page_table_key

type dlist_ip_table

type anon_params_type = int

type client_process_info = (* information about the client process.
                              Mainly the URL when it has been launched *)
    {
      cpi_ssl : bool;
      cpi_hostname : string;
      cpi_server_port : int;
      cpi_original_full_path : Ocsigen_lib.url_path;
      cpi_references : Polytables.t; (* holds informations about comet
					service and change_page_event *)
    }

type server_params = {
  sp_request : Ocsigen_extensions.request;
  sp_si : sess_info;
  sp_sitedata : sitedata;
  sp_cookie_info : tables cookie_info;
  sp_tab_cookie_info : tables cookie_info;
  mutable sp_user_cookies: Ocsigen_cookies.cookieset;
  (* cookies (un)set by the user during service *)
  mutable sp_user_tab_cookies: Ocsigen_cookies.cookieset;
  mutable sp_client_appl_name: string option; (* The application name,
                                                 as sent by the browser *)
  sp_suffix : Ocsigen_lib.url_path option;
  sp_fullsessname : fullsessionname option;
  mutable sp_client_process_info: client_process_info Lazy.t
        (* Contains the base URL information from which the client process
           has been launched (if any). All relative links and forms will be
           created with respect to this information (if present - from current
           URL otherwise).
           It is taken form a client process state if the application has been
           launched before (and not timeouted on server side).
           Otherwise, it is created and registered in a server side state
           the first time we need it.
        *);
}
and page_table = page_table_content Serv_Table.t

and page_table_content =
    Ptc of
      (page_table ref * page_table_key, na_key_serv) Ocsigen_lib.leftright
        Ocsigen_cache.Dlist.node option
        (* for limitation of number of dynamic anonymous coservices *) *
        
        ((anon_params_type * anon_params_type)
           (* unique_id, computed from parameters type.
              must be the same even if the actual service reference
              is different (after reloading the site)
              so that it replaces the former one
           *) *
            (int ref option (* max_use *) *
               (float * float ref) option
                 (* timeout and expiration date for the service *) *
            (bool -> server_params -> Ocsigen_http_frame.result Lwt.t)
            )) list

and naservice_table_content =
    (int (* generation (= number of reloads of sites
            after which that service has been created) *) *
       int ref option (* max_use *) *
       (float * float ref) option (* timeout and expiration date *) *
       (server_params -> Ocsigen_http_frame.result Lwt.t) *
       (page_table ref * page_table_key, na_key_serv) Ocsigen_lib.leftright
       Ocsigen_cache.Dlist.node option
       (* for limitation of number of dynamic coservices *)
    )

and naservice_table =
  | AVide
  | ATable of naservice_table_content NAserv_Table.t

and dircontent = Vide | Table of direlt ref Ocsigen_lib.String_Table.t
and direlt = Dir of dircontent ref | File of page_table ref
and tables =
    {mutable table_services : (int (* generation *) * 
                         int (* priority *) * 
                         dircontent ref) list;
     table_naservices : naservice_table ref;
    (* Information for the GC: *)
     mutable table_contains_services_with_timeout : bool;
     (* true if dircontent contains services with timeout *)
     mutable table_contains_naservices_with_timeout : bool;
     (* true if naservice_table contains services with timeout *)
     mutable csrf_get_or_na_registration_functions :
       (sp:server_params -> string) Ocsigen_lib.Int_Table.t;
     mutable csrf_post_registration_functions :
       (sp:server_params -> att_key_serv -> string) Ocsigen_lib.Int_Table.t;
      (* These two table are used for CSRF safe services:
         We associate to each service unique id the function that will
         register a new anonymous coservice each time we create a link or form.
         Attached POST coservices may have both a GET and POST 
         registration function. That's why there are two tables.
         The functions associated to each service may be different for
         each session. That's why we use these table, and not a field in
         the service record.
      *)
     service_dlist_add :
       ?sp:server_params -> 
       (page_table ref * page_table_key, na_key_serv) Ocsigen_lib.leftright ->
       (page_table ref * page_table_key, na_key_serv) Ocsigen_lib.leftright
         Ocsigen_cache.Dlist.node
       (* Add in a dlist
          for limiting the number of dynamic anonymous coservices in each table 
          (and avoid DoS).
          There is one dlist for each session, and one for each IP
          in global tables.
          The dlist parameter is the table and coservice number
          for attached coservices,
          and the coservice number for non-attached ones.
       *)
    } 
and sitedata = {
  site_dir : Ocsigen_lib.url_path;
  site_dir_string : string;
  config_info: Ocsigen_extensions.config_info;

   (* Timeouts:
       - default for site (browser sessions)
       - default for site (tab sessions)
       - then default for each full session name
      The booleans means "has been set from config file"
   *)
   mutable servtimeout: 
     (float option * bool) option *
     (float option * bool) option *
     ((fullsessionname * (float option * bool)) list);
   mutable datatimeout: 
     (float option * bool) option *
     (float option * bool) option *
     ((fullsessionname * (float option * bool)) list);
   mutable perstimeout: 
     (float option * bool) option *
     (float option * bool) option *
     ((fullsessionname * (float option * bool)) list);

  global_services : tables;
  session_services : tables servicecookiestable;
  session_data : datacookiestable;
  group_of_groups: [ `Session_group ] sessgrp Ocsigen_cache.Dlist.t; 
  (* Limitation of the number of groups per site *)
  mutable remove_session_data : string -> unit;
  mutable not_bound_in_data_tables : string -> bool;
  mutable exn_handler : exn -> Ocsigen_http_frame.result Lwt.t;
  mutable unregistered_services : Ocsigen_lib.url_path list;
  mutable unregistered_na_services : na_key_serv list;
  mutable max_volatile_data_sessions_per_group : int * bool;
  mutable max_volatile_data_sessions_per_subnet : int * bool;
  mutable max_volatile_data_tab_sessions_per_group : int * bool;
  mutable max_service_sessions_per_group : int * bool;
  mutable max_service_sessions_per_subnet : int * bool;
  mutable max_service_tab_sessions_per_group : int * bool;
  mutable max_persistent_data_sessions_per_group : int option * bool;
  mutable max_persistent_data_tab_sessions_per_group : int option * bool;
  mutable max_anonymous_services_per_session : int * bool;
  mutable max_anonymous_services_per_subnet : int * bool;
  dlist_ip_table : dlist_ip_table;
  mutable ipv4mask : int32 option * bool;
  mutable ipv6mask : (int64 * int64) option * bool;
  mutable get_client_process_info : unit -> client_process_info option;
  mutable set_client_process_info : client_process_info -> unit;
}



type info =
    (Ocsigen_extensions.request * sess_info * 
       tables cookie_info * tables cookie_info * Ocsigen_cookies.cookieset)

exception Eliom_retry_with of info

val make_server_params_ :
  sitedata ->
  info ->
  Ocsigen_lib.url_path option -> 
  fullsessionname option -> server_params
val empty_page_table : unit -> page_table
val empty_dircontent : unit -> dircontent
val empty_naservice_table : unit -> naservice_table
val service_tables_are_empty : tables -> bool
val empty_tables : int -> bool -> tables
val new_service_session_tables : sitedata -> tables
val split_prefix_param :
  string -> (string * 'a) list -> (string * 'a) list * (string * 'a) list
val get_session_info :
  Ocsigen_extensions.request ->
  int -> (Ocsigen_extensions.request * sess_info * 
            (tables cookie_info * Ocsigen_cookies.cookieset) option) Lwt.t
type ('a, 'b) foundornot = Found of 'a | Notfound of 'b

val make_full_cookie_name : string -> string -> string
val make_fullsessname : 
  sp:server_params -> [< cookie_scope ] -> string option -> fullsessionname
val make_fullsessname2 : 
  string -> [< cookie_scope ] -> string option -> fullsessionname



module Perstables :
  sig
    val empty : 'a list
    val add : 'a -> 'a list -> 'a list
    val fold : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  end
val perstables : string list ref
val create_persistent_table : string -> 'a Ocsipersist.table
val persistent_cookies_table :
  (fullsessionname * float option * timeout * perssessgrp option)
  Ocsipersist.table Lazy.t
val remove_from_all_persistent_tables : string -> unit Lwt.t
val absolute_change_sitedata : sitedata -> unit
val get_current_sitedata : unit -> sitedata
val end_current_sitedata : unit -> unit
val add_unregistered : sitedata -> Ocsigen_lib.url_path -> unit
val add_unregistered_na : sitedata -> na_key_serv -> unit
val remove_unregistered : sitedata -> Ocsigen_lib.url_path -> unit
val remove_unregistered_na : sitedata -> na_key_serv -> unit
val verify_all_registered : sitedata -> unit
val during_eliom_module_loading : unit -> bool
val begin_load_eliom_module : unit -> unit
val end_load_eliom_module : unit -> unit
val global_register_allowed : unit -> (unit -> sitedata) option


val eliom_params_after_action : 
  ((string * string) list * (string * string) list option *
     (string * string) list Ocsigen_lib.String_Table.t *
     (string * string) list Ocsigen_lib.String_Table.t *
     (string * string) list * bool)
  Polytables.key
 
val att_key_serv_of_req : att_key_req -> att_key_serv
val na_key_serv_of_req : na_key_req -> na_key_serv

val remove_naservice_table : 
  naservice_table -> NAserv_Table.key -> naservice_table

val get_mask4 : sitedata -> int32
val get_mask6 : sitedata -> (int64 * int64)
val ipv4mask : int32 ref
val ipv6mask : (int64 * int64) ref

val create_dlist_ip_table : int -> dlist_ip_table
val find_dlist_ip_table :
  int32 option * 'a ->
  (int64 * int64) option * 'a ->
  dlist_ip_table -> Ocsigen_lib.ip_address ->
  (page_table ref * page_table_key, na_key_serv)
    Ocsigen_lib.leftright Ocsigen_cache.Dlist.t
  
val get_cookie_info : server_params -> [< cookie_scope ] -> tables cookie_info

val tab_cookie_action_info_key : (tables cookie_info * 
                                    Ocsigen_cookies.cookieset *
                                    string Ocsigen_lib.String_Table.t) Polytables.key


val sp_key : server_params Lwt.key
val get_sp_option : unit -> server_params option
val get_sp : unit -> server_params
val sp_of_option : server_params option -> server_params

val found_stop_key : unit Polytables.key
