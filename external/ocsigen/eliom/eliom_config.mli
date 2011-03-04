(* Ocsigen
 * http://www.ocsigen.org
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


(** returns the hostname declared in the config file 
    ([<host defaulthostname="...">]).
 *)
val get_default_hostname : unit -> string

(** returns the port number declared in the config file ([<host defaulthttpport="...">]).
 *)
val get_default_port : unit -> int

(** returns the https port number declared in the config file ([<host defaulthttpsport="...">]).
 *)
val get_default_sslport : unit -> int

(** returns the default charset for this site *)
val get_config_default_charset : unit -> string


(** returns the information of the configuration file concerning that site
   (between [<site>] and [</site>]).

   {e Warning: You must call that function during the initialisation of
   your module (not during a Lwt thread or a service)
   otherwise it will raise the exception
   {!Eliom_common.Eliom_site_information_not_available}.
   If you want to build a statically linkable module, you must call this
   function inside the initialisation function given to
   {!Eliom_services.register_eliom_module}.}
 *)
val get_config : unit -> Simplexmlparser.xml list


(** returns the information concerning the request
    from the configuration files. *)
val get_config_info : unit -> Ocsigen_extensions.config_info

(**/**)
val get_config_info_sp : 
  Eliom_common.server_params -> Ocsigen_extensions.config_info

val get_config_default_charset_sp :
  Eliom_common.server_params -> string
