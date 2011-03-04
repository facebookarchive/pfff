(* Ocsimore
 * Copyright (C) 2009
 * Laboratoire PPS - Université Paris Diderot - CNRS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
(**
   @author Boris Yakobowski
*)



(** An alias for the services that are accepted in the admin menu. *)
type menu_link_service =
    (Eliom_services.get_service_kind,
     [ `Unregistrable | `Registrable ])
    Eliom_tools_common.one_page



(** The service that answers for Ocsimore static files. *)
val static_service :
  ((string list, unit, Eliom_services.get_service_kind, [ `WithSuffix ],
    [ `One of string list ] Eliom_parameters.param_name, unit,
    [ `Registrable ], Eliom_predefmod.Any.return)
   Eliom_services.service)

(** Path to a static file, suitable for inclusion in a <a> tag *)
val static_file_uri :
     sp:Eliom_sessions.server_params
  -> path:string list
  -> XHTML.M.uri


(** Allows to add HTML headers required by page components *)
module Header : sig

    type header

    (** Define a new header *)
    val create_header :
         (   Eliom_sessions.server_params
          -> [`Link | `Meta | `Object | `Script | `Style ] XHTML.M.elt list)
      -> header

    (** Call this function every time you need a header to be included
        in the page. If this function is called several times for the same
        page with the same header, the header will be included only once.
    *)
    val require_header : header -> sp:Eliom_sessions.server_params -> unit

    (** This function is called to generate the headers for one page.
        Only required headers are included.
    *)
    val generate_headers :
         sp:Eliom_sessions.server_params
      -> [`Link | `Meta | `Object | `Script | `Style ] XHTML.M.elt list

  end

(*
(** Function to be called when Obrowser is used inside a page.
    The relevant javascript files will be included *)
val add_obrowser_header : sp:Eliom_sessions.server_params -> unit
 *)


(** Function to be called on admin pages, and which add the
    relevant css (including for the admin menu) *)
val add_admin_pages_header : sp:Eliom_sessions.server_params -> unit


(** Registers the string passed as argument so that it is called
    when the onload event on the body tag of the page fires. The
    string must thus be well-formed javascript (without ; at the end) *)
val add_onload_function: Eliom_sessions.server_params -> string -> unit

(** Generic headers for an html page. The arguments [css] is added
    after the links resulting from the hooks added by the function
    [add_html_header_hook] above. The argument [body_classes] is
    used for the classes of the [body] html element. *)
val html_page :
  sp:Eliom_sessions.server_params ->
  ?body_classes:Xhtmltypes.nmtokens ->
  ?css:Xhtmltypes.link XHTML.M.elt list->
  ?title:string ->
  Xhtmltypes.body_content XHTML.M.elt list ->
  XHTML.M.html Lwt.t


(** Functions related to the administration menu *)


(** Adds an entire subsection, labelled by [name] to the admin menu.
    The service [root] is used to represent this section. For the
    list of links, if the function returns [false], the link is
    not displayed.
*)
val add_to_admin_menu :
  name:string ->
  links:(string *
         menu_link_service *
         (Eliom_sessions.server_params -> bool Lwt.t)) list ->
  root:menu_link_service ->
  unit

(* No need to export thisn
(** The admin menu itself. The option [service] parameter is the service
    currently active, which will be displayed in a different way *)
val admin_menu:
  ?service:menu_link_service ->
  Eliom_sessions.server_params ->
  Xhtmltypes.block XHTML.M.elt list Lwt.t
*)

(** Displays a complete admin page, with the admin menu and the status bar.
    If [allow_unlogged] is false, users that have not logged-in will not
    be able to see the page *)
val admin_page:
  sp:Eliom_sessions.server_params ->
  ?service:menu_link_service ->
  ?body_classes:string list ->
  ?css:Xhtmltypes.link XHTML.M.elt list ->
  ?title:string ->
  ?allow_unlogged:bool ->
  Xhtmltypes.div_content XHTML.M.elt list ->
  XHTML.M.html Lwt.t


val icon:
  sp:Eliom_sessions.server_params ->
  path:string ->
  text:string ->
  [> Xhtmltypes.img ] XHTML.M.elt


val add_status_function:
  (sp:Eliom_sessions.server_params
   -> Xhtmltypes.div_content XHTML.M.elt Lwt.t)
   -> unit

