(* Ocsigen
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


(** Predefined boxes for Eliom *)

open Eliom_services
open Eliom_parameters
open Eliom_state
open Eliom_tools_common

(** {1 XHTML tools} *)
module Xhtml : sig
(** {2 Menus } *)

  val menu :
    ?classe:Xhtmltypes.nmtoken list ->
    ?id:string ->
    (([< get_service_kind ] as 'a, [< registrable ] as 'b) one_page *
        Xhtmltypes.a_content XHTML.M.elt list)
    ->
    (('a, 'b) one_page *
        Xhtmltypes.a_content XHTML.M.elt list)
      list ->
    ?service:('a, 'b) one_page ->
    unit ->
    [> `Ul ] XHTML.M.elt
(** Creates a menu

    Example:

    [menu ~classe:["mainmenu"]
    [
    (home, <:xmllist< Home >>);
    (infos, <:xmllist< More infos >>)
    ] current ()]

    The [service] argument is used to find which item(s) to highlight. If
    service is [None], the current url is used.

*)

(** {2 Hierchical sites } *)


(**
   [hierarchical_menu_depth_first menu] constructs a function taking
   as parameters a service
   and displaying a hierarchical menu for this service.

   The menu is constructed by exploring the tree using
   a depth-first algorithm. It means that the first menu item will be
   displayed, followed by the whole sub-menu for this item, then the second
   menu item with its sub-menu, and so on.
   By default, only the sub-menus for to the url corresponding to
   the argument [service] are displayed. If you want all the sub-menus to be
   displayed, specify [~whole_tree=true]. If [service] is [None], the current
   page is used.
*)
  val hierarchical_menu_depth_first :
    ?classe:Xhtmltypes.nmtoken list ->
    ?id:string ->
    ?whole_tree:bool ->
    ([< Eliom_services.get_service_kind ] as 'a,
     [< Eliom_services.registrable ] as 'b,
     Xhtmltypes.a_content XHTML.M.elt list)
      hierarchical_site ->
    ?service:('a, 'b) one_page ->
    unit ->
    [> `Ul ] XHTML.M.elt list



(**
   [hierarchical_menu_breadth_first menu] constructs a function taking
   as parameters a service
   and displaying a hierarchical menu for this service.

   The menu is constructed by exploring the tree using
   a breadth_first algorithm. It means that the whole menu for one
   level will be displayed, followed by all sub-menus.

   Only the sub-menus for to the url corresponding to the argument [service]
   are displayed. If [service] is [None], the current url is used.
*)
  val hierarchical_menu_breadth_first :
    ?classe:Xhtmltypes.nmtoken list ->
    ?id:string ->
    ([< Eliom_services.get_service_kind ] as 'a,
     [< Eliom_services.registrable ] as 'b,
     Xhtmltypes.a_content XHTML.M.elt list)
      hierarchical_site ->
    ?service:('a, 'b) one_page ->
    unit ->
    [> `Ul ] XHTML.M.elt list


(** Returns the tags [<link rel="subsection" ...>] and
    [<link rev="subsection" ...>] for the given hierarchical site.
*)
  val structure_links :
    ([< Eliom_services.get_service_kind ] as 'a,
     [< Eliom_services.registrable ] as 'b,
     Xhtmltypes.a_content XHTML.M.elt list)
    hierarchical_site ->
    ?service:('a, 'b) one_page ->
    unit ->
    [> `Link ] XHTML.M.elt list
end

(** {2 XHTML5} *)

(** Same functions as above, but with XHTML5 elements *)
module Xhtml5 : sig
    
(** {2 Menus } *)

  val menu :
    ?classe:Xhtml5types.nmtoken list ->
    ?id:string ->
    (([< get_service_kind ] as 'a, [< registrable ] as 'b) one_page *
        Xhtml5types.flow5_without_interactive XHTML5.M.elt list)
    ->
    (('a, 'b) one_page *
        Xhtml5types.flow5_without_interactive XHTML5.M.elt list)
      list ->
    ?service:('a, 'b) one_page ->
    unit ->
    [> `Ul ] XHTML5.M.elt
(** Creates a menu

    Example:

    [menu ~classe:["mainmenu"]
    [
    (home, <:xmllist< Home >>);
    (infos, <:xmllist< More infos >>)
    ] current ()]

    The [service] argument is used to find which item(s) to highlight. If
    service is [None], the current url is used.

*)

(** {2 Hierchical sites } *)


(**
   [hierarchical_menu_depth_first menu] constructs a function taking
   as parameters a service
   and displaying a hierarchical menu for this service.

   The menu is constructed by exploring the tree using
   a depth-first algorithm. It means that the first menu item will be
   displayed, followed by the whole sub-menu for this item, then the second
   menu item with its sub-menu, and so on.
   By default, only the sub-menus for to the url corresponding to
   the argument [service] are displayed. If you want all the sub-menus to be
   displayed, specify [?whole_tree=true]. If [service] is [None], the current
   page is used.
*)
  val hierarchical_menu_depth_first :
    ?classe:Xhtml5types.nmtoken list ->
    ?id:string ->
    ?whole_tree:bool ->
    ([< Eliom_services.get_service_kind ] as 'a,
     [< Eliom_services.registrable ] as 'b,
     Xhtml5types.a_content XHTML5.M.elt list)
      hierarchical_site ->
    ?service:('a, 'b) one_page ->
    unit ->
    [> `Ul ] XHTML5.M.elt list



(**
   [hierarchical_menu_breadth_first menu] constructs a function taking
   as parameters a service and [~sp] (server parameters)
   and displaying a hierarchical menu for this service.

   The menu is constructed by exploring the tree using
   a breadth_first algorithm. It means that the whole menu for one
   level will be displayed, followed by all sub-menus.

   Only the sub-menus for to the url corresponding to the argument [service]
   are displayed. If [service] is [None], the current url is used.
*)
  val hierarchical_menu_breadth_first :
    ?classe:Xhtml5types.nmtoken list ->
    ?id:string ->
    ([< Eliom_services.get_service_kind ] as 'a,
     [< Eliom_services.registrable ] as 'b,
     Xhtml5types.a_content XHTML5.M.elt list)
      hierarchical_site ->
    ?service:('a, 'b) one_page ->
    unit ->
    [> `Ul ] XHTML5.M.elt list


(** Returns the tags [<link rel="subsection" ...>] and
    [<link rev="subsection" ...>] for the given hierarchical site.
*)
  val structure_links :
    ([< Eliom_services.get_service_kind ] as 'a,
     [< Eliom_services.registrable ] as 'b,
     Xhtml5types.a_content XHTML5.M.elt list)
    hierarchical_site ->
    ?service:('a, 'b) one_page ->
    unit ->
    [> `Link ] XHTML5.M.elt list
end
