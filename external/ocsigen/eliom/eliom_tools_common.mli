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

open Eliom_services
open Eliom_parameters
open Eliom_state


(** {2 Menus } *)

type ('a, 'b) one_page =
    (unit, unit,
     'a,
     [ `WithoutSuffix ],
     unit, unit,
     'b, Eliom_services.http) service


(** {2 Hierchical sites } *)

type ('a, 'b, 'c) hierarchical_site_item =
  | Disabled
  | Site_tree of ('a, 'b, 'c) hierarchical_site
and ('a, 'b, 'c) main_page =
  | Main_page of ('a, 'b) one_page
  | Default_page of ('a, 'b) one_page
  | Not_clickable
and ('a, 'b, 'c) hierarchical_site =
      (('a, 'b, 'c) main_page *
         ('c * ('a, 'b, 'c) hierarchical_site_item) list)
(** The type of hierarchical sites.
    A hierarchical site is a pair (main page, subpages).

    The difference between
    [Main_page], [Default_page] and [Not_clickable] is a bit subtle:

    - [Main_page] is when you want to create a main page for your
    subsite. All the subpages are subsections of that page.

    - [Default_page] is like [Main_page] but is not taken into account
    for computing which is the current page in the menu.
    Use it for example when there is no main page, but you want
    one of the subpages to be the default page for your subsite.
    The service you use as default page
    must appear another time in the subtree!

    - [Not_clickable] is when you do not want the menu entry to be a link
    but you want subpages.

    Each subpage is defined by the text to be displayed in menus
    and a [hierarchical_site_item].
    If the latter is [Disabled], the menu entry is disabled.

 *)

(**/**)

val menu_class : string
val last_class : string
val current_class : string
val disabled_class : string
val first_class : string
val level_class : string


