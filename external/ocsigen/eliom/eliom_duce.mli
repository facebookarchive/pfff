(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_duce
 * Copyright (C) 2007 Vincent Balat, Alain Frisch
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

(** A module to generate pages using OCamlduce *)

open Xhtmltypes_duce

module type XhtmlFormsSig = Eliom_mkforms.ELIOMFORMSIG with
  type form_content_elt = form_content
  and type form_content_elt_list = {{ [ form_content* ] }}
  and type form_elt = form
  and type 'a a_content_elt = a_content
  and type 'a a_content_elt_list = {{ [ a_content* ] }}
  and type 'a a_elt = a
  and type 'a a_elt_list = {{ [ a* ] }}
  and type div_content_elt = div_content
  and type div_content_elt_list = flows
  and type uri = string
  and type link_elt = link
  and type script_elt = script
  and type textarea_elt = textarea
  and type input_elt = input
  and type pcdata_elt = {{ [ PCDATA ] }}
  and type select_elt = select
  and type select_content_elt = select_content
  and type select_content_elt_list = {{ [ select_content* ] }}
  and type button_elt = button
  and type button_content_elt = button_content
  and type button_content_elt_list = {{ [ button_content* ] }}
  and type option_elt = option
  and type option_elt_list = {{ [ option* ] }}

  and type a_attrib_t = a_attrs
  and type form_attrib_t =
      {{ attrs ++ { accept-charset=?String accept=?String
                  onreset=?String onsubmit=?String enctype=?String } }}
  and type input_attrib_t = input_attrs
  and type textarea_attrib_t = {{ attrs ++ focus ++
        { onchange=?String
              onselect=?String
            readonly=?"readonly"
              disabled=?"disabled"
            name=?String } }}
  and type select_attrib_t = select_attrs
  and type link_attrib_t = link_attrs
  and type script_attrib_t =
      {{ id ++ { defer=?"defer" src=?String charset=?String } }}
  and type optgroup_attrib_t = {{ attrs ++ { disabled=?"disabled" } }}
  and type option_attrib_t = option_attrs
  and type button_attrib_t = button_attrs

  and type input_type_t = input_type_values
  and type button_type_t = button_type_values


module type XhtmlSig =
sig
  include Eliom_mkreg.ELIOMREGSIG with type options = unit and type return = Eliom_services.http

  include XhtmlFormsSig
end


module Xhtml : XhtmlSig with type page = html
(** Register and create form for Xhtml *)

module Xhtmlforms : XhtmlFormsSig
(** Register and create form for Xhtml *)


module Xml : XhtmlSig with type page = Ocamlduce.Load.anyxml
(** Register and create form for any XML data type *)


module Xmllist : XhtmlSig with type page = Ocamlduce.Load.anyxml list
(** Register and create form for list of XML data type *)

module Blocks : XhtmlSig with type page = blocks
(** Register and create form for list of [blocks] (subtype of xhtml) *)

module SubXhtml :
  functor(T : sig
            type content
            val print : (string -> unit ) -> content -> unit
          end) ->
    XhtmlSig with type page = T.content
