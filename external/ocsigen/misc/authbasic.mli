(* Ocsigen
 * http://www.ocsigen.org
 * Module authbasic.mli
 * Copyright (C) 2008 Stéphane Glondu
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

(** Module [Authbasic]: Basic HTTP Authentication. *)

(** This module implements Basic HTTP Authentication as described in
    {{:http://www.ietf.org/rfc/rfc2617.txt}RFC 2617}.  It can be used
    to add an authentication layer to sites with no built-in
    authentication (e.g. static files). Beware, passwords are
    transmitted in cleartext with this scheme, so the medium should be
    secured somehow (by e.g. SSL).

    This module implements only the HTTP-related part of the protocol,
    and is meant to be extended with various authentication schemes. A
    very naive one (authentication with a single user/password, given
    in the configuration file) is provided. *)


val register_basic_authentication_method :
  (Simplexmlparser.xml -> string -> string -> bool Lwt.t) -> unit
  (** This function registers an authentication plugin: it adds a new
      parser to the list of available authentication schemes.

      A parser takes as argument an XML tree (corresponding to the
      first son of an <authbasic> element in the configuration
      file) and returns an authentication function [f]. [f] will be
      called for each request with the supplied user and password and
      should return (cooperatively) a boolean telling whether access
      is granted or not. Exceptions are handled the same way as for
      extension parsers.

      The <authbasic> element must have a {i realm} attribute,
      giving some identifier to the resource which is protected
      (several resources on the same hostname can share the same
      realm). This gives a general customization scheme "for free"
      from the point of view of plugin developers and is totally
      transparent to the plugin. *)


val get_basic_authentication_method :
  Simplexmlparser.xml -> string -> string -> bool Lwt.t
  (** This function combines all the parsers registered with
      [register_basic_authentication_method]. It might be useful for
      other extensions. Not for the casual user. *)
