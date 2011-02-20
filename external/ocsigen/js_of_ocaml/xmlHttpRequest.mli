(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(** XmlHttpRequest object. *)

open Js

type readyState = UNSENT | OPENED | HEADERS_RECEIVED | LOADING | DONE

class type xmlHttpRequest = object ('self)
  method onreadystatechange : ('self Js.t, Dom_html.event Js.t) Dom_html.event_listener Js.writeonly_prop
  method readyState : readyState readonly_prop
  method _open :
    js_string t -> js_string t -> bool t -> unit meth
  method _open_full :
    js_string t -> js_string t -> bool t ->
    js_string t opt -> js_string t opt -> unit meth
  method setRequestHeader : js_string t -> js_string t -> unit meth
  method send : js_string t opt -> unit meth
  method send_document : Dom.element Dom.document -> unit meth
  method abort : unit meth
  method status : int readonly_prop
  method statusText : js_string t readonly_prop
  method getResponseHeader : js_string t -> js_string t opt meth
  method getAllResponseHeaders : js_string t meth
  method responseText : js_string t readonly_prop
  method responseXML : Dom.element Dom.document t opt readonly_prop
end

val create : unit -> xmlHttpRequest t

(** The next part of this module allow one to use Ocaml with no need for
     Javascript documentation. *)

type http_frame =
    {
      code: int;
      headers: string -> string option;
      content: string;
    }
(** The type for XHR results. The code field is the http status code of the
    answer. The headers field is a function associating values to any header
    name. *)

val send_string :
     ?headers:(string * string) list
  -> ?content_type:string
  -> ?post_args:((string * string) list) (* *)
  -> ?get_args:((string * string) list)  (* [] *)
  -> string
  -> http_frame Lwt.t
  (** [send_string ?headers ?content_type ?post_args ?get_args url] makes an
      asynchronous request to the specified [url] with specified options. The
      result is a cancelable thread returning an HTTP frame. If [post_args] is
      [None], a GET request is used. If [post_args] is [Some _] (even [Some []])
      then a POST request is made. *)

val send :
     ?headers:(string * string) list
  -> ?content_type:string
  -> ?post_args:((string * string) list) (* *)
  -> ?get_args:((string * string) list)  (* [] *)
  -> Url.url
  -> http_frame Lwt.t
  (** [send] is the same as {!send_string} except that the Url argument has type
      {!Url.url}. *)
