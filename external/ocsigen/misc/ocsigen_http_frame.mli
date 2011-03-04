(* Ocsigen
 * http://www.ocsigen.org
 * ocsigen_http_frame.ml Copyright (C) 2005
 * Denis Berthod, Vincent Balat, Jérôme Vouillon
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

open Ocsigen_cookies

type etag = string

(** [compute_new_ri_cookies now path ri_cookies cookies_to_set]
   adds the cookies from [cookies_to_set]
   to [ri_cookies], as if the cookies
   had been send to the browser and the browser
   was doing a new request to the url [path].
   Only the cookies that match [path] (current path) are added. *)
val compute_new_ri_cookies :
    float ->
      string list ->
        string Ocsigen_lib.String_Table.t ->
          cookie Ocsigen_lib.String_Table.t Cookies.t -> string Ocsigen_lib.String_Table.t



(** The type of answers to send *)
type result =
    {res_cookies: cookieset; (** cookies to set *)
     res_lastmodified: float option; (** Default: [None] *)
     res_etag: etag option;
     res_code: int; (** HTTP code, if not 200 *)
     res_stream: string Ocsigen_stream.t *
       (string Ocsigen_stream.t -> 
          int64 -> 
            string Ocsigen_stream.step Lwt.t) option
     ; (** Default: empty stream. 
           The second field is (optionaly)
           the function used to skip a part of the 
           stream, if you do not you want to use
           a basic reading of the stream. 
           For example, for static files, you can optimize it by using
           a [seek] function.
       *)
     (* It is not a new field of the record to remember to change it
        if we change the stream. *)
     res_content_length: int64 option;
     (** [None] means Transfer-encoding: chunked *)
     res_content_type: string option;
     res_headers: Http_headers.t; (** The headers you want to add *)
     res_charset: string option; (** Default: None *)
     res_location: string option; (** Default: None *)
   }


(** Default [result] to use as a base for constructing others. *)
val default_result : unit -> result

(** [result] for an empty page. *)
val empty_result : unit -> result


module type HTTP_CONTENT =
  sig
    type t
    type options
    val result_of_content : ?options:options -> t -> result Lwt.t
    val get_etag : ?options:options -> t -> etag option
  end
module Http_header :
  sig
    type http_method =
        GET | POST | HEAD | PUT | DELETE | TRACE
      | OPTIONS | CONNECT | LINK | UNLINK | PATCH
    type http_mode =
        Query of (http_method * string)
      | Answer of int
      | Nofirstline
    type proto = HTTP10 | HTTP11
    type http_header = {
      mode : http_mode;
      proto : proto;
      headers : Http_headers.t;
    }
    val get_firstline : http_header -> http_mode
    val get_headers : http_header -> Http_headers.t
    val get_headers_value : http_header -> Http_headers.name -> string
    val get_headers_values : http_header -> Http_headers.name -> string list
    val get_proto : http_header -> proto
    val add_headers : http_header -> Http_headers.name -> string -> http_header
  end
module Http_error :
  sig
    exception Http_exception of int * string option * Http_headers.t option
    val expl_of_code : int -> string
    val display_http_exception : exn -> unit
    val string_of_http_exception : exn -> string
  end


(** The type of HTTP frames.
   The content may be void (no body) or a stream.
   While sending, a stream will be sent with chunked encoding if no
   content-length is supplied.
   abort is the function to be called if you want to cancel the stream
   reading (closes the connection).
*)
type t =
  { frame_header : Http_header.http_header;
    frame_content : string Ocsigen_stream.t option;
    frame_abort : unit -> unit Lwt.t
  }
