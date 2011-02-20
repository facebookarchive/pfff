(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_throttle
 * Copyright (C) 2008 Stéphane Glondu
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

(** Rate limiters *)

(** This module defines rate limiters. A rate limiter is parametrized
    by its limit and a maximum waiting time. The [wait] function will
    collaboratively hang for a delay necessary to respect the
    limit. If that delay exceeds the maximum waiting time, [wait]
    returns [false]; otherwise it returns [true]. *)

module type S = sig
  type key
  type t

  val create : rate:int -> max:int -> n:int -> t
    (**
       @param rate maximum number of connections per second
       @param max maximum waiting time (in seconds)
       @param n initial size of the hash table
    *)

  val wait : t -> key -> bool Lwt.t
    (** @return [false] if maximum reached, [true] else *)
end

module Make (H : Hashtbl.HashedType) : S with type key = H.t
