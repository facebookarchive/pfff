(* Ocsigen
 * http://www.ocsigen.org
 * Module lwt_preemptive.ml
 * Copyright (C) 2005 Nataliya Guts, Vincent Balat, Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *               2009 Jérémie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later version.
 * See COPYING file for details.
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

(** This module allows to mix preemptive threads with [Lwt]
    cooperative threads. It maintains an extensible pool of preemptive
    threads to with you can detach computations. *)

val detach : ('a -> 'b) -> 'a -> 'b Lwt.t
  (** detaches a computation to a preemptive thread. *)

val init : int -> int -> (string -> unit) -> unit
  (** [init min max log] initialises this module. i.e. it launches the
      minimum number of preemptive threads and starts the {b
      dispatcher}.

      @param min is the minimum number of threads
      @param max is the maximum number of threads
      @param log is used to log error messages

      If {!Lwt_preemptive} has already been initialised, this call
      only modify bounds and the log function, and return the
      dispatcher thread. *)

val simple_init : unit -> unit
  (** [simple_init ()] does a {i simple initialization}. i.e. with
      default parameters if the library is not yet initialised.

      Note: this function is automatically called {!detach}. *)

val get_bounds : unit -> int * int
  (** [get_bounds ()] returns the minimum and the maximum number of
      preemptive threads. *)

val set_bounds : int * int -> unit
  (** [set_bounds (min, max)] set the minimum and the maximum number
      of preemptive threads. *)

val set_max_number_of_threads_queued : int -> unit
  (** Sets the size of the waiting queue, if no more preemptive
      threads are available. When the queue is full, {!detach} will
      sleep until a thread is available. *)

val get_max_number_of_threads_queued : unit -> int
  (** Returns the size of the waiting queue, if no more threads are
      available *)

(**/**)
val nbthreads : unit -> int
val nbthreadsbusy : unit -> int
val nbthreadsqueued : unit -> int
