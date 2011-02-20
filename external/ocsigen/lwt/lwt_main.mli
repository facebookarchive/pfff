(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_main
 * Copyright (C) 2009-2011 Jérémie Dimino
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

(** Main loop and event queue *)

(** This module controls the ``main-loop'' of Lwt. *)

val run : 'a Lwt.t -> 'a
  (** [run t] calls the Lwt scheduler repeatedly until [t] terminates,
      then returns the value returned by the thread. It [t] fails with
      an exception, this exception is raised.

      Note that you should avoid using [run] inside threads
      - The calling threads will not resume before [run]
        returns.
      - Successive invocations of [run] are serialized: an
        invocation of [run] will not terminate before all
        subsequent invocations are terminated.

      Note also that it is not safe to call [run] in a function
      registered with [Pervasives.at_exit], use the {!at_exit}
      function of this module instead. *)

val yield : unit -> unit Lwt.t
  (** [yield ()] is a threads which suspends itself and then resumes
      as soon as possible and terminates. *)

val enter_iter_hooks : (unit -> unit) Lwt_sequence.t
  (** Functions that are called before the main iteration. *)

val leave_iter_hooks : (unit -> unit) Lwt_sequence.t
  (** Functions that are called after the main iteration. *)

val exit_hooks : (unit -> unit Lwt.t) Lwt_sequence.t
  (** Sets of functions executed just before the program exit.

      Notes:
      - each hook is called exactly one time
      - exceptions raised by hooks are ignored *)

val at_exit : (unit -> unit Lwt.t) -> unit
  (** [at_exit hook] adds hook at the left of [exit_hooks]*)
