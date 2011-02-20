(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_event
 * Copyright (C) 2009 Jérémie Dimino
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

(** Signals utilities *)

open React

(** {6 Monadic interface} *)

val return : 'a -> 'a signal
  (** Same as [React.S.const] *)

val bind : 'a signal -> ('a -> 'b signal) -> 'b signal
  (** [bind signal f] is initially [f x] where [x] is the current
      value of [signal]. Each time [signal] changes to a new value
      [y], [bind signal f] is set to [f y], until the next change of
      [signal]. *)

(** {6 Lwt-specific utilities} *)

val with_finaliser : (unit -> unit) -> 'a signal -> 'a signal
  (** [with_finaliser f signal] returns a signal [signal'] which
      behave as [signal], except that [f] is called when [signal'] is
      garbage collected. *)

val limit : ?eq : ('a -> 'a -> bool) -> (unit -> unit Lwt.t) -> 'a signal -> 'a signal
  (** [limit f signal] limits the rate of [signal] update with [f].

      For example, to limit it to 1 per second, you can use: [limit
      (fun () -> Lwt_unix.sleep 1.0) signal]. *)

val delay : 'a signal Lwt.t -> 'a event
  (** [delay thread] is an event which does not occurs until [thread]
      returns. When [thread] returns a signal [s], [delay thread] will
      occurs with the current value of [s], and then behaves as
      [React.S.changes s]. *)

(** {6 Threaded versions of React transformation functions} *)

(** The following functions behave as their [React] counterpart,
    except that they takes functions that may yield. Also, since
    signals must always have a value, several functions takes an extra
    argument for the initial value.

    The [_s] suffix means that calls are serialized.
*)

val app_s : ?eq : ('b -> 'b -> bool) -> ('a -> 'b Lwt.t) signal -> 'b -> 'a signal -> 'b signal
  (** [app_s ?eq signal_f initial signal_x] *)

val map_s : ?eq : ('b -> 'b -> bool) -> ('a -> 'b Lwt.t) -> 'b -> 'a signal -> 'b signal
  (** [map_s ?eq f initial signal] *)

val filter_s : ?eq : ('a -> 'a -> bool) -> ('a -> bool Lwt.t) -> 'a -> 'a signal -> 'a signal
  (** [filter_s ?eq f initial signal] *)

val fmap_s : ?eq:('b -> 'b -> bool) -> ('a -> 'b option Lwt.t) -> 'b -> 'a signal -> 'b signal
  (** [fmap ?eq f initial signal] *)

val diff_s : ('a -> 'a -> 'b Lwt.t) -> 'a signal -> 'b event
  (** [diff_s f signal] *)

val sample_s : ('b -> 'a -> 'c Lwt.t) -> 'b event -> 'a signal -> 'c event
  (** [sample f event signal] *)

val accum_s : ?eq : ('a -> 'a -> bool) -> ('a -> 'a Lwt.t) event -> 'a -> 'a signal
  (** [accum ?eq event_f initial] *)

val fold_s : ?eq : ('a -> 'a -> bool) -> ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b event -> 'a signal
  (** [fold ?eq f initial event] *)

val merge_s : ?eq : ('a -> 'a -> bool) -> ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b signal list -> 'a signal
  (** [merge ?eq f initial signals] *)

val l1_s : ?eq : ('b -> 'b -> bool) -> ('a -> 'b Lwt.t) -> 'b -> ('a signal -> 'b signal)
val l2_s : ?eq : ('c -> 'c -> bool) -> ('a -> 'b -> 'c Lwt.t) -> 'c -> ('a signal -> 'b signal -> 'c signal)
val l3_s : ?eq : ('d -> 'd -> bool) -> ('a -> 'b -> 'c -> 'd Lwt.t) -> 'd -> ('a signal -> 'b signal -> 'c signal -> 'd signal)
val l4_s : ?eq : ('e -> 'e -> bool) -> ('a -> 'b -> 'c -> 'd -> 'e Lwt.t) -> 'e -> ('a signal -> 'b signal -> 'c signal -> 'd signal -> 'e signal)
val l5_s : ?eq : ('f -> 'f -> bool) -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f Lwt.t) -> 'f -> ('a signal -> 'b signal -> 'c signal -> 'd signal -> 'e signal -> 'f signal)
val l6_s : ?eq : ('g -> 'g -> bool) -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g Lwt.t) -> 'g -> ('a signal -> 'b signal -> 'c signal -> 'd signal -> 'e signal -> 'f signal -> 'g signal)

val run_s : ?eq : ('a -> 'a -> bool) -> 'a -> 'a Lwt.t signal -> 'a signal

(** {6 Notification} *)

type notifier
  (** Type of signal notifiers *)

val disable : notifier -> unit
  (** [disable notif] stops the corresponding signal to be
      monitored *)

val notify : ('a -> unit) -> 'a signal -> notifier
  (** [notify f s] calls [f] each time the value of [s] change *)

val notify_p : ('a -> unit Lwt.t) -> 'a signal -> notifier
  (** [notify_p f s] is the same as [notify] except that [f x] is a
      thread. Calls to [f] are made in parallel. *)

val notify_s : ('a -> unit Lwt.t) -> 'a signal -> notifier
  (** [notify_s f s] is the same as [notify] except that [f x] is a
      thread. Calls to [f] are serialized. *)

val always_notify : ('a -> unit) -> 'a signal -> unit
  (** Same as [notify] but does not return a notifier *)

val always_notify_p : ('a -> unit Lwt.t) -> 'a signal -> unit
  (** Same as [notify_p] but does not return a notifier *)

val always_notify_s : ('a -> unit Lwt.t) -> 'a signal -> unit
  (** Same as [notify_s] but does not return a notifier *)
