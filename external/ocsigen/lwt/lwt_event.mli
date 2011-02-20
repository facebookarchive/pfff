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

(** Events utilities *)

open React

(** {6 Lwt-specific utilities} *)

val with_finaliser : (unit -> unit) -> 'a event -> 'a event
  (** [with_finaliser f event] returns an event [event'] which behave
      as [event], except that [f] is called when [event'] is garbage
      collected. *)

val next : 'a event -> 'a Lwt.t
  (** [next ev] returns the next occurrence of [ev] *)

val limit : (unit -> unit Lwt.t) -> 'a event -> 'a event
  (** [limit f event] limits the rate of [event] with [f].

      For example, to limit the rate of an event to 1 per second you
      can use: [limit (fun () -> Lwt_unix.sleep 1.0) event]. *)

val from : (unit -> 'a Lwt.t) -> 'a event
  (** [from f] creates an event which occurs each [f ()] returns a
      value. If [f] raises an exception, the event is just stopped. *)

val to_stream : 'a event -> 'a Lwt_stream.t
  (** Creates a stream holding all values occurring on the given
      event *)

val of_stream : 'a Lwt_stream.t -> 'a event
  (** [of_stream stream] creates an event which occurs each time a
      value is available on the stream. *)

val delay : 'a event Lwt.t -> 'a event
  (** [delay thread] is an event which does not occurs until [thread]
      returns. Then it behaves as the event returned by [thread]. *)

(** {6 Threaded versions of React transformation functions} *)

(** The following functions behave as their [React] counterpart,
    except that they takes functions that may yield.

    As usual the [_s] suffix is used when calls are serialized, and
    the [_p] suffix is used when they are not.

    Note that [*_p] functions may not preserve event order.
*)

val app_s : ('a -> 'b Lwt.t) event -> 'a event -> 'b event
val app_p : ('a -> 'b Lwt.t) event -> 'a event -> 'b event

val map_s : ('a -> 'b Lwt.t) -> 'a event -> 'b event
val map_p: ('a -> 'b Lwt.t) -> 'a event -> 'b event

val filter_s : ('a -> bool Lwt.t) -> 'a event -> 'a event
val filter_p : ('a -> bool Lwt.t) -> 'a event -> 'a event

val fmap_s : ('a -> 'b option Lwt.t) -> 'a event -> 'b event
val fmap_p : ('a -> 'b option Lwt.t) -> 'a event -> 'b event

val diff_s : ('a -> 'a -> 'b Lwt.t) -> 'a event -> 'b event

val accum_s : ('a -> 'a Lwt.t) event -> 'a -> 'a event

val fold_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b event -> 'a event

val merge_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b event list -> 'a event

val run_s : 'a Lwt.t event -> 'a event
val run_p : 'a Lwt.t event -> 'a event

(** {6 Notification} *)

type notifier
  (** Type of event notifiers *)

val disable : notifier -> unit
  (** [disable notif] stops the corresponding event to be monitored *)

val notify : ('a -> unit) -> 'a event -> notifier
  (** [notify f ev] calls [f x] each time [ev] has a value [x] *)

val notify_p : ('a -> unit Lwt.t) -> 'a event -> notifier
  (** [notify_p f ev] is the same as [notify] except that [f x] is a
      thread. Calls to [f] are made in parallel. *)

val notify_s : ('a -> unit Lwt.t) -> 'a event -> notifier
  (** [notify_s f ev] is the same as [notify] except that [f x] is a
      thread. Calls to [f] are serialized. *)

val always_notify : ('a -> unit) -> 'a event -> unit
  (** Same as [notify] but does not return a notifier *)

val always_notify_p : ('a -> unit Lwt.t) -> 'a event -> unit
  (** Same as [notify_p] but does not return a notifier *)

val always_notify_s : ('a -> unit Lwt.t) -> 'a event -> unit
  (** Same as [notify_s] but does not return a notifier *)
