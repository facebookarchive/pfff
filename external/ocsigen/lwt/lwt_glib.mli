(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_glib
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

(** Glib integration *)

(** This modules is intended to allow the use of Lwt in GTK
    applications.

    If you are using GTK and Lwt in the same application, then you
    must call {!install} at the beginning of you program. *)

val install : ?mode : [ `glib_into_lwt | `lwt_into_glib ] -> unit -> unit
  (** Install the Glib<->Lwt integration.

      If [mode] is [`glib_into_lwt] then glib will use the Lwt main
      loop, and if [mode] is [`lwt_into_glib] then Lwt will use the
      Glib main loop.

      The first mode is better but for some unknown reason it does not
      work under Windows, so the second is used as default on Windows
      while the first one is used as default on Unix.

      If the integration is already active, this function does
      nothing. *)

val remove : unit -> unit
  (** Remove the Glib<->Lwt integration. *)
