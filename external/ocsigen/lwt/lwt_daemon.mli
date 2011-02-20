(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_io
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

(** Daemon helpers *)

val daemonize :
  ?syslog : bool ->
  ?stdin : [ `Dev_null | `Close | `Keep ] ->
  ?stdout : [ `Dev_null | `Close | `Keep | `Log_default | `Log of Lwt_log.logger ] ->
  ?stderr : [ `Dev_null | `Close | `Keep | `Log_default | `Log of Lwt_log.logger ] ->
  ?directory : string ->
  ?umask : [ `Keep | `Set of int ] ->
  unit -> unit
  (** Put the current running process into daemon mode. I.e. it forks
      and exit the parent, detach it from its controlling terminal,
      and redict standard intputs/outputs..

      Note: if the process is already a daemon, it does nothing.

      If [syslog] is [true] (the default), then {!Lwt_log.default} is
      set to [Lwt_log.syslog ~facility:`Daemon ()], otherwise it is
      kept unchanged.

      [stdin] is one of:
      - [`Dev_null] which means that [Unix.stdin] is reopened as
        [/dev/null], this is the default behavior
      - [`Close] means that [Unix.stdin] is simply closed
      - [`Keep] means that [Unix.stdin] is left unchanged

      [stdout] and [stderr] control how the two associated file
      descriptors are redirected:
      - [`Dev_null] means that the output is redirected to [/dev/null]
      - [`Close] means that the file descriptor is closed
      - [`Keep] means that it is left unchanged
      - [`Log logger] means that the output is redirected to this
        logger
      - [`Log_default] means that the output is redirected to the
        default logger

      Both [stdout] and [stderr] defaults to [`Log_default].

      Warning: do not redirect an output to a logger logging into this
      outpout, for example this code will create an infinite loop:

      {[
        let logger = Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stderr () in
        Lwt_daemon.daemonize ~syslog:false ~stderr:(`Log logger) ();
        prerr_endline "foo"
      ]}

      The current working directory is set to [directory], which
      defaults to ["/"].

      [umask] may be one of:
      - [`Keep] which means that the umask is left unchanged
      - [`Set n] which means that the umash is set to [n]

      It defaults to [`Set 0o022].
  *)
