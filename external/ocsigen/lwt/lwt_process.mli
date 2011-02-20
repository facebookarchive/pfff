(* Lightweight thread library for Objective Caml
 * http://www.output_channelsigen.org/lwt
 * Module Lwt_process
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

(** Process management *)

(** This modules allow you to spawn processes and communicate with them. *)

type command = string * string array
    (** A command is a program name with a list of arguments *)

val shell : string -> command
  (** A command executed with ["/bin/sh"] *)

(** All the following functions take an optionnal argument
    [timeout]. If specified, after expiration, the process will be
    sent a [Unix.sigkill] signal and channels will be closed. *)

(** {6 High-level functions} *)

(** {8 Redirections} *)

(** A file descriptor redirection. It describe how standard file
    descriptors are redirected in the child process. *)
type redirection =
    [ `Keep
        (** The file descriptor is left unchanged *)
    | `Dev_null
        (** Connect the file descriptor to [/dev/null] *)
    | `Close
        (** The file descriptor is closed *)
    | `FD_copy of Unix.file_descr
        (** The file descriptor is replaced by the given
            one *)
    | `FD_move of Unix.file_descr
        (** The file descriptor is replaced by the given one, which is
            then closed. *) ]

(** Note: all optionnal redirection argumetns default to [`Keep] *)

(** {8 Executing} *)

val exec :
  ?timeout : float ->
  ?env : string array ->
  ?stdin : redirection ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> Unix.process_status Lwt.t
  (** Executes the given command and returns its exit status. *)

(** {8 Receiving} *)

val pread :
  ?timeout : float ->
  ?env : string array ->
  ?stdin : redirection ->
  ?stderr : redirection ->
  command -> string Lwt.t
val pread_chars :
  ?timeout : float ->
  ?env : string array ->
  ?stdin : redirection ->
  ?stderr : redirection ->
  command -> char Lwt_stream.t
val pread_line :
  ?timeout : float ->
  ?env : string array ->
  ?stdin : redirection ->
  ?stderr : redirection ->
  command -> string Lwt.t
val pread_lines :
  ?timeout : float ->
  ?env : string array ->
  ?stdin : redirection ->
  ?stderr : redirection ->
  command -> string Lwt_stream.t

(** {8 Sending} *)

val pwrite :
  ?timeout : float ->
  ?env : string array ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> string -> unit Lwt.t
val pwrite_chars :
  ?timeout : float ->
  ?env : string array ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> char Lwt_stream.t -> unit Lwt.t
val pwrite_line :
  ?timeout : float ->
  ?env : string array ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> string -> unit Lwt.t
val pwrite_lines :
  ?timeout : float ->
  ?env : string array ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> string Lwt_stream.t -> unit Lwt.t

(** {8 Mapping} *)

val pmap :
  ?timeout : float ->
  ?env : string array ->
  ?stderr : redirection ->
  command -> string -> string Lwt.t
val pmap_chars :
  ?timeout : float ->
  ?env : string array ->
  ?stderr : redirection ->
  command -> char Lwt_stream.t -> char Lwt_stream.t
val pmap_line :
  ?timeout : float ->
  ?env : string array ->
  ?stderr : redirection ->
  command -> string -> string Lwt.t
val pmap_lines :
  ?timeout : float ->
  ?env : string array ->
  ?stderr : redirection ->
  command -> string Lwt_stream.t -> string Lwt_stream.t

(** {6 Spawning processes} *)

(** State of a sub-process *)
type state =
  | Running
      (** The process is still running *)
  | Exited of Unix.process_status
      (** The process has exited *)

class process_none :
  ?timeout : float ->
  ?env : string array ->
  ?stdin : redirection ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command ->
object
  method pid : int
    (** Pid of the sub-process *)

  method state : state
    (** Return the state of the process *)

  method kill : int -> unit
    (** [kill signum] sends [signum] to the process if it is still
        running *)

  method status : Unix.process_status Lwt.t
    (** Threads which wait for the sub-process to exit then returns its
        exit status *)

  method rusage : Lwt_unix.resource_usage Lwt.t
    (** Threads which wait for the sub-process to exit then returns
        its resource usages *)

  method close : Unix.process_status Lwt.t
    (** Closes the process and returns its exit status. This close all
        channels used to communicate with the process *)
end

val open_process_none :
  ?timeout : float ->
  ?env : string array ->
  ?stdin : redirection ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> process_none
val with_process_none :
  ?timeout : float ->
  ?env : string array ->
  ?stdin : redirection ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> (process_none -> 'a Lwt.t) -> 'a Lwt.t

class process_in :
  ?timeout : float ->
  ?env : string array ->
  ?stdin : redirection ->
  ?stderr : redirection ->
  command ->
object
  inherit process_none

  method stdout : Lwt_io.input_channel
    (** The standard output of the process *)
end

val open_process_in :
  ?timeout : float ->
  ?env : string array ->
  ?stdin : redirection ->
  ?stderr : redirection ->
  command -> process_in
val with_process_in :
  ?timeout : float ->
  ?env : string array ->
  ?stdin : redirection ->
  ?stderr : redirection ->
  command -> (process_in -> 'a Lwt.t) -> 'a Lwt.t

class process_out :
  ?timeout : float ->
  ?env : string array ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command ->
object
  inherit process_none

  method stdin : Lwt_io.output_channel
    (** The standard input of the process *)
end

val open_process_out :
  ?timeout : float ->
  ?env : string array ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> process_out
val with_process_out :
  ?timeout : float ->
  ?env : string array ->
  ?stdout : redirection ->
  ?stderr : redirection ->
  command -> (process_out -> 'a Lwt.t) -> 'a Lwt.t

class process :
  ?timeout : float ->
  ?env : string array ->
  ?stderr : redirection ->
  command ->
object
  inherit process_none

  method stdin : Lwt_io.output_channel
  method stdout : Lwt_io.input_channel
end

val open_process :
  ?timeout : float ->
  ?env : string array ->
  ?stderr : redirection ->
  command -> process
val with_process :
  ?timeout : float ->
  ?env : string array ->
  ?stderr : redirection ->
  command -> (process -> 'a Lwt.t) -> 'a Lwt.t

class process_full :
  ?timeout : float ->
  ?env : string array ->
  command ->
object
  inherit process_none

  method stdin : Lwt_io.output_channel
  method stdout : Lwt_io.input_channel
  method stderr : Lwt_io.input_channel
end

val open_process_full :
  ?timeout : float ->
  ?env : string array ->
  command -> process_full
val with_process_full :
  ?timeout : float ->
  ?env : string array ->
  command -> (process_full -> 'a Lwt.t) -> 'a Lwt.t
