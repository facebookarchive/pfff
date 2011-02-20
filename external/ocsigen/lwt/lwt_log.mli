(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_log
 * Copyright (C) 2002 Shawn Wagner <raevnos@pennmush.org>
 *               2009 Jérémie Dimino <jeremie@dimino.org>
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

(** Logging facility *)

(** This module provides functions to deal with logging.
    It support:

    - logging to the syslog daemon
    - logging to a channel (stderr, stdout, ...)
    - logging to a file
    - logging to multiple destination at the same time
*)

(** {6 Types} *)

(** Type of log levels. A level determines the importance of a
    message *)
type level =
  | Debug
      (** Debugging message. They can be automatically removed byt hte
          syntax extension. *)
  | Info
      (** Informational message. Suitable to be displayed when the
          program is in verbose mode. *)
  | Notice
      (** Same as {!Info}, but is displayed by default. *)
  | Warning
      (** Something strange happend *)
  | Error
      (** An error message, which should not means the end of the
          program. *)
  | Fatal
      (** A fatal error happened, in most cases the program will end
          after a fatal error. *)

type logger
  (** Type of a logger. A logger is responsible for dispatching messages
      and storing them somewhere.

      Lwt provides loggers sending log messages to a file, syslog,
      ... but you can also create you own logger. *)

type section
  (** Each logging message has a section. Sections can be used to
      structure your logs. For example you can choose different
      loggers according to the section.

      Each section carries a level, and messages with a lower log
      level than than the section level will be dropped.

      Section levels are initialised using the [LWT_LOG] environment
      variable, which must contains one or more rules of the form
      [pattern -> level] separated by ";". Where [pattern] is a string
      that may contain [*].

      For example, if [LWT_LOG] contains:
      {[
        access -> warning;
        foo[*] -> error
      ]}
      then the level of the section ["access"] is {!Warning} and the
      level of any section matching ["foo[*]"] is {!Error}.

      If [LWT_LOG] is not defined then the rule ["* -> notice"] is
      used instead. *)

(** {6 Logging functions} *)

val log : ?exn : exn -> ?section : section -> ?logger : logger -> level : level -> string -> unit Lwt.t
  (** [log ?section ?logger ~level message] logs a message.

      [section] defaults to {!Section.main}. If [logger] is not
      specified, then the default one is used instead (see
      {!default}).

      If [exn] is provided, then its string representation
      (= [Printexc.to_string exn]) will be append to the message, and if
      possible the backtrace will also be logged. *)

val log_f : ?exn : exn -> ?section : section -> ?logger : logger -> level : level -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  (** [log_f] is the same as [log] except that it takes a format
      string *)

(** The following functions are the same as {!log} except that their
    name determines which level is used.

    For example {!info msg} is the same as {!log ~level:Info msg}.
*)

val debug : ?exn : exn -> ?section : section -> ?logger : logger -> string -> unit Lwt.t
val debug_f : ?exn : exn ->  ?section : section -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a

val info : ?exn : exn -> ?section : section -> ?logger : logger -> string -> unit Lwt.t
val info_f : ?exn : exn ->  ?section : section -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a

val notice : ?exn : exn -> ?section : section -> ?logger : logger -> string -> unit Lwt.t
val notice_f : ?exn : exn ->  ?section : section -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a

val warning : ?exn : exn -> ?section : section -> ?logger : logger -> string -> unit Lwt.t
val warning_f : ?exn : exn ->  ?section : section -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a

val error : ?exn : exn -> ?section : section -> ?logger : logger -> string -> unit Lwt.t
val error_f : ?exn : exn ->  ?section : section -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a

val fatal : ?exn : exn -> ?section : section -> ?logger : logger -> string -> unit Lwt.t
val fatal_f : ?exn : exn ->  ?section : section -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a

(** Sections *)
module Section : sig
  type t = section

  val make : string -> section
    (** [make name] creates a section with the given name, with level
        initialised according to the [LWT_LOG] environment variable. *)

  val name : section -> string
    (** [name section] returns the name of [section] *)

  val main : section
    (** The main section. It is the section used by default when no
        one is provided. *)

  val level : section -> level
    (** [level section] returns the logging level of [section] *)

  val set_level : section -> level -> unit
    (** [set_level section] sets the logging level of the given
        section *)
end

(** {6 Log templates} *)

type template = string
    (** A template is for generating log messages.

        It is a string which may contains variables of the form
        [$(var)], where [var] is one of:

        - [date] which will be replaced with the current date
        - [milliseconds] which will be replaced by the fractionnal part
          of the current unix time
        - [name] which will be replaced by the program name
        - [pid] which will be replaced by the pid of the program
        - [message] which will be replaced by the message emited
        - [level] which will be replaced by a string representation of
          the level
        - [section] which will be replaced by the name of the
          message's section

        For example:
        - ["$(name): $(message)"]
        - ["$(date) $(name)[$(pid)]: $(message)"]
        - ["$(date).$(milliseconds) $(name)[$(pid)]: $(message)"]
    *)

val render : buffer : Buffer.t -> template : template -> section : section -> level : level -> message : string -> unit
  (** [render ~buffer ~template ~section ~level ~message] instantiate
      all variables of [template], and store the result in
      [buffer]. *)

(** {6 Loggers} *)

exception Logger_closed
  (** Exception raised when trying to use a closed logger *)

val make :  output : (section -> level -> string list -> unit Lwt.t) -> close : (unit -> unit Lwt.t) -> logger
  (** [make ~output ~close] creates a new logger.

      @param output is used to write logs. It is a function which
      receive a section, a level and a list lines that must
      be logged together.
      @param close is used to close the logger *)

val close : logger -> unit Lwt.t
  (** Close the given logger *)

val default : logger ref
  (** The default logger. It is used as default when no one is
      specified. Initially, it sends messages to the standard output
      for error messages. *)

val broadcast : logger list -> logger
  (** [broadcast loggers] is a logger which send messages to all the
      given loggers.

      Note: closing a broadcast logger does not close its
      components. *)

val dispatch : (section -> level -> logger) -> logger
  (** [dispatch f] is a logger which dispatch logging instructions to
      different logger according to their level and/or section.

      Here is an example:

      {[
        let access_logger = Lwt_log.file "access.log"
        and error_logger = Lwt_log.file "error.log" in

        Lwt_log.dispatch
          (fun section level ->
            match Lwt_log.Section.name section, level with
              | "access", _ -> access_logger
              | _, Lwt_log.Error -> error_logger)
      ]}
  *)

(** {6 Predefined loggers} *)

val null : logger
  (** Logger which drops everything *)

(** Syslog facility. Look at the SYSLOG(3) man page for a description
    of syslog facilities *)
type syslog_facility =
    [ `Auth | `Authpriv | `Cron | `Daemon | `FTP | `Kernel
    | `Local0 | `Local1 | `Local2 | `Local3 | `Local4 | `Local5 | `Local6 | `Local7
    | `LPR | `Mail | `News | `Syslog | `User | `UUCP | `NTP | `Security | `Console ]

val syslog : ?template : template -> ?paths : string list -> facility : syslog_facility -> unit -> logger
  (** [syslog ?template ?paths ~facility ()] creates an logger
      which send message to the system logger.

      @param paths is a list of path to try for the syslogd socket. It
             default to [\["/dev/log"; "/var/run/log"\]].
      @param template defaults to ["$(date) $(name)[$(pid)]: $(section): $(message)"]
  *)

val file : ?template : template -> ?mode : [ `Truncate | `Append ] -> ?perm : Unix.file_perm -> file_name : string -> unit -> logger Lwt.t
  (** [desf_file ?template ?mode ?perm ~file_name ()] creates an
      logger which will write messages to [file_name].

      - if [mode = `Truncate] then the file is truncated and previous
      contents will be lost.

      - if [mode = `Append], new messages will be appended at the end
      of the file

      @param mode defaults to [`Append]
      @param template defaults to ["$(date): $(section): $(message)"]
  *)

val channel :?template : template -> close_mode : [ `Close | `Keep ] -> channel : Lwt_io.output_channel -> unit -> logger
  (** [channel ?template ~close_mode ~channel ()] creates a logger
      from a channel.

      If [close_mode = `Close] then [channel] is closed when the
      logger is closed, otherwise it is left open.

      @param template defaults to ["$(name): $(section): $(message)"] *)
