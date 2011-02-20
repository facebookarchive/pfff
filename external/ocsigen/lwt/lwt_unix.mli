(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_unix
 * Copyright (C) 2005-2008 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *                    2009 Jérémie Dimino
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

(** Cooperative system calls *)

(** This modules redefine system calls, as in the [Unix] module of the
    standard library, but mapped into cooperative ones, which will not
    block the program, letting other threads run.

    The semantic of all operations is the following: if the action
    (for example reading from a {b file descriptor}) can be performed
    immediatly, it is done and returns immediatly, otherwise it
    returns a sleeping threads which is waked up when the operation
    completes.

    Moreover all sleeping threads returned by function of this modules
    are {b cancelable}, this means that you can cancel them with
    {!Lwt.cancel}. For example if you want to read something from a {b
    file descriptor} with a timeout, you can cancel the action after
    the timeout and the reading will not be performed if not already
    done.

    More precisely, assuming that you have two {b file descriptor}
    [fd1] and [fd2] and you want to read something from [fd1] or
    exclusively from [fd2], and fail with an exception if a timeout of
    1 second expires, without reading anything from [fd1] and [fd2],
    even if they become readable in the future.

    Then you can do:

    {[
      Lwt.pick [Lwt_unix.timeout 1.0; read fd1 buf1 ofs1 len1; read fd2 buf2 ofs2 len2]
    ]}

    In this case it is guaranteed that exactly one of the three
    operations will completes, and other will just be cancelled.
*)

val handle_unix_error : ('a -> 'b Lwt.t) -> 'a -> 'b Lwt.t
  (** Same as [Unix.handle_unix_error] but catches lwt-level
      exceptions *)

(** {6 Configuration} *)

(** For system calls that cannot be made asynchronously, Lwt uses one
    of the following method: *)
type async_method =
  | Async_none
      (** System calls are made synchronously, and may block the
          entire program. *)
  | Async_detach
      (** System calls are made in another system thread, thus without
          blocking other Lwt threads. The drawback is that it may
          degrade performances in some cases.

          This is the default. *)
  | Async_switch
      (** System calls are made in the main thread, and if one blocks
          the execution continue in another system thread. This method
          is the most efficint, also you will get better performances
          if you force all threads to run on the same cpu. On linux
          this can be done by using the command [taskset].

          Note that this method is still experimental. *)

val default_async_method : unit -> async_method
  (** Returns the default async method.

      This can be initialized using the environment variable
      ["LWT_ASYNC_METHOD"] with possible values ["none"],
      ["detach"] and ["switch"]. *)

val set_default_async_method : async_method -> unit
  (** Sets the default async method. *)

val async_method : unit -> async_method
  (** [async_method ()] returns the async method used in the current
      thread. *)

val async_method_key : async_method Lwt.key
  (** The key for storing the local async method. *)

val with_async_none : (unit -> 'a) -> 'a
  (** [with_async_none f] is a shorthand for:

      {[
        Lwt.with_value async_method_key (Some Async_none) f
      ]}
  *)

val with_async_detach : (unit -> 'a) -> 'a
  (** [with_async_none f] is a shorthand for:

      {[
        Lwt.with_value async_method_key (Some Async_detach) f
      ]}
  *)

val with_async_switch : (unit -> 'a) -> 'a
  (** [with_async_none f] is a shorthand for:

      {[
        Lwt.with_value async_method_key (Some Async_switch) f
      ]}
  *)

(** {6 Sleeping} *)

val sleep : float -> unit Lwt.t
  (** [sleep d] is a threads which remain suspended for [d] seconds
      and then terminates. *)

val yield : unit -> unit Lwt.t
  (** [yield ()] is a threads which suspends itself and then resumes
      as soon as possible and terminates. *)

val auto_yield : float -> (unit -> unit Lwt.t)
  (** [auto_yield timeout] returns a function [f] which will yield
      every [timeout] seconds. *)

exception Timeout
  (** Exception raised by timeout operations *)

val timeout : float -> 'a Lwt.t
  (** [timeout d] is a threads which remain suspended for [d] seconds
      then fail with {!Timeout} *)

val with_timeout : float -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_timeout d f] is a short-hand for:

      {[
        Lwt.pick [Lwt_unix.timeout d; f ()]
      ]}
  *)

(** {6 Operation on file-descriptors} *)

type file_descr
  (** The abstract type for {b file descriptor}s. A Lwt {b file
      descriptor} is a pair of a unix {b file descriptor} (of type
      [Unix.file_descr]) and a {b state}.

      A {b file descriptor} may be:

      - {b opened}, in which case it is fully usable
      - {b closed} or {b aborted}, in which case it is no longer
      usable *)

(** State of a {b file descriptor} *)
type state =
  | Opened
      (** The {b file descriptor} is opened *)
  | Closed
      (** The {b file descriptor} has been closed by {!close}. It must
          not be used for any operation. *)
  | Aborted of exn
      (** The {b file descriptor} has been aborted, the only operation
          possible is {!close}, all others will fail. *)

val state : file_descr -> state
  (** [state fd] returns the state of [fd] *)

val unix_file_descr : file_descr -> Unix.file_descr
  (** Returns the underlying unix {b file descriptor}. It always
      succeed, even if the {b file descriptor}'s state is not
      {!Open}. *)

val of_unix_file_descr : ?blocking : bool -> ?set_flags : bool -> Unix.file_descr -> file_descr
  (** Creates a lwt {b file descriptor} from a unix one.

      [blocking] is the blocking mode of the file-descriptor, it
      describe how Lwt will use it. In non-blocking mode, read/write
      on this file descriptor are made using non-blocking IO; in
      blocking mode they are made using the current async method.  If
      [blocking] is not specified it is guessed according to the file
      kind: socket and pipes are in non-blocking mode and others are
      in blocking mode.

      If [set_flags] is [true] (the default) then the file flags are
      modified according to the [blocking] argument, otherwise they
      are left unchanged.

      Note that the blocking mode is less efficient than the
      non-blocking one, so it should be used only for file descriptors
      that does not support asynchronous operations, such as regular
      files, or for shared descriptors such as {!stdout}, {!stderr} or
      {!stdin}. *)

val blocking : file_descr -> bool Lwt.t
  (** [blocking fd] returns whether [fd] is used in blocking or
      non-blocking mode. *)

val set_blocking : ?set_flags : bool -> file_descr -> bool -> unit
  (** [set_blocking fd b] puts [fd] in blocking or non-blocking
      mode. If [set_flags] is [true] (the default) then the file flags
      are modified, otherwise the modification is only done at the
      application level. *)

val abort : file_descr -> exn -> unit
  (** [abort fd exn] makes all current and further uses of the file
      descriptor fail with the given exception. This put the {b file
      descriptor} into the {!Aborted} state.

      If the {b file descrptor} is closed, this does nothing, if it is
      aborted, this replace the abort exception by [exn].

      Note that this only works for reading and writing operations on
      file descriptors supporting non-blocking mode. *)

(** {6 Process handling} *)

val wait : unit -> (int * Unix.process_status) Lwt.t
  (** Wrapper for [Unix.wait] *)

val waitpid : Unix.wait_flag list -> int -> (int * Unix.process_status) Lwt.t
  (** Wrapper for [Unix.waitpid] *)

(** Resource usages *)
type resource_usage = {
  ru_utime : float;
  (** User time used *)

  ru_stime : float;
  (** System time used *)
}

val wait4 : Unix.wait_flag list -> int -> (int * Unix.process_status * resource_usage) Lwt.t
  (** [wait4 flags pid] returns [(pid, status, rusage)] where [(pid,
      status)] is the same result as [Unix.waitpid flags pid], and
      [rusage] contains accounting information about the child.

      On windows it will always returns [{ utime = 0.0; stime = 0.0 }]. *)

val system : string -> Unix.process_status Lwt.t
  (** Wrapper for [Unix.system] *)

(** {6 Basic file input/output} *)

val stdin : file_descr
  (** The standard {b file descriptor} for input. This one is usually
      a terminal is the program is started from a terminal. *)

val stdout : file_descr
  (** The standard {b file descriptor} for output *)

val stderr : file_descr
  (** The standard {b file descriptor} for printing error messages *)

val openfile : string -> Unix.open_flag list -> Unix.file_perm -> file_descr Lwt.t
  (** Wrapper for [Unix.openfile]. *)

val close : file_descr -> unit Lwt.t
  (** Close a {b file descriptor}. This close the underlying unix {b
      file descriptor} and set its state to {!Closed} *)

val read : file_descr -> string -> int -> int -> int Lwt.t
  (** [read fd buf ofs len] has the same semantic as [Unix.read], but
      is cooperative *)

val write : file_descr -> string -> int -> int -> int Lwt.t
  (** [read fd buf ofs len] has the same semantic as [Unix.write], but
      is cooperative *)

val readable : file_descr -> bool
  (** Returns whether the given file descriptor is currently
      readable. *)

val writable : file_descr -> bool
  (** Returns whether the given file descriptor is currently
      writable. *)

val wait_read : file_descr -> unit Lwt.t
  (** waits (without blocking other threads) until there is something
      to read on the file descriptor *)

val wait_write : file_descr -> unit Lwt.t
  (** waits (without blocking other threads) until it is possible to
      write on the file descriptor *)

(** {6 Seeking and truncating} *)

val lseek : file_descr -> int -> Unix.seek_command -> int Lwt.t
  (** Wrapper for [Unix.lseek] *)

val truncate : string -> int -> unit Lwt.t
  (** Wrapper for [Unix.truncate] *)

val ftruncate : file_descr -> int -> unit Lwt.t
  (** Wrapper for [Unix.ftruncate] *)

(** {6 File status} *)

val stat : string -> Unix.stats Lwt.t
  (** Wrapper for [Unix.stat] *)

val lstat : string -> Unix.stats Lwt.t
  (** Wrapper for [Unix.lstat] *)

val fstat : file_descr -> Unix.stats Lwt.t
  (** Wrapper for [Unix.fstat] *)

val isatty : file_descr -> bool Lwt.t
  (** Wrapper for [Unix.isatty] *)

(** {6 File operations on large files} *)

module LargeFile : sig
  val lseek : file_descr -> int64 -> Unix.seek_command -> int64 Lwt.t
    (** Wrapper for [Unix.LargeFile.lseek] *)

  val truncate : string -> int64 -> unit Lwt.t
    (** Wrapper for [Unix.LargeFile.truncate] *)

  val ftruncate : file_descr -> int64 -> unit Lwt.t
    (** Wrapper for [Unix.LargeFile.ftruncate] *)

  val stat : string -> Unix.LargeFile.stats Lwt.t
    (** Wrapper for [Unix.LargeFile.stat] *)

  val lstat : string -> Unix.LargeFile.stats Lwt.t
    (** Wrapper for [Unix.LargeFile.lstat] *)

  val fstat : file_descr -> Unix.LargeFile.stats Lwt.t
    (** Wrapper for [Unix.LargeFile.fstat] *)
end

(** {6 Operations on file names} *)

val unlink : string -> unit Lwt.t
  (** Wrapper for [Unix.unlink] *)

val rename : string -> string -> unit Lwt.t
  (** Wrapper for [Unix.rename] *)

val link : string -> string -> unit Lwt.t
  (** Wrapper for [Unix.link] *)

(** {6 File permissions and ownership} *)

val chmod : string -> Unix.file_perm -> unit Lwt.t
  (** Wrapper for [Unix.chmod] *)

val fchmod : file_descr -> Unix.file_perm -> unit Lwt.t
  (** Wrapper for [Unix.fchmod] *)

val chown : string -> int -> int -> unit Lwt.t
  (** Wrapper for [Unix.chown] *)

val fchown : file_descr -> int -> int -> unit Lwt.t
  (** Wrapper for [Unix.fchown] *)

val access : string -> Unix.access_permission list -> unit Lwt.t
  (** Wrapper for [Unix.access] *)

(** {6 Operations on file descriptors} *)

val dup : file_descr -> file_descr
  (** Wrapper for [Unix.dup] *)

val dup2 : file_descr -> file_descr -> unit
  (** Wrapper for [Unix.dup2] *)

val set_close_on_exec : file_descr -> unit
  (** Wrapper for [Unix.set_close_on_exec] *)

val clear_close_on_exec : file_descr -> unit
  (** Wrapper for [Unix.clear_close_on_exec] *)

(** {6 Directories} *)

val mkdir : string -> Unix.file_perm -> unit Lwt.t
  (** Wrapper for [Unix.mkdir] *)

val rmdir : string -> unit Lwt.t
  (** Wrapper for [Unix.rmdir] *)

val chdir : string -> unit Lwt.t
  (** Wrapper for [Unix.chdir] *)

val chroot : string -> unit Lwt.t
  (** Wrapper for [Unix.chroot] *)

val opendir : string -> Unix.dir_handle Lwt.t
  (** Wrapper for [Unix.opendir] *)

val readdir : Unix.dir_handle -> string Lwt.t
  (** Wrapper for [Unix.dir] *)

val readdir_n : Unix.dir_handle -> int -> string array Lwt.t
  (** [readdir_n handle count] reads at most [count] entry from the
      given directory. It is more efficient that callling [count]
      times [readdir]. If the length of the returned array is smaller
      than [count], this means that the end of the directory has been
      reached. *)

val rewinddir : Unix.dir_handle -> unit Lwt.t
  (** Wrapper for [Unix.rewinddir] *)

val closedir : Unix.dir_handle -> unit Lwt.t
  (** Wrapper for [Unix.closedir] *)

val files_of_directory : string -> string Lwt_stream.t
  (** [files_of_directory dir] returns the stream of all files of
      [dir]. *)

(** {6 Pipes and redirections} *)

val pipe : unit -> file_descr * file_descr
  (** [pipe ()] creates pipe using [Unix.pipe] and returns two lwt {b
      file descriptor}s created from unix {b file_descriptor} *)

val pipe_in : unit -> file_descr * Unix.file_descr
  (** [pipe_in ()] is the same as {!pipe} but maps only the unix {b
      file descriptor} for reading into a lwt one. The second is not
      put into non-blocking mode. You usually want to use this before
      forking to receive data from the child process. *)

val pipe_out : unit -> Unix.file_descr * file_descr
  (** [pipe_out ()] is the inverse of {!pipe_in}. You usually want to
      use this before forking to send data to the child process *)

val mkfifo : string -> Unix.file_perm -> unit Lwt.t
  (** Wrapper for [Unix.mkfifo] *)

(** {6 Symbolic links} *)

val symlink : string -> string -> unit Lwt.t
  (** Wrapper for [Unix.symlink] *)

val readlink : string -> string Lwt.t
  (** Wrapper for [Unix.readlink] *)

(** {6 Locking} *)

val lockf : file_descr -> Unix.lock_command -> int -> unit Lwt.t
  (** Wrapper for [Unix.lockf] *)

(** {6 User id, group id} *)

val getlogin : unit -> string Lwt.t
  (** Wrapper for [Unix.getlogin] *)

val getpwnam : string -> Unix.passwd_entry Lwt.t
  (** Wrapper for [Unix.getpwnam] *)

val getgrnam : string -> Unix.group_entry Lwt.t
  (** Wrapper for [Unix.getgrnam] *)

val getpwuid : int -> Unix.passwd_entry Lwt.t
  (** Wrapper for [Unix.getpwuid] *)

val getgrgid : int -> Unix.group_entry Lwt.t
  (** Wrapper for [Unix.getgrgid] *)

(** {6 Signals} *)

type signal_handler_id
  (** Id of a signal handler, used to cancel it *)

val on_signal : int -> (int -> unit) -> signal_handler_id
  (** [on_signal signum f] calls [f] each time the signal with numnber
      [signum] is received by the process. It returns a signal handler
      identifier which can be used to stop monitoring [signum]. *)

val disable_signal_handler : signal_handler_id -> unit
  (** Stops receiving this signal *)

(** {6 Sockets} *)

val socket : Unix.socket_domain -> Unix.socket_type -> int -> file_descr
  (** [socket domain type proto] is the same as [Unix.socket] but maps
      the result into a lwt {b file descriptor} *)

val socketpair : Unix.socket_domain -> Unix.socket_type -> int -> file_descr * file_descr
  (** Wrapper for [Unix.socketpair] *)

val bind : file_descr -> Unix.sockaddr -> unit
  (** Wrapper for [Unix.bind] *)

val listen : file_descr -> int -> unit
  (** Wrapper for [Unix.listen] *)

val accept : file_descr -> (file_descr * Unix.sockaddr) Lwt.t
  (** Wrapper for [Unix.accept] *)

val accept_n : file_descr -> int -> ((file_descr * Unix.sockaddr) list * exn option) Lwt.t
  (** [accept_n fd count] accepts up to [count] connection in one time.

      - if no connection is available right now, it returns a sleeping
      thread

      - if more that 1 and less than [count] are available, it returns
      all of them

      - if more that [count] are available, it returns the next
      [count] of them

      - if an error happen, it returns the connections that have been
      successfully accepted so far and the error

      [accept_n] has the advantage of improving performances. If you
      want a more detailed description, you can have a look at:

      {{:http://portal.acm.org/citation.cfm?id=1247435}Acceptable strategies for improving web server performance} *)

val connect : file_descr -> Unix.sockaddr -> unit Lwt.t
  (** Wrapper for [Unix.connect] *)

val shutdown : file_descr -> Unix.shutdown_command -> unit
  (** Wrapper for [Unix.shutdown] *)

val getsockname : file_descr -> Unix.sockaddr
  (** Wrapper for [Unix.getsockname] *)

val getpeername : file_descr -> Unix.sockaddr
  (** Wrapper for [Unix.getpeername] *)

val recv : file_descr -> string -> int -> int -> Unix.msg_flag list -> int Lwt.t
  (** Wrapper for [Unix.recv] *)

val recvfrom : file_descr -> string -> int -> int -> Unix.msg_flag list -> (int * Unix.sockaddr) Lwt.t
  (** Wrapper for [Unix.recvfrom] *)

val send : file_descr -> string -> int -> int -> Unix.msg_flag list -> int Lwt.t
  (** Wrapper for [Unix.send] *)

val sendto : file_descr -> string -> int -> int -> Unix.msg_flag list -> Unix.sockaddr -> int Lwt.t
  (** Wrapper for [Unix.sendto] *)

(** An io-vector. Used by {!recv_msg} and {!send_msg}. *)
type io_vector = {
  iov_buffer : string;
  iov_offset : int;
  iov_length : int;
}

val io_vector : buffer : string -> offset : int -> length : int -> io_vector
  (** Creates an io-vector *)

val recv_msg : socket : file_descr -> io_vectors : io_vector list -> (int * Unix.file_descr list) Lwt.t
  (** [recv_msg ~socket ~io_vectors] receives data into a list of
      io-vectors, plus any file-descriptors that may accompany the
      message.

      This call is not available on windows. *)

val send_msg : socket : file_descr -> io_vectors : io_vector list -> fds : Unix.file_descr list -> int Lwt.t
  (** [send_msg ~socket ~io_vectors ~fds] sends data from a list of
      io-vectors, accompanied with a list of file-descriptor. If
      fd-passing is not possible on the current system and [fds] is
      not empty, it raises [Lwt_sys.Not_available "fd_passing"].

      This call is not available on windows. *)

type credentials = {
  cred_pid : int;
  cred_uid : int;
  cred_gid : int;
}

val get_credentials : file_descr -> credentials
  (** [get_credentials fd] returns credential informations from the
      given socket. *)

(** {8 Socket options} *)

val getsockopt : file_descr -> Unix.socket_bool_option -> bool
  (** Wrapper for [Unix.getsockopt] *)

val setsockopt : file_descr -> Unix.socket_bool_option -> bool -> unit
  (** Wrapper for [Unix.setsockopt] *)

val getsockopt_int : file_descr -> Unix.socket_int_option -> int
  (** Wrapper for [Unix.getsockopt_int] *)

val setsockopt_int : file_descr -> Unix.socket_int_option -> int -> unit
  (** Wrapper for [Unix.setsockopt_int] *)

val getsockopt_optint : file_descr -> Unix.socket_optint_option -> int option
  (** Wrapper for [Unix.getsockopt_optint] *)

val setsockopt_optint : file_descr -> Unix.socket_optint_option -> int option -> unit
  (** Wrapper for [Unix.setsockopt_optint] *)

val getsockopt_float : file_descr -> Unix.socket_float_option -> float
  (** Wrapper for [Unix.getsockopt_float] *)

val setsockopt_float : file_descr -> Unix.socket_float_option -> float -> unit
  (** Wrapper for [Unix.setsockopt_float] *)

val getsockopt_error : file_descr -> Unix.error option
  (** Wrapper for [Unix.getsockopt_error] *)

(** {6 Host and protocol databases} *)

val gethostname : unit -> string Lwt.t
  (** Wrapper for [Unix.gethostname] *)

val gethostbyname : string -> Unix.host_entry Lwt.t
  (** Wrapper for [Unix.gethostbyname] *)

val gethostbyaddr : Unix.inet_addr -> Unix.host_entry Lwt.t
  (** Wrapper for [Unix.gethostbyaddr] *)

val getprotobyname : string -> Unix.protocol_entry Lwt.t
  (** Wrapper for [Unix.getprotobyname] *)

val getprotobynumber : int -> Unix.protocol_entry Lwt.t
  (** Wrapper for [Unix.getprotobynumber] *)

val getservbyname : string -> string -> Unix.service_entry Lwt.t
  (** Wrapper for [Unix.getservbyname] *)

val getservbyport : int -> string -> Unix.service_entry Lwt.t
  (** Wrapper for [Unix.getservbyport] *)

val getaddrinfo : string -> string -> Unix.getaddrinfo_option list -> Unix.addr_info list Lwt.t
  (** Wrapper for [Unix.getaddrinfo] *)

val getnameinfo : Unix.sockaddr -> Unix.getnameinfo_option list -> Unix.name_info Lwt.t
  (** Wrapper for [Unix.getnameinfo] *)

(** {6 Terminal interface} *)

val tcgetattr : file_descr -> Unix.terminal_io Lwt.t
  (** Wrapper for [Unix.tcgetattr] *)

val tcsetattr : file_descr -> Unix.setattr_when -> Unix.terminal_io -> unit Lwt.t
  (** Wrapper for [Unix.tcsetattr] *)

val tcsendbreak : file_descr -> int -> unit Lwt.t
  (** Wrapper for [Unix.tcsendbreak] *)

val tcdrain : file_descr -> unit Lwt.t
  (** Wrapper for [Unix.tcdrain] *)

val tcflush : file_descr -> Unix.flush_queue -> unit Lwt.t
  (** Wrapper for [Unix.tcflush] *)

val tcflow : file_descr -> Unix.flow_action -> unit Lwt.t
  (** Wrapper for [Unix.tcflow] *)

(** {6 Low-level interaction} *)

exception Retry
  (** If an action raises {!Retry}, it will be requeued until the {b
      file descriptor} becomes readable/writable again. *)

exception Retry_read
  (** If an action raises {!Retry_read}, it will be requeued until the
      {b file descriptor} becomes readable. *)

exception Retry_write
  (** If an action raises {!Retry_read}, it will be requeued until the
      {b file descriptor} becomes writables. *)

type io_event = Read | Write

val wrap_syscall : io_event -> file_descr -> (unit -> 'a) -> 'a Lwt.t
  (** [wrap_syscall set fd action] wrap an action on a {b file
      descriptor}. It tries to execture action, and if it can not be
      performed immediately without blocking, it is registered for
      latter.

      In the latter case, if the thread is canceled, [action] is
      removed from [set]. *)

val check_descriptor : file_descr -> unit
  (** [check_descriptor fd] raise an exception if [fd] is not in the
      state {!Open} *)

val register_action : io_event -> file_descr -> (unit -> 'a) -> 'a Lwt.t
  (** [register_action set fd action] registers [action] on [fd]. When
      [fd] becomes [readable]/[writable] [action] is called.

      Note:

      - you must call [check_descriptor fd] before calling
      [register_action]

      - you should prefer using {!wrap_syscall}
  *)

type 'a job
  (** Type of jobs that run:

      - on another thread if the current async method is [Async_thread]
      - on the main thread if the current async method is [Async_none]. *)

val execute_job :
  ?async_method : async_method ->
  job : 'a job ->
  result : ('a job -> 'b) ->
  free : ('a job -> unit) -> 'b Lwt.t
  (** [execute_job ?async_method ~job ~get ~free] starts
      [job] and wait for its termination.

      [async_method] is how the job will be executed. It defaults to
      the async method of the current thread. [result] is used to get
      the result of the job, and [free] to free its associated
      resources. *)

(** {6 Notifications} *)

(** Lwt internally use a pipe to send notification to the main
    thread. The following functions allow to use this pipe. *)

val make_notification : ?once : bool -> (unit -> unit) -> int
  (** [new_notifier ?once f] registers a new notifier. It returns the
      id of the notifier. Each time a notification with this id is
      received, [f] is called.

      if [once] is specified, then the notification is stopped after
      the first time it is received. It defaults to [false]. *)

val send_notification : int -> unit
  (** [send_notification id] sends a notification.

      This function is thread-safe. *)

val stop_notification : int -> unit
  (** Stop the given notification. Note that you should not reuse the
      id after the notification has been stopped, the result is
      unspecified if you do so. *)

val set_notification : int -> (unit -> unit) -> unit
  (** [set_notification id f] replace the function associated to the
      notification by [f]. It raises [Not_found] if the given
      notification is not found. *)

(** {6 System threads pool} *)

(** If the program is using the async method {!Async_detach} or
    {!Async_switch}, Lwt will launch system threads to execute
    blocking system calls asynchronously. *)

val pool_size : unit -> int
  (** Maximum number of system threads that can be started. If this
      limit is reached, jobs will be executed synchronously. *)

val set_pool_size : int -> unit
  (** Change the size of the pool. *)

val thread_count : unit -> int
  (** The number of system threads running (excluding this one). *)

val thread_waiting_count : unit -> int
  (** The number threads waiting for a job. *)

(** {6 CPUs} *)

val get_cpu : unit -> int
  (** [get_cpu ()] returns the number of the CPU the current thread is
      running on. *)

val get_affinity : ?pid : int -> unit -> int list
  (** [get_affinity ?pid ()] returns the list of CPUs the process with
      pid [pid] is allowed to run on. If [pid] is not specified then
      the affinity of the current process is returned. *)

val set_affinity : ?pid : int -> int list -> unit
  (** [set_affinity ?pid cpus] sets the list of CPUs the given process
      is allowed to run on. *)

(**/**)

val run : 'a Lwt.t -> 'a
  (* Same as {!Lwt_main.run} *)

val has_wait4 : bool
  (* Deprecated, use [Lwt_sys.have `wait4]. *)
