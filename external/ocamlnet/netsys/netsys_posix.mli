(* $Id: netsys_posix.mli 1513 2010-12-17 18:23:53Z gerd $ *)

(** POSIX-specific system calls missing in the [Unix] module *)

(** {1 File descriptor polling} *)

type poll_array
  (** The array of [poll_cell] entries *)

type poll_req_events
type poll_act_events
  (** Poll events. [poll_req_events] is used to request that certain
      event types are observed. [poll_act_event] shows which 
      event types are actually possible
   *)

type poll_cell =
    { mutable poll_fd : Unix.file_descr;
      mutable poll_req_events : poll_req_events;
      mutable poll_act_events : poll_act_events;
    }
  (** The poll cell refers to the descriptor [poll_fd]. The [poll_req_events]
      are the events the descriptor is polled for. The [poll_act_events]
      are the actually reported events.
   *)

val have_poll : unit -> bool
  (** Whether there is a native [poll] implementation on this OS *)

val poll_req_events : bool -> bool -> bool -> poll_req_events
  (** [poll_req_events rd wr pri]: Create a set of in events consisting
      of the bits [rd], [wr], and [pri]. [rd] means to poll for 
      input data, [wr] to poll for output data, and [pri] to poll for urgent
      input data.
   *)

val poll_req_triple : poll_req_events -> bool * bool * bool
  (** Looks into a [poll_req_events] value, and returns the triple
      [(rd,wr,pri)].
   *)

val poll_null_events : unit -> poll_act_events
  (** Create an empty set of [poll_act_events], for initilization
      of poll cells.
   *)

val poll_result : poll_act_events -> bool
  (** Look whether there is any event in [poll_out_events] *)

val poll_rd_result : poll_act_events -> bool
val poll_wr_result : poll_act_events -> bool
val poll_pri_result : poll_act_events -> bool
val poll_err_result : poll_act_events -> bool
val poll_hup_result : poll_act_events -> bool
val poll_nval_result : poll_act_events -> bool
  (** Look for the bit in [poll_act_events] and return the status *)

val create_poll_array : int -> poll_array
  (** Create a poll array with the given size. The [poll_fd] member is
      initialized with [Unix.stdin], and the two event members are empty.
   *)

val set_poll_cell : poll_array -> int -> poll_cell -> unit
  (** [set_poll_cell a k c]: Sets the poll cell [k] to [c].
      The index [k] must be in the range from [0] to [N-1] when [N] is the
      length of the poll array.
   *)

val get_poll_cell : poll_array -> int -> poll_cell
  (** [get_poll_cell a k]: Returns the poll cell [k].
      The index [k] must be in the range from [0] to [N-1] when [N] is the
      length of the poll array.
   *)

val blit_poll_array : poll_array -> int -> poll_array -> int -> int -> unit
  (** [blit_poll_array a1 p1 a2 p2 len]: Copies the [len] cells at index [p1]
      from [a1] to [a2] at index [p2].
   *)

val poll_array_length : poll_array -> int
  (** Return the number of cells in the poll array *)

val poll : poll_array -> int -> float -> int
  (** [poll a n tmo]: Poll for the events of the cells 0 to [n-1] of 
      poll array [a], and set the [poll_revents] member of all cells.
      Wait for at most [tmo] seconds (a negative value means there is
      no timeout). Returns the number of ready file descriptors.

      On platforms without native support for [poll] the function is
      emulated using [Unix.select]. Note, however, that there is a
      performance penalty for the emulation, and that the output
      flags [poll_error_result], [poll_hangup_result], and
      [poll_invalid_result] are not emulated.
   *)

val restarting_poll :
      poll_array -> int -> float -> int
  (** A wrapper around [poll] that handles the [EINTR] condition *)

val poll_single : Unix.file_descr -> bool -> bool -> bool -> float -> bool
  (** [poll_single fd rd wr pri tmo]: Polls a single descriptor for the
      events given by [rd], [wr], and [pri]. In [tmo] the timeout can be
      passed. Returns [true] if one of the requested events is indicated
      for the descriptor. The [EINTR] case is not handled.
   *)


(** Actually, [poll_req_events] and [poll_act_events] are integers that
    are bitmasks of some constants. The following functions allow access to
    this detail. 
 *)

val int_of_req_events : poll_req_events -> int
val int_of_act_events : poll_act_events -> int
val req_events_of_int : int -> poll_req_events
val act_events_of_int : int -> poll_act_events
val const_rd_event : int
val const_wr_event : int
val const_pri_event : int
val const_err_event : int
val const_hup_event : int
val const_nval_event : int


(** {1 Fork helpers} *)

(** Ocamlnet invokes [Unix.fork] at some places to create child processes
    for doing real work. The following functions
    allow it to register a handler that is run in the forked child
    process. Note that this is done by the O'caml code calling [fork],
    and not via the POSIX [atfork()] facility.

    The handler should release OS resources like file descriptors that
    are by default shared with the parent process.

    The handler are not invoked when the only purpose of the [fork] is
    to [exec] a different process.
 *)

(** A [post_fork_handler] is a named function [unit -> unit] *)
class type post_fork_handler =
object
  method name : string
  method run : unit -> unit
end

val register_post_fork_handler : post_fork_handler -> unit
  (** Registers a new post fork handler (MT-Safe) *)

val remove_post_fork_handler : post_fork_handler -> unit
  (** Removes a post fork handler from the registry (MT-Safe) *)

val run_post_fork_handlers : unit -> unit
  (** Runs all post fork handlers. Exceptions are caught and printed to
      stderr.
   *)


(** {1 The "at" variants of system calls} *)

(** Note that a few "at" calls have been omitted because the same
    functionality can be achieved by first opening the file with
    [openat] and then by using a function that references the file
    by descriptor. An example for this is [fstatat]: After the
    [openat] call one can use [fstat] to get the stat record of the
    file.
 *)

val have_at : unit -> bool
  (** Whether the [*at] functions are available (they were only recently
      standardized and cannot be expected on all OS yet)
   *)

val at_fdcwd : Unix.file_descr
  (** Pseudo descriptor value to be used as first argument of [*at]
      functions
   *)

type at_flag = AT_EACCESS | AT_SYMLINK_NOFOLLOW | AT_REMOVEDIR
  (** Flags one can pass to "at" functions. Not all functions support
      all flags
   *)

val openat : Unix.file_descr -> string -> Unix.open_flag list -> 
             Unix.file_perm -> 
                Unix.file_descr
  (** Same as [Unix.openfile] but open relative to the directory given
      by first argument
   *)

val faccessat : Unix.file_descr -> string -> Unix.access_permission list ->
                at_flag list ->
                  unit
  (** Same as [Unix.access] but the file is taken relative to the directory
      given by first argument
   *)

val mkdirat : Unix.file_descr -> string -> int -> unit
  (** Same as [Unix.mkdir] but the file is taken relative to the directory
      given by first argument
   *)
 
val renameat : Unix.file_descr -> string -> Unix.file_descr -> string -> unit
  (** [renameat olddirfd oldpath newdirfd newpath] *)

val linkat : Unix.file_descr -> string -> Unix.file_descr -> string ->
             at_flag list -> unit
  (** [linkat olddirfd oldpath newdirfd newpath flags] *)

val unlinkat : Unix.file_descr -> string -> at_flag list -> unit
  (** Same as [Unix.unlink] but unlink the file relative to the directory
      given by first argument
   *)

val symlinkat : string -> Unix.file_descr -> string -> unit
  (** [symlinkat oldpath newdirfd newpath flags] *)

val mkfifoat : Unix.file_descr -> string -> int -> unit
  (** [mkfifoat dirfd path mode] *)

val readlinkat : Unix.file_descr -> string -> string
  (** [readlinkat dirfd path] *)

(* TODO: futimens *)

(** {1 Misc} *)

val int_of_file_descr : Unix.file_descr -> int
  (** Return the file descriptor as integer. See also
      {!Netsys.int64_of_file_descr} which works for all OS.
   *)

val file_descr_of_int : int -> Unix.file_descr
  (** Make a file descriptor from an integer *)

external sysconf_open_max : unit -> int = "netsys_sysconf_open_max"
  (** Return the maximum number of open file descriptor per process.
   * It is also ensured that for every file descriptor [fd]:
   * [fd < sysconf_open_max()]
   *)

external fchdir : Unix.file_descr -> unit = "netsys_fchdir"
  (** Set the current directory to the directory referenced by the
      file descriptor
   *)

external fdopendir : Unix.file_descr -> Unix.dir_handle = "netsys_fdopendir"
  (** Make a directory handle from a file descriptor. The descriptor
      is then "owned" by the directory handle, and will be closed by
      [Unix.closedir].

      This function is useful in conjunction with {!Netsys_posix.openat}
      to read directories relative to a parent directory.

      This is a recent addition to the POSIX standard; be prepared to
      get [Invalid_argument] because it is unavailable.
   *)

(* Process groups, sessions, terminals *)

external getpgid : int -> int = "netsys_getpgid"
  (** Return the process group ID of the process with the passed PID.
   * For the number 0, the process group ID of the current process is
   * returned.
   *)

val getpgrp : unit -> int
  (** Same as [getpgid 0], i.e. returns the process group ID of the
   * current process.
   *)

external setpgid : int -> int -> unit = "netsys_setpgid"
  (** [setpgid pid pgid]: Set the process group ID of the process [pid]
   * to [pgid]. If [pid = 0], the process group ID of the current process
   * is changed. If [pgid = 0], as process group ID the process ID of the
   * process referenced by [pid] is used.
   *
   * It is only possible for a process to join a process group if both
   * belong to the same session.
   *)

val setpgrp : unit -> unit
  (** Same as [setpgid 0 0]: A new process group ID is created, and the
   * current process becomes its sole member.
   *)

external tcgetpgrp : Unix.file_descr -> int = "netsys_tcgetpgrp"
  (** Return the process group ID of the foreground process group of
   * the session associated with the file descriptor, which must be
   * a tty.
   *)

external tcsetpgrp : Unix.file_descr -> int -> unit = "netsys_tcsetpgrp"
  (** Sets the foreground process group ID of the session associated
   * with the file descriptor, which must be a tty.
   *)

external ctermid : unit -> string = "netsys_ctermid"
  (** Returns the name of the controlling tty of the current process 
   * as pathname to a device file
   *)

external ttyname : Unix.file_descr -> string = "netsys_ttyname"
  (** Returns the name of the controlling tty referred to by the
   * file descriptor.
   *)

external getsid : int -> int = "netsys_getsid"
  (** Returns the session ID of the process with the passed PID.
   * For the PID 0, the session ID of the current process is returned.
   *)

(* Users and groups *)

external setreuid : int -> int -> unit = "netsys_setreuid"
  (** Changes both the real and the effective user ID of the current
   * process.
   *)

external setregid : int -> int -> unit = "netsys_setregid"
  (** Changes both the real and the effective group ID of the current
   * process.
   *)


(** {1 Fork+exec} *)

(** The following function has some similarity with posix_spawn, but
    is changed to our needs, Only special (although frequent) cases
    can be implemented with posix_spawn.
 *)

type wd_spec =
  | Wd_keep
  | Wd_chdir of string
  | Wd_fchdir of Unix.file_descr

type pg_spec =
  | Pg_keep
  | Pg_new_bg_group
  | Pg_new_fg_group
  | Pg_join_group of int

type fd_action =
  | Fda_close of Unix.file_descr
      (** Close the descriptor *)
  | Fda_close_ignore of Unix.file_descr
      (** Close the descriptor but ignore [EBADF] errors *)
  | Fda_close_except of bool array
      (** Closes all descriptors except those for which
          [except.(k)] is true where [k = int_of_file_descr fd].
          Descriptors outside the array index range are closed.
       *)
  | Fda_dup2 of Unix.file_descr * Unix.file_descr
      (** Duplicate the first descriptor to the second as [dup2] does *)

type sig_action =
  | Sig_default of int
  | Sig_ignore of int

val spawn : ?chdir:wd_spec ->
            ?pg:pg_spec ->
            ?fd_actions:fd_action list ->
            ?sig_actions:sig_action list ->
            ?env:string array ->
            string -> string array ->
              int
  (** [spawn cmd args]: Fork the process and exec [cmd] which gets the
      arguments [args]. On success, the PID of the new process is returned.

      - [chdir]: If set, the new process starts with this working directory
        (this is done before anything else)
      - [pg]: If set, the new process will be a member of this process group
      - [fd_actions]: If set, these descriptor actions are executed 
        sequentially
      - [sig_actions]: If set, these signal actions are executed sequentially
      - [env]: If set, the process gets this environment instead of the
        current one

      Any exceptions in the subprocess are detected, and reported. However,
      if [Fda_close] leads to [EBADF] for a descriptor, this error is
      ignored.

      If [pg=Pg_new_fg_group], one should include [Sig_ignore Sys.sigttou]
      in [sig_actions].
   *)


(** {1 Subprocesses and signals} *)

(** Watching subprocesses requires that the right signal handler is
    installed: [install_subprocess_handler]
 *)

type watched_subprocess

val watch_subprocess : int -> int -> bool -> 
                          Unix.file_descr * watched_subprocess
  (** [let fd, ws = watch_subprocess pid pgid kill_flag]: 
      Enters the subprocess [pid]
      into the watch list. If [pgid > 0], the process group ID is
      [pgid] (for [killpg_subprocess] and [killpg_all_subprocesses]).
      The [kill_flag] controls the process selection of
      [kill_all_subprocesses] and [killpg_all_subprocesses].

      The returned descriptor [fd] is open for reading and
      will indicate EOF when the subprocess is terminated. Via [ws]
      it is possible to query information about the subprocess. The
      installed signal handler will [wait] for the subprocess and
      put the process status into [ws].

      The caller has to close [fd] after the termination is signaled.
   *)

val ignore_subprocess : watched_subprocess -> unit
  (** Changes the arrangement so that the termination of the subprocess
      is no longer reported by the file descriptor. The file descriptor
      indicates EOF immediately (and can be closed by the caller).
      Nevertheless, the signal handler still [wait]s for the subprocess
      to avoid zombies.

      Any further access to [ws] will fail.
   *)
     
val forget_subprocess : watched_subprocess -> unit
  (** Frees OS resources. Any further access to the [ws] will fail. *)

val get_subprocess_status : watched_subprocess -> Unix.process_status option
  (** If the subprocess is terminated, this function returns the status.
      Otherwise [None] is returned
   *)

val kill_subprocess : int -> watched_subprocess -> unit
  (** Sends this signal to the subprocess if this process still exists.
      Never throws an exception.
   *)

val killpg_subprocess : int -> watched_subprocess -> unit
  (** Sends this signal to the process group of the subprocess if there
      is still a watched subprocess belonging to this group.
      Never throws an exception.
   *)

val kill_all_subprocesses : int -> bool -> bool -> unit
  (** [kill_all_subprocess signal override nogroup]: 
      Sends a signal to potentially
      all subprocesses. The signal is sent to a watched process if the process
      still exists, and these two conditions hold both:
      - [not nogroup || pgid = 0]: Processes with [pgid > 0] are excluded
        if [nogroup] is set
      - [kill_flag || override]: A process needs to have
        [kill_flag] set, or [override] is specified

      Never throws an exception if the signal handler is installed.
   *)

val killpg_all_subprocesses : int -> bool -> unit
  (** [killpg_all_subprocess signal override]: Sends a signal to potentially
      all subprocesses belonging to a process group (i.e. [pgid>0]).
    . The signal is sent to a process group if there are still watched
      subprocesses
      belonging to the group, and if either the [kill_flag] of any of the 
      subprocesses process was set to [true], or [override] is [true].

      Never throws an exception if the signal handler is installed.
   *)


val install_subprocess_handler : unit -> unit
  (** Installs a SIGCHLD handler for watching subprocesses. Note that only
      processes are [wait]ed for that are registered with 
      [watch_subprocess].

      The handler works both in the single-threaded and the multi-threaded
      case. [install_subprocess_handler] can safely called several times.
      The handler is installed every time the function is called, but the
      required data structures are only initialized at the first call.
   *)

val register_subprocess_handler : unit -> unit
  (** Uses the {!Netsys_signal} framework to manage the installation of
      the SIGCHLD handler.

      This is the preferred method of installing the SIGCHLD handler.
   *)


  (** {b Further notes.} *)

  (** The subprocess handler and [fork()]: The subprocess handler uses
      pipes for notification, and because of this it is sensitive to
      unpredicted duplicates of the pipe descriptors. [fork()] duplicates
      these pipe descriptors. If nothing is done about this issue, it
      can happen that the notification does not work anymore as it relies
      on detecting closed pipes.

      If [fork()] is immediately followed by [exec()] (as it is done
      to run subcommands), the problem does not occur, because the relevant
      descriptors are closed at [exec()] time.

      If [fork()] is used to start worker processes, however, we have
      to be careful. The descriptors need to be closed, so that the
      parent can continue to monitor subprocesses, and to allow the worker
      processes to use this mechanism. This module defines post fork
      handlers (see above), and a handler is automatically added that
      cleans the descriptors up. All user code has to do is to call
      [run_post_fork_handlers] immediately after [fork()] has spawned
      the new child, from the new child. This completely resets
      everything.
   *)

  (** The subprocess handler and multi-threading: The handler has been
      carefully designed, and works even in multi-threaded programs.
      However, one should know that multi-threading and [fork()] do not
      interact well with each other. Again, the problems do not occur
      if [fork()] is followed by [exec()]. There is no solution for the
      case that worker processes are started with [fork()], though.
      The (very generic) problem is that the state of mutexes and other
      multi-threading primitives is not well-defined after a [fork()].
   *)


(** {1 Syslog} *)

type level = Netlog.level
  (*  [ `Emerg | `Alert | `Crit | `Err | `Warning | `Notice | `Info | `Debug ]
   *)
  (** The log levels *)

type syslog_facility =
    [ `Authpriv
    | `Cron
    | `Daemon
    | `Ftp
    | `Kern
    | `Local0
    | `Local1
    | `Local2
    | `Local3
    | `Local4
    | `Local5
    | `Local6
    | `Local7
    | `Lpr
    | `Mail
    | `News
    | `Syslog
    | `User
    | `Uucp
    | `Default
    ]
  (** The facilities. Only [`User] and [`Local0] to [`Local7] are
      standard POSIX. If a facility is unavailable it is silently
      substituted by [`Local0]. The value [`Default] leaves this unspecified.
   *)

type syslog_option =
    [ `Cons
    | `Ndelay
    | `Odelay
    | `Nowait
    | `Pid
    ]
 (** The syslog options:
     - [`Cons]: Fall back to console logging if syslog is unavailable
     - [`Ndelay]: Open the connection immediately
     - [`Odelay]: Open the connection at the first call [syslog] (default)
     - [`Nowait]: Do not wait until it is ensured that the message is
       sent
     - [`Pid]: Log the PID with every message
  *)

val openlog : string option -> syslog_option list -> syslog_facility -> unit
  (** [openlog ident options facility]: Opens a log stream. [ident] is
      prepended to every message if given (usually the program name).
      The [facility] is the default facility for [syslog] calls.
   *)

val syslog : syslog_facility -> level -> string -> unit
  (** [syslog facility level message]: Logs [message] at [level] for
      [facility]
   *)

val closelog : unit -> unit
  (** Closes the log stream *)

(** Usually, the log stream is redirected to syslog by either:
    - setting [Netlog.current_logger] to [syslog facility], e.g.
      {[ Netlog.current_logger := Netsys_posix.syslog `User ]}
    - using the Netplex class for sending message to syslog (XXX)
 *)

(** {1 Sync} *)

external fsync : Unix.file_descr -> unit = "netsys_fsync"
  (** Sync data and metadata to disk *)
  
external fdatasync : Unix.file_descr -> unit = "netsys_fdatasync"
  (** Syncs only data to disk. If this is not implemented, same effect
      as [fsync]
   *)
 
(** {1 Optional POSIX functions} *)

external have_fadvise : unit -> bool = "netsys_have_posix_fadvise"
  (** Returns whether the OS supports the fadvise POSIX option *)

type advice =
  | FADV_NORMAL
  | FADV_SEQUENTIAL
  | FADV_RANDOM
  | FADV_NOREUSE
  | FADV_WILLNEED
  | FADV_DONTNEED


external fadvise : Unix.file_descr -> int64 -> int64 -> advice -> unit
                 = "netsys_fadvise"
  (** Advises to load pages into the page table from the file, or to remove
      such pages.
   *)

external have_fallocate : unit -> bool = "netsys_have_posix_fallocate"
  (** Returns whether the OS supports the fallocate POSIX option *)

external fallocate : Unix.file_descr -> int64 -> int64 -> unit
                   = "netsys_fallocate"
  (** Allocate space for the file and the specified file region *)


(** {1 POSIX Shared Memory} *)

external have_posix_shm : unit -> bool = "netsys_have_posix_shm"
  (** Returns whether the OS supports POSIX shared memory *)

type shm_open_flag =
  | SHM_O_RDONLY
  | SHM_O_RDWR
  | SHM_O_CREAT
  | SHM_O_EXCL
  | SHM_O_TRUNC

external shm_open : string -> shm_open_flag list -> int -> Unix.file_descr
  = "netsys_shm_open"
  (** Opens a shared memory object. The first arg is the name of the
    * object. The name must begin with a slash, but there must be no
    * further slash in it (e.g. "/sample"). The second arg are the
    * open flags. The third arg are the permission bits.
    *
    * The open flags are interpreted as follows:
    * - [SHM_O_RDONLY]: Open the object for read access
    * - [SHM_O_RDWR]: Open the object for read-write access
    * - [SHM_O_CREAT]: Create the object if it does not exist
    * - [SHM_O_EXCL]: If [SHM_O_CREAT] was also specified, and a an object
    *   with the given name already exists, return an error
    *   ([Unix.EEXIST]).
    * - [SHM_O_TRUNC]: If the object already exists, truncate it to 
    *   zero bytes
    *
    * One of [SHM_O_RDONLY] or [SHM_O_RDWR] must be given.
    *
    * On success, the function returns a file descriptor representing the
    * object. To access the object, one has to memory-map this file
    * use one of the [map_file] functions in the [Bigarray]
    * module, or in {!Netsys_mem}). Use [Unix.ftruncate] to resize the object.
    *
    * Note that it is unspecified whether this file pops up somewhere
    * in the file system, and if so, where.
    *
    * If a system error occurs, the function raises a [Unix.Unix_error]
    * exception.
   *)

external shm_unlink : string -> unit = "netsys_shm_unlink"
  (** Unlinks the name for a shared memory object *)

val shm_create : string -> int -> Unix.file_descr * string
  (** [let (fd,name) = shm_create prefix size]: Creates an shm object
      with a unique name. The name has the passed [prefix]. The [prefix]
      must start with "/" but must not contain any further "/". The object
      has a length of [size] bytes. The object has a permissions 0o600
      (independent of umask).
   *)


(** {1 POSIX semaphores} *)

val have_posix_semaphores : unit -> bool
  (** Returns [true] if POSIX semaphores are supported on this system *)

(** {b Constants.} *)

val sem_value_max : int
  (** The maximum value of a semaphore, but at most [max_int] *)

val sem_size : int
  (** The size of an anonymous semaphore in bytes ([sizeof(sem_t)]) *)

(** {b Types.} *)

type sem_kind = [ `Named | `Anonymous ]

type 'sem_kind semaphore

type named_semaphore = [ `Named ] semaphore
type anon_sempahore = [ `Anonymous ] semaphore

type sem_open_flag =
  | SEM_O_CREAT
  | SEM_O_EXCL

(** {b Named semaphores.} *)

val sem_open : 
  string -> sem_open_flag list -> int -> int -> named_semaphore
  (** [sem_open name flags mode init_value]: Opens a named semaphore
      which is optionally created. Sempahore names usually begin with
      a slash followed by a single name component (not containing a
      further slash).

      Interpretation of [flags]:
      - [SEM_O_CREAT]: The semaphore is created if not yet existing.
        The [mode] and [init_value] are interpreted if the creation
        actually occurs. [mode] is the permission of the semaphore.
        [init_value] is the (non-negative) initial value, up to
        [sem_value_max].
      - [SEM_O_EXCL]: The semaphore is only opened if the semaphore
        does not exist yet. Othwerwise an [EEXIST] error is returned
   *)

val sem_close : named_semaphore -> unit
  (** Closes a named semaphore. Semaphores are also automatically closed
      when the GC finds that the semaphore is unreachable.
   *)

val sem_unlink : string -> unit
  (** Unlinks the semaphore name *)

val sem_create : string -> int -> named_semaphore * string
  (** [let (sem,name) = sem_create prefix init_value]: Creates
      a new semaphore with a unique name. The name has the passed [prefix]. 
      The [prefix] must start with "/" but must not contain any further "/".
      The semaphore is initialized with [init_value]. The object has
      permissions 0o600 (modulo umask).
   *)


(** {b Anonymous semaphores.} *)

val sem_init : Netsys_types.memory -> int -> bool -> int -> 
                 anon_sempahore
  (** [sem_init mem pos pshared init_value]: Initializes the memory
      at position [pos] to [pos + sem_size() - 1] as anonymous semaphore.
      If [pshared] the semaphore is shared between processes. 
      [init_value] is the initial non-negative value (max is 
      [sem_value_max].
   *)

val sem_destroy : anon_sempahore -> unit
  (** Destroys the anonymous semaphore *)

val as_sem : Netsys_types.memory -> int -> anon_sempahore
  (** [as_sem mem pos]: Interprets the memory at position [pos]
      to [pos + sem_size() - 1] as anonymous semaphore.
      The memory region must already have been initialized.
   *)

(** {b Operations.} *)

val sem_getvalue : 'kind semaphore -> int
  (** Returns the value of the semaphore. If the value is bigger than
      what can be represented as [int], an [EINVAL] error is returned.

      The returned value is non-negative - if the underlying POSIX
      function reports a negative value zero is returned instead.
   *)

val sem_post : 'kind semaphore -> unit
  (** Unlocks the semaphore (increases the value by 1) *)

type sem_wait_behavior =
  | SEM_WAIT_BLOCK
  | SEM_WAIT_NONBLOCK

val sem_wait : 'kind semaphore -> sem_wait_behavior -> unit
  (** Locks the semaphore (decreases the value by 1). If the semaphore
      value is already zero, and [SEM_WAIT_BLOCK] is given, the function
      waits until another process or thread posts. If [SEM_WAIT_NONBLOCK]
      the error [EAGAIN] is returned.

      [sem_wait] may be interrupted by signals.
   *)

(** {1 Locales} *)

type langinfo =
    { nl_CODESET : string;  (** from [LC_CTYPE]: codeset name *)
      nl_D_T_FMT : string;  (** from [LC_TIME]: string for formatting date and time *)
      nl_D_FMT : string;  (** from [LC_TIME]: date format string *)
      nl_T_FMT : string;  (** from [LC_TIME]: time format string *)
      nl_T_FMT_AMPM : string;  (** from [LC_TIME]: a.m. or p.m. time format string *)
      nl_AM_STR : string;  (** from [LC_TIME]: Ante Meridian affix *)
      nl_PM_STR : string;  (** from [LC_TIME]: Post Meridian affix *)
      nl_DAY_1 : string;  (** from [LC_TIME]: name of the first day of the week (for example, Sunday) *)
      nl_DAY_2 : string;  (** from [LC_TIME]: name of the second day of the week (for example, Monday) *)
      nl_DAY_3 : string;  (** from [LC_TIME]: name of the third day of the week (for example, Tuesday) *)
      nl_DAY_4 : string;  (** from [LC_TIME]: name of the fourth day of the week (for example, Wednesday) *)
      nl_DAY_5 : string;  (** from [LC_TIME]: name of the fifth day of the week (for example, Thursday) *)
      nl_DAY_6 : string;  (** from [LC_TIME]: name of the sixth day of the week (for example, Friday) *)
      nl_DAY_7 : string;  (** from [LC_TIME]: name of the seventh day of the week (for example, Saturday) *)
      nl_ABDAY_1 : string;  (** from [LC_TIME]: abbreviated name of the first day of the week *)
      nl_ABDAY_2 : string;  (** from [LC_TIME]: abbreviated name of the second day of the week *)
      nl_ABDAY_3 : string;  (** from [LC_TIME]: abbreviated name of the third day of the week *)
      nl_ABDAY_4 : string;  (** from [LC_TIME]: abbreviated name of the fourth day of the week *)
      nl_ABDAY_5 : string;  (** from [LC_TIME]: abbreviated name of the fifth day of the week *)
      nl_ABDAY_6 : string;  (** from [LC_TIME]: abbreviated name of the sixth day of the week *)
      nl_ABDAY_7 : string;  (** from [LC_TIME]: abbreviated name of the seventh day of the week *)
      nl_MON_1 : string;  (** from [LC_TIME]: name of the first month of the year *)
      nl_MON_2 : string;  (** from [LC_TIME]: name of the second month *)
      nl_MON_3 : string;  (** from [LC_TIME]: name of the third month *)
      nl_MON_4 : string;  (** from [LC_TIME]: name of the fourth month *)
      nl_MON_5 : string;  (** from [LC_TIME]: name of the fifth month *)
      nl_MON_6 : string;  (** from [LC_TIME]: name of the sixth month *)
      nl_MON_7 : string;  (** from [LC_TIME]: name of the seventh month *)
      nl_MON_8 : string;  (** from [LC_TIME]: name of the eighth month *)
      nl_MON_9 : string;  (** from [LC_TIME]: name of the ninth month *)
      nl_MON_10 : string;  (** from [LC_TIME]: name of the tenth month *)
      nl_MON_11 : string;  (** from [LC_TIME]: name of the eleventh month *)
      nl_MON_12 : string;  (** from [LC_TIME]: name of the twelfth month *)
      nl_ABMON_1 : string;  (** from [LC_TIME]: abbreviated name of the first month *)
      nl_ABMON_2 : string;  (** from [LC_TIME]: abbreviated name of the second month *)
      nl_ABMON_3 : string;  (** from [LC_TIME]: abbreviated name of the third month *)
      nl_ABMON_4 : string;  (** from [LC_TIME]: abbreviated name of the fourth month *)
      nl_ABMON_5 : string;  (** from [LC_TIME]: abbreviated name of the fifth month *)
      nl_ABMON_6 : string;  (** from [LC_TIME]: abbreviated name of the sixth month *)
      nl_ABMON_7 : string;  (** from [LC_TIME]: abbreviated name of the seventh month *)
      nl_ABMON_8 : string;  (** from [LC_TIME]: abbreviated name of the eighth month *)
      nl_ABMON_9 : string;  (** from [LC_TIME]: abbreviated name of the ninth month *)
      nl_ABMON_10 : string;  (** from [LC_TIME]: abbreviated name of the tenth month *)
      nl_ABMON_11 : string;  (** from [LC_TIME]: abbreviated name of the eleventh month *)
      nl_ABMON_12 : string;  (** from [LC_TIME]: abbreviated name of the twelfth month *)
      nl_ERA : string;  (** from [LC_TIME]: era description segments *)
      nl_ERA_D_FMT : string;  (** from [LC_TIME]: era date format string *)
      nl_ERA_D_T_FMT : string;  (** from [LC_TIME]: era date and time format string *)
      nl_ERA_T_FMT : string;  (** from [LC_TIME]: era time format string *)
      nl_ALT_DIGITS : string;  (** from [LC_TIME]: alternative symbols for digits *)
      nl_RADIXCHAR : string;  (** from [LC_NUMERIC]: radix character *)
      nl_THOUSEP : string;  (** from [LC_NUMERIC]: separator for thousands *)
      nl_YESEXPR : string;  (** from [LC_MESSAGES]: affirmative response expression *)
      nl_NOEXPR : string;  (** from [LC_MESSAGES]: negative response expression *)
      nl_CRNCYSTR : string;  (** from [LC_MONETARY]: currency  *)
    }

val query_langinfo : string -> langinfo
  (** [query_langinfo locale]: Temporarily sets the passed [locale] and
      determines the language attributes. After that the orignal locale is
      restored. Pass "" as [locale] to get the locale requested in the
      environment.

      The value for "" is cached.
   *)

(** {1 Linux I/O Priorities} *)

(** These system calls are only available on Linux since kernel 2.6.13,
    and not even on every architecture. i386, x86_64, ia64, and PPC are
    known to work. 

    Per-process I/O priorities are currently only supported by the
    CFQ I/O scheduler.
 *)

val have_ioprio : unit -> bool
  (** Returns [true] if the system call [ioprio_get] is supported *)

type ioprio_target =
  | Ioprio_process of int   (** A single process *)
  | Ioprio_pgrp of int      (** A process group *)
  | Ioprio_user of int      (** All processes owned by this user *)

type ioprio =
  | Noprio                  (** I/O prioritization is unsupported by block layer *)
  | Real_time of int        (** 0..7 (higest..lowest prio) *)
  | Best_effort of int      (** 0..7 (higest..lowest prio) *)
  | Idle


external ioprio_get : ioprio_target -> ioprio = "netsys_ioprio_get"
    (** Retrieve the priority of the target. If several processes match the
        target, the highest priority is returned. If no process matches,
        the unix error [ESRCH] will be raised.
     *)

external ioprio_set : ioprio_target -> ioprio -> unit = "netsys_ioprio_set"
    (** Sets the priority of the target processes. *)


(** {1 Debugging} *)

module Debug : sig
  val enable : bool ref
    (** Enables {!Netlog}-style debugging *)

end
