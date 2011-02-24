(* $Id: netsys.mli 1401 2010-02-03 23:25:36Z gerd $ *)

(** System calls missing in the [Unix] module *)

(** {1 Generic file descriptors} *)

(** Not all OS provide generic read/write functions, or some emulation
    layer does not allow to use a descriptor with read/write. In the
    following functions, the style of the descriptor can be passed along
    with the descriptor to select the right I/O method. Effectively,
    the [fd_style] indicates which I/O function to call. Sometimes it is
    mandatory to call this function, sometimes it is only a good advice
    because the function provides the best interface for the kind of
    descriptor.
 *)

type fd_style =
    [ `Read_write
    | `Recv_send of Unix.sockaddr * Unix.sockaddr
    | `Recv_send_implied
    | `Recvfrom_sendto
    | `W32_pipe
    | `W32_pipe_server
    | `W32_event
    | `W32_process
    | `W32_input_thread
    | `W32_output_thread
    ]
  (** Some information what kind of operations are reasonable for descriptors:
      - [`Read_write]: The descriptor is neither a socket not one of the
        other special cases, so only read/write is possible if read/write
        is possible at all. This style is also used if it is meaningless
        to use data I/O like read/write at all.
      - [`Recv_send(sockaddr,peeraddr)]: The descriptor is a connected socket.
        recv/send are the preferred operations.
      - [`Recvfrom_sendto]: The descriptor is an unconnected socket, and
        it is possible to ask for addresses when exchanging data, so 
        recvfrom/sendto are the preferred operations.
      - [`Recv_send_implied]: The descriptor is a socket with implied 
        connection. There are no socket addresses.
        recv/send are the preferred operations. It is not possible to call
        [getsockname] or [getpeername].
      - [`W32_pipe]: The descriptor is a proxy descriptor for a Win32 named
        pipe as returned by {!Netsys_win32.pipe_descr}. 
      - [`W32_pipe_server]: The descriptor is a proxy descriptor for a Win32
        pipe server as returned by
        {!Netsys_win32.pipe_server_descr}. 
      - [`W32_event]: The descriptor is a Win32 proxy descriptor for an event
         as returned by {!Netsys_win32.create_event}. It is not possible to
        read/write with these descriptors.
      - [`W32_process]: The descriptor is a proxy descriptor for a Win32
        process as returned by
        {!Netsys_win32.create_process}. It is not possible to read/write
        with these descriptors.
      - [`W32_input_thread]: The descriptor is a proxy descriptor for a
        Win32-specific input thread
        as returned by
        {!Netsys_win32.create_input_thread}. 
      - [`W32_output_thread]: The descriptor is a proxy descriptor for a
        Win32-specific output thread
        as returned by
        {!Netsys_win32.create_output_thread}. 

      Win32: For the exact meaning of proxy descriptors, please see 
      {!Netsys_win32}. In short, a proxy descriptor is an abstract handle
      for the I/O object. The handle itself cannot be used for I/O, however,
      but only some specialized function. The proxy descriptor can only
      be used to dereference the I/O object. Note that the following functions
      like [gread] and [gwrite] automatically look up the I/O object behind
      the proxy and call the right I/O function.
   *)

val get_fd_style : Unix.file_descr -> fd_style
  (** Get the file descriptor style *)

val gread : fd_style -> Unix.file_descr -> string -> int -> int -> int
  (** [gread fd_style fd s pos len]: Reads up to [len] bytes from 
      descriptor [fd] which is supposed to support the I/O style 
      [fd_style], i.e. the right system call ([read], [recv],
      [recvfrom]) is chosen to read from the descriptor.
       After [n <= len] bytes have been read these are put into
      string [s] at positions [pos] to [pos+n-1], and [n] is returned.
      The function can fail with any I/O exception defined for the
      actually performed I/O operation. Whether the operation is blocking
      or non-blocking depends on the descriptor.

      If [len>0] but [n=0] the end of the input data is reached.
   *)

val blocking_gread : fd_style -> Unix.file_descr -> string -> int -> int -> int
  (** [let p = blocking_gread fd_style fd s pos len]: 
      Like [gread] up to [len] bytes are read from [fd] and stored in [s].
      If the I/O operation is blocking but the descriptor is in 
      non-blocking mode, this function blocks until the operation can
      be performed. If the operation is interrupted by a signal it is
      automatically restarted.

      If [n < len] the end of the input data is reached (where [n] is the
      returned number).

      See [wait_until_readable] below for further information which
      types of descriptors can be handled in non-blocking mode.
   *)

val really_gread : fd_style -> Unix.file_descr -> string -> int -> int -> unit
  (** [really_read fd_style fd s pos len]: Reads exactly [len] bytes from [fd]
      and stores them in [s] starting at [pos]. If the end of file condition
      is seen before [len] bytes are read, the exception [End_of_file]
      is raised, and it is unspecified how many bytes have been stored in
      [s]. Like [blocking_gread], non-blocking descriptors are forced
      to block until the operation becomes possible, and interruptions by
      signals are handled.

      See [wait_until_readable] below for further information which
      types of descriptors can be handled in non-blocking mode.
   *)

val gwrite : fd_style -> Unix.file_descr -> string -> int -> int -> int
  (** [gwrite fd_style fd s pos len]: Writes up to [len] bytes to
      descriptor [fd] which is supposed to support the I/O style 
      [fd_style], i.e. the right system call ([write], [send],
      [sendto]) is chosen to write to the descriptor.
    . The [n <= len] written bytes are taken from string [s],
      starting at position [pos] until [pos+n-1]. The number [n] is
      returned. The function can fail with any I/O exception defined for the
      actually performed I/O operation. Whether the operation is blocking
      or non-blocking depends on the descriptor.
   *)

val really_gwrite : fd_style -> Unix.file_descr -> string -> int -> int -> unit
  (** [really_write fd_style fd s pos len]: Writes exactly the [len] bytes
      from [s] to [fd] starting at [pos]. 
      If the I/O operation is blocking but the descriptor is in 
      non-blocking mode, this function blocks until the operation can
      be performed. If the operation is interrupted by a signal it is
      automatically restarted.

      See [wait_until_writable] below for further information which
      types of descriptors can be handled in non-blocking mode.
   *)

exception Shutdown_not_supported
  (** See [gshutdown] *)

val gshutdown : fd_style -> Unix.file_descr -> Unix.shutdown_command -> unit
  (** [gshutdown fd_style fd cmd]: If there is the possibility to shut down
      the connection for this kind of descriptor, the shutdown is tried.
      It is possible that the function raises the [EAGAIN] Unix error if
      the shutdown operation is non-blocking, and currently not possible. 
      It is suggested to wait until the descriptor is writable, and to try
      again then.

      If there is no shutdown operation for this kind of descriptor, the
      exception [Shutdown_not_supported] is raised. In this case it is
      usually sufficient to close the descriptor ([gclose], see below),
      and when all descriptors to the resource are closed, the resource
      is shut down by the OS.

      Details by [fd_style]:
       - [`Recv_send] and [`Recv_send_implied]: The socket is shut
         down as requested by [Unix.shutdown]. This only triggers the
         shutdown, but does not wait until it is completed. Also,
         errors are usually not immediately reported.
       - [`W32_pipe]: It is only possible to request [SHUTDOWN_ALL]
         for these descriptors.  For other shutdown types, the error
         [EPERM] is reported. The shutdown is synchronous and completed
         when the function returns.
       - [`W32_pipe_server]: It is only possible to request [SHUTDOWN_ALL]
         for these descriptors.  For other shutdown types, the error
         [EPERM] is reported. A shutdown means here to stop accepting
         new connections. The shutdown is synchronous and completed
         when the function returns.
       - [`W32_output_thread]:  It is only possible to request [SHUTDOWN_SEND]
         for these descriptors. A [SHUTDOWN_ALL] is also interpreted as
         [SHUTDOWN_SEND], and a [SHUTDOWN_RECEIVE] is ignored.
         A shutdown means here that the EOF is appended
         to the output buffer, and when the output thread has written the
         buffer contents, the underlying descriptor (not [fd]!) will be
         closed. The shutdown operation is non-blocking. If it is not
         possible at the moment of calling, the error [EAGAIN] is reported.
       - Other styles raise [Shutdown_not_supported].
   *)



val is_readable : fd_style -> Unix.file_descr -> bool
val is_writable : fd_style -> Unix.file_descr -> bool
val is_prird : fd_style -> Unix.file_descr -> bool
  (** Test whether the descriptor would not block if one of the input,
      output, or priority input operations were done.

      On POSIX systems the tests work for a wide variety of descriptor 
      types (but not for regular files which are assumed to be always
      readable and writable).
      If the [poll] interface is available it is preferred over the
      [select] interface to conduct the test.

      On Win32, the tests are limited to sockets, named pipes and
      event objects. (The latter two only in the form provided by
      {!Netsys_win32}, see there.)

      Generally, if the blocking status cannot be determined for
      a class of I/O operations, the functions return [true], in
      the hope that it is better to block than to never conduct
      the operation.
   *)

val wait_until_readable : fd_style -> Unix.file_descr -> float -> bool
val wait_until_writable : fd_style -> Unix.file_descr -> float -> bool
val wait_until_prird : fd_style -> Unix.file_descr -> float -> bool
  (** Wait until an operation for a single descriptor becomes possible.
      The float argument is the timeout (negative value means no timeout).
      Returns whether the operation is possible ([true]). Otherwise,
      there was a timeout ([false]).

      On POSIX systems this works for a wide variety of descriptor 
      types (but not for regular files which are assumed to be always
      readable and writable).
      If the [poll] interface is available it is preferred over the
      [select] interface to wait for I/O. The functions also catch
      interruptions by signals.

      On Win32, waiting is limited to sockets, named pipes and
      event objects. (The latter two only in the form provided by
      {!Netsys_win32}, see there.)

      Generally, if waiting is not supported for
      a class of I/O operations, the functions return immediately [true], in
      the hope that it is better to block than to never conduct
      the operation.
   *)

val gclose : fd_style -> Unix.file_descr -> unit
  (** Shuts down the system object referenced by the descriptor so far
      possible, and closes the descriptor.

      Errors are logged to {!Netlog} as [`Crit] events, and
      do not generate exceptions.

      The exact semantics of the close operation varies from descriptor
      style to descriptor style. Generally, [gclose] assumes that all
      I/O is done, and all buffers are flushed, and that one can tear
      down the underlying communication circuits. [gclose] is always
      the right choice when the I/O channel needs to be aborted after a
      fatal error, and it does not matter whether errors occur or not.
      If a data connection needs to be orderly closed (i.e. without
      data loss), one should first try to finish the communication,
      either by protocol means (e.g. wait for ACK messages), or by
      calling [gshutdown] first (see above).
   *)



(** {1 Functions for sockets} *)

val wait_until_connected : Unix.file_descr -> float -> bool
  (** After a non-blocking connect has been initiated, this function can be
      used to wait until (1) the connect is successful, or (2) the connect
      fails, or (3) the operation times out. The [float] argument is the
      timeout value (negative value means no timeout).
      The function returns [true] for the cases (1) and (2), and [false]
      for case (3). The cases (1) and (2) can be further analyzed by
      calling [connect_check] (see below).

      On POSIX, this function is identical to [wait_until_writable]. On
      Win32 the wait condition is slightly different.

      On Win32, this function also tolerates client proxy descriptors for
      Win32 named pipes. However, there is no waiting - the function 
      immediately returns.
   *)

val connect_check : Unix.file_descr -> unit
  (** Tests whether the socket is connected with the peer after calling
      [Unix.connect]. If the socket is connected, the function returns normally.
      Otherwise, the current socket error is raised as a [Unix.Unix_error]
      exception. This function is intended to be called after a 
      non-blocking connect has been initiated, and the success or error
      is indicated (e.g. after [wait_until_connected] returns).

      Side effect: The per-socket error code may be reset.

      On Win32, this function also tolerates client proxy descriptors for
      Win32 named pipes. However, there is no real check - the function 
      immediately returns.
   *)

val domain_of_inet_addr : Unix.inet_addr -> Unix.socket_domain
  (** Returns the socket domain of Internet addresses, i.e. whether the
    * address is IPv4 or IPv6
   *)

val getpeername : Unix.file_descr -> Unix.sockaddr
  (** like [Unix.getpeername], but errors are fixed up. [ENOTCONN] is
      ensured when the socked is unconnected or shut down.
   *)

(** {1 Helper functions} *)

val restart : ('a -> 'b) -> 'a -> 'b
  (** [restart f arg] calls [f arg], and restarts this call if the
    * exception [Unix_error(EINTR,_,_)] is caught.
    *
    * Note that there are some cases where this handling of [EINTR] is
    * not sufficient:
    * - Functions that have a timeout argument like [Unix.select]: When
    *   [EINTR] is caught the timeout should be adjusted.
    * - [Unix.connect] with a blocking descriptor because this is not
    *   well-enough specified by POSIX
   *)

val restart_tmo : (float -> 'b) -> float -> 'b
  (** [restart_tmo f tmo] calls [f] with a timeout argument [tmo], and
    * restarted the call if the exception [Unix_error(EINTR,_,_)] is caught.
    * In the restart case, the timeout argument is reduced by the
    * already elapsed time.
    *
    * Negative timeout arguments are interpreted as "no timeout".
   *)

val restarting_select : 
      Unix.file_descr list -> Unix.file_descr list -> Unix.file_descr list ->
      float ->
        (Unix.file_descr list * Unix.file_descr list * Unix.file_descr list)
  (** A wrapper around [Unix.select] that handles the [EINTR] condition.

      Note: This function calls [Unix.select] and shares all pros and cons
      with [Unix.select]. In particular, the OS often sets a limit on the 
      number (and/or the numeric value) of the descriptors (e.g. for
      Linux it is 1024, for Windows it is 64). On Ocaml 3.11 the Windows
      version of [Unix.select] includes some support for other types
      of descriptors than sockets.
   *)

val sleep : float -> unit
val restarting_sleep : float -> unit
  (** Sleep for the passed time. [restarting_sleep] additionally handles
      [EINTR].
   *)

val unix_error_of_code : int -> Unix.error
  (** Converts an integer error into the corresponding variant *)

val int64_of_file_descr : Unix.file_descr -> int64
  (** Returns the file descriptor as int64 number. Works for all OS. *)

val string_of_fd : Unix.file_descr -> string
  (** Return a string describing the descriptor (for debugging) *)

val string_of_sockaddr : Unix.sockaddr -> string
  (** Returns a human-readable string describing the address
      (for debug messages)
   *)

val string_of_fd_style : fd_style -> string
  (** Returns a string describing the fd style (debugging) *)

val is_stdin : Unix.file_descr -> bool
val is_stdout : Unix.file_descr -> bool
val is_stderr : Unix.file_descr -> bool
  (** Returns whether the descriptors are stdin/stdout/stderr *)

val set_close_on_exec : Unix.file_descr -> unit
val clear_close_on_exec : Unix.file_descr -> unit
  (** Working versions of the functions with the same name in [Unix] *)

external _exit : int -> unit = "netsys__exit"
  (** Exit the program immediately without running the atexit handlers.
   * The argument is the exit code, just as for [exit].
   *)


(** {1 Multicast Functions} *)

val mcast_set_loop : Unix.file_descr -> bool -> unit
  (** Whether sent multicast messages are received by the sending host *)

val mcast_set_ttl : Unix.file_descr -> int -> unit
  (** Set TTL/hops value *)

val mcast_add_membership : Unix.file_descr -> 
                           Unix.inet_addr -> Unix.inet_addr -> unit
  (** Join a multicast group.

      First inet addr is the group to join. Second inet addr selects the
      network interface (or [Unix.inet_addr_any]).
   *)

val mcast_drop_membership : Unix.file_descr -> 
                            Unix.inet_addr -> Unix.inet_addr -> unit
  (** Leave a multicast group.
   
     First inet addr is the group to leave. Second inet addr selects the
     network interface (or [Unix.inet_addr_any]).
   *)


(** {1 Profiling} *)

val moncontrol : bool -> unit
  (** Interface to the [moncontrol] routine of the GPROF profiler. 
      [moncontrol false] stops profiling; [moncontrol true] starts
      profiling again.

      This is a no-op if the program is not compiler for profiling.
   *)


(** {1 Deprecated} *)

(** The following interfaces have been replaced by more generic implementations
    that work on more platforms.
 *)

val blocking_read : Unix.file_descr -> string -> int -> int -> int
  (** Same as [blocking_gread `Read_write] *)

val really_read : Unix.file_descr -> string -> int -> int -> unit
  (** Same as [really_gread `Read_write] *)

val really_write : Unix.file_descr -> string -> int -> int -> unit
  (** Same as [really_gwrite `Read_write] *)

(** The following interfaces have been moved to {!Netsys_posix}. *)

type shm_open_flag =
    Netsys_posix.shm_open_flag =
  | SHM_O_RDONLY
  | SHM_O_RDWR
  | SHM_O_CREAT
  | SHM_O_EXCL
  | SHM_O_TRUNC

val have_posix_shm : unit -> bool
val shm_open : string -> shm_open_flag list -> int -> Unix.file_descr
val shm_unlink : string -> unit


(** {1 Further Documentation} *)

(** {2 How to orderly close I/O channels} 

    After reading from uni-directional descriptors, and seeing the
    EOF, it is usually sufficient to call [gclose] to free OS resources.

    After writing to uni-directional descriptors one should call
    [gshutdown] to send an EOF ([SHUTDOWN_SEND]). For some descriptors
    one will get the exception [Shutdown_not_supported] which can be
    ignored in this context The [gshutdown] function cannot,
    however, report in all cases whether the operation was successful.
    As a rule of thumb, error reporting works for local data connections,
    but not always for remote connections, and there is no way to fix
    this. After writing EOF, call [gclose] to free OS resources.

    For bidirectional connections, it is even more complicated. If the
    connection is local, a bidirectional connection behaves much like a pair
    of unidirectional connections. However, in the network case, we have
    to go down to the protocol level.

    For TCP the golden rule is that the client initiates the connection,
    and the client finishes the connection. The case that the server
    finishes the connection is not well-specified - or better, the server needs
    the ACK from the client after triggering the connection termination. 
    In practice we have the cases:

    - Client sends EOF, and server replies with EOF: This is the normal
      case for which TCP is designed. Client code can invoke
      [gshutdown] with [SHUTDOWN_SEND] and then waits until the EOF from 
      the server arrives,
      and then [gclose]s the descriptor. It may happen that the client
      gets an error if some problem occurs, so this is reliable from the
      perspective of the client. The server first sees the EOF from the
      client, and then responds with another [gshutdown], followed by 
      [gclose]. From the server's perspective it does not matter whether
      the operation results in an error or not - the client has lost
      interest anyway.
    - Client sends EOF, and server replies with data, and then EOF.
      Here, the client has to read the final data, and then wait for the
      server's EOF after sending its own EOF. On the server's side, 
      some data is written before the final EOF. The question is how
      the server can be sure that the data really arrived. Unfortunately,
      there is no way to do so. The server may not get all errors because
      these may arrive at the server computer after [gshutdown]. There
      is no way to fix this. (One should better fix the application protocol. 
      Note
      that even prominent researchers trapped into this problem. For example,
      the first version of HTTP had this problem.)
    - Server sends EOF, and client replies with EOF: This is the difficult
      case. Here, the server wants to be sure that the data sent immediately
      before its EOF really arrives at the client. After [gshutdown]
      it is forbidden to immediately [gclose], because this may result
      in a connection reset. Instead, the server has to wait for the 
      client's EOF. (This is called "lingering".) If the client's EOF is
      seen one can [gclose].
    - Server sends EOF, and client replies with data, followed by EOF:
      I admit I don't know whether TCP can handle this somehow.
 *)


(** {1 Debugging} *)

module Debug : sig
  val enable : bool ref
    (** Enables {!Netlog}-style debugging *)

end


(**/**)

(* internal: *)
val set_moncontrol : (bool -> unit) -> unit
