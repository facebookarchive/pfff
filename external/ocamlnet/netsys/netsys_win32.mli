(* $Id: netsys_win32.mli 1505 2010-12-15 19:15:22Z gerd $ *)

(** Primitives for Win32 *)

(** {1 Event objects} *)

type w32_event

val create_event : unit -> w32_event
  (** Create an event object *)

val set_event : w32_event -> unit
  (** Set the object to "signaled" state *)

val reset_event : w32_event -> unit
  (** Set the object to "non-signaled" state *)

val test_event : w32_event -> bool
  (** Test whether the object is in signaled state *)

val event_wait : w32_event -> float -> bool
  (** Wait until the event is set to signaled state. The float argument
      is the timeout in seconds. The function returns whether the object
      is in signaled state.
   *)

val event_descr : w32_event -> Unix.file_descr
  (** Returns the proxy descriptor for the event. See [lookup] below for
      more on proxy descriptors. This function always returns the same
      descriptor. The user has to close this descriptor if this function
      is called.
   *)


(** {1 Primitives for sockets} *)

val wsa_event_select : 
      w32_event -> Unix.file_descr -> Netsys_posix.poll_req_events -> unit
  (** associate event objects with socket conditions *)

val wsa_maximum_wait_events : 
      unit -> int
  (** max size of the array passed to [wsa_wait_for_multiple_events] *)


val wsa_wait_for_multiple_events : 
      w32_event array -> int -> int option
    (** Waits until one of the events in the array is in signaled state,
        or until a timeout happens. The int is the timeout in milliseconds.
        A negative timeout means infinity.

        The function returns the first index in the array that is signaled.

        On timeout, [None] is returned.

        The return value [WSA_WAIT_IO_COMPLETION] is mapped to the
        Unix error [EINTR].
     *)


val wsa_enum_network_events : 
      Unix.file_descr -> w32_event -> Netsys_posix.poll_act_events
    (** Checks whether an event has been recorded *)


val real_select : 
      Unix.file_descr list -> 
         Unix.file_descr list -> 
         Unix.file_descr list -> 
         float ->
           (Unix.file_descr list * Unix.file_descr list * Unix.file_descr list)
    (** Up to Ocaml 3.10, this function is identical to [Unix.select]. In
        3.11, the latter was changed to a smart implementation that promises
        to handle other types of handles in addition to sockets. As we do
        the same in [Netsys], this would be a duplication of work. Also,
        the older implementation is more mature.
     *)

(** {1 Support for named pipes} *)

(** Win32 named pipes work very much like Unix Domain sockets, only
    that the Win32 interface is different. This wrapper, however,
    mimicks socket behaviour as far as possible (and we also use
    an socket-like API with [listen] and [accept]). There is a 
    [w32_pipe_server] representing pipe servers. An individual pipe
    is wrapped into a [w32_pipe].

    Win32 named pipes do not allow to check whether an operation would block
    before starting the operation. There is so-called overlapped I/O,
    but it works differently than Unix-style multiplexing.

    The following functions add a layer to the Win32 primitives that
    helps using pipes in a way similar to multiplexing. We allocate
    buffers for input and output, and the functions [pipe_read] and
    [pipe_write] access these buffers in the first place. When reading,
    but the read buffer is empty, we start an  overlapped read operation  
    from the pipe handle. The arriving data refills the read buffer, and
    a [w32_event] is signaled to wake up any pending event loop.
    During the pending read from the pipe handle, the read buffer is
    locked, and [pipe_read] will return [EWOULDBLOCK].

    Writing is slightly more difficult. The first [pipe_write] puts
    the data into the write buffer, and immediately starts an overlapped
    I/O operation to write the data to the pipe handle. During this
    operation the write buffer is locked, and cannot be further used
    to accumulate data, even if there is space. So [pipe_write] will
    return [EWOULDBLOCK] while the operation takes place. A [w32_event] is
    signaled when the write operation is over.

    The only downside of this approach is that the caller has to use
    [pipe_read] and [pipe_write] to access pipes, instead of
    [Unix.read] and [Unix.write]. If generic r/w functions are
    required that work for numerous kinds of descriptors, there are
    {!Netsys.gread} and {!Netsys.gwrite} which support named
    pipes.
 *)

type w32_pipe_server
  (** A pipe server. Note that there is no such thing in the Win32 API.
      Actually, a [w32_pipe_server] contains the server endpoints of 
      a number of pipes, and a few helper objects.
   *)

type w32_pipe
  (** A pipe endpoint *)

type pipe_mode = Pipe_in | Pipe_out | Pipe_duplex

val rev_mode : pipe_mode -> pipe_mode
  (** Reverses the direction *)


val create_local_pipe_server : string -> pipe_mode -> int -> w32_pipe_server

  (** [create_local_named_pipe name mode n]: Create a pipe server.
      The [name] must have the format "\\.\pipe\<name>".
      In [n] the maximum number of instances is passed. The server is
      set up with a security descriptor so only clients on the same system
      can connect.
   *)

(** In the following, a terminology has been chosen that is similar to
    those of the socket API. The terms are different from those Microsoft
    prefers, however.
 *)

val pipe_listen : w32_pipe_server -> int -> unit
  (** Creates the backlog queue with [n] prepared server endpoints.

      One can check for new client connections by looking at the
      [pipe_connect_event].
   *)

val pipe_accept : w32_pipe_server -> w32_pipe
  (** Waits until the connect event is signaled (usually meaning that
      a new client connection is available), and returns the new
      pipe.
   *)

val pipe_connect : string -> pipe_mode -> w32_pipe
  (** [pipe_connect name mode]: Creates a client pipe handle, and tries
      to connect to the pipe server [name]. The function fails with the
      Unix error [EAGAIN] if there are currently no listening instances of the
      pipe at the server.

      The name must be of the form "\\.\pipe\<name>" (excluding connects
      to pipes on remote systems). This function allows only connects to
      local pipe servers, and enforces anonymous impersonation.

      Note that you also can connect to named pipes using [open_in] and
      [Unix.openfile], and that these functions do not protect against
      malicious servers that impersonate as the caller.
   *)

val pipe_pair : pipe_mode -> (w32_pipe * w32_pipe)
  (** Returns a pair of connected pipes (using automatically generated
      names). The left pipe is in the passed [pipe_mode], and the
      right pipe is in the matching complementaty mode.
   *)


val pipe_read : w32_pipe -> string -> int -> int -> int
  (** [pipe_read p s pos len]: Tries to read data from the pipe. If data
      is available, it is put into the [len] bytes at position [pos] of
      the string [s], and the actual number of read bytes is returned.

      If no data is available, the function fails with a Unix error of
      [EAGAIN].

      If the end of the pipe is reached, the function returns 0.
   *)

val pipe_write : w32_pipe -> string -> int -> int -> int
  (** [pipe_write p s pos len]: Tries to write data to the pipe. If space
      is available, the data is taken from the [len] bytes at position [pos] of
      the string [s], and the actual number of written bytes is returned.

      If no space is available, the function fails with a Unix error of
      [EAGAIN].
   *)

val pipe_shutdown : w32_pipe -> unit
  (** Cancels all pending I/O operations and closes the pipe handle.

      Note that there is no way to close only one direction of bidirectional
      pipes.

      See the comments on closing pipes below.
   *)

val pipe_shutdown_server : w32_pipe_server -> unit
  (** Closes the pipe server: All endpoints in the backlog queue are
      shutdown. Note that this can result in crashed connections -
      if the kernel establishes a connection but it is not yet
      [pipe_accept]ed, it is simply destroyed by this function.
   *)

val pipe_connect_event : w32_pipe_server -> w32_event
  (** The event object signals when a new client connection is available
   *)

val pipe_rd_event : w32_pipe -> w32_event
val pipe_wr_event : w32_pipe -> w32_event
  (** The event objects signaling that read and write operations are possible.
      The read event is in signaled state when the read buffer is non-empty
      (even for write-only pipes). The write event is in signaled state when
      the pipe is connected and the write buffer is empty (even for 
      read-only pipes).
   *)

val pipe_wait_connect : w32_pipe_server -> float -> bool
  (** Wait until a client connects to this server. The float argument
      is the timeout in seconds. The function returns whether there is
      data to read or write. If not, a timeout has occurred.
   *)

val pipe_wait_rd : w32_pipe -> float -> bool
val pipe_wait_wr : w32_pipe -> float -> bool
  (** Wait until the pipe becomes readable or writable. The float argument
      is the timeout in seconds. The function returns whether there is
      data to read or write. If not, a timeout has occurred.
   *)

(* val pipe_signal : w32_pipe_helper -> w32_event -> unit *)
  (* Associates the pipe with an event object. The event is signaled
     when the pipe changes its connection state to Pipe_deaf or
     Pipe_down. The event must be manually reset by the caller.
   *)


val pipe_server_descr : w32_pipe_server -> Unix.file_descr
  (** Returns the proxy descriptor for the pipe server. See [lookup] below for
      more on proxy descriptors. This function always returns the same
      descriptor. The user has to close this descriptor if this function
      is called.
   *)

val pipe_descr : w32_pipe -> Unix.file_descr
  (** Returns the proxy descriptor for the pipe. See [lookup] below for
      more on proxy descriptors. This function always returns the same
      descriptor. The user has to close this descriptor if this function
      is called.
   *)

val pipe_name : w32_pipe -> string
val pipe_server_name : w32_pipe_server -> string
  (** Returns the name of the pipe *)

val pipe_mode : w32_pipe -> pipe_mode
val pipe_server_mode : w32_pipe_server -> pipe_mode
  (** Returns the pipe/server mode *)

val unpredictable_pipe_name : unit -> string
  (** Returns a valid pipe name that can practically not be predicted *)

(** {b Shutting down pipes.} The suggested model is that the client shuts
    down the pipe first. A pipe client ensures that all data are transmitted
    by waiting until the pipe becomes writable again, and then calling
    [pipe_shutdown]. The server then sees EOF when reading from the pipe,
    or gets an [EPIPE] error when writing to the pipe. The server should
    then also [pipe_shutdown] the endpoint.

    When servers start the closure of connections, there is no clean way
    of ensuring that all written data are transmitted. There is the
    [FlushFileBuffers] Win32 function, but it is blocking.
 *)

(** {1 I/O threads} *)

(** I/O threads can be used to do read/write-based I/O in an asynchronous
    way for file handles that do not support asynchronous I/O by themselves,
    e.g. anonymous pipes.

    I/O threads are only available if the application is compiled as
    multi-threaded program.
 *)

type w32_input_thread

val create_input_thread : Unix.file_descr -> w32_input_thread
  (** Creates the input thread for this file descriptor. Data is being
     pumped from this handle to an internal buffer, and can be read from
     there by [input_thread_read].

     The thread continues to run until EOF is reached, an I/O error
     occurs, or until the
     thread is cancelled ([cancel_input_thread]).

     After starting the input thread, the file descriptor must not
     be used anymore. It is now owned by the input thread.
   *)

val input_thread_event : w32_input_thread -> w32_event
  (** This event is signaled when there is data to read, or the EOF
     is reached, or there is an error condition 
   *)

val input_thread_read : w32_input_thread -> string -> int -> int -> int
  (** [input_thread_read t s pos len]: Tries to read data from the buffer. 
      If data
      is available, it is put into the [len] bytes at position [pos] of
      the string [s], and the actual number of read bytes is returned.

      If no data is available, the function fails with a Unix error of
      [EAGAIN] (non-blocking).

      If the end of the data is reached, the function returns 0.

      For cancelled requests, the function raises [EPERM].
   *)

val cancel_input_thread : w32_input_thread -> unit
  (** Stops the input thread. No more data will be pumped from the handle
      to the internal buffer. It is no error to cancel a thread that is
      already cancelled. There is no way to restart the thread later.

      The thread is automatically cancelled by the GC finaliser. However,
      users are encouraged to call [cancel_input_thread] as soon as
      the thread is no longer needed, because a thread is an expensive
      resource.

      Implementation note: Actually, cancellation is only fully implemented
      on Windows Vista. On XP the actual cancellation may be delayed
      indefinetely.
   *)

val input_thread_proxy_descr : w32_input_thread -> Unix.file_descr
  (** Returns the proxy descriptor *)


type w32_output_thread

val create_output_thread : Unix.file_descr -> w32_output_thread
  (** Creates the output thread for this file descriptor. Data is being
     pumped an internal buffer to this descriptor, and can be written
     there by [output_thread_read].

     The thread continues to run until it is explicitly closed, or 
     an I/O error occurs, or until the
     thread is cancelled ([cancel_output_thread]).

     After starting the output thread, the file descriptor must not
     be used anymore. It is now owned by the output thread.
   *)

val output_thread_event : w32_output_thread -> w32_event
  (** This event is signaled when there is space in the buffer,
      or when there is an error condition 
   *)

val output_thread_write : w32_output_thread -> string -> int -> int -> int
  (** [output_thread_write t s pos len]: Tries to write data to the buffer. 
      If this
      is possible, the substring starting at position [pos] of the string [s]
      with a length of [len] is appended to the buffer. The actual number
      of written bytes is returned.

      If no space is available in the buffer, the function fails with a
      Unix error of [EAGAIN] (non-blocking).

      For cancelled requests, the function raises [EPERM].
   *)

val close_output_thread : w32_output_thread -> unit
  (** Adds the EOF condition to the buffer. When the buffer is written
      to the descriptor, the descriptor will be closed.

      Note that this is also an asynchronous operation, like
      [output_thread_write]. If closing is not possible at a certain
      moment, the Unix error [EGAIN] is raised. This ensures that all
      errors of previous writes can be reported.

      The output thread terminates after a successful close.

      For cancelled requests, the function raises [EPERM].
   *)

val cancel_output_thread : w32_output_thread -> unit
  (** Stops the output thread. This is different from closing as the
      data that is still in the buffer but not yet written may be
      dropped (if possible). Also, there is no error reporting.

      It is no error to cancel a thread that is
      already cancelled or closed. 
      There is no way to restart the thread later.

      The thread is automatically cancelled by the GC finaliser. However,
      users are encouraged to call [cancel_output_thread] or
      [close_output_thread] as soon as
      the thread is no longer needed, because a thread is an expensive
      resource.

      Implementation note: Actually, cancellation is only fully implemented
      on Windows Vista. On XP the actual cancellation may be delayed
      indefinetely.
   *)

val output_thread_proxy_descr : w32_output_thread -> Unix.file_descr
  (** Returns the proxy descriptor *)


(** {1 Processes} *)

(** Keep in mind that Win32 distinguishes between two kinds of
    executables: console applications, and GUI applications. The kind
    is set at link time, and stored in the executable file. 
    Years ago, these kinds meant different worlds,
    and a GUI application could not act like a console application,
    and vice versa. Nowaways, however, the distinction is mostly gone,
    and the application kind only affects defaults at program startup:
     
    - Console: A GUI application starts without console. However, it is 
      possible to allocate a console later. A console application always
      starts with a console which is created by the OS if missing.
    - Standard handles: For a GUI application, stdin/stdout/stderr are
      initially set to the invalid file handle. Nevertheless, this
      feature of standard handles exists, and one can set these handles
      later. Also, the handles can be inherited by the parent process.
      For console applications, the standard handles are normally set
      to the console, and applications can redirect them.
    - Main program: Of course, there is also the difference which C
      function is called at program startup - hey, but this is O'Caml!
    - Waiting for completion: It is uncommon to wait for the completion
      of GUI applications. The command interpreter seems to implement
      a magic so that it is not waited until the program is finished
      when a GUI application is started. For console applications this
      is of course done. (Note that this feature is the main reason
      why programmers still have to link applications as console
      applications, and cannot simply get the same effect from a
      application that is linked as GUI and then opens a console.)
 *)

type create_process_option =
  | CP_change_directory of string
      (** The initial working directory is set to this path. By default
          the new process starts with the current working directory of
          the caller.
       *)
  | CP_set_env of string
      (** The process environment is set to this encoded array of environment
          variables. By default the current environment is passed down
          to the new process.

          The string is created from an array of "name=value" settings
          by separating all elements by null bytes, and by putting two
          null bytes at the end.
       *)
  | CP_std_handles of Unix.file_descr * Unix.file_descr * Unix.file_descr
      (** Sets the standard handles of the new console process.
       *)
  | CP_create_console
      (** Creates a new console window. The standard handles of the new
          process may also be modified - however, the exact effect is not
          well documented by Microsoft. I have the impression that the logic
          is this: handles pointing to the parent console are replaced by
          handles pointing to the new console. Also, invalid handles
          are replaced by handles of the new console. It does not matter how
          the standard handles are passed down - either implicitly or by
          [CP_std_handles]. So you cannot create a new console, and
          keep standard handles that are connected to the old console.
          Best practice is to avoid the combination of [CP_std_handles] and
          [CP_create_console] when there is already a console.

          This flag does not have any effect when the started app is
          a GUI app.
       *)
  | CP_detach_from_console
      (** The new process detaches from the console at startup, even if it is
          a console application. Unless [CP_std_handles] is specified,
          the new process will initially not have standard handles (i.e.
          the standard handles are invalid handles)!
          GUI apps detach from the console anyway.
       *)
  | CP_inherit_console
      (** The new console process inherits the console from the caller, if
          present. Otherwise the new console process starts without console.
          For GUI apps there is not any effect: They do not have a console
          anyway.
       *)
  | CP_inherit_or_create_console
      (** If present, the console is inherited from the caller. If not
          present, a new console is created for console applications. 
          This mode is the default.
       *)
  | CP_unicode_environment
      (** Indicates that the environment is a Unicode environment *)
  | CP_ansi_environment
      (** Indicates that the environment is an ANSI environment. This
          is the default.
       *)
  | CP_new_process_group
      (** The new process is run in a new process group *)
  | CP_inherit_process_group
      (** The new process is run in the same process group as the caller.
          This is the default
       *)

val cp_set_env : string array -> create_process_option
  (** Returns the [CP_set_env] option for this array of environment
      variables (in the [Unix.environment] format)
   *)

val search_path : string option -> string -> string option -> string
  (** [search_path path_opt name ext_opt]: Uses the SearchPath function
      to locate a file. If [name] does not end with [ext_opt], this
      extension is added during the search. If [path_opt] is [None],
      the default search path is used.
   *)

type w32_process
  (** A handle to spawned processes *)

val create_process :
      string -> string -> create_process_option list -> 
        w32_process
  (** [create_process cmd cmdline options]: Spawns a new process that runs
      concurrently with the calling process. [cmd] is the command
      to execute (it is not searched by path, and the file suffix must be
      given). [cmdline] is the full command-line.

      If the exit code of the new process does not play any role, it is
      ok to just ignore the returned process handle (which will be
      automatically closed by a GC finalizer). 
   *)

val close_process : w32_process -> unit
  (** Closes the handle in the [w32_process] value, if it is still open *)

val get_process_status: w32_process -> Unix.process_status option
  (** Returns the process result if the process is finished, and [None]
      otherwise
   *)

val as_process_event : w32_process -> w32_event
  (** Casts the process handle to an event handle. The process handle
      is in signaled state as soon as the spawned process is terminated.
      The event handle can be used in [event_wait] (above) and 
      [wsa_wait_for_multiple_events] to wait for the termination of the
      process.
   *)

val emulated_pid : w32_process -> int
  (** Returns the MSVCRT.DLL notion of the process identifier (pid).
      This kind of pid is used in the [Unix] library to refer to
      processes, especially in [waitpid]. Note that the pid is actually
      a handle, and it must be closed by calling [Unix.waitpid].

      Each call of [emulated_pid] returns a new handle.
   *)

val win_pid : w32_process -> int
  (** Returns the Windows notion of the process identifier (pid) *)

val process_descr : w32_process -> Unix.file_descr
  (** Returns the proxy descriptor of the process *)

val terminate_process : w32_process -> unit
  (** Terminates the process *)




(** {1 Consoles} *)

val has_console : unit -> bool
  (** True if there is a console *)

val is_console : Unix.file_descr -> bool
  (** Tests whether the descriptor is the input or the output stream of the
      console.
   *)

val get_console_input : unit -> Unix.file_descr
  (** Get the input stream of the console. If there is no console yet,
      a new one is opened.

      The returned descriptor needs to be closed by the caller when done
      with it.
   *)

val get_console_output : unit -> Unix.file_descr
  (** Get the output stream of the console. If there is no console yet,
      a new one is opened

      The returned descriptor needs to be closed by the caller when done
      with it.
   *)


(** We use a simplified model of the console where only the visible part
    of the buffer is represented. All coordinates are relative to the
    visible part of the buffer.
 *)

type w32_console_attr =
    { mutable cursor_x : int; (** from 0 (leftmost) to [width-1] (rightmost) *)
      mutable cursor_y : int; (** from 0 (topmost) to [height-1] (bottommost) *)
      mutable cursor_size : int;    (** from 1 to 100 *)
      mutable cursor_visible : bool;
      mutable text_attr : int;
    }

type w32_console_info =
    {
      mutable width : int;    (** screen width of the console in chars *)
      mutable height : int;   (** screen height in lines *)
    }


val get_console_attr : unit -> w32_console_attr
val set_console_attr : w32_console_attr -> unit
  (** Get/set console attributes. *)

val get_console_info : unit -> w32_console_info
  (** Get r/o console info. *)

val fg_blue : int
val fg_green : int
val fg_red : int
val fg_intensity : int
val bg_blue : int
val bg_green : int
val bg_red : int
val bg_intensity : int  
  (** Bits of [text_attr] *)


type w32_console_mode = 
    { mutable enable_echo_input : bool;
      mutable enable_insert_mode : bool;
      mutable enable_line_input : bool;
      mutable enable_processed_input : bool;
      mutable enable_quick_edit_mode : bool;
      mutable enable_processed_output : bool;
      mutable enable_wrap_at_eol_output : bool;
    }
  (** See the msdn docs for GetConsoleMode for details *)

val get_console_mode : unit -> w32_console_mode
val set_console_mode : w32_console_mode -> unit
  (** Get/set the console mode. *)

val init_console_codepage : unit -> unit
  (** Sets the code page of the console to the ANSI code page of the
      system. Unfortunately, the console uses the OEM code page by default
      (e.g. code page 437 instead of 1252). This function changes the
      code page back to the ANSI version.

      Note, however, that the docs say: "If the current font is a
      raster font, SetConsoleOutputCP does not affect how extended characters
      are displayed." (grrmmpf) So you should also switch to a different
      font - otherwise you get input in the ANSI code page, and do output
      in the OEM code page.

      For Windows novices: Historically, there were two types of 8 bit
      character sets. The older type is an IBM code page, and predates
      the ISO-8859 series of character sets. This code page was used
      at MS-DOS times.  Microsoft calls this code page the "OEM" code
      page. Later, when ISO-8859 was created, Microsoft switched to
      code pages that are similar to this standard, but also do not
      fully match them. These newer code pages have names like
      "Windows-1252", and are now called ANSI code pages by Microsoft.
      The 8-bit versions of the Win32 calls (which are used by the
      Ocaml runtime)normally use the ANSI code page.
   *)

val clear_until_end_of_line : unit -> unit
  (** Writes a space character from the current cursor position to the
      end of the line
   *)

val clear_until_end_of_screen : unit -> unit
  (** Writes a space character from the current cursor position to the
      end of the screen
   *)

val clear_console : unit -> unit
  (** Clears the screen and the buffer, and sets the cursor to (0,0). *)


(** {1 Miscelleneous} *)

val get_active_code_page : unit -> int
  (** Get the active code page. See 
      http://msdn.microsoft.com/en-us/library/dd317756%28v=VS.85%29.aspx
      for a list of codes. Also see {!Netconversion.win32_code_pages}.
   *)


(** {1 Proxy Descriptors} *)

(** For a number of objects ([w32_event], [w32_pipe], and [w32_pipe_server]) 
    it is possible
    to obtain proxy descriptors. These have type [Unix.file_descr] and they
    contain a real file handle. The purpose of these descriptors is to
    be used as proxy objects that can be passed to functions expecting
    file descriptors as input. However, you cannot do anything with the
    proxies except looking the corresponding real objects up. Proxy
    descriptors are used in interfaces that only allow to pass
    [Unix.file_descr] values in and out.

    Proxy descriptors have to be closed by the caller once they have
    been handed out to the caller. Closing the proxy descriptor does not
    make the descriptor unusable (lookups still work), and the referenced 
    object is also 
    unaffected. It is up to the user when [Unix.close] is best called -
    it is even allowed to do it immediately after requesting the proxy
    descriptor, e.g. via [pipe_descr]. After closing the proxy, however,
    it is possible that the system generates another file descriptor
    that looks equal to the closed proxy. It is often best to close at the
    moment when one is really done with the proxy.
 *)

type w32_object =
    | W32_event of w32_event
    | W32_pipe of w32_pipe
    | W32_pipe_server of w32_pipe_server
    | W32_process of w32_process
    | W32_input_thread of w32_input_thread
    | W32_output_thread of w32_output_thread

val lookup : Unix.file_descr -> w32_object
  (** Returns the real object behind a proxy descriptor, or raises
      [Not_found]. Note that the returned object needs not to be physically
      identical to the original object. It behaves, however, exactly the
      same way.
   *)

val lookup_event : Unix.file_descr -> w32_event
val lookup_pipe : Unix.file_descr -> w32_pipe
val lookup_pipe_server : Unix.file_descr -> w32_pipe_server
val lookup_process : Unix.file_descr -> w32_process
val lookup_input_thread : Unix.file_descr -> w32_input_thread
val lookup_output_thread : Unix.file_descr -> w32_output_thread
  (** Returns the real object. If not found, or if the object is of unexpected
      type, [Failure] is raised.
   *)

val unregister : Unix.file_descr -> unit
  (** Removes this descriptor from the lookup table. This should only be done
      after it is closed. Calling [unregister] is optional, and the removal
      will take place anyway when the descriptor is collected by the GC.
   *)


(** {1 Miscelleneous} *)

val test_close_on_exec : Unix.file_descr -> bool
  (** Tests whether the handle is not inheritable *)

val modify_close_on_exec : Unix.file_descr -> bool -> unit
  (** Sets the close-on-exec flag, i.e. whether the handle is not inheritable.
      Note that [Unix.set_close_on_exec] and [Unix.clear_close_on_exec]
      have a serious problem, and do not always work.
   *)

val is_crt_fd : Unix.file_descr -> int -> bool
  (** Tests whether the descriptor has a certain CRT counterpart.
      E.g. use [is_crt_fd 0] to check whether [fd] is [Unix.stdin]
      (physically)
   *)

val fill_random : string -> unit
  (** Fills the string with random bytes. A cryptographically secure RNG
      is used
   *)


(** {1 Debugging} *)

module Debug : sig
  val enable : bool ref
    (** Enables {!Netlog}-style debugging of the Ocaml wrapper *)

  val debug_c_wrapper : bool -> unit
    (** Sets whether to debug the C wrapper part. The debug messages are
        simply written to stderr
     *)
end


(**/**)

val input_thread_descr : w32_input_thread -> Unix.file_descr
val output_thread_descr : w32_output_thread -> Unix.file_descr
  (* These functions return the I/O descriptor. User code must never
     use these descriptors!
   *)
val gc_proxy : unit -> unit
  (* testing only *)
