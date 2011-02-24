(* $Id: netsys_pollset_generic.mli 1219 2009-04-14 13:28:56Z ChriS $ *)

open Netsys_pollset

val standard_pollset : unit -> pollset
  (** Returns a good standard implementation of pollset for this platform.
      It will be good for cases where only a small to medium number of
      descriptors is watched (several hundred should be ok). It is no problem
      when the pollset lives only for a short time. Also, it is not strictly
      necessary to call the [dispose] method after use.

      On POSIX platforms, this returns a
      {!Netsys_pollset_posix.poll_based_pollset}.

      On Win32, this returns {!Netsys_pollset_win32.pollset} in single-threaded
      programs, and a {!Netsys_pollset_win32.threaded_pollset} in
      multi-threaded programs. Note that Win32 restricts the number of
      descriptors that can be watched per thread to 64. The [threaded_pollset]
      version overcomes this limit, but it is only available if your 
      program is compiled for multi-threading. Because of this, it may be
      useful to enable multi-threading for your program even when your code
      does not make use of it otherwise.
   *)

val performance_pollset : unit -> pollset
  (** Returns the best available pollset on this platform for high performance
      servers that have to deal with many different file descriptors. Note
      that this pollset may have higher initial costs, and may not be 
      suitable for cases where pollsets live only for a short time. Also,
      make sure the [dispose] method is called after use.

      Currently, this is the same as [standard_pollset], because no better
      implementations exist. In the future, [performance_pollset] will use
      kernel-based polling on some OS ("epoll", "kqueue", "/dev/poll", etc.)
 *)

val select_emulation : 
      pollset ->
      Unix.file_descr list ->
      Unix.file_descr list ->
      Unix.file_descr list ->
      float ->
        (Unix.file_descr list * Unix.file_descr list * Unix.file_descr list)
  (** [let select = select_emulation pset]: Returns a function with the
      same meaning as [Unix.select]. It is emulated on top of [pset].

      Using the function is only recommended when the lists of file
      descriptors are short. Long lists impose a big performance penalty.

      Unlike [Unix.select] the descriptors may be returned in any order.
   *)
