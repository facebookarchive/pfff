(* $Id: netfs.mli 1520 2010-12-22 23:52:22Z gerd $ *)

(** Class type [stream_fs] for filesystems with stream access to files *)

(** The class type {!Netfs.stream_fs}
    is an abstraction for both kernel-level and user-level
    filesystems. It is used as parameter for algorithms (like globbing)
    that operate on filesystems but do not want to assume any particular
    filesystem. Only stream access is provided (no seek).

    {b File paths:}

    The filesystem supports hierarchical file names. File paths use
    Unix conventions, i.e.
    - [/] is the root
    - Path components are separated by slashes. Several consecutive slashes
      are allowed but mean the same as a single slash.
    - [.] is the same directory
    - [..] is the parent directory

    All paths need to be absolute (i.e. start with [/]).

    There can be additional constraints on paths:
    - Character encoding restriction: A certain ASCII-compatible character
      encoding is assumed (including UTF-8)  
    - Character exclusion: Certain characters may be excluded

    Implementations may impose more constraints that cannot be expressed
    here (case insensitivity, path length, exclusion of special names etc.).

    {b Virtuality:}

    There is no assumption that [/] is the real root of the local filesystem.
    It can actually be anywhere - a local subdirectory, or a remote directory,
    or a fictive root. There needs not to be any protection against "running
    beyond root", e.g. with the path [/..].

    This class type also supports remote filesystems, and thus there is no
    concept of file handle (because this would exclude a number of
    implementations).

    {b Errors: }

    Errors should generally be indicated by raising [Unix_error]. For
    many error codes the interpretation is already given by POSIX. Here
    are some more special cases:
    - [EINVAL]: should also be used for invalid paths, or when a flag
      cannot be supported (and it is non-ignorable)
    - [ENOSYS]: should also be used if an operation is generally unavailable

    In case of hard errors (like socket errors when communicating with the
    remote server) there is no need to stick to [Unix_error], though.

    {b Subtyping:}

    The class type {!Netfs.stream_fs} is subtypable, and subtypes can add
    more features by:
    - adding more methods
    - adding more flags to existing methods

    {b Omitted:}

    Real filesystems usually provide a lot more features than what is
    represented here, such as:
    - Access control and file permissions
    - Metadata like timestamps
    - Random access to files

    This definition here is intentionally minimalistic. In the future
    more extended versions of this class type may be defined that
    cover more common filesystem features.
 *)

(** {2 The class type [stream_fs]} *)

type read_flag =
    [ `Skip of int64 | `Binary | `Streaming | `Dummy ]

type write_flag =
    [ `Create | `Exclusive | `Truncate | `Binary | `Streaming | `Dummy ]

type size_flag =
    [ `Dummy ]

type test_flag =
    [ `Link | `Dummy ]

type remove_flag =
    [ `Recursive | `Dummy ]

type rename_flag =
    [ `Dummy ]

type symlink_flag =
    [ `Dummy ]

type readdir_flag =
    [ `Dummy ]

type readlink_flag =
    [ `Dummy ]

type mkdir_flag =
    [ `Path | `Nonexcl | `Dummy ]

type rmdir_flag =
    [ `Dummy ]

type copy_flag =
    [ `Dummy ]

(** Note [`Dummy]: this flag is always ignored. There are two reasons
    for having it:
    - Ocaml does not allow empty variants
    - it is sometimes convenient to have it
      (e.g. in: [if <condition> then `Create else `Dummy])
 *)

type test_type =
    [ `N | `E | `D | `F | `H | `R | `W | `X | `S ]
     (** Tests:
         - [`N]: the file name exists
         - [`E]: the file exists
	 - [`D]: the file exists and is a directory
	 - [`F]: the file exists and is regular
	 - [`H]: the file exists and is a symlink (possibly to a non-existing
           target)
	 - [`R]: the file exists and is readable
	 - [`W]: the file exists and is writable
	 - [`X]: the file exists and is executable
	 - [`S]: the file exists and is non-empty
      *)

class type stream_fs =
object
  method path_encoding : Netconversion.encoding option
    (** The encoding must be ASCII-compatible
	({!Netconversion.is_ascii_compatible}). If [None] the
	ASCII encoding is assumed for codes 0-127, and no meaning is
	defined for byte codes 128-255.
     *)

  method path_exclusions : (int * int) list
    (** Code points that must not occur in path components between slashes.
	This is given as ranges [(from,to)]. The code points are interpreted
	as Unicode code points if an encoding is available, and as byte codes
	otherwise. For example, for Unix the code points 0 and 47 (slash)
	are normally the only excluded code points.
     *)

  method nominal_dot_dot : bool
    (** Whether the effect of [..] can be obtained by stripping off the
	last path component, i.e. whether
	[Filename.dirname path <=> path ^ "/.."]
     *)

  method read : read_flag list -> string -> Netchannels.in_obj_channel
    (** [read flags filename]: Opens the file [filename] for reading,
	and returns the input stream. Flags:
	- [`Skip n]: Skips the first [n] bytes of the file. On many
	  filesystems this is more efficient than reading [n] bytes and
	  dropping them; however, there is no guarantee that this 
	  optimization exists.
	- [`Binary]: Opens the file in binary mode (if there is such
	  a distinction)
        - [`Streaming] for network filesystems: If possible, open the
	  file in streaming mode, and avoid to copy the whole file to the local
	  disk before returning the {!Netchannels.in_obj_channel}.
	  Streaming mode is faster, but has also downsides. Especially,
	  the implementation of [read] can do less to recover from
	  transient network problems (like retrying the whole download).
          Support for this flag is optional, and it is ignored if
	  there is no extra streaming mode.
     *)

  method write : write_flag list -> string -> Netchannels.out_obj_channel
    (** [write flags filename]: Opens (and optionally creates) the [filename]
	for writing, and returns the output stream. Flags:
	- [`Create]: If the file does not exist, create it
	- [`Truncate]: If the file exists, truncate it to zero before
	  writing
	- [`Exclusive]: The [`Create] is done exclusively
	- [`Binary]: Opens the file in binary mode (if there is such
	  a distinction)
	- [`Streaming]: see [read] (above) for explanations

	Some filesystems refuse this operation if neither [`Create] nor
	[`Truncate] is specified because overwriting an existing file
	is not supported. There are also filesystems that cannot even
	modify files by truncating them first, but only allow to write
	to new files.

	It is unspecified whether the file appears in the directory directly
	after calling [write] or first when the stream is closed.
     *)

  method size : size_flag list -> string -> int64
    (** Returns the size of a file *)

  method test : test_flag list -> string -> test_type -> bool
    (** Returns whether the test is true. For filesystems that know
	symbolic links, the test operation normally follows symlinks
	(except for the [`N] and [`H] tests). By specifying the [`Link] flag
	symlinks are not followed.
     *)

  method test_list : test_flag list -> string -> test_type list -> bool list
    (** Similar to [test] but this function performs all tests in the list
	at once, and returns a bool for each test.
     *)

  method remove : remove_flag list -> string -> unit
    (** Removes the file or symlink. Implementation are free to also
	support the removal of empty directories.
        
        Flags:
	- [`Recursive]: Remove the contents of the non-empty directory
	  recursively. This is an optional feature. There needs not to
	  be any protection against operations done by other processes
	  that affect the directory tree being deleted.
     *)


  method rename : rename_flag list -> string -> string -> unit
    (** Renames the file. There is no guarantee that a rename is atomic
     *)

  method symlink : symlink_flag list -> string -> string -> unit
    (** [symlink flags oldpath newpath]: Creates a symlink. This
	is an exclusive create, i.e. the operation fails if [newpath]
	already exists.
     *)

  method readdir : readdir_flag list -> string -> string list
    (** Reads the contents of a directory. Whether "." and ".." are returned
	is platform-dependent. The entries can be returned in any order.
     *)

  method readlink : readlink_flag list -> string -> string
    (** Reads the target of a symlink *)

  method mkdir : mkdir_flag list -> string -> unit
    (** Creates a new directory. Flags:
	- [`Path]: Creates missing parent directories. This is an
	  optional feature. (If not supported, ENOENT is reported.)
	- [`Nonexcl]: Non-exclusive create.
     *)

  method rmdir : rmdir_flag list -> string -> unit
    (** Removes an empty directory *)

  method copy : copy_flag list -> string -> string -> unit
    (** Copies a file to a new name. This does not descent into directories.
	Also, symlinks are resolved, and the linked file is copied.
     *)
end


val local_fs : ?encoding:Netconversion.encoding -> ?root:string -> 
               unit -> stream_fs
  (** [local_fs()]: Returns a filesystem object for the local filesystem.

      - [encoding]: Specifies the character encoding of paths. The default
        is system-dependent.
      - [root]: the root of the returned object is the directory [root]
        of the local filesystem.
   *)

(** {2 OS Notes} *)

(** {b Unix} in general: There is no notion of character encoding of
    paths. Paths are just bytes. Because of this, the default encoding
    is [None]. If a different encoding is passed to [local_fs], these
    bytes are just interpreted in this encoding. There is no conversion.

    For desktop programs, though, usually the character encoding of the
    locale is taken for filenames. You can get this by passing

    {[
    let encoding = 
      Netconversion.user_encoding()
    ]}

    as [encoding] argument.
 *)

(** {b Windows}: If the [root] argument is {b not} passed to [local_fs]
    it is possible to access the whole filesystem:

    - Paths starting with drive letters like [c:/] are also considered
      as absolute
    - Additionally, paths starting with slashes like [/c:/] mean the same
    - UNC paths starting with two slashes like [//hostname] are supported

    However, when a [root] directory is passed, these additional
    notations are not possible anymore - paths must start with [/],
    and there is neither support for drive letters nor for UNC paths.

    The [encoding] arg defaults to current ANSI codepage, 
    and it is
    not supported to request a different encoding. (The difficulty is
    that the Win32 bindings of the relevant OS functions always assume
    the ANSI encoding.)

    There is no support for backslashes as path separators (such paths
    will be rejected), for better compatibility with other platforms.
 *)


(** {2 Other impementations of [stream_fs]} *)

(** List:

    - {!Http_fs} allows one to access HTTP-based filesystems
    - {!Shell_fs} allows one to access filesystems by executing shell
      commands. This works locally and via ssh.

 *)


(** {2 Algorithms} *)

val copy : ?replace:bool -> ?streaming:bool ->
           stream_fs -> string -> stream_fs -> string -> unit
  (** [copy orig_fs orig_name dest_fs dest_name]: Copies the file [orig_name]
      from [orig_fs] to the file [dest_name] in [dest_fs]. By default,
      the destination file is truncated and overwritten if it already
      exists.

      If [orig_fs] and [dest_fs] are the same object, the [copy] method
      is called to perform the operation. Otherwise, the data is read
      chunk by chunk from the file in [orig_fs] and then written to
      the destination file in [dest_fs].

      Symlinks are resolved, and the linked file is copied, not the
      link as such.

      The copy does not preserve ownerships, file permissions, or
      timestamps. (The [stream_fs] object does not represent these.)
      There is no protection against copying an object to itself.

      - [replace]: If set, the destination file is removed and created again
        if it already exists
      - [streaming]: use streaming mode for reading and writing files
   *)

val copy_into : ?replace:bool -> ?subst:(int->string) -> ?streaming:bool ->
                stream_fs -> string -> stream_fs -> string -> 
                  unit
  (** [copy_into orig_fs orig_name dest_fs dest_name]: 
      Like [copy], but this version also supports recursive copies. The
      [dest_name] must be an existing directory, and the file or tree at
      [orig_name] is copied into it.

      Symlinks are copied as symlinks.

      If [replace] and the destination file/directory already exists,
      it is deleted before doing the copy.

      - [subst]: See {!Netfs.convert_path}
      - [streaming]: use streaming mode for reading and writing files
   *)

type file_kind = [ `Regular | `Directory | `Symlink | `Other | `None ]

val iter : pre:(string -> file_kind -> file_kind -> unit) -> 
           ?post:(string -> unit) ->
            stream_fs -> string -> unit
  (** [iter pre fs start]: Iterates over the file hierarchy at [start].
      The function [pre] is called for every filename. The filenames
      passed to [pre] are relative to [start]. The [start] must
      be a directory.

      For directories, the [pre] function is called for the directory
      before it is called for the members of the directories.
      The function [post] can additionally be passed. It is only called
      for directories, but after the members.

      [pre] is called as [pre rk lk] where [rk] is the file kind after
      following symlinks and [lk] the file kind without following symlinks
      (the link itself).

      Example: [iter pre fs "/foo"] would call
      - [pre "dir" `Directory `Directory] (meaning the directory "/foo/dir")
      - [pre "dir/file1" `File `File]
      - [pre "dir/file2" `File `Symlink]
      - [post "dir"]

      Note: symlinks to non-existing files are reported as
      [pre name `None `Symlink].

   *)

val convert_path : ?subst:(int -> string) ->
                   stream_fs -> stream_fs -> string -> string
  (** [convert_path oldfs newfs oldpath]: The encoding of [oldpath]
      (which is assumed to reside in [oldfs]) is converted to the encoding
      of [newfs] and returned.

      It is possible that the conversion is not possible, and
      the function [subst] is then called with the problematic code point as
      argument (in the encoding of [oldfs]). The default [subst] function
      just raises {!Netconversion.Cannot_represent}.

      If one of the filesystem objects does not specify an encoding,
      the file name is not converted, but simply returned as-is. This
      may result in errors when [newfs] has an encoding while [oldfs]
      does not have one because the file name might use byte representations
      that are illegal in [newfs]. 
   *)
