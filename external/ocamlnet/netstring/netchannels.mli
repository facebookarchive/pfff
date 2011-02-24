(* $Id: netchannels.mli 1519 2010-12-20 04:19:42Z gerd $
 * ----------------------------------------------------------------------
 *
 *)


(** Object-oriented I/O: Basic types and classes 
 *
 * {b Contents}
 * 
 * - {!Netchannels.types}
 * - {!Netchannels.input}
 * - {!Netchannels.output}
 * - {!Netchannels.delegation}
 * - {!Netchannels.lifting}
 * - {!Netchannels.descriptors}
 * - {!Netchannels.transactional}
 * - {!Netchannels.filters}
 *   {ul {- {!Netchannels.filters_notes}}}
 *
 * The tutorial has been moved to {!Netchannels_tut}.
 *)

(** {1:types Types} *)

(* ***************************** Types ******************************** *)

(** There are three levels of class types for channels:
 *  - [rec_in_channel] and [rec_out_channel]: Primitive, but standardized level
 *  - [raw_in_channel] and [raw_out_channel]: Unix level
 *  - [in_obj_channel] and [out_obj_channel]: Application level
 *
 * The "rec" level has been recently introduced to improve interoperability
 * with other libraries (e.g. camomile). The idea is to standardize the
 * real core methods of I/O, so they have the same meaning in all libraries.
 * Read
 * "{{:http://www.ocaml-programming.de/rec/IO-Classes.html}Basic I/O class types}"
 * for more.
 *
 * The "raw" level represents the level of Unix file descriptors.
 *
 * The application level is what should be used in programs. In addition
 * to the "raw" level one can find a number of convenience methods,
 * e.g. [input_line] to read a line from the channel. The downside is that
 * these methods usually work only for blocking I/O.
 *
 * One can lower the level by coercion, e.g. to turn an [in_obj_channel]
 * into a [rec_in_channel], apply the function
 *
 * [(fun ch -> (ch : in_obj_channel :> rec_in_channel))]
 *
 * To higher the level, apply [lift_in] or [lift_out], defined below.
 *)

(** {b Interface changes:} Since ocamlnet-0.98, the semantics of
 * the methods [input] and [output] has slightly changed. When the end
 * of the channel is reached, [input] raises now [End_of_file]. In previous
 * releases of ocamlnet, the value 0 was returned. When the channel cannot
 * process data, but is in non-blocking mode, both methods now return the
 * value 0. In previous releases of ocamlnet, the behaviour was not
 * defined.
 *
 * {b Ocamlnet-3.0} changed the behavior of [close_out]. Errors are no longer
 * reported - instead, the exception is logged to {!Netlog}. For a stricter
 * error handling, it is suggested to call [flush] first. Also, [close_in]
 * and [close_out] no longer raise [Closed_channel] when the channel is
 * already closed. Read more about this in the section
 * {!Netchannels.rec_out_channel.close_error}.
 *)

exception Closed_channel
  (** Raised when channel operations are called when the channel is closed *)

exception Buffer_underrun
  (** Raised by input methods if the internal buffer of the channel is too
   * empty to read even one byte of data.
   * This exception is only used by certain implementations of channel
   * classes.
   *)

exception Command_failure of Unix.process_status
  (** Raised by [close_in] or [close_out] if the channel is connected with
   * another process, and the execution of that process fails.
   *)

(** Recommended input class type for library interoperability. *)
class type rec_in_channel = object
  (** Description
   * 
   * This class type is defined in 
   * "{{:http://www.ocaml-programming.de/rec/IO-Classes.html}Basic I/O class types}"
   * as collaborative effort of several library creators.
   *)

  method input : string -> int -> int -> int
    (** Reads octets from the channel and puts them into the string. The
     * first [int] argument is the position of the substring, and the second
     * [int] argument is the length of the substring where the data are
     * stored. The method returns the number of octets actually read and
     * stored.
     *
     * When the end of the channel is reached and there is no further octet
     * to read, the exception [End_of_file] will be raised. {b This has
     * been changed in ocamlnet-0.97! In previous releases the number 0 
     * was returned at the end of the channel.}
     *
     * When the channel is non-blocking, and there are currently no bytes
     * to read, the number 0 will be returned. {b This has
     * been changed in ocamlnet-0.97! In previous releases this behaviour
     * was undefined.}
     *
     * When the channel is closed, the exception [Closed_channel] will be
     * raised if an ocamlnet implementation is used. For implementations
     * of other libraries there is no standard for this case.
     *)

  method close_in : unit -> unit
    (** Closes the channel for input.
     *
     * When the channel is already closed, this is a no-op.
     *
     * Error policy: Exceptions are only raised in cases of serious
     * corruption, e.g. if the underlying descriptor is invalid.
     *)
end

(** Basic Unix-level class type for input channels as used by ocamlnet. In addition
   * to the recommended standard, ocamlnet always support a position counter
   *)
class type raw_in_channel = object
  inherit rec_in_channel
  method pos_in : int
    (** Returns the current channel position. This position can be expected
     * to be consistent with the returned number of bytes of [input], i.e.
     * when [input] returns [n], the position is advanced by [n].
     *
     * As seek operations are outside the scope of [Netchannels], 
     * implementations may or may not take seek operations into account.
     *)
end

(** Recommended output class type for library interoperability. *)
class type rec_out_channel = object
  (** Description
   *
   * This class type is defined in 
   * "{{:http://www.ocaml-programming.de/rec/IO-Classes.html}Basic I/O class types}"
   * as collaborative effort of several library creators.
   *)

  method output : string -> int -> int -> int
    (** Takes octets from the string and writes them into the channel. The
     * first [int] argument is the position of the substring, and the second
     * [int] argument is the length of the substring where the data can
     * be found. The method returns the number of octets actually written.
     *
     * The implementation may choose to collect written octets in a buffer
     * before they actually delivered to the underlying resource. 
     *
     * When the channel is non-blocking, and there are currently no bytes
     * to write, the number 0 will be returned. {b This has
     * been changed in ocamlnet-0.97! In previous releases this behaviour
     * was undefined.}
     *
     * When the channel is closed, the exception [Closed_channel] will be
     * raised if an ocamlnet implementation is used. For implementations
     * of other libraries there is no standard for this case.
     *)
  method flush : unit -> unit
    (** If there is a write buffer, it will be flushed. Otherwise, nothing
     * happens.
     *)
  method close_out : unit -> unit
    (** Flushes the buffer, if any, and closes the channel for output.
     *
     * When the channel is already closed, this is a no-op.
     *)

 (** {2:close_error How to close channels in case of errors}

     The [close_out] method has actually two tasks: First, it writes out
     all remaining data (like [flush]), and second, it releases OS
     resources (e.g. closes file descriptors). There is the question
     what has to happen when the write part fails - is the resource released
     anyway or not?

     We choose here a pragmatic approach under the assumption that
     an OS error at close time is usually unrecoverable, and it is
     more important to release the OS resource. Also, we
     assume that the user is wise enough to call [flush] first if
     it is essential to know write errors at close time. Under these
     assumptions:

     - The [flush] method fully reports any errors when writing out
       the remaining data.
     - When [flush] raises an error exception, it should discard
       any data in the buffer. This is not obligatory, however,
       but considered good practice, and is subject to discussion.
     - The [close_out] method usually does not report errors by
       raising exceptions, but only by logging them via {!Netlog}.
       The OS resource is released in any case. As before, this
       behavior is not obligatory, but considered as good practice,
       and subject to discussion.

     This ensures that the following code snippet reports all errors, but also
     releases OS resources:
     
     {[
       try 
         ch # flush();
         ch # close_out();
       with error -> 
          ch # close_out(); raise error
     ]}

     There are some cases where data can be first written when it is
     known that the channel is closed. These data would not be written
     by a preceding [flush]. In such cases:

     - The best way to deal with it is to define another method,
       e.g. called [write_eof], that marks the data as logically 
       being complete, so a following [flush] can do the complete
       shutdown cycle of the channel.
     - At least, however, one should allow then that a double
       [close_out] releases the descriptor: the first [close_out]
       will report the error condition as exception, but discard
       all data in the channel. The second [close_out] finally
       releases the OS resource.

     In any way, hard errors indicating bugs of the program logic
     (like invalid file descriptors) should always be immediately
     reported.
  *)
end

(** Basic Unix-level class type for output channels as used by ocamlnet. In addition
 * to the recommended standard, ocamlnet always support a position counter
 *)
class type raw_out_channel = object
  inherit rec_out_channel
  method pos_out : int
    (** Returns the current channel position. This position can be expected
     * to be consistent with the returned number of bytes of [output], i.e.
     * when [output] returns [n], the position is advanced by [n].
     *
     * As seek operations are outside the scope of [Netchannels], 
     * implementations may or may not take seek operations into account.
     *)
end

(** A channel supporting both input and output. The input and output
 * aspects are strictly separated
 *)
class type raw_io_channel = object
  inherit raw_in_channel
  inherit raw_out_channel
end

(** Further methods usually supported by ocamlnet channel implementations.
 * These methods are only reasonable when the channel is of blocking type,
 * i.e. waits for input when not enough data are available to perform an
 * operation. Implementations may choose to fail when they detect the
 * channel is non-blocking.
 *)
class type compl_in_channel = object

  method really_input : string -> int -> int -> unit
    (** Reads exactly as many octets from the channel as the second [int]
     * argument specifies. The octets are placed at the position denoted
     * by the first [int] argument into the string.
     *
     * When the end of the channel is reached before the passed number of
     * octets are read, the exception [End_of_file] is raised.
     *)

  method input_char : unit -> char
    (** Reads exactly one character from the channel, or raises [End_of_file]
     *)

  method input_line : unit -> string
    (** Reads the next line from the channel. When the channel is already
     * at the end before [input_line] is called, the exception [End_of_file]
     * is raised.
     *)

  method input_byte : unit -> int
    (** Reads exactly one octet from the channel and returns its code, 
     * or raises [End_of_file]
     *)

end

(** The application-level input channel supports raw and complemented methods *)
class type in_obj_channel = object
  inherit raw_in_channel
  inherit compl_in_channel
end

(** Further methods usually supported by ocamlnet channel implementations.
 * These methods are only reasonable when the channel is of blocking type,
 * i.e. waits for output readiness when the underlying resource currently
 * cannot process enough data. Implementations may choose to fail when they
 * detect the channel is non-blocking.
 *)
class type compl_out_channel = object
  method really_output : string -> int -> int -> unit
    (** Writes exactly as many octets to the channel as the second [int]
     * argument specifies. The octets are taken from the string position 
     * denoted by the first [int] argument.
     *)
  method output_char : char -> unit
    (** Writes exactly one character *)
  method output_string : string -> unit
    (** Writes exactly the passed string *)
  method output_byte : int -> unit
    (** Writes exactly one byte passed as integer code *)
  method output_buffer : Buffer.t -> unit
    (** Writes exactly the contents of the buffer *)
  method output_channel : ?len:int -> in_obj_channel -> unit
    (** Writes the contents of an [in_obj_channel] until the end of the
     * input channel is reached.
     *
     * @param len If passed, at most this number of octets are read from
     * the input channel and written to this channel.
     *)
end


(** The application-level output channel supports raw and complemented methods *)
class type out_obj_channel = object
  inherit raw_out_channel
  inherit compl_out_channel
end


(** A channel supporting both input and output. The input and output
 * aspects are strictly separated
 *)
class type io_obj_channel = object
  inherit in_obj_channel
  inherit out_obj_channel
end


(** A transactional output channel has a buffer for uncommitted data.
 * This means that all data written to this channel is collected in the
 * buffer until either [commit_work] or [rollback_work] is called.
 *
 * When the channel is closed, the buffer may optionally be committed.
 * This is implementation-defined.
 *
 * The method [flush] does not have any effect on the transaction
 * buffer.
 *)
class type trans_out_obj_channel = object

  inherit out_obj_channel

  method commit_work : unit -> unit
    (** Flushes the transaction buffer, and writes its contents to the
     * underlying resource.
     *)

  method rollback_work : unit -> unit
    (** Empties the transaction buffer *)
end


(* ***************************** Input channels *********************** *)

(** {1:input Input channels} *)

class input_channel :
  in_channel ->
    in_obj_channel
  (** Creates an input channel from an [in_channel], which must be open.
   *
   * The method [pos_in] reflects the real position in the channel as
   * returned by [Pervasives.pos_in]. This works for both seekable and
   * non-seekable channels.
   *
   * The method [close_in] also closes the underlying [in_channel].
   *)


class input_command : 
  string ->
    in_obj_channel
  (** Runs the command with [/bin/sh], and reads the data the command prints
   * to stdout. 
   *
   * The method [pos_in] returns the number of read octets.
   *
   * When [close_in] is invoked, the subprocess is [wait]ed for. If the
   * process exits with code 0, the method returns normally. Otherwise,
   * the exception [Command_failure] is raised.
   *)


class input_string :
  ?pos:int -> ?len:int -> string ->
    in_obj_channel
  (** Creates an input channel from a (constant) string. 
   *
   * The method [pos_in] reflects the real position in the string, i.e.
   * a character read at position [k] can be found at [s.[k]] in the string
   * [s].
   *
   * @param pos The data of the channel begins at this position of the string.
   *   Default: 0
   * @param len The data of the channel consists of this number of bytes.
   *   Default: until the end of the string
   *)


val create_input_netbuffer :
  Netbuffer.t ->
    in_obj_channel   *   (* shutdown: *) (unit -> unit)
  (** Creates an input channel and a shutdown function for a netbuffer. 
   * This is a destructive
   * implementation: Every time data is read, the octets are taken from the
   * beginning of the netbuffer, and they are deleted from the netbuffer
   * (recall that a netbuffer works like a queue of characters).
   *
   * Conversely, the user of this class may add new data to the netbuffer 
   * at any time. When the shutdown function is called, the EOF condition
   * is recorded, and no further data must be added.
   *
   * If the netbuffer becomes empty, the input methods raise [Buffer_underrun]
   * when the EOF condition has not yet been set, and they raise
   * [End_of_file] when the EOF condition has been recorded.
   *)

val lexbuf_of_in_obj_channel : in_obj_channel -> Lexing.lexbuf
  (** Creates a lexical buffer from an input channel. The input channel
   * is not closed when the end is reached
   *
   * This function does not work for non-blocking channels.
   *)

val string_of_in_obj_channel : in_obj_channel -> string
  (** Reads from the input channel until EOF and returns the characters
   * as string. The input channel is not closed.
   *
   * This function does not work for non-blocking channels.
   *)

val lines_of_in_obj_channel : in_obj_channel -> string list
  (** Reads from the input channel until EOF and returns the lines
   * as string list. The input channel is not closed.
   *
   * This function does not work for non-blocking channels.
   *)

val with_in_obj_channel : 
  (#in_obj_channel as 'a) -> ('a -> 'b) -> 'b
  (** [with_in_obj_channel ch f]:
   * Computes [f ch] and closes [ch]. If an exception happens, the channel is
   * closed, too.
   *)


(* *************************** Output channels ************************ *)

(** {1:output Output channels} *)

class output_channel :
  ?onclose:(unit -> unit) ->              (* default: fun _ -> () *)
  out_channel ->
    out_obj_channel
  (** Creates an output channel writing into an [out_channel].
   *
   * The method [pos_out] reflects the real position in the channel as
   * returned by [Pervasives.pos_out]. This works for both seekable and
   * non-seekable channels.
   *
   * The method [close_out] also closes the underlying [out_channel].
   * There is some implicit logic to either use [close_out] or [close_out_noerr]
   * depending on whether the immediately preceding operation already reported
   * an error.
   *
   * @param onclose this function is called when the [close_out] method is
   * invoked, just after the underlying [out_channel] has been closed.
   *)


class output_command : 
  ?onclose:(unit -> unit) ->              (* default: fun _ -> () *)
  string ->
    out_obj_channel
  (** Runs the command with [/bin/sh], and data written to the channel is
   * piped to stdin of the command.
   *
   * The method [pos_out] returns the number of written octets.
   *
   * When [close_out] is invoked, the subprocess is [wait]ed for. If the
   * process exits with code 0, the method returns normally. Otherwise,
   * the exception [Command_failure] is raised. (The channel is closed
   * even if this exception is raised.)
   *
   * @param onclose this function is called when the [close_out] method is
   * invoked, just after the underlying descriptor has been closed.
   *)


class output_buffer :
  ?onclose:(unit -> unit) ->              (* default: fun _ -> () *)
  Buffer.t ->
    out_obj_channel
  (** This output channel writes the data into the passed buffer.
   *
   * The method [pos_out] returns the number of written octets.
   *
   * @param onclose this function is called when the [close_out] method is
   * invoked, just after the underlying descriptor has been closed.
   *)

class output_netbuffer :
  ?onclose:(unit -> unit) ->              (* default: fun _ -> () *)
  Netbuffer.t ->
    out_obj_channel
  (** This output channel writes the data into the passed netbuffer.
   *
   * The method [pos_out] returns the number of written octets.
   *
   * @param onclose this function is called when the [close_out] method is
   * invoked, just after the underlying descriptor has been closed.
   *)

class output_null :
  ?onclose:(unit -> unit) ->              (* default: fun _ -> () *)
  unit ->
    out_obj_channel
  (** This output channel discards all written data. 
   *
   * The method [pos_out] returns the number of discarded bytes.
   *
   * @param onclose this function is called when the [close_out] method is
   * invoked, just after the underlying descriptor has been closed.
   *)

val with_out_obj_channel : 
  (#out_obj_channel as 'a) -> ('a -> 'b) -> 'b
  (** [with_out_obj_channel ch f]:
   * Computes [f ch] and closes [ch]. If an exception happens, the channel is
   * closed, too.
   *)


(* ********************* Delegation *********************************** *)

(** {1:delegation Delegation classes} *)

(** Delegation classes just forward method calls to an parameter
 * object, i.e. when method [m] of the delegation class is called,
 * the definition of [m] is just to call the method with the same
 * name [m] of the parameter object. This is very useful in order
 * to redefine methods individually.
 *
 * For example, to redefine the method [pos_in] of an [in_obj_channel],
 * use
 * {[
 * class my_channel = object(self)
 *   inherit in_obj_channel_delegation ...
 *   method pos_in = ...
 * end
 * ]}
 *
 * As a special feature, the following delegation classes can suppress
 * the delegation of [close_in] or [close_out], whatever applies.
 * Just pass [close:false] to get this effect, e.g.
 * {[
 * class input_channel_don't_close c =
 *   in_obj_channel_delegation ~close:false (new input_channel c)
 * ]}
 * This class does not close [c : in_channel] when the [close_in]
 * method is called.
 *)

class rec_in_channel_delegation : ?close:bool -> rec_in_channel ->
  rec_in_channel

class raw_in_channel_delegation : ?close:bool -> raw_in_channel ->
  raw_in_channel

class in_obj_channel_delegation : ?close:bool -> in_obj_channel ->
  in_obj_channel

class rec_out_channel_delegation : ?close:bool -> rec_out_channel ->
  rec_out_channel

class raw_out_channel_delegation : ?close:bool -> raw_out_channel ->
  raw_out_channel

class out_obj_channel_delegation : ?close:bool -> out_obj_channel ->
  out_obj_channel


(* ********************* Raw channels ********************************* *)

(** {1:lifting Lifting channels} *)

(** The following classes and functions add missing methods to reach
 * a higher level in the hierarchy of channel class types. For most
 * uses, the [lift_in] and [lift_out] functions work best.
 *)

val lift_in :
      ?eol:string list ->
      ?buffered:bool ->
      ?buffer_size:int ->
      [ `Rec of rec_in_channel | `Raw of raw_in_channel ] ->
      in_obj_channel
  (** Turns a [rec_in_channel] or [raw_in_channel], depending on the passed
   * variant, into a full [in_obj_channel] object. (This is a convenience
   * function, you can also use the classes below directly.) If you
   * want to define a class for the lifted object, use
   * {[
   * class lifted_ch ... =
   *   in_obj_channel_delegation (lift_in ...)
   * ]}
   *
   * @param eol The accepted end-of-line delimiters. The method 
   *   [input_line] recognizes any of the passed strings as EOL
   *   delimiters. When more than one delimiter matches, the longest
   *   is taken. Defaults to [ ["\n"] ]. The default cannot be
   *   changed when [buffered=false] (would raise [Invalid_argument]).
   *   The delimiter strings must neither be empty, nor longer than
   *   [buffer_size].
   * @param buffered Whether a buffer is added, by default {b true}
   * @param buffer_size The size of the buffer, if any, by default 4096
   *)

val lift_out :
      ?buffered:bool ->
      ?buffer_size:int ->
      [ `Rec of rec_out_channel | `Raw of raw_out_channel ] ->
      out_obj_channel
  (** Turns a [rec_out_channel] or [raw_out_channel], depending on the passed
   * variant, into a full [out_obj_channel] object. (This is a convenience
   * function, you can also use the classes below directly.) If you
   * want to define a class for the lifted object, use
   * {[
   * class lifted_ch ... =
   *   out_obj_channel_delegation (lift_out ...)
   * ]}
   *
   * @param buffered Whether a buffer is added, by default {b true}
   * @param buffer_size The size of the buffer, if any, by default 4096
   *)

(** This class implements the methods from [compl_in_channel] by calling
 * the methods of [raw_in_channel]. There is no additional buffering.
 * The performance of the method [input_line] is very bad (consider
 * to override it, e.g. by [enhanced_input_line] as defined below).
 *)
class virtual augment_raw_in_channel :
object
  inherit compl_in_channel
  method virtual input : string -> int -> int -> int
    (** As in [raw_in_channel] *)
  method virtual close_in : unit -> unit
    (** As in [raw_in_channel] *)
  method virtual pos_in : int
    (** As in [raw_in_channel] *)
end


class lift_rec_in_channel : ?start_pos_in:int -> rec_in_channel -> in_obj_channel
(** This class implements [pos_in] and the methods from [compl_in_channel] 
 * by calling the methods of [rec_in_channel]. 
 * There is no additional buffering.
 *
 * The performance of the method [input_line] is very bad (consider
 * to override it, e.g. by [enhanced_input_line] as defined below).
 *
 * The method [pos_in] is implemented by counting the number of octets
 * read by the [input] method.
 *
 * @param start_pos_in The initial value of the counter for [pos_in].
 *   Defaults to 0.
 *)

(** This class implements the methods from [compl_out_channel] by calling
 * the methods of [raw_out_channel]. There is no additional buffering.
 *)
class virtual augment_raw_out_channel :
object
  inherit compl_out_channel
  method virtual output : string -> int -> int -> int
    (** As in [raw_out_channel] *)
  method virtual close_out : unit -> unit
    (** As in [raw_out_channel] *)
  method virtual flush : unit -> unit
    (** As in [raw_out_channel] *)
  method virtual pos_out : int
    (** As in [raw_out_channel] *)
end


class lift_raw_out_channel : raw_out_channel -> out_obj_channel
(** This class implements the methods from [compl_out_channel] by calling
 * the methods of [raw_out_channel]. There is no additional buffering.
 *)


class lift_rec_out_channel : 
         ?start_pos_out:int -> rec_out_channel -> out_obj_channel
(** This class implements [pos_out] and the methods from [compl_out_channel] 
 * by calling the methods of [rec_out_channel]. 
 * There is no additional buffering.
 *
 * The method [pos_out] is implemented by counting the number of octets
 * read by the [output] method.
 *
 * @param start_pos_out The initial value of the counter for [pos_out].
 *   Defaults to 0.
 *)


type input_result =
    [ `Data of int
    | `Separator of string
    ]
(** This type is for the method [enhanced_input] of [enhanced_raw_in_channel].
 * - [`Data n] means that [n] bytes have been copied to the target string
 * - [`Separator s] means that no bytes have been copied, but that an
 *   end-of-line separator [s] has been found
 *)


(** Defines private methods reading text line by line *)
class type enhanced_raw_in_channel =
object 
  inherit raw_in_channel
  method private enhanced_input_line : unit -> string
    (** An improved implementation of [input_line] that uses the buffer *)
  method private enhanced_input : string -> int -> int -> input_result
    (** Works similar to [input], but distinguishes between normal data
     * and end-of-line separators. The latter are returned as
     * [`Separator s]. When normal data is found, it is copied to the
     * string, and [`Data n] is returned to indicate that [n] bytes
     * were copied.
     *)
end


class buffered_raw_in_channel : 
        ?eol:string list ->
        ?buffer_size:int ->     (* default: 4096 *)
	raw_in_channel ->
	  enhanced_raw_in_channel
  (** This class adds a buffer to the underlying [raw_in_channel].
   * As additional feature, the method [enhanced_input_line] is a fast
   * version of [input_line] that profits from the buffer.
   *
   * @param eol The accepted end-of-line delimiters. The method 
   *   [enhanced_input_line] recognizes any of the passed strings as EOL
   *   delimiters. When more than one delimiter matches, the longest
   *   is taken. Defaults to [ ["\n"] ]. Note that [input_line]
   *   always only recognizes ["\n"] as EOL character, this cannot
   *   be changed.
   *   The delimiter strings must neither be empty, nor longer than
   *   [buffer_size].
   * @param buffer_size The size of the buffer, by default 4096.
   *)

class buffered_raw_out_channel : 
        ?buffer_size:int ->     (* default: 4096 *)
	raw_out_channel ->
	  raw_out_channel
  (** This class adds a buffer to the underlying [raw_out_channel]. 
   *
   * @param buffer_size The size of the buffer, by default 4096.
   *)



(* ********************** Channels over descriptors ******************* *)

(** {1:descriptors Channels over descriptors} *)

class input_descr :
  ?blocking:bool ->
  ?start_pos_in:int ->
  Unix.file_descr ->
    raw_in_channel
  (** Creates a [raw_in_channel] for the passed file descriptor, which must
   * be open for reading. 
   *
   * The [pos_in] method returns logical positions, i.e. it counts the number
   * of read octets. It is not tried to determine the real file position.
   *
   * The method [close_in] also closes the file descriptor.
   *
   * This class also supports Win32 proxy descriptors referring to an input
   * channel.
   *
   * @param blocking Whether the channel waits for data if it is not
   * possible to read from the (non-blocking) descriptor. Defaults to [true].
   * @param start_pos_in The position to which [pos_in] is initialized when
   * the channel is created, by default 0
   *)


class output_descr :
  ?blocking:bool ->
  ?start_pos_out:int ->
  Unix.file_descr ->
    raw_out_channel
  (** Creates a [raw_out_channel] for the passed file descriptor, which must
   * be open for writing. 
   *
   * The [pos_out] method returns logical positions, i.e. it counts the number
   * of written octets. It is not tried to determine the real file position.
   *
   * The method [close_out] also closes the file descriptor.
   *
   * This class also supports Win32 proxy descriptors referring to an output
   * channel.
   *
   * @param blocking Whether the channel waits until it can output if it is not
   * possible to write to the (non-blocking) descriptor. Defaults to [true].
   * @param start_pos_out The position to which [pos_out] is initialized when
   * the channel is created, by default 0
   *)

class socket_descr :
  ?blocking:bool ->
  ?start_pos_in:int ->
  ?start_pos_out:int ->
  Unix.file_descr ->
    raw_io_channel
  (** Creates a [raw_io_channel] for the passed socket descriptor, which must
   * be open for reading and writing, and not yet shut down in either
   * direction. The [raw_io_channel] is used to represent a bidirectional
   * channel: [close_out] shuts the socket down for sending, [close_in]
   * shuts the socket down for reading, and when both directions are down,
   * the descriptor is closed.
   *
   * The [pos_in] and [pos_out] methods returns logical positions.
   *
   * This class supports sockets and Win32 named pipes. Note, however,
   * that for Win32 named pipes it is not possible to shut down only one
   * direction of the bidirectional data channel.
   *
   * @param blocking See {!input_descr} and {!output_descr}
   * @param start_pos_in The position to which [pos_in] is initialized when
   * the channel is created, by default 0
   * @param start_pos_out The position to which [pos_out] is initialized when
   * the channel is created, by default 0
   *)


(* ********************* Transactional output channels **************** *)

(** {1:transactional Transactional channels} *)

type close_mode = [ `Commit | `Rollback ]
  (** Whether a [close_out] implies a commit or rollback operation *)

class buffered_trans_channel :
  ?close_mode:close_mode ->
  out_obj_channel ->
    trans_out_obj_channel
  (** A transactional output channel with a transaction buffer implemented
   * in memory
   *
   * @param close_mode Specifies the semantics of [close_out], by default
   * [`Commit]
   *)

val make_temporary_file : 
  ?mode:int -> ?limit:int -> ?tmp_directory:string -> ?tmp_prefix:string -> 
  unit ->
    (string * in_channel * out_channel)
  (** Creates a temporary file in the directory [tmp_directory] with a name
   * prefix [tmp_prefix] and a unique suffix. The function returns 
   * the triple (name, inch, outch) containing the file [name],
   * the file opened as in_channel [inch] and as out_channel [outch].
   *
   * @param tmp_directory Defaults to {!Netsys_tmp.tmp_directory()}
   * @param tmp_prefix By default ["netstring"]. This needs not to be
   *   unique, but just descriptive.
   * @param mode The creation mask of the file; defaults to 0o600, i.e. the
   *   file is private for the current user
   * @param limit Limits the number of trials to find the unique suffix.
   *   Defaults to 1000.
   *)

class tempfile_trans_channel :
  ?close_mode:close_mode ->
  ?tmp_directory:string ->
  ?tmp_prefix:string ->
  out_obj_channel ->
    trans_out_obj_channel
  (** A transactional output channel with a transaction buffer implemented
   * as temporary file
   *
   * @param close_mode Specifies the semantics of [close_out], by default
   *   [`Commit]
   * @param tmp_directory See [make_temporary_file]
   * @param tmp_prefix See [make_temporary_file]
   *)



(* ************************ Pipes and filters ************************* *)

(** {1:filters Pipes and Filters} *)

(** Note that this has nothing to do with "pipes" on the Unix level.
 * It is, however, the same idea: Connecting two I/O resources with an
 * intermediate buffer.
 *)

class pipe :
  ?conv:(Netbuffer.t -> bool -> Netbuffer.t -> unit) ->
  ?buffer_size:int -> 
  unit ->
    io_obj_channel
  (** A [pipe] has two internal buffers (realized by Netbuffer). The
   * output methods of the class write to the incoming buffer. When
   * new data are appended to the incoming buffer, the conversion function
   * [conv] is called; the arguments are the incoming buffer and the outgoing
   * buffer. The conversion function must convert the data available in the
   * incoming buffer and append the result to the outgoing buffer. Finally,
   * the input methods of the class return the data found in the outgoing
   * buffer.
   *
   * The conversion function is called as follows:
   * [conv incoming_buffer at_eof outgoing_buffer]
   *
   * The conversion function is allowed to do nothing if the incoming data
   * are not complete enough to be converted. It is also allowed to convert
   * only the beginning of the incoming buffer.
   *
   * If the outgoing buffer is empty, the input methods will raise
   * [Buffer_underrun].
   *
   * If [close_out] is invoked, the end of the data stream will be recorded.
   * In this case, the conversion function is called with [at_eof = true],
   * and it is expected that this function converts the whole data found
   * in the incoming buffer.
   *
   * [close_in] implies [close_out].
   *
   * The conversion function may raise exceptions. The exceptions will
   * fall through to the caller of the input methods. (The output methods
   * and [close_in], [close_out] never fail because of such exceptions.)
   *
   * The default conversion function copies everything from the incoming
   * buffer to the outgoing buffer without modification.
   *)

class output_filter : io_obj_channel -> out_obj_channel -> out_obj_channel
  (** An [output_filter] filters the data written to it through the
   * [io_obj_channel] (usually a [pipe]), and writes the filtered data
   * to the passed [out_obj_channel].
   *
   * If the filter is closed, the [io_obj_channel] will be closed, too,
   * but not the destination [out_obj_channel] (so you can still append
   * further data).
   *)

class input_filter : in_obj_channel -> io_obj_channel -> in_obj_channel
  (** An [input_filter] filters the data read from it through the
   * [io_obj_channel] (usually a [pipe] after the data have been 
   * retrieved from the passed [in_obj_channel].
   *
   * An [input_filter] object never generates [Buffer_underrun] exceptions.
   * However, if the passed [in_obj_channel] or [io_obj_channel] raises such
   * an exception, the exception will fall through the calling chain.
   *
   * If the filter is closed, the [io_obj_channel] will be closed, too,
   * but not the source [in_obj_channel] (so you can still read further
   * data from it).
   *)

(** {2:filters_notes Notes, Examples} *)

(** If you have the choice, prefer [output_filter] over [input_filter].
 * The latter is slower.
 *
 * The primary application of filters is to encode or decode a channel
 * on the fly. For example, the following lines write a BASE64-encoded file:
 *
 * {[let ch = new output_channel (open_out "file.b64") in
 * let encoder = new Netencoding.Base64.encoding_pipe ~linelength:76 () in
 * let ch' = new output_filter encoder ch in
 * ... (* write to ch' *)
 * ch' # close_out();
 * ch  # close_out();  (* you must close both channels! *)
 * ]}
 *
 * All bytes written to [ch'] are BASE64-encoded and the encoded bytes are
 * written to [ch].
 *
 * There are also pipes to decode BASE64, and to encode and decode the
 * "Quoted printable" format. Encoding and decoding work even if the
 * data is delivered in disadvantageous chunks, because the data is
 * "re-chunked" if needed. For example, BASE64 would require that data
 * arrive in multiples of three bytes, and to cope with that, the BASE64 pipe
 * only processes the prefix of the input buffer that is a multiple of three,
 * and defers the encoding of the extra bytes till the next opportunity.
 *)


