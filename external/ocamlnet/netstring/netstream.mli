(* $Id: netstream.mli 1003 2006-09-24 15:17:15Z gerd $
 * ----------------------------------------------------------------------
 *
 *)


(** A netstream is an input channel that is read block by block. The 
 * fragment of the channel currently loaded into memory is called the
 * current window of the netstream. The window can be thought as
 * look-ahead buffer.
 *
 * {b Picture:}
 *
 * {[
 * 0                pos_in        pos_in + window_length            EOF
 * +------------------+-------------------+--------------------------+
 *                    ====================
 *                     The current window
 * ]}
 *
 * You can use a netstream like an [in_obj_channel], and read characters
 * and strings from the stream. The bytes come first from the look-ahead
 * buffer, and if there are not enough bytes, they are loaded from the
 * underlying channel. After every read operation it is tried to enlarge
 * the window such that it contains at least one block.
 *
 * If you want that the window becomes larger, you can call [want] (to
 * enlarge the window to a certain size) or [want_another_block] (to load
 * just another block from the underlying channel). Note that this affects only
 * the current window and not the future size of the window.
 *
 * Note [Buffer_underrun]: netstreams can cope with underruns of underlying
 * channels. An underrun happens when it is not possible to ensure the
 * minimum window size. However, it is possible that the window size
 * sinks under the minimum, but the [Buffer_underrun] is deferred until the
 * next call of an input method. Furthermore, there is a problem in the [skip]
 * method which may only be partially executed, i.e. the method skips some
 * bytes and then raises [Buffer_underrun]. 
 *)


(** An [in_obj_stream] extends [in_obj_channel] by look-ahead methods *)
class type in_obj_stream =
object
  inherit Netchannels.in_obj_channel
    (** The normal input operations work as usual. The window is moved after
     * every read sequence of bytes by exactly the number of bytes, and 
     * if the window length becomes smaller than the block size, it will
     * be ensured that the window will be enlarged to the block size (or
     * to the rest of the stream until EOF, whatever is smaller).
     *)

  method block_size : int
    (** The block size of the stream *)

  method window : Netbuffer.t
    (** The look-ahead window. The first byte of the window is the byte that
     * would be read next by [input_char]. The length of the window is returned
     * by the method [window_length]. This length may be smaller than the
     * current length of the netbuffer, i.e. the netbuffer may contain 
     * additional data that must be ignored.
     *)

  method want : int -> unit
    (** Increases the length of the window such that the length is at least
     * the passed number of bytes or that the window reaches EOF (whatever
     * happens first).
     *)

  method want_another_block : unit -> unit
    (** The same as: [want block_size] *)

  method window_length : int
    (** Returns the length of the window *)

  method window_at_eof : bool
    (** Whether the window is at eof *)

  method skip : int -> unit
    (** Skip the n bytes of the stream. It is not an error to skip more bytes
     * than available in the remaining stream.
     *)

end


class input_stream : 
        ?len:int -> 
	?block_size:int -> 
	Netchannels.in_obj_channel -> 
	  in_obj_stream
  (** Make an [in_obj_stream] on top of an [in_obj_channel]. The [block_size]
   * can be specified; it defaults to 4096. 
   *
   * If [len] is passed, this parameter limits the length of the channel:
   * Only the first [len] bytes are read from the input channel, then an EOF
   * is simulated even if the input channel is longer.
   *)


class sub_stream :
        ?len:int ->             (* default: no maximum length *)
	?delimiter:string ->    (* default: no delimiter *)
	in_obj_stream ->
	  in_obj_stream
  (** A sub stream is the part of the whole stream from the current position
   * to an arbitrary other position that is determined by [len] and
   * [delimiter]. [len] specifies the maximum length of the sub stream.
   * [delimiter] is an arbitrary string that indicates the end of the
   * sub stream (the delimiter is not part of the sub stream; i.e. the
   * sub stream ends immediately before the delimiter).
   *
   * While reading from the sub stream, not only the current position of
   * the sub stream moves, but also the current position of the main
   * stream. This means that it must be avoided to read data from the
   * main stream while the sub stream is in use. The typical pattern
   * is:
   * - Read from the main stream until the beginning of a section is
   *   recognized
   * - Create a sub stream at this point
   * - Read from the sub stream until EOF
   * - Continue reading the main stream. The next character of the main
   *   stream is exactly the character following the EOF of the sub stream
   *)

val print_in_obj_stream : Format.formatter -> in_obj_stream -> unit
  (** A top-loop printer for streams *)
