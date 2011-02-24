(* $Id: netbuffer.mli 1476 2010-08-31 00:05:15Z gerd $
 * ----------------------------------------------------------------------
 *
 *)


(** A Netbuffer.t is a buffer that can grow and shrink dynamically. *)

type t

val create : int -> t
    (** Creates a netbuffer which allocates initially this number of bytes. 
     * The logical length is zero.
     *)

val contents : t -> string
    (** Returns the contents of the buffer as fresh string. *)

val length : t -> int
    (** Returns the logical length of the buffer *)

(** {2 Extracting strings} *)

val get : t -> int -> char
    (** [get nb pos]: Get the character at [pos] *)

val nth : t -> int -> char
    (** Alias for [get] *)

val sub : t -> int -> int -> string
    (** [sub nb k n]: returns the n characters starting at position [n] from 
     * netbuffer [nb] as fresh string
     *)

val blit_to_string : t -> int -> string -> int -> int -> unit
    (** [blit_to_string nb srcpos dest destpos len]: Copies the [len] bytes at
     * position [srcpos] from [nb] to the string [dest] at position [destpos].
     *)

val blit : t -> int -> string -> int -> int -> unit
    (** Compatibility name for [blit_to_string], now deprecated *)

(** {2 Appending strings} *)

val add_string : t -> string -> unit
    (** [add_string nb s]: Adds a copy of the string [s] to the logical end of
     * the netbuffer [nb]. If necessary, [nb] grows.
     *)

val add_sub_string : t -> string -> int -> int -> unit
    (** [add_sub_string nb s k n]: Adds the substring of [s] starting at position
     * [k] with length [n] to the logical end of the netbuffer [nb]. If necessary,
     * [nb] grows.
     *
     * This is semantically the same as
     * [add_string nb (String.sub s k n)], but the extra copy is avoided.
     *)

val add_substring : t -> string -> int -> int -> unit
    (** Alias for add_sub_string *)

val add_char : t -> char -> unit
    (** [add_char nb c]: Adds a single char at the end of the buffer *)

val add_char_2 : t -> char -> char -> unit
    (** [add_char_2 nb c1 c2]: Adds two chars at the end of the buffer *)

val add_char_4 : t -> char -> char -> char -> char -> unit
    (** [add_char_4 nb c1 c2 c3 c4]: Adds four chars at the end of the buffer *)

val add_inplace : ?len:int -> t -> (string -> int -> int -> int) -> int
    (** [add_inplace nb f]: Calls the function [f] to add bytes to the
     * netbuffer [nb]. The arguments of [f] are the buffer, the position
     * in the buffer, and the maximum length. The function [f] must return
     * the actual number of added bytes; this number is also returned by
     * [add_inplace].
     *
     * Example: let n = add_inplace nb (Pervasives.input ch)
     *
     * The argument [len] is the number of bytes to add (second argument of
     * [f]). It defaults to the number of free bytes in the buffer after space
     * for at least one byte has been allocated.
     *)

val add_buffer : t -> t -> unit
  (** [add_buffer nb1 nb2]: Adds the contents of [nb2] to the end of [nb1] *)

val area_for_additions : ?len:int -> t -> (string * int * int)
val advance : t -> int -> unit
  (** These two functions work together, so that the effect of [add_inplace]
      can be obtained in two steps. First, the user calls
      {[
        let (s,pos,len) = area_for_additions nb
      ]}
      to get the area where to put new data of length [n], with [n <= len].
      After this the data is made valid by
      {[
        advance n
      ]}
   *)


(** {2 Inserting strings} *)

val insert_string : t -> int -> string -> unit
    (** [insert_string nb p s]: Inserts the value of string [s] at position
     * [p] into the netbuffer [nb]
     *)

val insert_sub_string : t -> int -> string -> int -> int -> unit
    (** [insert_string nb p s k n]: Inserts a substring of string [s] at position
     * [p] into the netbuffer [nb]. The substring is denoted by position [k]
     * and has length [n]
     *)

val insert_char : t -> int -> char -> unit
    (** [insert_char nb p c]: Inserts character [c] at position [p] into
     * the netbuffer [nb]
     *)

(** {2 Overwriting strings} *)

val set : t -> int -> char -> unit
    (** [set nb pos c]: Sets the character at [pos] to [c] *)

val put_string : t -> int -> string -> unit
    (** [put_string nb pos s]: Copies the string [s] to the position [pos]
        of netbuffer [nb]
     *)

val blit_from_string : string -> int -> t -> int -> int -> unit
    (** [blit_from_string src srcpos dest destpos len]: Copies the [len] bytes
     * at position [srcpos] from the string [src] to the netbuffer [dest] at
     * position [destpos].
     *
     * It is possible to copy the string beyond the end of the buffer. The
     * buffer is automatically enlarged in this case.
     *)

(** {2 Deleting} *)

val delete : t -> int -> int -> unit
    (** [delete nb k n]: Deletes the [n] bytes at position [k] of netbuffer 
     * [nb] in-place.
     *
     * The netbuffer does not shrink, however, i.e. the free space is not
     * given back to the memory manager.
     *)

val clear : t -> unit
    (** Deletes all contents from the buffer. As [delete], the netbuffer does
     * not shrink.
     *)

val reset : t -> unit
    (** Empty the buffer, deallocate the internal string, and replace it
        with a new string of length [n] that was allocated by
        {!Netbuffer.create} [n].
     *)

val try_shrinking : t -> unit
    (** [try_shrinking nb]: If the length of the buffer is less than half of
     * the allocated space, the netbuffer is reallocated in order to save
     * memory.
     *)

(** {2 Searching} *)

val index_from : t -> int -> char -> int
    (** [index_from nb k c]: Searches the character [c] in the netbuffer beginning
     * at position [k]. If found, the position of the left-most occurence is
     * returned. Otherwise, [Not_found] is raised.
     *)

(** {2 Memory} *)

val blit_to_memory : t -> int -> Netsys_mem.memory -> int -> int -> unit
    (** [blit_to_memory nb srcpos dest destpos len]: Copies the [len] bytes at
     *  position [srcpos] from [nb] to the membuffer [dest] at position
	[destpos].
     *)

val add_sub_memory : t -> Netsys_mem.memory -> int -> int -> unit
  (** Same as [add_sub_string], but gets data from a memory buffer *)


(** {2 Miscelleneous} *)

val unsafe_buffer : t -> string
    (** {b Warning! This is a low-level function!}
     * Returns the current string that internally holds the buffer.
     * The byte positions 0 to length - 1 actually store the contents of
     * the buffer. You can directly read and modify the buffer. Note that
     * there is no protection if you read or write positions beyond the
     * length of the buffer.
     *)

val print_buffer : t -> unit
    (** For the toploop *)


(* MISSING: searching substrings *)
