(* $Id: netpagebuffer.mli 1476 2010-08-31 00:05:15Z gerd $ *)

(** Buffer for page-aligned I/O *)

(** This kind of buffer uses page-aligned bigarrays. Data can only be
    added to the end, or deleted at the beginning of the buffer.

    The idea of this buffer is that input data is added to the last
    page of the buffer only (with [add_inplace]). If then all previous
    input was already a multiple of the page size, it is ensured that
    the new input is added at a page boundary. This kind of input operation
    can often be accelerated by the OS ("zero copy network I/O").
 *)

type t

val create : int -> t
  (** crea [pagesize]: creates new buffer with this [pagesize], which must
      be a whole multiple of the page size of the OS
    *)

val contents : t -> string
  (** Returns the contents as string *)

val length : t -> int
  (** The length *)

val sub : t -> int -> int -> string
  (** Returns a substring *)

val blit_to_string : t -> int -> string -> int -> int -> unit
  (** Blits contents to a string *)

val blit_to_memory : t -> int -> Netsys_mem.memory -> int -> int -> unit
  (** Blits contents to another memory buffer *)

val add_string : t -> string -> unit
  (** Adds a string to the end of the buffer *)

val add_sub_string : t -> string -> int -> int -> unit
  (** Adds a sub string to the end of the buffer *)

val add_sub_memory : t -> Netsys_mem.memory -> int -> int -> unit
  (** Adds a sub memory buffer to the end of the buffer *)

val add_inplace : t -> (Netsys_mem.memory -> int -> int -> int) -> int
  (** [add_inplace b f]: Calls [f m pos len] where [m] is the last page
      of the buffer, and [pos] is the first free byte on the page, and
      [len] is the number of free bytes on the page. The function [f] is
      expected to store new data in [m] from [pos] to [pos+n-1] and to
      return [n]. The number [n] is also returned as final result.

      It is ensured that [f] is called with a value of [len>=1].
   *)

val page_for_additions : t -> (Netsys_mem.memory * int * int)
  (** [let (m,pos,len) = page_for_additions b]: Returns the last page 
      in [m], the first free byte on the page in [pos], and 
      the number of free bytes on the page in [len]. (The same values
      the function [f] would have got as arguments in [add_inplace].)
   *)

val advance : t -> int -> unit
  (** [advance b n]: Marks further [n] bytes in the last page of the
      buffer as used. These bytes are not modified in any way - it is
      expected that the user calls [page_for_additions] first, and sets
      these [n] bytes to new values directly.
   *)

val page_for_consumption : t -> (Netsys_mem.memory * int * int)
  (** [let (m,pos,len) = page_for_consumption b]: Returns the first page 
      in [m], the first used byte on the page in [pos], and 
      the number of used bytes on the page in [len].
   *)


val delete_hd : t -> int -> unit
  (** [delete_hd b n]: Deletes [n] bytes from the beginning of the buffer
   *)

val clear : t -> unit
  (** Deletes all contents of the buffer *)

(** {2 Searching} *)

val index_from : t -> int -> char -> int
    (** [index_from nb k c]: Searches the character [c] in the buffer beginning
     * at position [k]. If found, the position of the left-most occurence is
     * returned. Otherwise, [Not_found] is raised.
     *)

