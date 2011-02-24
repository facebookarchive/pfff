(* $Id: netaux.mli 1003 2006-09-24 15:17:15Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** Internal auxiliary functions 
 *
 * This is an internal module.
 *)

(* Auxiliary stuff *)


module KMP : sig
  (* An implementation of the Knuth-Morris-Pratt algorithm *)
  (* Credits go to Alain Frisch who suggested this algorithm *)

  type pattern

  val make_pattern : string -> pattern
    (* Prepares the passed pattern *)

  val find_pattern : pattern -> ?pos:int -> ?len:int -> string -> int
    (* Searches the position where the pattern or a prefix of the pattern
     * occurs in the substring from position [pos] to [pos+len-1]. 
     * Possible return values p:
     * - pos <= p <= pos+len-length(pattern):
     *   The pattern occurs at position p in the string, i.e.
     *   string.[p+k] = pattern.[k], for all 0 <= k < length(pattern).
     *   Furthermore, the returned position p is the first such position.
     * - pos+len-length(pattern) < p < pos+len
     *   The string ends with a prefix of the pattern, i.e.
     *   string.[p+k] = pattern[k], for all 0 <= k < pos+len-p.
     * - p = pos+len
     *   Neither does the pattern occur in the string, nor is the
     *   (non-empty) suffix of the string a prefix of the pattern.
     *
     * Defaults:
     * ~pos = 0
     * ~len = length(string)-pos = "until the end of the string"
     *)

end


module ArrayAux : sig
  val int_blit : int array -> int -> int array -> int -> int -> unit
    (** A specialisation of [Array.blit] for int arrays. 
     * (Performance reasons.)
     *)

  val int_series : int array -> int -> int array -> int -> int -> int -> unit
    (** [int_series src srcpos dst dstpos len n]:
     * Computes for every [i], [0 <= i < len]:
     * [dst.(dstpos+i) = n + SUM(j=0..(i-1): src.(srcpos+j)) ]
     *
     * It is expected that [src == dst] implies [srcpos >= dstpos].
     *)

    (**/**)
    
  val int_blit_ref : 
    (int array -> int -> int array -> int -> int -> unit) ref
    (* Used by [Netaccel] to override the built-in implementation *)

  val int_series_ref : 
    (int array -> int -> int array -> int -> int -> int -> unit) ref
    (* Used by [Netaccel] to override the built-in implementation *)
end
