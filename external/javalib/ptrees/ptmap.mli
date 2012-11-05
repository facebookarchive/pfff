(*
 * Copyright (C) 2008 Jean-Christophe Filliatre
 * Copyright (C) 2008, 2009 Laurent Hubert (CNRS)
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1, with the special exception on linking described in file
 * LICENSE.

 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *)


(** Maps over integers implemented as Patricia trees.

    This implementation follows Chris Okasaki and Andrew Gill's paper
    {e Fast Mergeable Integer Maps}.*)

(** The following signature is similar to [Map.S with type key = int].
    The documentation is only given for function that differs from
    [Map.S with type key = int].

    The module {!Ptset} is based on the same data-structure.

*)

module type S = sig
  type (+'a) t

  type key = int

  val empty : 'a t

  val is_empty : 'a t -> bool

  (** [add ~merge:f k d m] returns a map containing the same bindings as
      [m], plus a binding of [k] to [d].  If [k] was already bound to
      [d'] in [m], then the value [f d' d] is added instead of [d].  If
      no merge function is specified, then the previous bindings is
      simply discard.
      @author Laurent Hubert*)
  val add : ?merge:('a -> 'a -> 'a) -> int -> 'a -> 'a t -> 'a t

  val modify : int -> ('a option -> 'a) -> 'a t -> 'a t

  val find : int -> 'a t -> 'a

(** [findi_element f t] returns the first couple (index,element) in map [t] for which [f ind elt], with [ind] an index and [elt] the corresponding element, returns true.

    @raise Not_found if [t] is empty or if [f] returns false for all
    elements.*)
  val findi_element : (int -> 'a -> bool) -> 'a t -> int * 'a

  val find_element : ('a -> bool) -> 'a t -> 'a

  val remove : int -> 'a t -> 'a t

  val mem :  int -> 'a t -> bool

  val iter : (int -> 'a -> unit) -> 'a t -> unit

  val map : ('a -> 'b) -> 'a t -> 'b t

  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

  val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  (** [merge f m1 m2] returns a map that has the bindings of [m1] and [m2] and
      which binds [k] to [f d1 d2] if [m1] and [m2] binds the same [k] to
      different [d1] and [d2], respectively. If [d1] equals [d2], [f d1 d2] is
      supposed to return [d1].

      @author Laurent Hubert*)
  val merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  (** [merge_first m1 m2] is the same as [merge (fun a _ -> a) m1 m2] but it
      reuses more data from the first map.*)
  val merge_first : 'a t -> 'a t -> 'a t

  val diff : ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t

  (** [choose_and_remove t] returns (i,d,t') such that [t'] equals to
      [remove i t] and [d] equals to [find i t].

      @raise Not_found if [t] is empty.
      @author Laurent Hubert*)
  val choose_and_remove : 'a t -> int * 'a * ('a t)

  (** [inter m1 m2] returns a map with all the bindings [(k,d)] of [m1] such
      that [mem k m2]. *)
  val inter : 'a t -> 'a t -> 'a t

  (* [inter_map2 f m1 m2] returns a map that binds only the keys [k]
     that are binded both in [m1] and [m2]. The associated value is
     [f (find k m1) (find k m2)]. [f] must satisfy [f x x = x] for all
     [x].

     @author David Pichardie *)
  val inter_map2 : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  (** [subset m1 m2] returns [true] if [m1] is a subset of [m2], [false]
      otherwise. *)
  val subset : 'a t -> 'a t -> bool

  val cardinal: 'a t -> int

  val exists: (int -> 'a -> bool) -> 'a t -> bool

  val filter: ('a -> bool) -> 'a t -> 'a t

  val filteri: (int -> 'a -> bool) -> 'a t -> 'a t

  val filter_map : ('a -> 'b option) -> 'a t -> 'b t

  val partition: ('a -> bool) -> 'a t -> 'a t * 'a t

  (** [elements m] return the unsorted list of bindings of [m].  *)
  val elements: 'a t -> (int * 'a) list

end

include S
