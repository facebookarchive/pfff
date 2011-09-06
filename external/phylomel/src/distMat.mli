(**
   Distance matrices
*)

(** This module allows us to manipulate distance matrices. *)
(** The genetic distance is the number of different markers.
    In a distance matrix m, [get m i j] is the distance between
    genotypes of index i and j.

   {b Example :}

   g0, g1, g2 are 3 genotypes :
      - g0 = 0 0 1 2
      - g1 = 0 0 1 3
      - g2 = 1 0 1 2


   Their distance is given by the distance function [Genotype.diff] :
   - diff(g0,g1) = 1
   - diff(g0,g2) = 1
   - diff(g1,g2) = 2
   
   The distance matrix is :
   [[|
     [||];
     [|1|];
     [|1;2|]
   |]]
*)

(** This exception is raised when one calls [get m i j] where [i = j] *)
exception Out_of_range

type 'a t = 'a array array
val mk_dist : ('a -> 'a -> 'b) -> 'a array -> 'b array array
val create : int -> 'a -> 'a array array
val init : int -> (int -> int -> 'a) -> 'a array array
val iter : ('a -> unit) -> 'a array array -> unit
val iterij : (int -> int -> 'a -> unit) -> 'a array array -> unit
val map : ('a -> 'b) -> 'a array array -> 'b array array
val modif : ('a -> 'a) -> 'a array array -> unit
val mapij : (int -> int -> 'a -> 'b) -> 'a array array -> 'b array array
val modifij : (int -> int -> 'a -> 'a) -> 'a array array -> unit
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array array -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a array array -> 'b -> 'b
val get : 'a t -> int -> int -> 'a
val unsafe_get : 'a t -> int -> int -> 'a
val set : 'a t -> int -> int -> 'a -> unit
val max : 'a array array -> 'a
