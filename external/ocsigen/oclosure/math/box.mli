(*
   OClosure Project - 2010
   Class goog.math.Box
   
   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef MATH
open Js
#endif

class type box = object
(** 
   Creates a copy of the box with the same dimensions. 
*)
  method clone : box t meth

(** 
   Returns whether the box contains a coordinate or another box.
*)
  method contains : box t -> bool t meth

(**
   Expands box with the given margins.
*)
  method expand : int -> int opt -> int opt -> int opt -> box t meth

(**
   Returns a nice string representing the box.
*)
  method toString : js_string t meth
end

val box : (int -> int -> int -> int -> box t) constr
