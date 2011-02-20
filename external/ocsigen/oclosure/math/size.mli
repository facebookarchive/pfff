(*
   OClosure Project - 2010
   Class goog.math.Size
   
   @author : Cardoso Gabriel
   @version 0.2
*)
#ifndef MATH
open Js
#endif

class type size = object
  method width : int prop
  method height : int prop
end

val size : (int -> int -> size t) constr
