(*
   Tools module
   
   Provide some tools for the OClosure library
   
   @author Cardoso Gabriel
   @version 0.2
*)

open Js

module Union : sig
  type (+'a, +'b) t
  val i1 : 'a -> ('a, 'b) t
  val i2 : 'b -> ('a, 'b) t
end

(** The expression [variable s] where [s] has the form 
   ["[oclosure]goog.package.ClassName[/oclosure]"] is considered as a pure
   expression by the compiler. Used in constructors
*)
val variable : string -> 'a
