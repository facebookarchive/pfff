(*
   OClosure Project - 2010
   Class goog.structs.Pool
   
   @author Cardoso Gabriel
   @version 0.2
*)
open Js

class type pool = object
  inherit Disposable.disposable
end

val pool : int opt -> int opt -> pool t constr
