(*
   OClosure Project - 2010
   Class goog.ui.IdGenerator
   
   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
#endif

class type idGenerator = object

  (**
     Gets the next unique ID.
     @return The next unique identifier.
  *)
  method getNexUniqueId : js_string t meth
end


(** Creates a new id generator. *)
val idGenerator : idGenerator t constr 

module IdGenerator : sig
  (**
     Default instance for id generation. Done as an instance instead of statics
     so it's possible to inject a mock for unit testing purposes.
  *)
  val getInstance : unit -> 'a
end
