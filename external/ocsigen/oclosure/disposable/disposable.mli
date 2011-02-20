(* 
   OClosure Project - 2010

   Class goog.Disposable

   @author Cardoso Gabriel
*)

open Js

class type disposable = object
  method dispose : unit meth
  method getDisposed : bool t meth
  method isDisposed : bool t meth
end
