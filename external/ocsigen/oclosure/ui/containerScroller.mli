(*
   OClosure Project - 2010
   Class goog.ui.ContainerScroller
   
   @author : Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
open Container
#endif

class type containerScroller = object
  inherit Disposable.disposable

  method disposeInternal : unit meth
end

val containerScroller : (container t -> containerScroller t) constr
