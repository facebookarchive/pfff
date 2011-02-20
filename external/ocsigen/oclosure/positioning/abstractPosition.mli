(* 
    * OClosure Project - 2010
    * Class goog.positioning.AbstractPosition
    * 
    * Abstract position object. Encapsulates position and overflow handling.
    * 
    * @author Bozman Cagdas
    * @version 0.1
*)

#ifndef POSITIONING 
open Js
open Corner
#endif

class type abstractPosition = object
  (**  Repositions the element. Abstract method, should be overloaded **)
  method reposition: #Dom_html.element t -> Corner.corner -> Math.box t opt -> Math.size t opt -> unit meth
end

val abstractPosition : abstractPosition t constr

