(*
   OClosure Project - 2010
   Class goog.ui.CustomButton
   
   @author : Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
open Button
open ControlContent
#endif

class type customButton = object
  inherit button
end

val customButton : (controlContent -> 
customButton #buttonRenderer t opt -> Gdom.domHelper t opt -> 
customButton t) constr

