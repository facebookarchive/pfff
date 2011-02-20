(*
   OClosure Project - 2010

   Class goog.ui.Slider

   @author Bourdin Pierre
*)
#ifndef UI
open Js
open SliderBase
#endif
class type slider = object
  inherit sliderBase

(** @inheritDoc *)
  method createThumbs : unit meth

end

val slider : (Gdom.domHelper t opt -> slider t) constr
