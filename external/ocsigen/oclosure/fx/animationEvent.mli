(*
   OClosure Project - 2010
   Class goog.fx.AnimationEvent
   
   @author Cardoso Gabriel 
   @version 0.2
*)
#ifndef FX
open Js
open Animation
#endif

class type animationEvent = object
  inherit Events.event

(**
   Returns the coordinates as integers (rounded to nearest integer).
   @return An array of the coordinates rounded to
   the nearest integer.
*)
  method coordAsInts : int js_array t meth
end

(**
   Class for an animation event object.
   @param type Event type.
   @param anim An animation object.
*)
val animationEvent : (js_string t -> animation t 
  -> animationEvent t) constr
