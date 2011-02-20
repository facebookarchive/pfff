(*
   OClosure Project - 2010
   Class goog.ui.AnimatedZippy
   
   @author : Oran Charles, Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
open Tools
open Zippy
#endif

class type animatedZippy = object
  inherit zippy

(**  Duration of expand/collapse animation, in milliseconds. *)
  method animationDuration : int prop

  (**  Acceleration function for expand/collapse animation. *)
  method animationAcceleration : float -> float t meth
  
  (** Sets expanded state.*)
  method setExpanded : bool t -> unit meth

end

val animatedZippy : ((#Dom_html.element t, js_string t) Union.t opt 
		     -> (#Dom_html.element t, js_string t) Union.t opt 
		       -> bool t opt -> animatedZippy t) constr 
