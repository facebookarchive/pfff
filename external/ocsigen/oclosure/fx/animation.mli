(*
   OClosure Project - 2010
   Class goog.fx.Animation
   
   @author Cardoso Gabriel 
   @version 0.2
*)
#ifndef FX
open Js
#endif

class type animation = object
  inherit Events.eventTarget

(**
   Handles the actual iteration of the animation in a timeout
   @param now The current time.
*)
  method cycle : float -> unit meth

(**
   Disposes of the animation.  Stops an animation, fires a 'destroy' event and
   then removes all the event handlers to clean up memory.
*)
  method disposeInternal : unit meth

(**
   Pauses the animation (iff it's playing).
*)
  method pause : unit meth

(**
   Starts or resumes an animation.
   @param opt_restart Whether to restart the
   animation from the beginning if it has been paused.
   @return Whether animation was started.
*)
  method play : bool t opt -> bool t meth

(**
   Stops the animation.
   @param gotoEnd If true the animation will move to the end coords.
*)
  method stop : bool t -> unit meth  
end

(**
   Constructor for an animation object.
   @param start Array for start coordinates.
   @param end Array for end coordinates.
   @param duration Length of animation in milliseconds.
   @param opt_acc Acceleration function, returns 0-1 for inputs 0-1.
*)
val animation : (int js_array t -> int js_array t -> int -> 
  (float t -> float t) opt -> animation t) constr
