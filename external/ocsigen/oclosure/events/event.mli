(* 
   OClosure Project - 2010
   Class goog.events.Event
   
   A base class for event objects, so that they can support 
   preventDefault and stopPropagation.
   
   @author Oran Charles
   @version 0.1
*)
#ifndef EVENTS
open Js
#endif

class type event = object
  inherit Disposable.disposable
  (** @inheritDoc *)
  method disposeInternal : unit meth

  (** Stops event propagation. *)
  method stopPropagation : unit meth

  (** Prevents the default action, for example a link redirecting to a url.*)
  method preventDefault : unit meth
end

val event : (js_string t -> event t) constr
