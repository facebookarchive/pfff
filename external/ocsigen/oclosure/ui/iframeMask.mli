(*
   OClosure Project - 2010
   Class goog.ui.IframeMask
   
   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
#endif

class type iframeMask = object
  inherit Disposable.disposable

(**
   Applies the iframe mask to the screen.
 *)
  method applyMask : unit meth

(**
   Removes the iframe from the DOM.
   @override
 *)
  method disposeInternal : unit meth

(**
   Removes the mask from the screen.
 *)
  method hideMask : unit meth

(**
   Listens on the specified target, hiding and showing the iframe mask
   when the given event types are dispatched.
   @param target The event target to listen on.
   @param showEvent When this event fires, the mask will be applied.
   @param hideEvent When this event fires, the mask will be hidden.
   @param opt_snapElement When the mask is applied, it will
       automatically snap to this element. If no element is specified, it will
       use the default snap element.
 *)
  method listenOnTarget : #Events.eventTarget t -> js_string t -> js_string t
    -> #Dom_html.element t -> unit meth

(**
   Removes all handlers attached by listenOnTarget.
 *)
  method removeHandlers : unit meth

(**
   Sets the opacity of the mask. Will take effect the next time the mask
   is applied.
   @param opacity A value between 0 and 1, with 1 being
       totally opaque.
 *)
  method setOpacity : float -> unit meth

(**
   Sets the element to use as the bounds of the mask. Takes effect immediately.
   @param snapElement The snap element, which the iframe will be
       "snapped" around.
*)
  method setSnapElement : #Dom_html.element t -> unit meth

(**
   Sets the z-index of the mask. Will take effect the next time the mask
   is applied.
   @param zIndex A z-index value.
*)
  method setZIndex : int -> unit meth
end

(**
   Controller for an iframe mask. The mask is only valid in the current
   document, or else the document of the given DOM helper.
   @param opt_domHelper The DOM helper for the relevant
       document.
   @param opt_iframePool An optional source of iframes.
       Iframes will be grabbed from the pool when they're needed and returned
       to the pool (but still attached to the DOM) when they're done.
*)
val iframeMask : Gdom.domHelper t opt -> Structs.pool t opt ->
  iframeMask t constr
