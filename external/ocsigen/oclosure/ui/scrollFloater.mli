(* 
   OClosure Project - 2010
   Class goog.ui.ScrollFloater
   @author : Emmanuel CRESPIN
   @version 0.1
*) 
#ifndef UI
open Component
open Js
#endif
class type scrollFloater = object
  inherit component
  (** Delegates dom creation to superclass, then constructs and
     decorates required DOM elements.*)
  method createDom : unit meth
  
  (** Decorates the floated element with the standard ScrollFloater CSS class.*)
  method decorateInternal : #Dom_html.element t -> unit meth
  
  (** @inheritDoc*)
  method enterDocument : unit meth
  
  (** @inheritDoc*)
  method diposeInternal : unit meth
  
  (** Sets whether the element should be floated if it scrolls out of view.*)
  method setScrollingEnabled : bool t -> unit meth
  
  (** Whether the component is enabled for scroll-floating.*)
  method isScrollingEnabled : bool t meth
  
  (** Whether the component is currently scroll-floating.*)
  method isFloating : bool t meth

end

val scrollFloater : (#Dom_html.element t -> Gdom.domHelper t opt 
  -> scrollFloater t) constr
