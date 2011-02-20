(* 
   OClosure Project - 2010 
   Class goog.ui.Popup
   
   @author Esther Baruk
   @version 0.1
*)

#ifndef UI
open PopupBase
open Js
#endif

class type popup = object
  inherit popupBase
    (**  Returns the corner of the popup to used in the positioning algorithm.*)
  method getPinnedCorner : Positioning.corner meth

    (** Sets the corner of the popup to used in the positioning algorithm.*)
  method setPinnedCorner : Positioning.corner -> unit meth

    (** Returns the position helper object associated with the popup.*)
  method getPosition : Positioning.abstractPosition t meth

    (** Sets the position helper object associated with the popup.*)
  method setPosition : Positioning.abstractPosition t -> unit meth

    (** Returns the margin to place around the popup.*)
  method getMargin : Math.box t opt meth

    (** Sets the margin to place around the popup*)
  method setMargin : (Math.box t, int) Tools.Union.t opt -> int opt -> int opt 
    -> int opt -> unit meth

    (** Repositions the popup according to the current state.*)
  method reposition : unit meth
end

val popup : (#Dom_html.element t opt -> Positioning.abstractPosition t opt -> popup t) constr
