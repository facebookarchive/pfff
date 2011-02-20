(*
   OClosure Project - 2010
   Class goog.ui.DimensionPicker
   
   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Control
open Js
#endif

class type dimensionPicker = object
  inherit control

  method disposeInternal : unit meth

  method enterDocument : unit meth

  method exitDocument : unit meth

  method getSize : Math.size t meth

  method getValue : Math.size t meth

  method handleKeyEvent_ : Events.keyEvent t -> bool t meth

  method setValue : int -> int opt -> unit meth
end

class type ['dimPckr] dimensionPickerRenderer = object
  inherit ['dimPckr] controlRenderer

  method getCssClass : js_string t meth

  method getGridOffsetX : dimensionPicker t -> int meth

  method getGridOffsetY : dimensionPicker t -> int meth

  method getMouseMoveElement : dimensionPicker t -> Dom_html.element t meth

  method initializeDom : dimensionPicker t -> unit meth

  method positionMouseCatcher : dimensionPicker t -> unit meth

  method setHighlightedSize : dimensionPicker t -> int -> int -> unit meth

  method updateSize : dimensionPicker t -> #Dom_html.element t -> unit meth
end

val dimensionPicker : (dimensionPicker dimensionPickerRenderer t opt 
		       -> Gdom.domHelper t opt 
		       -> dimensionPicker t) constr 

val dimensionPickerRenderer : 
    (#dimensionPicker dimensionPickerRenderer t) constr
  
