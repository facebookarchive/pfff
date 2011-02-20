(*
   OClosure Project - 2010

   Class goog.ui.TwoThumbSlider
   
   @author Charles Oran
   @version 0.2
*)

#ifndef UI
open Js
open SliderBase
#endif
module TwoThumbSlider : sig

  (** The prefix we use for the CSS class names for the slider and its elements. *)
  val _CSS_CLASS_PREFIX : js_string t

  (** CSS class name for the value thumb element. *)
  val _VALUE_THUMBS_CSS_CLASS : js_string t

  (** CSS class name for the extent thumb element. *)
  val _EXTENT_THUMBS_CSS_CLASS : js_string t
  
end

class type twoThumbSlider = object
  inherit sliderBase

  (**
     @param orient orientation of the slider.
     @return The CSS class applied to the twothumbslider element.
  *)
  method getCssClass : js_string t meth

  (**
     Creates the thumb members for a twothumbslider. If the element contains a child with a class name 'goog-twothumbslider-value-thumb' (or 'goog-twothumbslider-extent-thumb', respectively), then that will be used as the valueThumb (or as the extentThumb, respectively)
  *)
  method createThumbs : unit meth 

end

(**
   This creates a TwoThumbSlider object.
   @param opt_domHelper Optional DOM helper.
*)
val twoThumbSlider : (Gdom.domHelper t opt -> twoThumbSlider t) constr
