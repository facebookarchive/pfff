(* 
    OClosure Project - 2010
    
    Class goog.ui.GaugeTheme

    @author : Oran Charles
    @version 0.2
*)
#ifndef UI
open Js
#endif
open Ggraphics

class type gaugeTheme = object 

  (** Returns the stroke for the external border of the gauge.
      @return The stroke to use. *)
  method getExternalBorderStroke : stroke t meth

  (**
     Returns the fill for the external border of the gauge.
     @param cx X coordinate of the center of the gauge.
     @param cy Y coordinate of the center of the gauge.
     @param r Radius of the gauge.
     @return The fill to use.
  *)
  method getExternalBorderFill : int -> int -> float -> fill t meth

  (**
     Returns the stroke for the internal border of the gauge.
     @return The stroke to use.
  *)
  method getInternalBorderStroke : stroke t meth

  (**
     Returns the fill for the internal border of the gauge.
     @param cx X coordinate of the center of the gauge.
     @param cy Y coordinate of the center of the gauge.
     @param r Radius of the gauge.
     @return The fill to use.
  *)
  method getInternalBorderFill : int -> int -> float -> fill t meth

  (**
     Returns the stroke for the major ticks of the gauge.
     @return The stroke to use.
  *)
  method getMajorTickStroke : stroke t meth

  (**
     Returns the stroke for the minor ticks of the gauge.
     @return The stroke to use.
  *)
  method getMinorTickStroke : stroke t meth

  (**
     Returns the stroke for the hinge at the center of the gauge.
     @return The stroke to use.
  *)
  method getHingeStroke : stroke t meth

  (**
     Returns the fill for the hinge at the center of the gauge.
     @param cx  X coordinate of the center of the gauge.
     @param cy  Y coordinate of the center of the gauge.
     @param r  Radius of the hinge.
     @return The fill to use.
  *)
  method getHingeFill : int -> int -> float -> fill t meth

  (**
     Returns the stroke for the gauge needle.
     @return The stroke to use.
  *)
  method getNeedleStroke : stroke t meth

  (**
     Returns the fill for the hinge at the center of the gauge.
     @param cx X coordinate of the center of the gauge.
     @param cy Y coordinate of the center of the gauge.
     @param r Radius of the gauge.
     @return The fill to use.
  *)
  method getNeedleFill : int -> int -> float -> fill t meth

  (**
     Returns the color for the gauge title.
     @return The color to use.
  *)
  method getTitleColor : js_string t meth

  (**
     Returns the color for the gauge value.
     @return The color to use.
  *)
  method getValueColor : js_string t meth

  (**
     Returns the color for the labels (formatted values) of tick marks.
     @return The color to use.
  *)
  method getTickLabelColor : js_string t meth

end

(**  A class for the default color theme for a Gauge. Users can extend this 
   class to provide a custom color theme, and apply the custom color theme by 
   calling goog.ui.Gauge#setTheme. *)
val gaugeTheme : gaugeTheme t constr
