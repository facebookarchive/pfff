(*
   OClosure Project - 2010
   Class goog.ui.HsvPalette
   
   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Component
open Js
#endif

class type hsvPalette = object
  inherit component

(**
   HsvPalettes cannot be used to decorate pre-existing html, since the
   structure they build is fairly complicated.
   @param element Element to decorate.
   @return Returns always false.
 *)
  method canDecorate : #Dom_html.element t -> bool t meth

(** @inheritDoc *)
  method createDom : unit meth

(** @inheritDoc *)
  method disposeInternal : unit meth

(**
   Renders the color picker inside the provided element. This will override the
   current content of the element.
 *)
  method enterDocument : unit meth

(**
   Alpha transparency of the currently selected color, in [0, 1].
   For the HSV palette this always returns 1. The HSVA palette overrides
   this method.
   @return The current alpha value.
 *)
  method getAlpha : float t meth

(**
   Gets the color that is currently selected in this color picker.
   @return The string of the selected color.
*)
  method getColor : js_string t meth

(**
   Sets which color is selected and update the UI.
   @param color The selected color.
 *)
  method setColor : js_string t -> unit meth

(**
   Alters the hue, saturation, and/or value of the currently selected color and
   updates the UI.
   @param opt_hue (optional) hue in [0, 1].
   @param opt_saturation (optional) saturation in [0, 1].
   @param opt_value (optional) value in [0, 255].
 *)
  method setHsv : float opt -> float opt -> int opt -> unit meth
end

(**
   Creates an HSV palette. Allows a user to select the hue, saturation and
   value/brightness.
   @param opt_domHelper Optional DOM helper.
   @param opt_color Optional initial color (default is red).
   @param opt_class Optional base for creating classnames (default is
       goog.getCssName('goog-hsv-palette')).
 *)
val hsvPalette : (Gdom.domHelper t opt -> js_string t opt -> js_string t opt ->
	       hsvPalette t) constr
