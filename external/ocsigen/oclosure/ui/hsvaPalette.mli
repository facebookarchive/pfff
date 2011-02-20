(*
   OClosure Project - 2010
   Class goog.ui.HsvaPalette
   
   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open HsvPalette
open Js
#endif

class type hsvaPalette = object
  inherit hsvPalette

(** @inheritDoc *)
  method createDom : unit meth

(**
   Destroys this widget and removes all event listeners.
   @override
 *)
  method disposeInternal : unit meth

(** @inheritDoc *)
  method getAlpha : float t meth

(**
   Gets the color that is currently selected in this color picker, in #rrggbbaa
   format.
   @return The string of the selected color with alpha.
 *)
  method getColorRgbaHex : js_string t meth

(**
   Sets which color is selected and update the UI. The passed color should be
   in #rrggbb format. The alpha value will be set to 1.
   @param alpha The selected alpha value, in [0, 1].
 *)
  method setAlpha : float -> unit meth

(**
   Sets which color is selected and update the UI. The passed color should be
   in #rrggbb format. The alpha value will be set to 1.
   @param color The selected color.
 *)
  method setColor : js_string t -> unit meth

(**
   Sets which color is selected and update the UI. The passed color should be
   in #rrggbbaa format. The alpha value will be set to 1.
   @param color The selected color with alpha.
 *)
  method setColorRgbaHex : js_string t -> unit meth

(** @inheritDoc *)
  method updateInput : unit meth
end

(**
   Creates an HSVA palette. Allows a user to select the hue, saturation,
   value/brightness and alpha/opacity.
   @param opt_domHelper Optional DOM helper.
   @param opt_color Optional initial color, without alpha (default is
       red).
   @param opt_alpha Optional initial alpha (default is 1).
   @param opt_class Optional base for creating classnames (default is
       'goog-hsva-palette').
 *)
val hsvaPalette : (Gdom.domHelper t opt -> js_string t opt -> float opt -> 
		     js_string t opt -> hsvaPalette t) constr
