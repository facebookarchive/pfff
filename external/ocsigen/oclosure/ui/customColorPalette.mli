(*
   OClosure Project - 2010
   Class goog.ui.CustomColorPalette
   
   @author : 
   @version 0.2
*)
#ifndef UI
open Js
open ColorPalette
open Palette
#endif

class type customColorPalette = object
  inherit colorPalette
  method performActionInternal : #Events.event t -> bool t meth

  method promptForCustomColor : unit meth
end

val customColorPalette : (js_string t js_array t -> 
  customColorPalette #paletteRenderer t opt -> Gdom.domHelper t opt -> 
    customColorPalette t) constr
