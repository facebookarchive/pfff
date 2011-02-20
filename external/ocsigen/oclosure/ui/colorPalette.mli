(* 
    OClosure Project - 2010
    Class goog.ui.ColorPalette
    
    Create a new, empty color picker. 
    
    @author Bozman Cagdas
    @version 0.2
*)
#ifndef UI
open Palette
open Js
#endif

class type colorPalette = object
  inherit palette
  
  method getColors : js_string t js_array t meth

  method getSelectedColor : js_string t opt meth

  method setColors : js_string t js_array t -> unit meth

  method setSelectedColor : js_string t opt -> unit meth
end

val colorPalette : (js_string t js_array t opt -> palette #paletteRenderer t opt
  -> Gdom.domHelper t opt -> colorPalette t) constr
