(* 
    OClosure Project - 2010
    Class goog.ui.ColorPicker
    
    Create a new, empty color picker. 
    
    @author Bozman Cagdas
    @version 0.1
*)
open Gdom
#ifndef UI
open ColorPalette
open Component
open Js
#endif

type colors = js_string t js_array t

type color = js_string t

class type colorPicker = object
  inherit component

    (** Sets the array of colors to be displayed by the color picker. *)
  method addColor : colors -> unit meth

 (** ColorPickers cannot be used to decorate pre-existing html,
     since the structure they build is fairly complicated. *)
  method canDecorate : #Dom_html.element t -> bool t meth

(** Renders the color picker inside the provided element.
     This will override the current content of the element. *)
  method enterDocument : unit meth

 (** Sets the focus to the color picker's palette. *)
  method focus : unit meth

 (**  Gets the array of colors displayed by the color picker.
      Modifying this array will lead to unexpected behavior.*)
  method getColors : colors meth

 (** Gets the color that is currently selected in this color picker. *)
  method getSelectedColor : js_string t meth

 (** The index of the color selected. *)
  method getSelectedIndex : int meth

 (**  Gets the number of columns displayed.*)
  method getSize : Math.size t opt meth

 (** Returns true if the component is focusable, false otherwise.
     The default is true. 
     Focusable components always have a tab index and 
     allocate a key handler to handle keyboard events while focused. *)
  method isFocusable : bool t meth

 (** Sets the array of colors to be displayed by the color picker. *)
  method setColors : colors -> unit meth

 (** Sets the number of columns. 
     Will throw an error after the picker has been rendered.*)
  method setColumnCount : int -> unit meth

 (** Sets whether the component is focusable. 
     The default is true. 
     Focusable components always have a tab index and
     allocate a key handler to handle keyboard events while focused. *)
  method setFocusable : bool t -> unit meth

 (** *)
  method setSelectedColor : color -> unit meth
 
 (**  Sets which color is selected. 
      A value that is out-of-range means that no color is selected. *)
  method setSelectedIndex : int -> unit meth

 (**  Sets the size of the palette.
      Will throw an error after the picker has been rendered. *)
  method setSize : int -> unit meth
end

val colorPicker : (domHelper t opt -> colorPalette t opt -> colorPicker t) constr

module ColorPicker : sig 
  val createSimpleColorGrid : Gdom.domHelper t opt -> colorPicker t 

  val _SIMPLE_GRID_COLORS : js_string t js_array t 

  module EventType : sig
    val _CHANGE : js_string t
  end
end
