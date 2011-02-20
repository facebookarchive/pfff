(* 
    OClosure Project - 2010
    Class goog.ui.Palette
    
    A palette is a grid of DOM nodes that the user can highlight or
    select via the keyboard or the mouse.
    The selection state of the palette is controlled an ACTION event.
    Event listeners may retrieve the selected item using the #getSelectedItem
    or #getSelectedIndex method.
    Use this class as the base for components like color palettes or
    emoticon pickers.
    Use #setContent to set/change the items in the palette after construction.
    See palette.html demo for example usage. 
    
    @author Bozman Cagdas
    @version 0.1
*)

#ifndef UI
open Control
open Js
#endif

class type palette = object
  inherit control

  method disposeInternal : unit meth

  method getCaption : js_string t opt meth

  method getHighlightedIndex : int meth

  method getHighlightedItem : Dom.node t meth

  method getSelectedIndex : int meth

  method getSelectedItem : Dom.node t meth

  method getSize : Math.size t meth

  method handleKeyEvent : #Events.event t -> bool t meth

  method handleMouseDown : #Events.event t -> unit meth

  method handleMouseOut : #Events.event t -> unit meth

  method handleMouseOver : #Events.event t -> unit meth

  method handleSelectionChange : #Events.event t -> unit meth

  method performActionInternal : #Events.event t -> bool t meth

  method setCaption : js_string t -> unit meth

  method setHighlightedIndex : int -> unit meth

  method setSelectedIndex : int -> unit meth

  method setSelectedItem : Dom.node t -> unit meth

  method setSize : (Math.size t, int) Tools.Union.t -> int opt -> unit meth
end

class type ['pal] paletteRenderer = object
  inherit ['pal] controlRenderer
  method canDecorate : #Dom_html.element t -> bool t meth

  method createDom : 'pal t -> Dom_html.element t meth
end

val paletteRenderer : #palette paletteRenderer t constr

val palette : (#Dom.node t js_array t -> palette #paletteRenderer t opt -> Gdom.domHelper t opt -> palette t) constr

