(*
   OClosure Project - 2010
   Class goog.ui.Control
   
   @author Cardoso Gabriel
   @version 0.2
*)

#ifndef UI
open Js
open Component
open ControlContent
#endif

class type control = object
  inherit component

  method addClassName : js_string t -> unit meth

  method canDecorate : #Dom_html.element t -> bool t meth

  method createDom : unit meth

  method disposeInternal : unit meth

  method enableClassName : js_string t -> bool t -> unit meth

  method enterDocument : unit meth

  method exitDocument : unit meth

  method getCaption : js_string t opt meth

  method getContent : controlContent meth

  method getContentElement : Dom_html.element t meth

  method getExtraClassNames : js_string t js_array t opt meth

  method getKeyEventTarget : Dom_html.element t meth

  method getState : Component.State.state_pre meth

  method handleBlur : #Events.event t -> unit meth

  method handleDblClick : #Events.event t -> unit meth

  method handleFocus : #Events.event t -> unit meth

  method handleKeyEvent : #Events.event t -> bool t meth

  method handleMouseDown : #Events.event t -> unit meth

  method handleMouseOut : #Events.event t -> unit meth

  method handleMouseOver : #Events.event t -> unit meth

  method handleMouseUp : #Events.event t -> unit meth

  method hasState : Component.State.state_pre -> bool t meth

  method isActive : bool t meth

  method isAllowTextSelection : bool t meth

  method isAutoState : Component.State.state_pre -> bool t meth

  method isChecked : bool t meth

  method isDispatchTransitionEvents : Component.State.state_pre -> bool t meth

  method isEnabled : bool t meth

  method isFocused : bool t meth

  method isHandleMouseEvents : bool t meth

  method isHighlighted : bool t meth

  method isOpen : bool t meth

  method isSelected : bool t meth

  method isSupportedState : Component.State.state_pre -> bool t meth

  method isVisible : bool t meth

  method removeClassName : js_string t -> unit meth

  method setActive : bool t -> unit meth

  method setAllowTextSelection : bool t -> unit meth

  method setAutoStates : Component.State.state_pre -> bool t -> unit meth

  method setCaption : js_string t -> unit meth

  method setChecked : bool t -> unit meth

  method setContent : controlContent -> unit meth

  method setDispatchTransitionEvents : Component.State.state_pre -> bool t -> unit meth

  method setEnabled : bool t -> unit meth

  method setFocused : bool t -> unit meth

  method setHandleMouseEvents : bool t -> unit meth

  method setHighlighted : bool t -> unit meth

  method setOpen : bool t -> unit meth

  method setRightToLeft : bool t -> unit meth

  method setSelected : bool t -> unit meth

  method setState : Component.State.state_pre -> bool t -> unit meth

  method setSupportedState : Component.State.state_pre -> bool t -> unit meth

  method setVisible : bool t -> bool t opt -> bool t meth
end

class type ['ctrl] controlRenderer = object
  constraint 'ctrl = #control
  method canDecorate : #Dom_html.element t -> bool t meth

  method createDom : 'ctrl t -> Dom_html.element t meth

  method decorate : 'ctrl t -> #Dom_html.element t -> Dom_html.element t meth

  method enableClassName : ('ctrl t, #Dom_html.element t) Tools.Union.t ->
    js_string t -> bool t -> unit meth

  method enableExtraClassName : 'ctrl t -> js_string t -> bool t -> unit meth

  method getAriaRole : Gdom.A11y.role_pre optdef meth

  method getContentElement : #Dom_html.element t -> Dom_html.element t meth

  method getCssClass : js_string t meth

  method getIe6ClassCombinations : js_string t js_array t js_array t meth

  method getKeyEventTarget : 'ctrl t -> Dom_html.element t meth

  method getStructuralCssClass : js_string t meth

  method initializeDom : 'ctrl t -> unit meth

  method isFocusable : 'ctrl t -> bool t meth

  method setAllowTextSelection : #Dom_html.element t -> bool t -> unit meth

  method setAriaRole : #Dom_html.element t -> unit meth

  method setContent : #Dom_html.element t -> controlContent ->
    unit meth

  method setFocusable : 'ctrl t -> bool t -> unit meth

  method setRightToLeft : #Dom_html.element t -> bool t -> unit meth

  method setState : 'ctrl t -> Component.State.state_pre -> bool t -> unit meth

  method setVisible : #Dom_html.element t -> bool t -> unit meth
end

val controlRenderer : #control controlRenderer t constr

val control : controlContent -> control #controlRenderer t -> 
  Gdom.domHelper t opt -> control t constr
