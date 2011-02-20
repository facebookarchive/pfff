(* 
   OClosure Project - 2010
   Class goog.ui.NativeButtonRenderer
   
   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
open Button
open Component
#endif

class type ['but] nativeButtonRenderer = object
  inherit ['but] buttonRenderer

(**
   Overrides goog.ui.ButtonRenderer#canDecorate by returning true only
   if the element is an HTML button.
   @param element Element to decorate.
   @return Whether the renderer can decorate the element.
   @override
 *)
  method canDecorate : #Dom_html.element t -> bool t meth

(**
   Returns the button's contents wrapped in a native HTML button element.  Sets
   the button's disabled attribute as needed.
   @param button Button to render.
   @return Root element for the button (a native HTML button element).
   @override
 *)
  method createDom : 'but t -> Dom_html.element t meth

(** @inheritDoc *)
  method decorate : 'but t -> #Dom_html.element t -> Dom_html.element t meth

(** @inheritDoc *)
  method getAriaRole : Gdom.A11y.role_pre optdef meth

(**
   @inheritDoc
   Native buttons store their value in the HTML button's [value]
   attribute.
 *)
  method getValue : #Dom_html.element t -> js_string t opt meth

(**
   @inheritDoc
   Native buttons natively support BiDi and keyboard focus.
 *)
  method initializeDom : 'but t -> unit meth

(**
   @inheritDoc
   Native buttons are always focusable as long as they are enabled.
 *)
  method isFocusable : 'but t -> bool t meth

(**
   @inheritDoc
   Native buttons don't support text selection.
 *)
  method setAllowTextSelection : #Dom_html.element t -> bool t -> unit meth

(**
   @inheritDoc
   Native buttons natively support keyboard focus.
 *)
  method setFocusable :'but t -> bool t -> unit meth

(**
   @inheritDoc
   Native buttons natively support right-to-left rendering.
 *)
  method setRightToLeft : #Dom_html.element t -> bool t -> unit meth

(**
   @inheritDoc
   Native buttons also expose the DISABLED state in the HTML button's
   [disabled] attribute.
 *)
  method setState : 'but t -> Component.State.state_pre -> bool t 
    -> unit meth

(**
   @inheritDoc
   Native buttons also expose their value in the HTML button's [value]
   attribute.
 *)
  method setValue : #Dom_html.element t -> js_string t meth

(**
   @inheritDoc
   Native buttons don't need ARIA states to support accessibility, so this is
   a no-op.
 *)
  method updateAriaState : unit meth
end

(**
   Renderer for goog.ui.Buttons.  Renders and decorates native HTML
   button elements.  Since native HTML buttons have built-in support for many
   features, overrides many expensive (and redundant) superclass methods to
   be no-ops.
 *)
val nativeButtonRenderer : #button nativeButtonRenderer t constr

