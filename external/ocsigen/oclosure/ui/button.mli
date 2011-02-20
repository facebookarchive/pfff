(* 
   OClosure Project - 2010
   Class goog.ui.Button
   
   @author Cardoso Gabriel
   @version 0.2
*)

#ifndef UI
open Control
open ControlContent
open Js
#endif

module Side : sig
   type side =   
     (** Neither side. *)
       NONE
     (** Left for LTR, right for RTL. *)
     | START
     (** Right for LTR, left for RTL. *) 
     | END
     (** Both sides. *)
     | BOTH
end

class type button = object
  inherit control

(** @inheritDoc *)
  method disposeInternal : unit meth

(** @inheritDoc *)
  method enterDocument : unit meth

(**
   Returns the tooltip for the button.
   @return Tooltip text (undefined if none).
*)
  method getTooltip : js_string t optdef meth

(**
   Returns the value associated with the button.
   @return Button value (undefined if none).
*)
  method getValue : js_string t optdef meth

(**
   Collapses the border on one or both sides of the button, allowing it to be
   combined with the adjancent button(s), forming a single UI componenet with
   multiple targets.
   @param sides Bitmap of one or more goog.ui.Button.Sides for
   which borders should be collapsed.
*)
  method setCollapsed : Side.side -> unit meth

(**
   Sets the tooltip for the button, and updates its DOM.
   @param tooltip New tooltip text.
*)
  method setTooltip : js_string t -> unit meth

(**
   Sets the value associated with the button, and updates its DOM.
   @param value New button value.
*)
  method setValue : js_string t opt -> unit meth
end

class type ['but] buttonRenderer = object
  constraint 'but = #button
  inherit ['but] controlRenderer

(**
   Returns the ARIA role to be applied to buttons.
   @return ARIA role.
*)
  method getAriaRole : Gdom.A11y.role_pre optdef meth

(** @inheritDoc *)
  method getCssClass : js_string t meth

(**
   Takes a button's root element, and returns its tooltip text.
   @param element The button's root element.
   @return The tooltip text.
*)
  method getTooltip : #Dom_html.element t -> js_string t optdef meth

(**
   Takes a button's root element, and returns the value associated with it.
   No-op in the base class.
   @param element The button's root element.
   @return The button's value (undefined if none).
*)
  method getValue : #Dom_html.element t -> js_string t opt meth
end

(**
   Default renderer for goog.ui.Buttons.  Extends the superclass with
   the following button-specific API methods:
   - [getValue] - returns the button element's value
   - [setValue] - updates the button element to reflect its new value
   - [getTooltip] - returns the button element's tooltip text
   - [setTooltip] - updates the button element's tooltip text
   - [setCollapsed] - removes one or both of the button element's
   borders

   For alternate renderers, see goog.ui.NativeButtonRenderer,
   goog.ui.CustomButtonRenderer, and goog.ui.FlatButtonRenderer.
*)
val buttonRenderer : #button buttonRenderer t constr


(**
   A button control, rendered as a native browser button by default.
   
   @param content Text caption or existing DOM
   structure to display as the button's caption.
   @param opt_renderer Renderer used to render or
   decorate the button; defaults to goog.ui.NativeButtonRenderer.
   @param opt_domHelper Optional DOM hepler, used for
   document interaction.
*)
val button : (controlContent -> button #buttonRenderer t opt -> Gdom.domHelper t opt -> button t) constr
