(* 
   OClosure Project - 2010 
   Class goog.ui.Tooltip
   
   @author Esther Baruk
   @version 0.1
*)
#ifndef UI
open Js
open Popup
#endif
open Gdom
open Positioning
open Tools

type state =
  | INACTIVE
  | WAITING_TO_SHOW
  | SHOWING
  | WAITING_TO_HIDE
  | UPDATING

class type tooltip = object
  inherit popup
    (** Returns the dom helper that is being used on this component.*)
  method getDomHelper : domHelper meth
      (** Attach to element. Tooltip will be displayed when the cursor is 
         over the element or when the element has been active for a 
         few milliseconds.*)
  method attach : (#Dom_html.element t, js_string t) Union.t -> unit meth

    (** Detach from element(s).*)
  method detach : (#Dom_html.element t, js_string t) Union.t opt -> unit meth

    (** Sets delay in milliseconds before tooltip is displayed for an element.*)
  method setshowDelayMs : int -> unit meth

    (** Returns the delay in milliseconds before tooltip is displayed for an
        element.*)
  method getShowDelayMs : int meth

    (** Sets delay in milliseconds before tooltip is hidden once the cursor 
        leaves the element.*)
  method setHideDelayMs : int -> unit meth

    (** Returns the delay in milliseconds before tooltip is hidden once the
       cursor leaves the element.*)
  method getHideDelayMs : int meth

    (** Sets tooltip message as plain text.*)
  method setText : js_string t -> unit meth

    (** Sets tooltip message as HTML markup.*)
  method setHtml : js_string t -> unit meth

    (** Sets tooltip element.*)
  method setElement : #Dom_html.element t -> unit meth

    (** Returns the tooltip message as plain text.*)
  method getText : js_string t meth

    (** The tooltip message as HTML.*)
  method getHtml : js_string t meth

    (** Returns current state of tooltip.*)
  method getState : state meth

    (** Sets whether tooltip requires the mouse to have moved or 
       * the anchor receive focus before the tooltip will be shown.*)  
  method setRequireInteraction : bool t -> unit meth

    (** Returns true if the coord is in the tooltip.*)
  method isCoordinateInTooltip : Math.coordinate t -> bool t meth

    (** Called by timer from mouse over handler. Shows tooltip if 
       * cursor is still over the same element.*)
  method maybeShow : #Dom_html.element t -> abstractPosition t opt -> unit meth

    (** Shows tooltip for a specific element.*)
  method showForElement : #Dom_html.element t -> abstractPosition t opt -> unit meth

    (** Called by timer from mouse out handler. Hides tooltip if cursor 
       * is still outside element and tooltip, or if a child of tooltip has the
       * focus.*)
  method maybeHide : #Dom_html.element t -> unit meth

    (** Returns whether tooltip element contains an active child tooltip,
       * and should thus not be hidden. When the child tooltip is hidden, it
       * will check if the parent should be hidden too.*)
  method hasActiveChild : bool meth

     (** Destroys widget and removes all event listeners.*)
  method disposeInternal : unit meth

(** Css class name for tooltip *)
  method className : js_string t prop
end

val tooltip : ((#Dom_html.element t, js_string t) Union.t opt -> js_string t opt -> domHelper t opt -> tooltip t) constr
