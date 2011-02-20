(* 
    OClosure Project - 2010 
    Class goog.ui.PopupMenu
    
    @author Gabriel Cardoso
    @version 0.2
*)
#ifndef UI
open Js
open Menu
#endif

class type popupMenu = object
  inherit menu

(**
   Attaches the menu to a new popup position and anchor element.  A menu can
   only be attached to an element once, since attaching the same menu for
   multiple positions doesn't make sense.
   @param element Element whose click event should trigger the menu.
   @param opt_targetCorner Corner of the target that
       the menu should be anchored to.
   @param opt_menuCorner Corner of the menu that
       should be anchored.
   @param opt_contextMenu Whether the menu should show on
       goog.events.EventType.CONTEXTMENU events, false if it should
       show on goog.events.EventType.MOUSEDOWN events. Default is
       MOUSEDOWN.
   @param opt_margin Margin for the popup used in positioning
       algorithms.
 *)
  method attach : #Dom_html.element t -> Positioning.corner opt -> 
    Positioning.corner opt -> bool t opt -> Math.box t opt -> unit meth

(**
   Decorate an existing HTML structure with the menu. Menu items will be
   constructed from elements with classname 'goog-menuitem', separators will be
   made from HR elements.
   @param element Element to decorate.
 *)
  method decorateInternal : #Dom_html.element t -> unit meth

(**
   Detaches a menu from a given element.
   @param element Element whose click event should trigger the menu.
 *)
  method detach : #Dom_html.element t -> unit meth

(**
   Detaches all listeners
 *)
  method detachAll : unit meth

 (** @inheritDoc *)
 method disposeInternal : unit meth

(**
   The menu has been added to the document.
 *)
  method enterDocument : unit meth

(**
   @return The current element where the popup is anchored, if it's
       visible.
 *)
  method getAttachedElement : Dom_html.element t meth
 
(**
   Gets whether the menu is in toggle mode
   @return toggle.
 *)
  method getToggleMode : bool t meth

(**
   Hides the menu.
 *)
  method hide : unit meth

(**
   Sets whether the menu should toggle if it is already open.  For context
   menus this should be false, for toolbar menus it makes more sense to be true.
   @param toggle The new toggle mode.
 *)
  method setToggleMode : bool t -> unit meth

(**
   Shows the menu immediately at the given client coordinates.
   @param x The client-X associated with the show event.
   @param y The client-Y associated with the show event.
   @param opt_menuCorner Corner of the menu that
       should be anchored.
 *)
  method showAt : int -> int -> Positioning.corner opt -> unit meth

(**
   Shows the menu immediately attached to the given element
   @param element The element to show at.
   @param targetCorner The corner of the target to
       anchor to.
   @param opt_menuCorner Corner of the menu that
       should be anchored.
 *)
  method showAtElement : #Dom_html.element t -> Positioning.corner ->
    Positioning.corner opt -> unit meth

(**
   Show the menu using given positioning object.
   @param position The positioning instance.
   @param opt_menuCorner The corner of the menu to be
       positioned.
   @param opt_margin A margin specified in pixels.
   @param opt_anchor The element which acts as visual anchor for this
       menu.
 *)
  method showWithPosition : Positioning.abstractPosition t -> 
    Positioning.corner opt -> Math.box t opt -> #Dom_html.element t opt ->
    unit meth
end

(**
   A basic menu class.
   @param opt_domHelper Optional DOM helper.
   @param opt_renderer Renderer used to render or
       decorate the container; defaults to goog.ui.MenuRenderer.
 *)
val popupMenu : (Gdom.domHelper t opt -> popupMenu #menuRenderer t opt 
		   -> popupMenu t) constr
  
