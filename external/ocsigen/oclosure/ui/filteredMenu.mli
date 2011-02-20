(*
   OClosure Project - 2010
   Class goog.ui.FilteredMenu
   
   @author Cardoso Gabriel
   @version 0.2
*)
#ifndef UI
open Js
open Menu
open Component
open MenuItem
#endif

class type filteredMenu = object
  inherit menu

(** @inheritDoc *)
  method createDom : unit meth

(** @inheritDoc *)
  method decorateInternal : #Dom_html.element t -> unit meth

(** @inheritDoc *)
  method disposeInternal : unit meth

(**
   @return Whether multiple items can be entered comma separated.
 *)
  method getAllowMultiple : bool t meth


(** @inheritDoc *)
  method getContentElement : Dom_html.element t meth

(**
   Gets a list of items entered in the search box.
   @return The entered items.
 *)
  method getEnteredItems : js_string t js_array t meth


(**
   Returns the filter string.
   @return Current filter or an an empty string.
 *)
  method getFilter : js_string t meth
 

(**
   Returns the index of first item that is affected by the filter.
   @return Index of first item that is affected by filter.
 *)
  method getFilterFromIndex : int meth

(**
   Returns the filter input element.
   @return Input element.
 *)
  method getFilterInputElement : Dom_html.element t meth


(**
   @return The filter label.
 *)
  method getFilterLabel : js_string t meth

(**
   Handles filter input events.
   @param e The event object.
 *)
  method handleFilterEvent : Events.browserEvent t -> unit meth

(**
   Handles the menu's behavior for a key event. The highlighted menu item will
   be given the opportunity to handle the key behavior.
   @param e A browser event.
   @return Whether the event was handled.
 *)
  method handleKeyEvent : Events.keyEvent t -> bool t meth

(**
   Returns whether the specified child should be affected (shown/hidden) by the
   filter criteria.
   @param child Menu item to check.
   @return Whether the menu item is persistent.
 *)
  method hasPersistentVisibility : #component t -> bool t meth

(**
   Sets whether multiple items can be entered comma separated.
   @param b Whether multiple items can be entered.
 *)
  method setAllowMultiple : bool t -> unit meth

(**
   Sets the filter string.
   @param str Filter string.
 *)
  method setFilter : js_string t opt -> unit meth

(**
   Sets the index of first item that should be affected by the filter. Menu
   items with a lower index will not be affected by the filter.
   @param index Index of first item that should be affected by filter.
 *)
  method setFilterFromIndex : int -> unit meth

(**
   Sets the filter label (the label displayed in the filter input element if no
   text has been entered).
   @param label Label text.
 *)
  method setFilterLabel : js_string t opt -> unit meth


(**
   Sets the highlighted index, unless the HIGHLIGHT event is intercepted and
   cancelled.  -1 = no highlight. Also scrolls the menu item into view.
   @param index Index of menu item to highlight.
 *)
  method setHighlightedIndex : int -> unit meth

(**
   Sets whether the specified child should be affected (shown/hidden) by the
   filter criteria.
   @param child Menu item to change.
   @param persistent Whether the menu item should be persistent.
 *)
  method setPersistentVisibility : #menuItem t -> bool t -> unit meth


(** @inheritDoc *)
  method setVisible : bool t -> bool t opt -> bool t meth
end

(**
   Filtered menu class.
   @param opt_renderer Renderer used to render filtered
   menu; defaults to goog.ui.MenuRenderer.
   @param opt_domHelper Optional DOM helper.
 *)
val filteredMenu : (filteredMenu #menuRenderer t opt -> 
  Gdom.domHelper t opt -> filteredMenu t) constr
