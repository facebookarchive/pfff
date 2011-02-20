(*
   OClosure Project - 2010

   Class goog.ui.TabBar
   
   @author Oran Charles
   @version 0.2
*)

#ifndef UI
open Js
open Container
open Control
#endif
open Tools

module TabBar : sig
  (** Tab bar location relative to tab contents. *)
  type location =
      (** Above tab contents. *)
      TOP
	(** Below tab contents. *)
    |BOTTOM
	(** To the left of tab contents (to the right if the page is right-to-left). *)
    |START
	(** To the right of tab contents (to the left if the page is right-to-left).*)
    |END

  type location_pre

  val location_pre_of_location : location -> location_pre

  val location_of_location_pre : location_pre -> location
end

class type tabBar = object 
  inherit container

  (**
     Removes the tab from the tab bar.  Overrides the superclass implementation 
     by deselecting the tab being removed.  Since #removeChildAt uses
     removeChild internally, we only need to override this method.
     @param tab Tab to remove.
     @param opt_unrender Whether to call [exitDocument] on the removed tab, and 
     detach its DOM from the document (defaults to false).
     @return The removed tab, if any.
  *)
  method removeChild : (js_string t, #control t) Union.t -> bool t opt -> control t meth

  (**
     @return Tab bar location relative to tab contents.
  *)
  method getLocation : TabBar.location_pre meth

  (**
     Sets the location of the tab bar relative to tab contents.
     @param location Tab bar location relative to tab contents.
     @throws If the tab bar has already been rendered.
  *)
  method setLocation : TabBar.location_pre -> unit meth

  (**
     @return Whether keyboard navigation should change the selected tab, or just move the highlight.
  *)
  method isAutoSelectTabs : bool t meth

  (**
     Enables or disables auto-selecting tabs using the keyboard.  If auto-select is enabled, keyboard navigation switches tabs immediately, otherwise it just moves the highlight.
     @param enable Whether keyboard navigation should change the selected tab, or just move the highlight.
  *)
  method setAutoSelectTabs : bool t -> unit meth

  (**
     Highlights the tab at the given index in response to a keyboard event.
     Overrides the superclass implementation by also selecting the tab if
     isAutoSelectTabs returns true.
     @param index Index of tab to highlight.
  *)
  method setHighlightedIndexFromKeyEvent : int -> unit meth

  (**
     @return The currently selected tab (null if none).
  *)
  method getSelectedTab : control t opt meth

  (**
     Selects the given tab.
     @param tab Tab to select (null to select none).
  *)
  method setSelectedTab : #control t opt -> unit meth

  (**
     @return Index of the currently selected tab (-1 if none).
  *)
  method getSelectedTabIndex : int meth

  (**
     Selects the tab at the given index.
     @param index Index of the tab to select (-1 to select none).
  *)
  method setSelectedTabIndex : int -> unit meth

  (**
     If the specified tab is the currently selected tab, deselects it, and
     selects the closest selectable tab in the tab bar (first looking before,
     then after the deselected tab).  Does nothing if the argument is not the
     currently selected tab.  Called internally when a tab is removed, hidden,
     or disabled, to ensure that another tab is selected instead.
     @param tab Tab to deselect (if any).
  *)
  method deselectIfSelected : #control t-> unit meth

  (**
     Returns true if the tab is selectable, false otherwise.  Only visible and
     enabled tabs are selectable.
     @param tab Tab to check.
     @return Whether the tab is selectable.
  *)
  method isSelectableTab : #control t -> bool t meth

  (**
     Handles events dispatched by tabs as they become selected.
     @param e Select event to handle.
     @protected
  *)
  method handleTabSelect : #Events.event t -> unit meth

  (**
     Handles events dispatched by tabs as they become deselected.
     @param e Unselect event to handle.
  *)
  method handleTabUnselect : #Events.event t -> unit meth

  (**
     Handles events displayed by tabs.
     @param e Disable event to handle.
  *)
  method handleTabDisable : #Events.event t -> unit meth

  (**
     Handles events displayed by tabs.
     @param e Hide event to handle.
  *)
  method handleTabHide : #Events.event t -> unit meth

  (**
     Handles focus events dispatched by the tab bar's key event target.  If no tab is currently highlighted, highlights the selected tab or the first tab if no tab is selected either.
     @param e Focus event to handle.
  *)
  method handleFocus : #Events.event t -> unit meth

  (**
     Returns the goog.ui.Container.Orientation that is implied by the givengoog.ui.TabBar.Location.
     @param location Tab bar location.
     @return Corresponding orientation.
  *)
  method getOrientationFromLocation : TabBar.location_pre -> Container.orientation_pre meth

end

class type ['cont] tabBarRenderer = object
  inherit ['cont] containerRenderer

  (**
     Returns the CSS class name to be applied to the root element of all tab bars rendered or decorated using this renderer.
     @return  Renderer-specific CSS class name.
  *)
  method getCssClass : js_string t meth

  (**
     Sets the tab bar's state based on the given CSS class name, encountered
     during decoration.  Overrides the superclass implementation by recognizing class names representing tab bar orientation and location.
     @param tabBar Tab bar to configure.
     @param className CSS class name.
     @param baseClass Base class name used as the root of state-specific class names (typically the renderer's own class name).
  *)
  method setStateFromClassName : 'cont t -> js_string t -> js_string t -> unit meth

  (**
     Returns all CSS class names applicable to the tab bar, based on its state.
     Overrides the superclass implementation by appending the location-specific class name to the list.
     @param tabBar Tab bar whose CSS classes are to be returned.
     @return Array of CSS class names applicable to the tab bar.
  *)
  method getClassNames : 'cont t -> js_string t js_array t meth

end

(**
   Default renderer for goog.ui.TabBar, based on the TabPane code.  The tab bar's DOM structure is determined by its orientation and location relative to tab contents.  For example, a horizontal tab bar located above tab contents looks like this:
   <pre>
   <div class="goog-tab-bar goog-tab-bar-horizontal goog-tab-bar-top">
   ...(tabs here)...
   </div>
   </pre>
*)
val tabBarRenderer : tabBar #tabBarRenderer t constr


(**
   Tab bar UI component.  A tab bar contains tabs, rendered above, below,
   before, or after tab contents.  Tabs in tab bars dispatch the following
   events:
   - goog.ui.Component.EventType.ACTION when activated via the
   keyboard or the mouse,
   - goog.ui.Component.EventType.SELECT when selected, and
   - goog.ui.Component.EventType.UNSELECT when deselected.
   Clients may listen for all of the above events on the tab bar itself, and
   refer to the event target to identify the tab that dispatched the event.
   When an unselected tab is clicked for the first time, it dispatches both a
   SELECT event and an ACTION event; subsequent clicks on an
   already selected tab only result in events.

   @param opt_location Tab bar location; defaults to goog.ui.TabBar.Location.TOP.
   @param opt_renderer Renderer used to render or decorate the container; defaults to goog.ui.TabBarRenderer.
   @param opt_domHelper DOM helper, used for document interaction.
*)
val tabBar : (TabBar.location_pre opt -> tabBar #tabBarRenderer t opt -> Gdom.domHelper t opt -> tabBar t) constr 

