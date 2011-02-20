(* 
    OClosure Project - 2010 
    Class goog.ui.PopupColorPicker

    @author Cardoso Gabriel
    @version 0.2
*)
#ifndef UI
open Js
open Component
open PopupBase
open ColorPicker
#endif

class type popupColorPicker = object
  inherit component

(**
   Add an array of colors to the colors displayed by the color picker.
   Does not add duplicated colors.
   @param colors The array of colors to be added.
 *)
  method addColors : js_string t js_array t -> unit meth

(**
   Attaches the popup color picker to an element.
   @param element The element to attach to.
 *)
  method attach : #Dom_html.element t -> unit meth

(**
   ColorPickers cannot be used to decorate pre-existing html, since the
   structure they build is fairly complicated.
   @param element Element to decorate.
   @return Returns always false.
 *)
  method canDecorate : #Dom_html.element t -> bool t meth

(**
   Clear the colors displayed by the color picker.
 *)
  method clearColors : unit meth

(** @inheritDoc *)
  method createDom : unit meth

(**
   Detatches the popup color picker from an element.
   @param element The element to detach from.
 *)
  method detach : #Dom_html.element t -> unit meth

(** @inheritDoc *)
  method disposeInternal : unit meth

(**
   @return Whether the color picker can automatically move focus to
       its key event target when it is set to visible.
 *)
  method getAllowAutoFocus : bool t meth

(**
   Returns whether the Popup dismisses itself when the user clicks outside of
   it.
   @return Whether the Popup autohides on an external click.
 *)
  method getAutoHide : bool t meth

(**
   Returns the region inside which the Popup dismisses itself when the user
   clicks, or null if it was not set. Null indicates the entire document is
   the autohide region.
   @return The DOM element for autohide, or null if it hasn't been
       set.
 *)
  method getAutoHideRegion : Dom_html.element t meth

(**
   @return The color picker instance.
 *)
  method getColorPicker : colorPicker t meth

(**
   @return The last element that triggered the popup.
 *)
  method getLastTarget : #Dom_html.element t meth

 (**
   Returns the goog.ui.PopupBase from this picker. Returns null if the
   popup has not yet been created.
   NOTE: This should *ONLY* be called from tests. If called before createDom(),
   this should return null.
   @return The popup or null if it hasn't been created.
 *)
 method getPopup : popupBase t opt meth

(**
   @return Whether the picker remembers the last selected color
       between popups.
 *)
  method getRememberSelection : bool t meth

(**
   Gets the color that is currently selected in this color picker.
   @return The hex string of the color selected, or null if no
       color is selected.
 *)
  method getSelectedColor : js_string t opt meth

(**
   Gets whether the colorpicker is in toggle mode
   @return toggle.
 *)
  method getToggleMode : bool t meth

(**
   Sets whether the color picker can automatically move focus to its key event
   target when it is set to visible.
   @param allow Whether to allow auto focus.
 *)
  method setAllowAutoFocus : bool t -> unit meth

(**
   Sets whether the Popup dismisses itself when the user clicks outside of it -
   must be called after the Popup has been created (in createDom()),
   otherwise it does nothing.
   @param autoHide Whether to autohide on an external click.
 *)
  method setAutoHide : bool t -> unit meth

(**
   Sets the region inside which the Popup dismisses itself when the user
   clicks - must be called after the Popup has been created (in createDom()),
   otherwise it does nothing.
   @param element The DOM element for autohide.
 *)
  method setAutoHideRegion : #Dom_html.element t -> unit meth

(**
   Sets whether the color picker can accept focus.
   @param focusable True iff the color picker can accept focus.
 *)
  method setFocusable : bool t -> unit meth

 (**
   Set the pinned corner of the popup.
   @param corner The corner of the popup which is
       pinned to the attaching element.
 *)
 method setPinnedCorner : Positioning.corner -> unit meth

(**
   Sets which corner of the attaching element this popup shows up.
   @param corner The corner of the attaching element
       where to show the popup.
 *)
  method setPopupCorner : Positioning.corner -> unit meth

(**
   Sets whether the picker remembers the last selected color between popups.
   @param remember Whether to remember the selection.
 *)
  method setRememberSelection : bool t -> unit meth

(**
   Sets whether the color picker should toggle off if it is already open.
   @param toggle The new toggle mode.
 *)
  method setToggleMode : bool t -> unit meth
end

(**
   Popup color picker widget.
   @param opt_domHelper Optional DOM helper.
   @param opt_colorPicker Optional color picker to use
       for this popup.
 *)
val popupColorPicker : (Gdom.domHelper t opt -> colorPicker t opt 
			  -> popupColorPicker t) constr 
