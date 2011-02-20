(* 
   OClosure Project - 2010 
   Class goog.ui.PopupBase
   
   @author Esther Baruk
   @version 0.2
*)

#ifndef UI
open Js
#endif

module PopupBase : sig
  type _type = TOGGLE_DISPLAY | MOVE_OFFSCREEN

  type type_pre

  val type_pre_of_type : _type -> type_pre
  
  val type_of_type_pre : type_pre -> _type
end


class type popupBase = object
  inherit Events.eventTarget
    (** Returns the type of popup this is.*)
  method getType : js_string t meth
      
    (** Specifies the type of popup to use.*)
  method setType : js_string t -> unit meth
      
    (** Returns whether the popup should hide itself asynchronously 
       * using a timeout instead of synchronously.*)
  method shouldHideAsync : bool t meth
      
    (** Sets whether the popup should hide itself asynchronously 
       * using a timeout instead of synchronously.*)
  method setShouldHideAsync : bool t -> unit meth
      
    (** Returns the dom element that should be used for the popup.*)
  method getElement : Dom_html.element t meth
      
    (** Specifies the dom element that should be used for the popup.*)
  method setElement : #Dom_html.element t -> unit meth
      
    (** Returns whether the Popup dismisses itself when the user clicks
       * outside of it.*)
  method getAutoHide : bool t meth
      
    (** Sets whether the Popup dismisses itself when the user clicks outside of it.*)
  method setAutoHide : bool t -> unit meth
      
    (** Returns whether the Popup autohides on the escape key.*)
  method getHideOnEscape : bool t meth
      
    (** Sets whether the Popup dismisses itself on the escape key.*)
  method setHideOnEscape : bool t -> unit meth
      
    (** Returns whether cross iframe dismissal is enabled.*)
  method getEnableCrossIframeDismissal : bool t meth
      
    (** Sets whether clicks in other iframes should dismiss this popup.  
       * In some cases it should be disabled, because it can cause spurious*)
  method setEnableCrossIframeDismissal : bool t -> unit meth
      
    (** Returns the region inside which the Popup dismisses itself when 
       * the user clicks, or null if it's the entire document.*)
  method getAutoHideRegion : bool t meth
      
    (** Sets the region inside which the Popup dismisses itself when 
       * the user clicks.*)
  method setAutoHideRegion : bool t -> unit meth
      
    (** Returns the time when the popup was last shown. Time in ms since
       * epoch or -1 if the popup was never shown.*)
  method getLastShowTime : float t meth
      
    (** Returns the time when the popup was last hidden. Time in ms since
       * epoch or -1 if the popup was never hidden or is currently showing.*)
  method getLastHideTime : float meth
      
    (** Returns whether the popup is currently visible.*)
  method isVisible : bool t meth
      
    (** Returns whether the popup is currently visible or was visible 
       * within about 150 ms ago. This is used by clients to handle a 
       * very specific, but common, popup scenario. The button that 
       * launches the popup should close the popup on mouse down if 
       * the popup is already open. The problem is that the popup closes 
       * itself during the capture phase of the mouse down and thus the 
       * button thinks it's hidden and this should show it again. This method 
       * provides a good heuristic for clients. Typically in their event 
       * handler they will have code that is:
       *     if (menu.isOrWasRecentlyVisible()) then
               menu.setVisible(false) meth
       *     else 
       *          ... (* code to position menu and initialize other state *)
               menu.setVisible(true) meth
       *     *)
  method isOrWasRecentlyVisible : bool t meth
      
    (** Sets whether the popup should be visible.*)
  method setVisible : bool t -> unit meth
      
    (** Shows the popup element.*)
  method showPopupElement : unit meth
      
    (** Called before the popup is shown. Derived classes can 
       * override to hook this event but should make sure to call 
       * the parent class method.*)
  method onBeforeShow : bool t meth
end
 
val popubBase : (#Dom_html.element t -> PopupBase.type_pre opt 
  -> popupBase t) constr
