(*
   OClosure Project - 2010

   Class goog.ui.Container
   
   @author Cardoso Gabriel/Oran Charles
   @version 0.2
*)

#ifndef UI
open Component
open Control
open Js
#endif
open Tools

module Container : sig
  type orientation =
    | HORIZONTAL
    | VERTICAL
	
  type orientation_pre

  val orientation_pre_of_orientation : orientation -> orientation_pre

  val orientation_of_orientation_pre : orientation_pre -> orientation 
end

class type container = object
  inherit component

  (**
     Returns the DOM element on which the container is listening for keyboard
     events (null if none).
     @return Element on which the container is listening for key
     events.
  *)
  method getKeyEventTarget : Dom_html.element t meth


  (**
     Attaches an element on which to listen for key events.
     @param element The element to attach, or null/undefined
     to attach to the default element.
  *)
  method setKeyEventTarget : #Dom_html.element t optdef -> unit meth 


  (**
     Returns the keyboard event handler for this container, lazily created the
     first time this method is called.  The keyboard event handler listens for
     keyboard events on the container's key event target, as determined by its
     renderer.
     @return Keyboard event handler for this container.
  *)
  method getKeyHandler : Events.keyHandler t meth 

(*
  (**
     Returns the renderer used by this container to render itself or to decorate
     an existing element.
     @return Renderer used by the container.
  *)
  method getRenderer : container containerRenderer t meth 

  (**
     Registers the given renderer with the container.  Changing renderers after
     the container has already been rendered or decorated is an error.
     @param renderer Renderer used by the container.
  *)
  method setRenderer : container containerRenderer t -> unit meth 
*)

  (**
     Creates the container's DOM.  Overrides goog.ui.Component#createDom.
  *)
  method createDom : unit meth 

  (**
     Returns the DOM element into which child components are to be rendered,
     or null if the container itself hasn't been rendered yet.  Overrides
     goog.ui.Component#getContentElement by delegating to the renderer.
     @return Element to contain child elements (null if none).
  *)
  method getContentElement : Dom_html.element t meth 


  (**
     Returns true if the given element can be decorated by this container.
     Overrides goog.ui.Component#canDecorate.
     @param element Element to decorate.
     @return True iff the element can be decorated.
  *)
  method canDecorate : #Dom_html.element t -> bool t meth 


  (**
     Decorates the given element with this container. Overrides
     goog.ui.Component#decorateInternal.  Considered protected.
     @param element Element to decorate.
  *)
  method decorateInternal : #Dom_html.element t -> unit meth 


  (**
     Configures the container after its DOM has been rendered, and sets up event
     handling.  Overrides goog.ui.Component#enterDocument.
  *)
  method enterDocument : unit meth 

  (**
     Cleans up the container before its DOM is removed from the document, and
     removes event handlers.  Overrides goog.ui.Component#exitDocument.
  *)
  method exitDocument : unit meth 

  (**
     Handles ENTER events raised by child controls when they are navigated to.
     @param e ENTER event to handle.
     @return Whether to prevent handleMouseOver from handling
     the event.
  *)
  method handleEnterItem : Events.event t -> bool t meth 

  (**
     Handles HIGHLIGHT events dispatched by items in the container when
     they are highlighted.
     @param e Highlight event to handle.
  *)
  method handleHighlightItem : #Events.event t -> unit meth 


  (**
     Handles UNHIGHLIGHT events dispatched by items in the container when
     they are unhighlighted.
     @param e Unhighlight event to handle.
  *)
  method handleUnHighlightItem : #Events.event t -> unit meth 


  (**
     Handles OPEN events dispatched by items in the container when they are
     opened.
     @param e Open event to handle.
  *)
  method handleOpenItem : #Events.event t -> unit meth 


  (**
     Handles CLOSE events dispatched by items in the container when they are
     closed.
     @param e Close event to handle.
  *)
  method handleCloseItem : #Events.event -> unit meth 


  (**
     Handles mousedown events over the container.  The default implementation
     sets the "mouse button pressed" flag and, if the container is focusable,
     grabs keyboard focus.
     @param e Mousedown event to handle.
  *)
  method handleMouseDown : #Events.browserEvent t -> unit meth 


  (**
     Handles mouseup events over the document.  The default implementation
     clears the "mouse button pressed" flag.
     @param e Mouseup event to handle.
  *)
  method handleDocumentMouseUp : #Events.browserEvent t -> unit meth 


  (**
     Handles mouse events originating from nodes belonging to the controls hosted
     in the container.  Locates the child control based on the DOM node that
     dispatched the event, and forwards the event to the control for handling.
     @param e Mouse event to handle.
  *)
  method handleChildMouseEvents : #Events.browserEvent t -> unit meth 

  (**
     Returns the child control that owns the given DOM node, or null if no such
     control is found.
     @param node DOM node whose owner is to be returned.
     @return Control hosted in the container to which the node
     belongs (if found).
  *)
  method getOwnerControl : #Dom.node t -> #control t opt -> unit meth 

  (**
     Handles focus events raised when the container's key event target receives
     keyboard focus.
     @param e Focus event to handle.
  *)
  method handleFocus : #Events.event t -> unit meth 

  (**
     Handles blur events raised when the container's key event target loses
     keyboard focus.  The default implementation clears the highlight index.
     @param e Blur event to handle.
  *)
  method handleBlur : #Events.browserEvent t -> unit meth 

  (**
     Attempts to handle a keyboard event, if the control is enabled, by calling
     handleKeyEventInternal.  Considered protected; should only be used
     within this package and by subclasses.
     @param e Key event to handle.
     @return Whether the key event was handled.
  *)
  method handleKeyEvent : Events.keyEvent t -> bool t meth 

  (**
     Attempts to handle a keyboard event; returns true if the event was handled,
     false otherwise.  If the container is enabled, and a child is highlighted,
     calls the child control's [handleKeyEvent] method to give the control
     a chance to handle the event first.
     @param e Key event to handle.
     @return Whether the event was handled by the container (or one of
     its children).
  *)
  method handleKeyEventInternal : Events.keyEvent t -> bool t meth 

  (**
     Adds the specified control as the last child of this container.  See
     goog.ui.Container#addChildAt for detailed semantics.
     @param child The new child control.
     @param opt_render Whether the new child should be rendered
     immediately after being added (defaults to false).
  *)
  method addChild : #control t -> bool t opt -> unit meth 


  (**
     Overrides goog.ui.Container#getChild to make it clear that it
     only returns goog.ui.Controls.
     @param id Child component ID.
     @return The child with the given ID; null if none.
  *)
  method getChild : js_string t -> component t opt meth


  (**
     Overrides goog.ui.Container#getChildAt to make it clear that it
     only returns goog.ui.Controls.
     @param index 0-based index.
     @return The child with the given ID; null if none.
  *)
  method getChildAt : int -> component t opt meth


  (**
     Adds the control as a child of this container at the given 0-based index.
     Overrides goog.ui.Component#addChildAt by also updating the
     container's highlight index.  Since goog.ui.Component#addChild uses
     addChildAt internally, we only need to override this method.
     @param control New child.
     @param index Index at which the new child is to be added.
     @param opt_render Whether the new child should be rendered
     immediately after being added (defaults to false).
  *)
  method addChildAt : control t -> int -> bool t opt -> unit meth 

  (**
     Removes a child control.  Overrides goog.ui.Component#removeChild by
     updating the highlight index.  Since goog.ui.Component#removeChildAt
     uses #removeChild internally, we only need to override this method.
     @param control The ID of the child to remove, or
     the control itself.
     @param opt_unrender Whether to call [exitDocument] on the
     removed control, and detach its DOM from the document (defaults to
     false).
     @return The removed control, if any.
  *)
  method removeChild : (js_string t,#control t) Tools.Union.t -> bool t opt -> control t meth  

  (**
     Returns the container's orientation.
     @return Container orientation.
  *)
  method getOrientation : Container.orientation_pre opt meth 


  (**
     Sets the container's orientation.
     @param orientation Container orientation.
  *)
  method setOrientation : Container.orientation_pre -> unit meth 


  (**
     Returns true if the container's visibility is set to visible, false if
     it is set to hidden.  A container that is set to hidden is guaranteed
     to be hidden from the user, but the reverse isn't necessarily true.
     A container may be set to visible but can otherwise be obscured by another
     element, rendered off-screen, or hidden using direct CSS manipulation.
     @return Whether the container is set to be visible.
  *)
  method isVisible : bool t  meth 


  (**
     Shows or hides the container.  Does nothing if the container already has
     the requested visibility.  Otherwise, dispatches a SHOW or HIDE event as
     appropriate, giving listeners a chance to prevent the visibility change.
     @param visible Whether to show or hide the container.
     @param opt_force If true, doesn't check whether the container
     already has the requested visibility, and doesn't dispatch any events.
     @return Whether the visibility was changed.
  *)
  method setVisible : bool t -> bool t opt -> bool t meth 


  (**
     Returns true if the container is enabled, false otherwise.
     @return Whether the container is enabled.
  *)
  method isEnabled : bool t meth 


  (**
     Enables/disables the container based on the [enable] argument.
     Dispatches an [ENABLED or DISABLED] event prior to changing
     the container's state, which may be caught and canceled to prevent the
     container from changing state.  Also enables/disables child controls.
     @param enable Whether to enable or disable the container.
  *)
  method setEnabled : bool t -> unit meth 


  (**
     Returns true if the container is focusable, false otherwise.  The default
     is true.  Focusable containers always have a tab index and allocate a key
     handler to handle keyboard events while focused.
     @return Whether the component is focusable.
  *)
  method isFocusable : bool t meth 


  (**
     Sets whether the container is focusable.  The default is true.  Focusable
     containers always have a tab index and allocate a key handler to handle
     keyboard events while focused.
     @param focusable Whether the component is to be focusable.
  *)
  method setFocusable : bool t -> unit meth 


  (**
     Returns true if the container allows children to be focusable, false
     otherwise.  Only effective if the container is not focusable.
     @return Whether children should be focusable.
  *)
  method isFocusableChildrenAllowed : bool t meth 


  (**
     Sets whether the container allows children to be focusable, false
     otherwise.  Only effective if the container is not focusable.
     @param focusable Whether the children should be focusable.
  *)
  method setFocusableChildrenAllowed : bool t -> unit meth 

  (**
     Returns the index of the currently highlighted item (-1 if none).
     @return Index of the currently highlighted item.
  *)
  method getHighlightedIndex : int meth 

  (**
     Highlights the item at the given 0-based index (if any).  If another item
     was previously highlighted, it is un-highlighted.
     @param index Index of item to highlight (-1 removes the current
     highlight).
  *)
  method setHighlightedIndex : int -> unit meth 

  (**
     Highlights the given item if it exists and is a child of the container;
     otherwise un-highlights the currently highlighted item.
     @param item Item to highlight.
  *)
  method setHighlighted : #control t -> unit meth 


  (**
     Returns the currently highlighted item (if any).
     @return Highlighted item (null if none).
  *)
  method getHighlighted : control t meth 


  (**
     Highlights the first highlightable item in the container
  *)
  method highlightFirst : unit meth 


  (**
     Highlights the last highlightable item in the container.
  *)
  method highlightLast : unit meth 


  (**
     Highlights the next highlightable item (or the first if nothing is currently
     highlighted).
  *)
  method highlightNext : unit meth 


  (**
     Highlights the previous highlightable item (or the last if nothing is
     currently highlighted).
  *)
  method highlightPrevious : unit meth 


  (**
     Helper function that manages the details of moving the highlight among
     child controls in response to keyboard events.
     @param fn Function that accepts the
     current and maximum indices, and returns the next index to check.
     @param startIndex Start index.
     @return Whether the highlight has changed.
     
  *)
  method highlightHelper : (int -> int -> int ) callback -> int -> bool t meth

  (**
     Returns whether the given item can be highlighted.
     @param item The item to check.
     @return Whether the item can be highlighted.
  *)
  method canHighlightItem : #control t -> bool t meth 

  (**
     Helper method that sets the highlighted index to the given index in 
     response to a keyboard event.  The base class implementation simply calls 
     the setHighlightedIndex method, but subclasses can override this
     behavior as needed.
     @param index Index of item to highlight.
     
  *)
  method setHighlightedIndexFromKeyEvent : int -> unit meth 


  (**
     Returns true if the mouse button is pressed, false otherwise.
     @return Whether the mouse button is pressed.
  *)
  method isMouseButtonPressed : bool t meth 

  (**
     Sets or clears the "mouse button pressed" flag.
     @param pressed Whether the mouse button is presed.
  *)
  method setMouseButtonPressed : bool t -> unit meth 
end

and ['cont] containerRenderer = object
  constraint 'cont = #container
  method canDecorate : #Dom_html.element t -> bool t meth

  method createDom : 'cont t -> Dom_html.element t meth

  method decorate : 'cont t -> #Dom_html.element t -> Dom_html.element t meth

  method decorateChildren : 'cont t -> #Dom_html.element t -> Dom_html.element t meth

  method enableTabIndex : #Dom_html.element t -> bool t meth

  method getAriaRole : Gdom.A11y.role_pre optdef meth

  method getClassNames : 'cont t -> js_string t js_array t meth

  method getContentElement : #Dom_html.element t -> Dom_html.element t meth

  method getCssClass : js_string t meth

  method getDecoratorForChild : #Dom_html.element t -> control t opt meth

  method getDefaultOrientation : Container.orientation_pre meth
  
  method getKeyEventTarget : 'cont t -> Dom_html.element t opt meth

  method hasTabIndex : #Dom_html.element t -> bool t meth

  method initializeDom : 'cont t -> unit meth
end

val containerRenderer : (#container containerRenderer t) constr

val container : (Container.orientation_pre opt -> container #containerRenderer t opt -> Gdom.domHelper t opt -> container t) constr

