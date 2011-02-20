(*
   OClosure Project - 2010
   Class goog.ui.Component
   
   Class that provides the basic implementation for disposable objects. 
   
   @author Cardoso Gabriel
   @version 0.1
*)
#ifndef UI
open Js
#endif

class type component = object
  inherit Events.eventTarget

  (**
     Determines if a given element can be decorated by this type of component.
     This method should be overridden by inheriting objects.
     @param element Element to decorate.
     @return True if the element can be decorated, false otherwise.
  *)
  method canDecorate : #Dom_html.element t -> bool t meth

  (**
     Creates the initial DOM representation for the component.  The default
     implementation is to set this.element_ = div.
  *)
  method createDom : unit meth

  (** Decorates the element for the UI component. *)
  method decorate : #Dom_html.element t -> unit meth

  (**
     Disposes of the component.  Calls [exitDocument], which is expected to
     remove event handlers and clean up the component.  Propagates the call to
     the component's children, if any. Removes the component's DOM from the
     document unless it was decorated.
     @override
  *)
  method disposeInternal : unit meth

  (**
     Called when the component's element is known to be in the document. Anything
     using document.getElementById etc. should be done at this stage.
     
     If the component contains child components, this call is propagated to its
     children.
  *)
  method enterDocument : unit meth

  (**
     Called by dispose to clean up the elements and listeners created by a
     component, or by a parent component/application who has removed the
     component from the document but wants to reuse it later.

     If the component contains child components, this call is propagated to its
     children.

     It should be possible for the component to be rendered again once this method
     has been called.
  *)
  method exitDocument : unit meth

  (** Returns the child with the given ID, or null if no such child exists. *)
  method getChild : js_string t -> component t opt meth

  (** Returns the child at the given index, or null if the index is out of 
      bounds. *) 
  method getChildAt : int -> component t opt meth  

  (**
     Returns the number of children of this component.
     @return The number of children.
  *)
  method getChildCount : int meth

  (**
     Returns an array containing the IDs of the children of this component, or an
     empty array if the component has no children.
     @return Child component IDs.
  *)
  method getChildIds : js_string t js_array t meth

  (**
     Returns the DOM element into which child components are to be rendered,
     or null if the component itself hasn't been rendered yet.  This default
     implementation returns the component's root element.  Subclasses with
     complex DOM structures must override this method.
     @return Element to contain child elements (null if none).
  *)
  method getContentElement : Dom_html.element t meth

  (**
     Returns the dom helper that is being used on this component.
     @return The dom helper used on this component.
  *)
  method getDomHelper : Gdom.domHelper t meth

  (** Gets the component's element. *)
  method getElement : Dom_html.element t meth

  (**
     Helper function for returning an element in the document with a unique id
     generated using makeId().
     @param idFragment The partial id.
     @return The element with the unique id, or null if it cannot be
     found.
  *)
  method getElementByFragment : js_string t -> Dom_html.element t meth

  (**
     Helper function for returning the fragment portion of an id generated using
     makeId().
     @param id Id generated with makeId().
     @return Fragment.
  *)
  method getFragmentFromId : js_string t -> js_string t meth

  (**
     Gets the unique ID for the instance of this component.  If the instance
     doesn't already have an ID, generates one on the fly.
     @return Unique component ID.
  *)
  method getId : js_string t meth

  (** Returns the component's parent, if any. *)
  method getParent : component t opt meth

  (**
     Returns true if the component has children.
     @return True if the component has children.
  *)
  method hasChildren : bool t meth

  (**
     Returns the 0-based index of the given child component, or -1 if no such
     child is found.
     @param child The child component.
     @return 0-based index of the child component; -1 if not found.
  *)
  method indexOfChild : component t opt -> int meth

  (**
     Determines whether the component has been added to the document.
     @return TRUE if rendered. Otherwise, FALSE.
  *)
  method isInDocument : bool t meth

  (**
     Returns true if the component is rendered right-to-left, false otherwise.
     The first time this function is invoked, the right-to-left rendering property
     is set if it has not been already.
     @return Whether the control is rendered right-to-left.
  *)
  method isRightToLeft : bool t meth

  (**
     Helper function for subclasses that gets a unique id for a given fragment,
     this can be used by components to
     generate unique string ids for DOM elements
     @param idFragment A partial id.
     @return Unique element id.
  *)
  method makeId : js_string t -> js_string t meth

  (**
     Removes the child at the given index from this component, and returns it.
     Throws an error if the argument is out of bounds, or if the specified child
     isn't found in the parent.  See goog.ui.Component#removeChild for
     detailed semantics.
     
     @param index 0-based index of the child to remove.
     @param opt_unrender If true, calls [exitDocument] on the
     removed child component, and detaches its DOM from the document.
     @return The removed component, if any.
  *)
  method removeChildAt : int -> bool t opt -> component t meth

  (**
     Removes every child component attached to this one.
     
     @param opt_unrender If true, calls #exitDocument on the
     removed child components, and detaches their DOM from the document.
  *)
  method removeChildren : bool t opt -> unit meth

  (** 
      Renders the component. If a parent element is supplied, it should already
      be in the document and then the component's element will be appended to 
      it. If there is no optional parent element and the element doesn't have 
      a parentNode then it will be appended to the document body. Throws an 
      Error if the component is already rendered.
  *)
  method render : #Dom_html.element t opt -> unit meth

  (**
     Renders the component before another element. The other element should be in
     the document already.
     
     Throws an Error if the component is already rendered.
     
     @param siblingElement  Element to render the component before.
  *)
  method renderBefore : #Dom_html.element t -> unit meth

  (**
     Assigns an ID to this component instance.  It is the caller's responsibility
     to guarantee that the ID is unique.  If the component is a child of a parent
     component, then the parent component's child index is updated to reflect the
     new ID; this may throw an error if the parent already has a child with an ID
     that conflicts with the new ID.
     @param id Unique component ID.
  *)
  method setId : js_string t -> unit meth

  (**
     Sets the parent of this component to use for event bubbling.  Throws an error
     if the component already has a parent or if an attempt is made to add a
     component to itself as a child.  Callers must use [removeChild]
     or [removeChildAt] to remove components from their containers before
     calling this method.
     @param parent The parent component.
  *)
  method setParent : component t -> unit meth

  (**
     Overrides goog.events.EventTarget#setParentEventTarget to throw an
     error if the parent component is set, and the argument is not the parent.

     @param parent Parent EventTarget (null if none).
  *)
  method setParentEventTarget : Events.eventTarget t opt -> unit meth

  (**
     Set is right-to-left. This function should be used if the component needs
     to know the rendering direction during dom creation (i.e. before
     enterDocument is called and is right-to-left is set).
     @param rightToLeft Whether the component is rendered
     right-to-left.
  *)
  method setRightToLeft : bool t -> unit meth

  (**
     @return Whether the component was decorated.
  *)
  method wasDecorated : bool t meth
end


(**
   Default implementation of UI component.

   @param opt_domHelper Optional DOM helper.
*)
val component : Gdom.domHelper t opt -> component t constr

module Component : sig
  val addChild : #component t -> #component t -> bool t opt -> unit 

  (** Adds the specified component as a child of this component at the given
      0-based index.

      Both [addChild] and [addChildAt] assume the following contract
      between parent and child components:
      - the child component's element must be a descendant of the parent
      component's element, and
      - the DOM state of the child component must be consistent with the DOM
      state of the parent component (see [isInDocument]).

      In particular, [parent.addChild(child)] will throw an error if the
      child component is already in the document, but the parent isn't.

      Clients of this API may call [addChild] and [addChildAt] with
      [opt_render] set to true.  If [opt_render] is true, calling these
      methods will automatically render the child component's element into the
      parent component's element.  However, [parent.addChild(child, true)]
      will throw an error if:
      - the parent component has no DOM (i.e. [parent.getElement()] is
      null), or
      - the child component is already in the document, regardless of the
      parent's DOM state.

      Finally, this method also throws an error if the new child already has a
      different parent, or the given index is out of bounds.

      @param child The new child component.
      @param index 0-based index at which the new child component is to be
      added; must be between 0 and the current child count (inclusive).
      @param opt_render If true, the child component will be rendered
      into the parent.
      @return
  *)
  val addChildAt : #component t -> #component t -> int -> bool t opt -> unit 

  (**
     Removes the given child from this component, and returns it.  Throws an error
     if the argument is invalid or if the specified child isn't found in the
     parent component.  The argument can either be a string (interpreted as the
     ID of the child component to remove) or the child component itself.
     
     If [opt_unrender] is true, calls goog.ui.component#exitDocument
     on the removed child, and subsequently detaches the child's DOM from the
     document.  Otherwise it is the caller's responsibility to clean up the child
     component's DOM.
     
     @param child The ID of the child to remove,
     or the child component itself.
     @param opt_unrender If true, calls [exitDocument] on the
     removed child component, and detaches its DOM from the document.
     @return The removed component, if any.
  *)
  val removeChild : #component t -> #component t -> bool t opt -> component t  meth

  val removeChild_id : #component t -> js_string t -> bool t opt -> component t meth

  module State : sig
    type state = 	
	ALL | DISABLED | HOVER | ACTIVE | SELECTED | CHECKED | FOCUSED | OPENED
    type state_pre
    val state_of_state_pre : state_pre -> state
    val state_pre_of_state : state -> state_pre
  end 

  (**
     Common events fired by components so that event propagation is useful.  Not
     all components are expected to dispatch or listen for all event types.
     Events dispatched before a state transition should be cancelable to prevent
     the corresponding state change.
     @enum string
  *)
  module EventType : sig
    (** Dispatched before the component becomes visible. *)
    val _BEFORE_SHOW : js_string t
      (**
	 Dispatched after the component becomes visible.
	 NOTE(user): For goog.ui.Container, this actually fires before containers
	 are shown.  Use goog.ui.Container.EventType.AFTER_SHOW if you want an event
	 that fires after a goog.ui.Container is shown.
      *)
    val _SHOW : js_string t

    (** Dispatched before the component becomes hidden. *)
    val _HIDE : js_string t
      
    (** Dispatched before the component becomes disabled. *)
    val _DISABLE : js_string t
      
    (** Dispatched before the component becomes enabled. *)
    val _ENABLE : js_string t
      
    (** Dispatched before the component becomes highlighted. *)
    val _HIGHLIGHT : js_string t
      
    (** Dispatched before the component becomes un-highlighted. *)
    val _UNHIGHLIGHT : js_string t
      
    (** Dispatched before the component becomes activated. *)
    val _ACTIVATE : js_string t
      
    (** Dispatched before the component becomes deactivated. *)
    val _DEACTIVATE : js_string t
      
    (** Dispatched before the component becomes selected. *)
    val _SELECT : js_string t
      
    (** Dispatched before the component becomes un-selected. *)
    val _UNSELECT : js_string t
      
    (** Dispatched before a component becomes checked. *)
    val _CHECK : js_string t
      
    (** Dispatched before a component becomes un-checked. *)
    val _UNCHECK : js_string t
      
    (** Dispatched before a component becomes focused. *)
    val _FOCUS : js_string t
      
    (** Dispatched before a component becomes blurred. *)
    val _BLUR : js_string t
      
    (** Dispatched before a component is opened (expanded). *)
    val _OPEN : js_string t
      
    (** Dispatched before a component is closed (collapsed). *)
    val _CLOSE : js_string t
      
    (** Dispatched after a component is moused over. *)
    val _ENTER : js_string t
      
    (** Dispatched after a component is moused out of. *)
    val _LEAVE : js_string t
      
    (** Dispatched after the user activates the component. *)
    val _ACTION : js_string t
      
    (** Dispatched after the external-facing state of a component is changed. *)
    val _CHANGE : js_string t
  end
end
