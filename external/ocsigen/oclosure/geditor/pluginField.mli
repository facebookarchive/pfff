open Js

class type plugin = object
  inherit Events.eventTarget

(**
 * @return The dom helper object associated with the
 *     currently active field.
 *)
  method getFieldDomHelper : Gdom.domHelper t opt meth

(**
 * Registers the field object for use with this plugin.
 * @param fieldObject The editable field object.
 *)
  method registerFieldObject : field t -> unit meth

(**
 * Unregisters and disables this plugin for the current field object.
 * @param fieldObj The field object. For single-field
 *     plugins, this parameter is ignored.
 *)
  method unregisterFieldObject : field t -> unit meth

(**
 * Enables this plugin for the specified, registered field object. A field
 * object should only be enabled when it is loaded.
 * @param fieldObject The field object.
 *)
  method enable : field t -> unit meth

(**
 * Disables this plugin for the specified, registered field object.
 * @param fieldObject The field object.
 *)
  method disable : field t -> unit meth

(**
 * Returns whether this plugin is enabled for the field object.
 *
 * @param fieldObject The field object.
 * @return Whether this plugin is enabled for the field object.
 *)
  method isEnabled : field t -> bool t meth

(**
 * Set if this plugin should automatically be disposed when the registered
 * field is disposed.
 * @param autoDispose Whether to autoDispose.
 *)
  method setAutoDispose : bool t -> unit meth

(**
 * @return Whether or not this plugin should automatically be disposed
 *     when it's registered field is disposed.
 *)
  method isAutoDispose : bool t meth

(**
 * @return If true, field will not disable the command
 *     when the field becomes uneditable.
 *)
  method activeOnUneditableFields : bool t meth

(**
 * @param command The command to check.
 * @return If true, field will not dispatch change events
 *     for commands of this type. This is useful for "seamless" plugins like
 *     dialogs and lorem ipsum.
 *)
  method isSilentCommand : js_string t -> bool t meth

(** @inheritDoc *)
  method disposeInternal : unit meth

(**
 * Handles execCommand. This default implementation handles dispatching
 * BEFORECHANGE, CHANGE, and SELECTIONCHANGE events, and calls
 * execCommandInternal to perform the actual command. Plugins that want to
 * do their own event dispatching should override execCommand, otherwise
 * it is preferred to only override execCommandInternal.
 *
 * This version of execCommand will only work for single field plugins.
 * Multi-field plugins must override execCommand.
 *
 * @param command The command to execute.
 * @param var_args Any additional parameters needed to
 *     execute the command.
 * @return The result of the execCommand, if any.
 *)
(*  method execCommand : *)

(**
 * Whether the string corresponds to a command this plugin handles.
 * @param command Command string to check.
 * @return Whether the plugin handles this type of command.
 *)
  method isSupportedCommand : js_string t -> bool t meth
end

and field = object
  inherit Events.eventTarget

(**
 * @return Whether we're in modal interaction mode. When this
 *     returns true, another plugin is interacting with the field contents
 *     in a synchronous way, and expects you not to make changes to
 *     the field's DOM structure or selection.
 *)
  method inModalMode : bool t meth

(**
 * @param inModalMode Sets whether we're in modal interaction mode.
 *)
  method setModalMode : bool t -> unit meth

(**
 * Returns a string usable as a hash code for this field. For field's
 * that were created with an id, the hash code is guaranteed to be the id.
 * TODO(user): I think we can get rid of this.  Seems only used from editor.
 * @return The hash code for this editable field.
 *)
  method getHashCode : js_string t meth

(**
 * Returns the editable DOM element or null if this field
 * is not editable.
 * <p>On IE or Safari this is the element with contentEditable=true
 * (in whitebox mode, the iFrame body).
 * <p>On Gecko this is the iFrame body
 * TODO(user): How do we word this for subclass version?
 * @return The editable DOM element, defined as above.
 *)
  method getElement : Dom_html.element t meth

(**
 * Returns original DOM element that is being made editable by Trogedit or
 * null if that element has not yet been found in the appropriate document.
 * @return The original element.
 *)
  method getOriginalElement : Dom_html.element t meth

(**
 * Registers a keyboard event listener on the field.  This is necessary for
 * Gecko since the fields are contained in an iFrame and there is no way to
 * auto-propagate key events up to the main window.
 * @param type Event type to listen for or array of
 *    event types, for example goog.events.EventType.KEYDOWN.
 * @param listener Function to be used as the listener.
 * @param opt_capture Whether to use capture phase (optional,
 *    defaults to false).
 *)
  method addListener : 
      (js_string t, js_string t js_array t) Tools.Union.t 
       -> (unit -> unit) callback -> bool t opt -> unit meth

(**
 * Returns the registered plugin with the given classId.
 * @param classId classId of the plugin.
 * @return Registered plugin with the given classId.
 *)
  method getPluginByClassId : js_string t -> plugin t meth

(**
 * Sets the value that will replace the style attribute of this field's
 * element when the field is made non-editable. This method is called with the
 * current value of the style attribute when the field is made editable.
 * @param cssText The value of the style attribute.
 *)
  method setInitialStyle : js_string t -> unit meth

(**
 * Reset the properties on the original field element to how it was before
 * it was made editable.
 *)
  method resetOriginalElemProperties : unit meth

(**
 * Checks the modified state of the field.
 * Note: Changes that take place while the goog.editor.Field.EventType.CHANGE
 * event is stopped do not effect the modified state.
 * @param opt_useIsEverModified Set to true to check if the field
 *   has ever been modified since it was created, otherwise checks if the field
 *   has been modified since the last goog.editor.Field.EventType.DELAYEDCHANGE
 *   event was dispatched.
 * @return Whether the field has been modified.
 *)
  method isModified : bool t opt -> bool t meth

(**
 * @return Whether the field is implemented as an iframe.
 *)
  method usesIframe : bool t meth

(**
 * @return Whether the field should be rendered with a fixed
 *     height, or should expand to fit its contents.
 *)
  method isFixedHeight : bool t meth

(**
 * Sets the application window.
 * @param appWindow The window where dialogs and bubbles should be
 *     rendered.
 *)
  method setAppWindow : Dom_html.window t -> unit meth

(**
 * Returns the "application" window, where dialogs and bubbles
 * should be rendered.
 * @return The window.
 *)
  method getAppWindow : Dom_html.window t meth

(**
 * Sets the zIndex that the field should be based off of.
 * TODO(user): Get rid of this completely.  Here for Sites.
 *     Should this be set directly on UI plugins?
 *
 * @param zindex The base zIndex of the editor.
 *)
  method setBaseZindex : int -> unit meth

(**
 * Returns the zindex of the base level of the field.
 *
 * @return The base zindex of the editor.
 *)
  method getBaseZindex : int meth

(**
 * Removes all listeners and destroys the eventhandler object.
 * @override
 *)
 method disposeInternal : unit meth

(**
 * Attach an wrapper to this field, to be thrown out when the field
 * is disposed.
 * @param wrapper The wrapper to attach.
 *)
  method attachWrapper : #Disposable.disposable t -> unit meth

(**
 * Removes all wrappers and destroys them.
 *)
  method removeAllWrappers : unit meth

(**
 * Returns prepared contents that can be injected into the editable field.
 * @param contents The contents to prepare.
 * @return The prepared contents.
 *)
  method getInjectableContents : js_string t -> js_string t meth

(*
 * Executes an editing command as per the registered plugins.
 * @param command The command to execute.
 * @param var_args Any additional parameters needed to execute the
 *     command.
 * @return False if the command wasn't handled, otherwise, the result of
 *     the command.
 *
  method execCommand : js_string t -> js_string t meth *)

(*
 * Gets the value of command(s).
 * @param commands String name(s) of the command.
 * @return Value of each command. Returns false (or array of falses)
 *     if designMode is off or the field is otherwise uneditable, and
 *     there are no activeOnUneditable plugins for the command.
 *
  method queryCommandValue : (js_string t, js_string t js_array t) Tools.Union.t
    -> *)

(**
 * @return The dom helper for the editable node.
 *)
  method getEditableDomHelper : Gdom.domHelper t opt meth 

(*
 * @return Closure range object wrapping the selection
 *     in this field or null if this field is not currently editable.
  method getRange : *) 

(**
 * Dispatch a selection change event, optionally caused by the given browser
 * event.
 * @param opt_e Optional browser event causing this
 *     event.
 *)
  method dispatchSelectionChangeEvent : #Events.browserEvent t -> unit meth 

(**
 * This dispatches the beforechange event on the editable field
 *)
  method dispatchBeforeChange : unit meth

(**
 * Temporarily ignore change events. If the time has already been set, it will
 * fire immediately now.  Further setting of the timer is stopped and
 * dispatching of events is stopped until startChangeEvents is called.
 * @param opt_stopChange Whether to ignore base change events.
 * @param opt_stopDelayedChange Whether to ignore delayed change
 *     events.
 *)
  method stopChangeEvents : bool t opt -> bool t opt -> unit meth 

(**
 * Start change events again and fire once if desired.
 * @param opt_fireChange Whether to fire the change event
 *      immediately.
 * @param opt_fireDelayedChange Whether to fire the delayed change
 *      event immediately.
 *)
  method startChangeEvents : bool t opt -> bool t opt -> unit meth

(**
 * Stops the event of the given type from being dispatched.
 * @param eventType type of event to stop.
 *)
  method stopEvent : js_string t -> unit meth

(**
 * Re-starts the event of the given type being dispatched, if it had
 * previously been stopped with stopEvent().
 * @param eventType type of event to start.
 *)
  method startEvent : js_string t -> unit meth

(**
 * Block an event for a short amount of time. Intended
 * for the situation where an event pair fires in quick succession
 * (e.g., mousedown/mouseup, keydown/keyup, focus/blur),
 * and we want the second event in the pair to get "debounced."
 *
 * WARNING: This should never be used to solve race conditions or for
 * mission-critical actions. It should only be used for UI improvements,
 * where it's okay if the behavior is non-deterministic.
 *
 * @param eventType type of event to debounce.
 *)
  method debounceEvent : js_string t meth

(**
 * Calls a function to manipulate the dom of this field. This method should be
 * used whenever Trogedit clients need to modify the dom of the field, so that
 * delayed change events are handled appropriately. Extra delayed change events
 * will cause undesired states to be added to the undo-redo stack. This method
 * will always fire at most one delayed change event, depending on the value of
 * [opt_preventDelayedChange].
 *
 * @param func The function to call that will manipulate the dom.
 * @param opt_preventDelayedChange Whether delayed change should be
 *      prevented after calling [func]. Defaults to always firing
 *      delayed change.
 *)
  method manipulateDom : (unit -> unit) callback -> bool t opt -> unit meth

(**
 * Dispatches a command value change event.
 * @param opt_commands Commands whose state has
 *     changed.
 *)
  method dispatchCommandValueChange : js_string t js_array t -> unit meth
    

(**
 * Dispatches the appropriate set of change events. This only fires
 * synchronous change events in blended-mode, iframe-using mozilla. It just
 * starts the appropriate timer for goog.editor.Field.EventType.DELAYEDCHANGE.
 * This also starts up change events again if they were stopped.
 *
 * @param opt_noDelay True if
 *      goog.editor.Field.EventType.DELAYEDCHANGE should be fired syncronously.
 *)
  method dispatchChange : bool t opt -> unit meth

(**
 * Handle a change in the Editable Field.  Marks the field has modified,
 * dispatches the change event on the editable field (moz only), starts the
 * timer for the delayed change event.  Note that these actions only occur if
 * the proper events are not stopped.
 *)
  method handleChange : unit meth

(**
 * Don't wait for the timer and just fire the delayed change event if it's
 * pending.
 *)
  method clearDelayedChange : unit meth

(**
 * @return Whether the selection is editable.
 *)
  method isSelectionEditable : bool t meth

(**
 * Retrieve the HTML contents of a field.
 *
 * Do NOT just get the innerHTML of a field directly--there's a lot of
 * processing that needs to happen.
  * @return The scrubbed contents of the field.
 *)
  method getCleanContents : js_string t meth 

(**
 * Sets the contents of the field.
 * @param addParas Boolean to specify whether to add paragraphs
 *    to long fields.
 * @param html html to insert.  If html:null, then this defaults
 *    to a nsbp for mozilla and an empty string for IE.
 * @param opt_dontFireDelayedChange True to make this content change
 *    not fire a delayed change event.
 * @param opt_applyLorem Whether to apply lorem ipsum styles.
 *)
  method setHtml : bool t -> js_string t opt -> bool t opt -> bool t opt 
    -> unit meth

(**
 * @return Whether the field is uneditable.
 *)
  method isUneditable : bool t meth 

(**
 * @return Whether the field has finished loading.
 *)
  method isLoaded : bool t meth

(**
 * @return Whether the field is in the process of loading.
 *)
  method isLoading : bool t meth

(**
 * Gives the field focus.
 *)
  method focus : unit meth

(**
 * Gives the field focus and places the cursor at the start of the field.
 *)
  method focusAndPlaceCursorAtStart : unit meth 

(**
 * Place the cursor at the start of this field. It's recommended that you only
 * use this method (and manipulate the selection in general) when there is not
 * an existing selection in the field.
 *)
  method placeCursorAtStart : unit meth

(**
 * Makes a field editable.
 *
 * @param opt_iframeSrc URL to set the iframe src to if necessary.
 *)
  method makeEditable : js_string t opt -> unit meth

(**
 * Closes the field and cancels all pending change timers.  Note that this
 * means that if a change event has not fired yet, it will not fire.  Clients
 * should check fieldOj.isModified() if they depend on the final change event.
 * Throws an error if the field is already uneditable.
 *
 * @param opt_skipRestore True to prevent copying of editable field
 *     contents back into the original node.
 *)
  method makeUneditable : bool t opt -> unit meth
end

val field : (js_string t -> Dom_html.document t opt -> field t) constr

module Field : sig
(**
  * Registers [plugin] with the editable [field].
  * @param field The field on which the plugin will be registered.
  * @param plugin The plugin to register.
  *)
  val registerPlugin : field t -> #plugin t -> unit

(**
  * Unregisters [plugin] with [field].
  * @param field The field on wich the plugin will be unregistered.
  * @param plugin The plugin to unregister.
  *)
  val unregisterPlugin : field t -> #plugin t -> unit

  module EventType : sig
  (**
   * Dispatched when the command state of the selection may have changed. This
   * event should be listened to for updating toolbar state.
   *)
    val _COMMAND_VALUE_CHANGE: js_string t
  (**
   * Dispatched when the field is loaded and ready to use.
   *)
    val _LOAD: js_string t
  (**
   * Dispatched when the field is fully unloaded and uneditable.
   *)
    val _UNLOAD: js_string t
  (**
   * Dispatched before the field contents are changed.
   *)
    val _BEFORECHANGE: js_string t
  (**
   * Dispatched when the field contents change, in FF only.
   * Used for internal resizing, please do not use.
   *)
    val _CHANGE: js_string t
  (**
   * Dispatched on a slight delay after changes are made.
   * Use for autosave, or other times your app needs to know
   * that the field contents changed.
   *)
    val _DELAYEDCHANGE: js_string t
  (**
   * Dispatched before focus in moved into the field.
   *)
    val _BEFOREFOCUS: js_string t
  (**
   * Dispatched when focus is moved into the field.
   *)
    val _FOCUS: js_string t
  (**
   * Dispatched when the field is blurred.
   *)
    val _BLUR: js_string t
  (**
   * Dispach before tab is handled by the field.  This is a legacy way
   * of controlling tab behavior.  Use trog.plugins.AbstractTabHandler now.
   *)
    val _BEFORETAB: js_string t
  (**
   * Dispatched when the selection changes.
   * Use handleSelectionChange from plugin API instead of listening
   * directly to this event.
   *)
    val _SELECTIONCHANGE: js_string t
  end
end
