(*
   OClosure Project - 2010
   Class goog.ui.KeyboardShortcutHandler

   @author Cardoso Gabriel
   @version 0.2
 *)
#ifndef UI
open Js
#endif

class type keyboardShortcutHandler = object
  inherit Events.eventTarget

(**
   Removes all event listeners and clears shortcuts.
 *)
  method disposeInternal : unit meth

(**
   Returns whether all shortcuts (including modifier shortcuts) are treated as
   if the keys had been passed to the setGlobalKeys function.
   @return Whether all shortcuts are treated as globals.
 *)
  method getAllShortcutsAreGlobal : bool t meth

(**
   Returns whether the default action will always be prevented when a shortcut
   event is fired. The default value is true.
   @return Whether preventDefault will always be called.
 *)
  method getAlwaysPreventDefault : bool t meth

(**
   Returns whether the event will always be stopped from propagating beyond its
   target when a shortcut event is fired. The default value is false.
   @return Whether stopPropagation will always be called.
 *)
  method getAlwaysStopPropagation : bool t meth

 (**
   Returns event type for a specific shortcut.
   @param identifier Identifier for the shortcut task.
   @return Theh event type.
 *)
  method getEventType : js_string t -> js_string t meth

(**
   @return The global keys, i.e. keys that are safe to always
       regard as shortcuts, even if entered in a textarea or input field.
 *)
  method getGlobalKeys : int js_array t meth

(**
   Returns whether shortcuts with modifiers are treated as if the keys had been
   passed to the setGlobalKeys function.  Ignored if you have called
   setAllShortcutsAreGlobal(true).  Applies only to form elements (not
   content-editable).
   @return Whether shortcuts with modifiers are treated as globals.
 *)
  method getModifierShortcutsAreGlobal : bool t meth

(**
   Verifies if a particular keyboard shortcut is registered already. It has
   the same interface as the unregistering of shortcuts.

   param keyCode Numeric code for key
   param opt_modifiers Bitmap indicating required modifier keys.
                   goog.ui.KeyboardShortcutHandler.Modifiers.SHIFT, CONTROL,
                   ALT, or META.

   The two parameters can be repeated any number of times to create a shortcut
   using a sequence of strokes.

   A string representation of the shortcut can be supplied instead see
   [registerShortcut] for syntax. In that case the method only takes one
   argument.

   @param var_args String representation, or
       array or list of alternating key codes and modifiers.
   @return Whether the specified keyboard shortcut is registered.
 *)
  method isShortcutRegistered : js_string t -> bool t meth

(**
   Registers a keyboard shortcut.
   @param identifier Identifier for the task performed by the keyboard
                   combination. Multiple shortcuts can be provided for the same
                   task by specifying the same identifier.
   @param See below.

   Examples using string representation for shortcuts:
     g               registerShortcut(str, "g")
     Ctrl+g          registerShortcut(str, "ctrl+g")
     Ctrl+Shift+g    registerShortcut(str, "ctrl+shift+g")
     Ctrl+g a        registerShortcut(str, "ctrl+g a")
     Ctrl+g Shift+a  registerShortcut(str, "ctrl+g shift+a")
     g a             registerShortcut(str, "g a").
 *)
  method registerShortcut : js_string t -> js_string t meth

 (**
   Sets whether to treat all shortcuts (including modifier shortcuts) as if the
   keys had been passed to the setGlobalKeys function.
   @param allShortcutsGlobal Whether to treat all shortcuts as global.
 *)
 method setAllShortcutsAreGlobal : bool t -> unit meth

(**
   Sets whether to always prevent the default action when a shortcut event is
   fired. If false, the default action is prevented only if preventDefault is
   called on  either of the corresponding SHORTCUT_TRIGGERED or SHORTCUT_PREFIX
   events. If true, the default action is prevented whenever a shortcut event
   is fired. The default value is true.
   @param alwaysPreventDefault Whether to always call preventDefault.
 *)
  method setAlwaysPreventDefault : bool t -> unit meth

(**
   Sets whether to always stop propagation for the event when fired. If false,
   the propagation is stopped only if stopPropagation is called on either of the
   corresponding SHORT_CUT_TRIGGERED or SHORTCUT_PREFIX events. If true, the
   event is prevented from propagating beyond its target whenever it is fired.
   The default value is false.
   @param alwaysStopPropagation Whether to always call
       stopPropagation.
 *)
  method setAlwaysStopPropagation : bool t -> unit meth

(**
   Sets the global keys; keys that are safe to always regarded as shortcuts,
   even if entered in a textarea or input field.
   @param keys List of keys.
 *)
  method setGlobalKeys : int js_array t -> unit meth

(**
   Sets whether to treat shortcuts with modifiers as if the keys had been
   passed to the setGlobalKeys function.  Ignored if you have called
   setAllShortcutsAreGlobal(true).  Applies only to form elements (not
   content-editable).
   @param modifierShortcutsGlobal Whether to treat shortcuts with
       modifiers as global.
 *)
  method setModifierShortcutsAreGlobal : bool t -> unit meth

(**
   Unregisters all keyboard shortcuts.
 *)
  method unregisterAll : unit meth

(**
   Unregisters a keyboard shortcut by keyCode string representation of sequence.

   @param shortcut A string representation of the shortcut see
   [registerShortcut] for syntax.
 *)
  method unregisterShortcut : js_string t -> unit meth
end

(**
   Component for handling keyboard shortcuts. A shortcut is registered and bound
   to a specific identifier. Once the shortcut is triggered an event is fired
   with the identifier for the shortcut. This allows keyboard shortcuts to be
   customized without modifying the code that listens for them.

   Supports keyboard shortcuts triggered by a single key, a stroke stroke (key
   plus at least one modifier) and a sequence of keys or strokes.

   @param keyTarget Event target that the
       key event listener is attached to, typically the applications root
       container.
 *)
val keyboardShortcutHandler :
    ((#Events.eventTarget t, #Dom_html.eventTarget t) Tools.Union.t opt 
     -> keyboardShortcutHandler t) constr
