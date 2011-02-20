(*
   Class goog.ui.editor.ToolbarController

   @author Gabriel Cardoso
*)

#ifndef UI_EDITOR
open Js
#endif
#ifndef UI
open Toolbar
#endif

class type toolbarController = object
  inherit Events.eventTarget

(**
 * Programmatically blurs the editor toolbar, un-highlighting the currently
 * highlighted item, and closing the currently open menu (if any).
 *)
  method blur : unit meth
      
  method disposeInternal : unit meth
      
(**
 * Returns the toolbar UI component that manages the editor.  Useful for
 * classes that extend [goog.ui.editor.ToolbarController].
 * @return The toolbar UI component.
 *)
  method getToolbar : toolbar t meth

(**
 * @return Whether the toolbar is enabled.
 *)
  method isEnabled : bool t meth

(**
 * @return Whether the toolbar is visible.
 *)
  method isVisible : bool t meth

(**
 * Enables or disables the toolbar.
 * @param enabled Whether to enable or disable the toolbar.
 *)
  method setEnabled : bool t -> unit meth

(**
 * Shows or hides the toolbar.
 * @param visible Whether to show or hide the toolbar.
 *)
  method setVisible : bool t -> unit meth
end

(**
 * A class for managing the editor toolbar.  Acts as a bridge between
 * a [goog.editor.Field] and a [goog.ui.Toolbar].
 *
 * The [toolbar] argument must be an instance of [goog.ui.Toolbar]
 * or a subclass.  This class doesn't care how the toolbar was created.  As
 * long as one or more controls hosted  in the toolbar have IDs that match
 * built-in [goog.editor.Command]s, they will function as expected.  It is
 * the caller's responsibility to ensure that the toolbar is already rendered
 * or that it decorates an existing element.
 *
 *
 * @param field Editable field to be controlled by the
 *     toolbar.
 * @param toolbar Toolbar to control the editable field.
 *)
val toolbarController : 
    (Geditor.field t -> toolbar t -> toolbarController t) constr
