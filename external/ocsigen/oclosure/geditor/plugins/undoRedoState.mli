(**
   class goog.editor.plugins.UndoRedoState

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
#endif

class type undoRedoState = object
  inherit Events.eventTarget

(**
 * Checks if two undo-redo states are the same.
 * @param state The state to compare.
 * @return Wether the two states are equal.
 *)
  method equals : undoRedoState t -> bool t meth

(**
 * @return Whether or not the undo and redo actions of this state
 *     complete asynchronously. If true, the state will fire an ACTION_COMPLETED
 *     event when an undo or redo action is complete.
 *)
  method isAsynchronous : bool t meth

(**
 * Redoes the action represented by this state.
 *)
  method redo : unit meth

(**
 * Undoes the action represented by this state.
 *)
  method undo : unit meth
end

(**
 * Represents an undo and redo action for a particular state transition.
 *
 * @param asynchronous Whether the undo or redo actions for this
 *     state complete asynchronously. If true, then this state must fire
 *     an ACTION_COMPLETED event when undo or redo is complete.
 *)
val undoRedoState : (bool t -> undoRedoState t) constr
