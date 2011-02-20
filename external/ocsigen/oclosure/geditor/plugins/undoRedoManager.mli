(**
   class goog.editor.plugins.UndoRedoManager

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
open UndoRedoState
#endif

class type undoRedoManager = object
  inherit Events.eventTarget

(**
 * Add state to the undo stack. This clears the redo stack.
 *
 * @param state The state to add to the undo
 *     stack.
 *)
  method addState : undoRedoState t -> unit meth

(**
 * Clear the undo/redo stack.
 *)
  method clearHistory : unit meth

(**
 * @return Wether the redo stack has items on it, i.e., if it is
 *     possible to perform a redo operation.
 *)
  method hasRedoState : bool t meth

(**
 * @return Wether the undo stack has items on it, i.e., if it is
 *     possible to perform an undo operation.
 *)
  method hasUndoState : bool t meth

(**
 * Performs the redo operation of the state at the top of the redo stack, moving
 * that state to the top of the undo stack. If redo undo stack is empty, does
 * nothing.
 *)
  method redo : unit meth

(**
 * @return The state at the top of
 *     the redo stack without removing it from the stack.
 *)
  method redoPeek : undoRedoState t optdef meth

(**
 * Set the max undo stack depth (not the real memory usage).
 * @param depth Depth of the stack.
 *)
  method setMaxUndoDepth : int -> unit meth

(**
 * Performs the undo operation of the state at the top of the undo stack, moving
 * that state to the top of the redo stack. If the undo stack is empty, does
 * nothing.
 *)
  method undo : unit meth

(**
 * @return The state at the top of
 *     the undo stack without removing it from the stack.
 *)
  method undoPeek : undoRedoState t optdef meth
end

(**
 * Manages undo and redo operations through a series of [UndoRedoState]
 * maintained on undo and redo stacks.
 *)
val undoRedoManager : undoRedoManager t constr
