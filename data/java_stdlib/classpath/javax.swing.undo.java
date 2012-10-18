package javax.swing.undo;
class UndoableEditSupport {
  int realSource;
  int listeners;
  int compoundEdit;
  int updateLevel;
}
class UndoableEdit {
}
class UndoManager {
  int limit;
  int indexOfNextAdd;
  int serialVersionUID;
}
class StateEditable {
  int RCSID;
}
class StateEdit {
  int undoRedoName;
  int postState;
  int preState;
  int object;
  int RCSID;
}
class CompoundEdit {
  int inProgress;
  int edits;
  int serialVersionUID;
}
class CannotUndoException {
}
class CannotRedoException {
}
class AbstractUndoableEdit {
  int alive;
  int hasBeenDone;
  int RedoName;
  int UndoName;
  int serialVersionUID;
}
