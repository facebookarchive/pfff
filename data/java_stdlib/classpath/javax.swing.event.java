package javax.swing.event;
class UndoableEditListener {
}
class UndoableEditEvent {
  int edit;
  int serialVersionUID;
}
class TreeWillExpandListener {
}
class TreeSelectionListener {
}
class TreeSelectionEvent {
  int newLeadSelectionPath;
  int oldLeadSelectionPath;
  int areNew;
  int paths;
}
class TreeModelListener {
}
class TreeModelEvent {
  int path;
  int children;
  int childIndices;
}
class TreeExpansionListener {
}
class TreeExpansionEvent {
  int path;
}
class TableModelListener {
}
class TableModelEvent {
  int type;
  int lastRow;
  int firstRow;
  int column;
  int UPDATE;
  int INSERT;
  int HEADER_ROW;
  int DELETE;
  int ALL_COLUMNS;
  int serialVersionUID;
}
class TableColumnModelListener {
}
class TableColumnModelEvent {
  int toIndex;
  int fromIndex;
}
class SwingPropertyChangeSupport {
  int serialVersionUID;
}
class PopupMenuListener {
}
class PopupMenuEvent {
}
class MouseInputListener {
}
class MouseInputAdapter {
}
class MenuListener {
}
class MenuKeyListener {
}
class MenuKeyEvent {
  int manager;
  int path;
}
class MenuEvent {
}
class MenuDragMouseListener {
}
class MenuDragMouseEvent {
  int manager;
  int path;
}
class ListSelectionListener {
}
class ListSelectionEvent {
  int isAdjusting;
  int lastIndex;
  int firstIndex;
}
class ListDataListener {
}
class ListDataEvent {
  int index1;
  int index0;
  int type;
  int INTERVAL_REMOVED;
  int INTERVAL_ADDED;
  int CONTENTS_CHANGED;
  int serialVersionUID;
}
class InternalFrameListener {
}
class InternalFrameEvent {
  int INTERNAL_FRAME_OPENED;
  int INTERNAL_FRAME_LAST;
  int INTERNAL_FRAME_ICONIFIED;
  int INTERNAL_FRAME_FIRST;
  int INTERNAL_FRAME_DEICONIFIED;
  int INTERNAL_FRAME_DEACTIVATED;
  int INTERNAL_FRAME_CLOSING;
  int INTERNAL_FRAME_CLOSED;
  int INTERNAL_FRAME_ACTIVATED;
  int serialVersionUID;
}
class InternalFrameAdapter {
}
class HyperlinkListener {
}
class HyperlinkEvent {
  int element;
  int description;
  int url;
  int type;
  int serialVersionUID;
  class EventType {
    int type;
    int ACTIVATED;
    int EXITED;
    int ENTERED;
  }
}
class EventListenerList {
  int listenerList;
  int NO_LISTENERS;
  int serialVersionUID;
}
class DocumentListener {
}
class DocumentEvent {
  class EventType {
    int type;
    int CHANGE;
    int REMOVE;
    int INSERT;
  }
  class ElementChange {
  }
}
class ChangeListener {
}
class ChangeEvent {
}
class CellEditorListener {
}
class CaretListener {
}
class CaretEvent {
}
class AncestorListener {
}
class AncestorEvent {
  int ancestorParent;
  int ancestor;
  int sourceComponent;
  int ANCESTOR_MOVED;
  int ANCESTOR_REMOVED;
  int ANCESTOR_ADDED;
  int serialVersionUID;
}
