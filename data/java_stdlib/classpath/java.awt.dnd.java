package java.awt.dnd;
class MouseDragGestureRecognizer {
}
class InvalidDnDOperationException {
  int serialVersionUID;
}
class DropTargetListener {
}
class DropTargetEvent {
  int context;
  int serialVersionUID;
}
class DropTargetDropEvent {
  int isLocalTx;
  int location;
  int actions;
  int dropAction;
  int serialVersionUID;
}
class DropTargetDragEvent {
  int location;
  int srcActions;
  int dropAction;
  int serialVersionUID;
}
class DropTargetContext {
  int dtcp;
  int targetActions;
  int dropTarget;
  class TransferableProxy {
    int transferable;
    int isLocal;
  }
  int serialVersionUID;
}
class DropTargetAdapter {
}
class DropTarget {
  int active;
  int autoscroller;
  int dropTargetListener;
  int dropTargetContext;
  int peer;
  int actions;
  int flavorMap;
  int component;
  class DropTargetAutoScroller {
    int inner;
    int outer;
    int timer;
    int point;
    int component;
    int DELAY;
    int HYSTERESIS;
  }
  int serialVersionUID;
}
class DragSourceMotionListener {
}
class DragSourceListener {
}
class DragSourceEvent {
  int y;
  int x;
  int locationSpecified;
  int serialVersionUID;
}
class DragSourceDropEvent {
  int dropSuccess;
  int dropAction;
  int serialVersionUID;
}
class DragSourceDragEvent {
  int gestureModifiers;
  int targetActions;
  int dropAction;
  int serialVersionUID;
}
class DragSourceContext {
  int offset;
  int image;
  int sourceActions;
  int useCustomCursor;
  int dragSourceListener;
  int trigger;
  int transferable;
  int cursor;
  int peer;
  int CHANGED;
  int OVER;
  int ENTER;
  int DEFAULT;
  int serialVersionUID;
}
class DragSourceAdapter {
}
class DragSource {
  int context;
  int peer;
  int ds;
  int dragSourceMotionListener;
  int dragSourceListener;
  int flavorMap;
  int DefaultLinkNoDrop;
  int DefaultMoveNoDrop;
  int DefaultCopyNoDrop;
  int DefaultLinkDrop;
  int DefaultMoveDrop;
  int DefaultCopyDrop;
  int serialVersionUID;
}
class DragGestureRecognizer {
  int events;
  int sourceActions;
  int dragGestureListener;
  int component;
  int dragSource;
  int serialVersionUID;
}
class DragGestureListener {
}
class DragGestureEvent {
  int dgr;
  int events;
  int action;
  int origin;
  int component;
  int dragSource;
  int serialVersionUID;
}
class DnDEventMulticaster {
}
class DnDConstants {
  int ACTION_REFERENCE;
  int ACTION_LINK;
  int ACTION_COPY_OR_MOVE;
  int ACTION_MOVE;
  int ACTION_COPY;
  int ACTION_NONE;
}
class Autoscroll {
}
