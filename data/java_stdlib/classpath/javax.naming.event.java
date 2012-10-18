package javax.naming.event;
class ObjectChangeListener {
}
class NamingListener {
}
class NamingExceptionEvent {
  int exception;
  int serialVersionUID;
}
class NamingEvent {
  int newBinding;
  int oldBinding;
  int type;
  int changeInfo;
  int OBJECT_CHANGED;
  int OBJECT_RENAMED;
  int OBJECT_REMOVED;
  int OBJECT_ADDED;
  int serialVersionUID;
}
class NamespaceChangeListener {
}
class EventDirContext {
}
class EventContext {
  int SUBTREE_SCOPE;
  int ONELEVEL_SCOPE;
  int OBJECT_SCOPE;
}
