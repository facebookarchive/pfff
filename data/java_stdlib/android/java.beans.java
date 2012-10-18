package java.beans;
class PropertyChangeSupport {
  int listeners;
  int sourceBean;
  int serialPersistentFields;
  int serialVersionUID;
}
class PropertyChangeListenerProxy {
  int propertyName;
}
class PropertyChangeListener {
}
class PropertyChangeEvent {
  int propagationId;
  int newValue;
  int oldValue;
  int propertyName;
  int serialVersionUID;
}
class IndexedPropertyChangeEvent {
  int index;
  int serialVersionUID;
}
