package gnu.java.beans.encoder;
class Writer {
}
class StAXWriter {
  int indent;
  int writer;
}
class ScannerState {
  int name;
  int context;
  int calls;
  int transitions;
  int transitionNames;
  int defaultSuccessor;
  int DEFAULT_STATE_NAME;
  int TRANSITION_LAST;
  int TRANSITION_FIRST;
  int TRANSITION_OBJECT_REFERENCE;
  int TRANSITION_STRING_REFERENCE;
  int TRANSITION_NULL_OBJECT;
  int TRANSITION_LIST_GET;
  int TRANSITION_LIST_SET;
  int TRANSITION_ARRAY_GET;
  int TRANSITION_ARRAY_SET;
  int TRANSITION_PRIMITIVE_ARRAY_INSTANTIATION;
  int TRANSITION_OBJECT_ARRAY_INSTANTIATION;
  int TRANSITION_PRIMITIVE_INSTANTIATION;
  int TRANSITION_OBJECT_INSTANTIATION;
  int TRANSITION_CLASS_RESOLUTION;
  int TRANSITION_STATIC_FIELD_ACCESS;
  int TRANSITION_STATIC_METHOD_INVOCATION;
  int TRANSITION_METHOD_INVOCATION;
}
class ScanEngine {
  int objects;
  int writer;
  int root;
  int current;
  int parents;
  int states;
  int DEBUG;
}
class Root {
  class RootElement {
  }
  int started;
  int current;
  int rootElement;
  int parents;
}
class ReportingScannerState {
}
class PrimitivePersistenceDelegate {
}
class ObjectId {
  int klass;
  int id;
  int nameIndices;
}
class MapPersistenceDelegate {
}
class IgnoringScannerState {
}
class GenericScannerState {
  int skipValues;
  int root;
  int initialSkipElements;
  int skipElements;
}
class Context {
  int call;
  int state;
}
class CollectionPersistenceDelegate {
}
class ClassPersistenceDelegate {
}
class ArrayPersistenceDelegate {
  int NULL_VALUES;
}
