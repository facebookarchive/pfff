package gnu.classpath.jdwp;
class VMVirtualMachine {
  int canSetDefaultStratum;
  int canRequestVMDeathEvent;
  int canGetSourceDebugExtension;
  int canUseInstanceFilters;
  int canPopFrames;
  int canUnrestrictedlyRedefineClasses;
  int canAddMethod;
  int canRedefineClasses;
  int canGetMonitorInfo;
  int canGetCurrentContendedMonitor;
  int canGetOwnedMonitorInfo;
  int canGetSyntheticAttribute;
  int canGetBytecodes;
  int canWatchFieldAccess;
  int canWatchFieldModification;
}
class VMMethod {
  int _methodId;
  int _class;
  int SIZE;
}
class VMIdManager {
  int _ridTable;
  int _classTable;
  int _idTable;
  int _oidTable;
  int _refQueue;
  int _idm;
  class ReferenceKey {
    int _hash;
  }
  class IdFactory {
    int _idList;
    int _lastRid;
    int _lastId;
    int _ridLock;
    int _idLock;
  }
}
class VMFrame {
  int id;
  int loc;
  int obj;
  int thread;
  int SIZE;
}
class JdwpConstants {
  class InvokeOptions {
    int INVOKE_NONVIRTUAL;
    int INVOKE_SINGLE_THREADED;
  }
  class SuspendPolicy {
    int ALL;
    int EVENT_THREAD;
    int NONE;
  }
  class StepSize {
    int LINE;
    int MIN;
  }
  class StepDepth {
    int OUT;
    int OVER;
    int INTO;
  }
  class Tag {
    int CLASS_OBJECT;
    int CLASS_LOADER;
    int THREAD_GROUP;
    int THREAD;
    int STRING;
    int BOOLEAN;
    int VOID;
    int SHORT;
    int LONG;
    int INT;
    int DOUBLE;
    int FLOAT;
    int OBJECT;
    int CHAR;
    int BYTE;
    int ARRAY;
  }
  class TypeTag {
    int ARRAY;
    int INTERFACE;
    int CLASS;
  }
  class ClassStatus {
    int ERROR;
    int INITIALIZED;
    int PREPARED;
    int VERIFIED;
  }
  class SuspendStatus {
    int SUSPENDED;
  }
  class ThreadStatus {
    int WAIT;
    int MONITOR;
    int SLEEPING;
    int RUNNING;
    int ZOMBIE;
  }
  class ModKind {
    int INSTANCE_ONLY;
    int STEP;
    int FIELD_ONLY;
    int EXCEPTION_ONLY;
    int LOCATION_ONLY;
    int CLASS_EXCLUDE;
    int CLASS_MATCH;
    int CLASS_ONLY;
    int THREAD_ONLY;
    int CONDITIONAL;
    int COUNT;
  }
  class EventKind {
    int THREAD_DEATH;
    int VM_START;
    int VM_DISCONNECTED;
    int VM_DEATH;
    int VM_INIT;
    int METHOD_EXIT;
    int METHOD_ENTRY;
    int EXCEPTION_CATCH;
    int FIELD_MODIFICATION;
    int FIELD_ACCESS;
    int CLASS_LOAD;
    int CLASS_UNLOAD;
    int CLASS_PREPARE;
    int THREAD_END;
    int THREAD_START;
    int USER_DEFINED;
    int EXCEPTION;
    int FRAME_POP;
    int BREAKPOINT;
    int SINGLE_STEP;
  }
  class Error {
    int INVALID_COUNT;
    int NATIVE_METHOD;
    int TRANSPORT_INIT;
    int TRANSPORT_LOAD;
    int INVALID_ARRAY;
    int INVALID_CLASS_LOADER;
    int INVALID_STRING;
    int INVALID_LENGTH;
    int INVALID_INDEX;
    int ALREADY_INVOKING;
    int INVALID_TAG;
    int UNATTACHED_THREAD;
    int INTERNAL;
    int VM_DEAD;
    int ACCESS_DENIED;
    int OUT_OF_MEMORY;
    int ILLEGAL_ARGUMENT;
    int INVALID_EVENT_TYPE;
    int ABSENT_INFORMATION;
    int NULL_POINTER;
    int NOT_IMPLEMENTED;
    int METHOD_MODIFIERS_CHANGE_NOT_IMPLEMENTED;
    int CLASS_MODIFIERS_CHANGE_NOT_IMPLEMENTED;
    int NAMES_DONT_MATCH;
    int UNSUPPORTED_VERSION;
    int DELETE_METHOD_NOT_IMPLEMENTED;
    int HIERARCHY_CHANGE_NOT_IMPLEMENTED;
    int INVALID_TYPESTATE;
    int SCHEMA_CHANGE_NOT_IMPLEMENTED;
    int ADD_METHOD_NOT_IMPLEMENTED;
    int FAILS_VERIFICATION;
    int CIRCULAR_CLASS_DEFINITION;
    int INVALID_CLASS_FORMAT;
    int INTERRUPT;
    int NOT_MONITOR_OWNER;
    int INVALID_MONITOR;
    int NOT_FOUND;
    int DUPLICATE;
    int INVALID_SLOT;
    int TYPE_MISMATCH;
    int NOT_CURRENT_FRAME;
    int OPAQUE_FRAME;
    int NO_MORE_FRAMES;
    int INVALID_FRAMEID;
    int INVALID_FIELDID;
    int INVALID_LOCATION;
    int INVALID_METHODID;
    int CLASS_NOT_PREPARED;
    int INVALID_CLASS;
    int INVALID_OBJECT;
    int THREAD_SUSPENDED;
    int THREAD_NOT_SUSPENDED;
    int INVALID_PRIORITY;
    int INVALID_THREAD_GROUP;
    int INVALID_THREAD;
    int NONE;
  }
  class CommandSet {
    class Event {
      int COMPOSITE;
      int CS_VALUE;
    }
    int MAXIMUM;
    class ClassObjectReference {
      int REFLECTED_TYPE;
      int CS_VALUE;
    }
    class StackFrame {
      int POP_FRAMES;
      int THIS_OBJECT;
      int SET_VALUES;
      int GET_VALUES;
      int CS_VALUE;
    }
    class EventRequest {
      int CLEAR_ALL_BREAKPOINTS;
      int CLEAR;
      int SET;
      int CS_VALUE;
    }
    class ClassLoaderReference {
      int VISIBLE_CLASSES;
      int CS_VALUE;
    }
    class ArrayReference {
      int SET_VALUES;
      int GET_VALUES;
      int LENGTH;
      int CS_VALUE;
    }
    class ThreadGroupReference {
      int CHILDREN;
      int PARENT;
      int NAME;
      int CS_VALUE;
    }
    class ThreadReference {
      int SUSPEND_COUNT;
      int INTERRUPT;
      int STOP;
      int CURRENT_CONTENDED_MONITOR;
      int OWNED_MONITORS;
      int FRAME_COUNT;
      int FRAMES;
      int THREAD_GROUP;
      int STATUS;
      int RESUME;
      int SUSPEND;
      int NAME;
      int CS_VALUE;
    }
    class StringReference {
      int VALUE;
      int CS_VALUE;
    }
    class ObjectReference {
      int IS_COLLECTED;
      int ENABLE_COLLECTION;
      int DISABLE_COLLECTION;
      int INVOKE_METHOD;
      int MONITOR_INFO;
      int SET_VALUES;
      int GET_VALUES;
      int REFERENCE_TYPE;
      int CS_VALUE;
    }
    class Field {
      int CS_VALUE;
    }
    class Method {
      int VARIABLE_TABLE_WITH_GENERIC;
      int IS_OBSOLETE;
      int BYTE_CODES;
      int VARIABLE_TABLE;
      int LINE_TABLE;
      int CS_VALUE;
    }
    class InterfaceType {
      int CS_VALUE;
    }
    class ArrayType {
      int NEW_INSTANCE;
      int CS_VALUE;
    }
    class ClassType {
      int NEW_INSTANCE;
      int INVOKE_METHOD;
      int SET_VALUES;
      int SUPERCLASS;
      int CS_VALUE;
    }
    class ReferenceType {
      int METHODS_WITH_GENERIC;
      int FIELDS_WITH_GENERIC;
      int SIGNATURE_WITH_GENERIC;
      int SOURCE_DEBUG_EXTENSION;
      int CLASS_OBJECT;
      int INTERFACES;
      int STATUS;
      int NESTED_TYPES;
      int SOURCE_FILE;
      int GET_VALUES;
      int METHODS;
      int FIELDS;
      int MODIFIERS;
      int CLASS_LOADER;
      int SIGNATURE;
      int CS_VALUE;
    }
    class VirtualMachine {
      int ALL_CLASSES_WITH_GENERIC;
      int SET_DEFAULT_STRATUM;
      int REDEFINE_CLASSES;
      int CAPABILITIES_NEW;
      int RELEASE_EVENTS;
      int HOLD_EVENTS;
      int DISPOSE_OBJECTS;
      int CLASS_PATHS;
      int CAPABILITIES;
      int CREATE_STRING;
      int EXIT;
      int RESUME;
      int SUSPEND;
      int IDSIZES;
      int DISPOSE;
      int TOP_LEVEL_THREAD_GROUPS;
      int ALL_THREADS;
      int ALL_CLASSES;
      int CLASSES_BY_SIGNATURE;
      int VERSION;
      int CS_VALUE;
    }
  }
  class Version {
    int MINOR;
    int MAJOR;
  }
}
class Jdwp {
  int _initCount;
  int _initLock;
  int _group;
  int _shutdown;
  int _connection;
  int _PROPERTY_SUSPEND;
  int _properties;
  int _ppThread;
  int _packetProcessor;
  int isDebugging;
  int _instance;
}
