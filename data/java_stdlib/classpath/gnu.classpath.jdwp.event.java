package gnu.classpath.jdwp.event;
class VmInitEvent {
  int _initialThread;
}
class VmDeathEvent {
}
class ThreadStartEvent {
  int _thread;
}
class ThreadEndEvent {
  int _thread;
}
class SingleStepEvent {
  int _instance;
  int _location;
  int _thread;
}
class MethodExitEvent {
  int _instance;
  int _location;
  int _thread;
}
class MethodEntryEvent {
  int _instance;
  int _location;
  int _thread;
}
class ExceptionEvent {
  int _klass;
  int _catchLocation;
  int _location;
  int _thread;
  int _exception;
  int _instance;
}
class EventRequest {
  int _kind;
  int _suspendPolicy;
  int _id;
  int _filters;
  int _idLock;
  int _last_id;
  int SUSPEND_ALL;
  int SUSPEND_THREAD;
  int SUSPEND_NONE;
  int EVENT_VM_DEATH;
  int EVENT_VM_INIT;
  int EVENT_METHOD_EXIT;
  int EVENT_METHOD_ENTRY;
  int EVENT_FIELD_MODIFY;
  int EVENT_FIELD_ACCESS;
  int EVENT_CLASS_LOAD;
  int EVENT_CLASS_UNLOAD;
  int EVENT_CLASS_PREPARE;
  int EVENT_THREAD_END;
  int EVENT_THREAD_START;
  int EVENT_USER_DEFINED;
  int EVENT_EXCEPTION;
  int EVENT_FRAME_POP;
  int EVENT_BREAKPOINT;
  int EVENT_SINGLE_STEP;
}
class EventManager {
  int _requests;
  int _instance;
}
class Event {
  int _eventKind;
  int EVENT_EXCEPTION_CAUGHT;
  int EVENT_EXCEPTION_CLASS;
  int EVENT_FIELD;
  int EVENT_INSTANCE;
  int EVENT_LOCATION;
  int EVENT_THREAD;
  int EVENT_CLASS;
}
class ClassUnloadEvent {
  int _signature;
}
class ClassPrepareEvent {
  int STATUS_ERROR;
  int STATUS_INITIALIZED;
  int STATUS_PREPARED;
  int STATUS_VERIFIED;
  int _status;
  int _class;
  int _thread;
}
class BreakpointEvent {
  int _instance;
  int _location;
  int _thread;
}
