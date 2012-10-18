package sun.awt;
class CausedFocusEvent {
  int cause;
  class Cause {
    int RETARGETED;
    int CLEAR_GLOBAL_FOCUS_OWNER;
    int ACTIVATION;
    int NATIVE_SYSTEM;
    int ROLLBACK;
    int AUTOMATIC_TRAVERSE;
    int MANUAL_REQUEST;
    int TRAVERSAL_BACKWARD;
    int TRAVERSAL_FORWARD;
    int TRAVERSAL_DOWN;
    int TRAVERSAL_UP;
    int TRAVERSAL;
    int MOUSE_EVENT;
    int UNKNOWN;
  }
}
