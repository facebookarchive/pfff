package java.lang.ref;
class WeakReference {
}
class SoftReference {
}
class ReferenceQueue {
  int unenqueued;
  int head;
  int NANOS_PER_MILLI;
}
class Reference {
  int pendingNext;
  int queueNext;
  int queue;
  int referent;
}
class PhantomReference {
}
class FinalizerReference {
  class Sentinel {
    int finalized;
  }
  int next;
  int prev;
  int zombie;
  int head;
  int queue;
}
