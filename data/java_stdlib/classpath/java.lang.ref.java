package java.lang.ref;
class WeakReference {
}
class SoftReference {
}
class ReferenceQueue {
  int lock;
  int first;
}
class Reference {
  int lock;
  int nextOnQueue;
  int queue;
  int referent;
}
class PhantomReference {
}
