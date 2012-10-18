package java.util.concurrent.atomic;
class UnsafeAccess {
  int THE_ONE;
}
class AtomicStampedReference {
  int pairOffset;
  int UNSAFE;
  int pair;
  class Pair {
    int stamp;
    int reference;
  }
}
class AtomicReferenceFieldUpdater {
  class AtomicReferenceFieldUpdaterImpl {
    int cclass;
    int vclass;
    int tclass;
    int offset;
    int unsafe;
  }
}
class AtomicReferenceArray {
  int array;
  int shift;
  int base;
  int unsafe;
  int serialVersionUID;
}
class AtomicReference {
  int value;
  int valueOffset;
  int unsafe;
  int serialVersionUID;
}
class AtomicMarkableReference {
  int pairOffset;
  int UNSAFE;
  int pair;
  class Pair {
    int mark;
    int reference;
  }
}
class AtomicLongFieldUpdater {
  class LockedUpdater {
    int cclass;
    int tclass;
    int offset;
    int unsafe;
  }
  class CASUpdater {
    int cclass;
    int tclass;
    int offset;
    int unsafe;
  }
}
class AtomicLongArray {
  int array;
  int shift;
  int base;
  int unsafe;
  int serialVersionUID;
}
class AtomicLong {
  int value;
  int VM_SUPPORTS_LONG_CAS;
  int valueOffset;
  int unsafe;
  int serialVersionUID;
}
class AtomicIntegerFieldUpdater {
  class AtomicIntegerFieldUpdaterImpl {
    int cclass;
    int tclass;
    int offset;
    int unsafe;
  }
}
class AtomicIntegerArray {
  int array;
  int shift;
  int base;
  int unsafe;
  int serialVersionUID;
}
class AtomicInteger {
  int value;
  int valueOffset;
  int unsafe;
  int serialVersionUID;
}
class AtomicBoolean {
  int value;
  int valueOffset;
  int unsafe;
  int serialVersionUID;
}
