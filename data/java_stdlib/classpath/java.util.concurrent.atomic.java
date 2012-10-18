package java.util.concurrent.atomic;
class AtomicStampedReference {
  int atomicRef;
  class ReferenceIntegerPair {
    int integer;
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
  int scale;
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
  int atomicRef;
  class ReferenceBooleanPair {
    int bit;
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
  int scale;
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
  int scale;
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
