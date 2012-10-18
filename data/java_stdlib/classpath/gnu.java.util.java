package gnu.java.util;
class ZoneInfo {
  int serialVersionUID;
  int gmtZone;
  int lastRule;
  int transitions;
  int useDaylight;
  int dstSavings;
  int rawOffset;
  int IS_DST;
  int OFFSET_SHIFT;
  int OFFSET_MASK;
  int SECS_SHIFT;
}
class WeakIdentityHashMap {
  int buckets;
  int theEntrySet;
  class WeakBucket {
    class WeakEntry {
      int key;
    }
    int slot;
    int next;
    int value;
  }
  class WeakEntrySet {
  }
  int modCount;
  int threshold;
  int loadFactor;
  int size;
  int queue;
  int NULL_KEY;
  int DEFAULT_LOAD_FACTOR;
  int DEFAULT_CAPACITY;
}
class LRUCache {
  int capacity;
}
class EmptyEnumeration {
  int instance;
}
class DoubleEnumeration {
  int e2;
  int e1;
  int hasChecked;
  int hasMore;
}
class Base64 {
  int BASE_64_PAD;
  int BASE_64;
}
