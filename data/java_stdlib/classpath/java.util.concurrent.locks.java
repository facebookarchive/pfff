package java.util.concurrent.locks;
class ReentrantReadWriteLock {
  class WriteLock {
    int sync;
    int serialVersionUID;
  }
  class ReadLock {
    int sync;
    int serialVersionUID;
  }
  class FairSync {
    int serialVersionUID;
  }
  class NonfairSync {
    int serialVersionUID;
  }
  class Sync {
    int cachedHoldCounter;
    int readHolds;
    class ThreadLocalHoldCounter {
    }
    class HoldCounter {
      int tid;
      int count;
    }
    int EXCLUSIVE_MASK;
    int MAX_COUNT;
    int SHARED_UNIT;
    int SHARED_SHIFT;
    int serialVersionUID;
  }
  int sync;
  int writerLock;
  int readerLock;
  int serialVersionUID;
}
class ReentrantLock {
  class FairSync {
    int serialVersionUID;
  }
  class NonfairSync {
    int serialVersionUID;
  }
  class Sync {
    int serialVersionUID;
  }
  int sync;
  int serialVersionUID;
}
class ReadWriteLock {
}
class LockSupport {
  int parkBlockerOffset;
  int unsafe;
}
class Lock {
}
class Condition {
}
class AbstractQueuedSynchronizer {
  int waitStatusOffset;
  int tailOffset;
  int headOffset;
  int stateOffset;
  int unsafe;
  class ConditionObject {
    int THROW_IE;
    int REINTERRUPT;
    int lastWaiter;
    int firstWaiter;
    int serialVersionUID;
  }
  int spinForTimeoutThreshold;
  int state;
  int tail;
  int head;
  class Node {
    int nextWaiter;
    int thread;
    int next;
    int prev;
    int waitStatus;
    int EXCLUSIVE;
    int SHARED;
    int CONDITION;
    int SIGNAL;
    int CANCELLED;
  }
  int serialVersionUID;
}
class AbstractQueuedLongSynchronizer {
  int waitStatusOffset;
  int tailOffset;
  int headOffset;
  int stateOffset;
  int unsafe;
  class ConditionObject {
    int THROW_IE;
    int REINTERRUPT;
    int lastWaiter;
    int firstWaiter;
    int serialVersionUID;
  }
  int spinForTimeoutThreshold;
  int state;
  int tail;
  int head;
  class Node {
    int nextWaiter;
    int thread;
    int next;
    int prev;
    int waitStatus;
    int EXCLUSIVE;
    int SHARED;
    int CONDITION;
    int SIGNAL;
    int CANCELLED;
  }
  int serialVersionUID;
}
class AbstractOwnableSynchronizer {
  int exclusiveOwnerThread;
  int serialVersionUID;
}
