package java.util.concurrent;
class TimeoutException {
  int serialVersionUID;
}
class TimeUnit {
  int DAYS;
  int HOURS;
  int MINUTES;
  int SECONDS;
  int MILLISECONDS;
  int MICROSECONDS;
  int NANOSECONDS;
  int MAX;
  int C6;
  int C5;
  int C4;
  int C3;
  int C2;
  int C1;
  int C0;
}
class ThreadPoolExecutor {
  class DiscardOldestPolicy {
  }
  class DiscardPolicy {
  }
  class AbortPolicy {
  }
  class CallerRunsPolicy {
  }
  int ONLY_ONE;
  class Worker {
    int completedTasks;
    int firstTask;
    int thread;
    int serialVersionUID;
  }
  int shutdownPerm;
  int defaultHandler;
  int maximumPoolSize;
  int corePoolSize;
  int allowCoreThreadTimeOut;
  int keepAliveTime;
  int handler;
  int threadFactory;
  int completedTaskCount;
  int largestPoolSize;
  int termination;
  int workers;
  int mainLock;
  int workQueue;
  int TERMINATED;
  int TIDYING;
  int STOP;
  int SHUTDOWN;
  int RUNNING;
  int CAPACITY;
  int COUNT_BITS;
  int ctl;
}
class ThreadFactory {
}
class SynchronousQueue {
  int waitingConsumers;
  int waitingProducers;
  int qlock;
  class FifoWaitQueue {
    int serialVersionUID;
  }
  class LifoWaitQueue {
    int serialVersionUID;
  }
  class WaitQueue {
  }
  int transferer;
  class TransferQueue {
    int cleanMeOffset;
    int tailOffset;
    int headOffset;
    int UNSAFE;
    int cleanMe;
    int tail;
    int head;
    class QNode {
      int itemOffset;
      int nextOffset;
      int UNSAFE;
      int isData;
      int waiter;
      int item;
      int next;
    }
  }
  class TransferStack {
    int headOffset;
    int UNSAFE;
    int head;
    class SNode {
      int matchOffset;
      int nextOffset;
      int UNSAFE;
      int mode;
      int item;
      int waiter;
      int match;
      int next;
    }
    int FULFILLING;
    int DATA;
    int REQUEST;
  }
  int spinForTimeoutThreshold;
  int maxUntimedSpins;
  int maxTimedSpins;
  int NCPUS;
  class Transferer {
  }
  int serialVersionUID;
}
class Semaphore {
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
class ScheduledThreadPoolExecutor {
  class DelayedWorkQueue {
    class Itr {
      int lastRet;
      int cursor;
      int array;
    }
    int available;
    int leader;
    int size;
    int lock;
    int queue;
    int INITIAL_CAPACITY;
  }
  class ScheduledFutureTask {
    int heapIndex;
    int outerTask;
    int period;
    int time;
    int sequenceNumber;
  }
  int sequencer;
  int removeOnCancel;
  int executeExistingDelayedTasksAfterShutdown;
  int continueExistingPeriodicTasksAfterShutdown;
}
class ScheduledFuture {
}
class ScheduledExecutorService {
}
class RunnableScheduledFuture {
}
class RunnableFuture {
}
class RejectedExecutionHandler {
}
class RejectedExecutionException {
  int serialVersionUID;
}
class PriorityBlockingQueue {
  int allocationSpinLockOffset;
  int UNSAFE;
  class Itr {
    int lastRet;
    int cursor;
    int array;
  }
  int q;
  int allocationSpinLock;
  int notEmpty;
  int lock;
  int comparator;
  int size;
  int queue;
  int MAX_ARRAY_SIZE;
  int DEFAULT_INITIAL_CAPACITY;
  int serialVersionUID;
}
class LinkedBlockingQueue {
  class Itr {
    int currentElement;
    int lastRet;
    int current;
  }
  int notFull;
  int putLock;
  int notEmpty;
  int takeLock;
  int last;
  int head;
  int count;
  int capacity;
  class Node {
    int next;
    int item;
  }
  int serialVersionUID;
}
class LinkedBlockingDeque {
  class DescendingItr {
  }
  class Itr {
  }
  class AbstractItr {
    int lastRet;
    int nextItem;
    int next;
  }
  int notFull;
  int notEmpty;
  int lock;
  int capacity;
  int count;
  int last;
  int first;
  class Node {
    int next;
    int prev;
    int item;
  }
  int serialVersionUID;
}
class FutureTask {
  class Sync {
    int runner;
    int exception;
    int result;
    int callable;
    int CANCELLED;
    int RAN;
    int RUNNING;
    int READY;
    int serialVersionUID;
  }
  int sync;
}
class Future {
}
class Executors {
  class DelegatedScheduledExecutorService {
    int e;
  }
  class FinalizableDelegatedExecutorService {
  }
  class DelegatedExecutorService {
    int e;
  }
  class PrivilegedThreadFactory {
    int ccl;
    int acc;
  }
  class DefaultThreadFactory {
    int namePrefix;
    int threadNumber;
    int group;
    int poolNumber;
  }
  class PrivilegedCallableUsingCurrentClassLoader {
    int ccl;
    int acc;
    int task;
  }
  class PrivilegedCallable {
    int acc;
    int task;
  }
  class RunnableAdapter {
    int result;
    int task;
  }
}
class ExecutorService {
}
class ExecutorCompletionService {
  class QueueingFuture {
    int task;
  }
  int completionQueue;
  int aes;
  int executor;
}
class Executor {
}
class ExecutionException {
  int serialVersionUID;
}
class Exchanger {
  int max;
  int arena;
  class Slot {
    int qe;
    int qd;
    int qc;
    int qb;
    int qa;
    int q9;
    int q8;
    int q7;
    int q6;
    int q5;
    int q4;
    int q3;
    int q2;
    int q1;
    int q0;
  }
  class Node {
    int waiter;
    int item;
  }
  int NULL_ITEM;
  int CANCEL;
  int TIMED_SPINS;
  int SPINS;
  int FULL;
  int CAPACITY;
  int NCPU;
}
class Delayed {
}
class DelayQueue {
  class Itr {
    int lastRet;
    int cursor;
    int array;
  }
  int available;
  int leader;
  int q;
  int lock;
}
class CyclicBarrier {
  int count;
  int generation;
  int barrierCommand;
  int parties;
  int trip;
  int lock;
  class Generation {
    int broken;
  }
}
class CountDownLatch {
  int sync;
  class Sync {
    int serialVersionUID;
  }
}
class CopyOnWriteArraySet {
  int al;
  int serialVersionUID;
}
class CopyOnWriteArrayList {
  class CowIterator {
    int index;
    int to;
    int from;
    int snapshot;
  }
  class Slice {
    int to;
    int from;
    int expectedElements;
  }
  class CowSubList {
    int slice;
  }
  int elements;
  int serialVersionUID;
}
class ConcurrentSkipListSet {
  int mapOffset;
  int unsafe;
  int m;
  int serialVersionUID;
}
class ConcurrentSkipListMap {
  int headOffset;
  int UNSAFE;
  class SubMap {
    class SubMapEntryIterator {
    }
    class SubMapKeyIterator {
    }
    class SubMapValueIterator {
    }
    class SubMapIter {
      int nextValue;
      int next;
      int lastReturned;
    }
    int valuesView;
    int entrySetView;
    int keySetView;
    int isDescending;
    int hiInclusive;
    int loInclusive;
    int hi;
    int lo;
    int m;
    int serialVersionUID;
  }
  class EntrySet {
    int m;
  }
  class Values {
    int m;
  }
  class KeySet {
    int m;
  }
  class EntryIterator {
  }
  class KeyIterator {
  }
  class ValueIterator {
  }
  class Iter {
    int nextValue;
    int next;
    int lastReturned;
  }
  int GT;
  int LT;
  int EQ;
  class ComparableUsingComparator {
    int cmp;
    int actualKey;
  }
  class HeadIndex {
    int level;
  }
  class Index {
    int rightOffset;
    int UNSAFE;
    int right;
    int down;
    int node;
  }
  class Node {
    int nextOffset;
    int valueOffset;
    int UNSAFE;
    int next;
    int value;
    int key;
  }
  int descendingMap;
  int values;
  int entrySet;
  int keySet;
  int randomSeed;
  int comparator;
  int head;
  int BASE_HEADER;
  int seedGenerator;
  int serialVersionUID;
}
class ConcurrentNavigableMap {
}
class ConcurrentMap {
}
class ConcurrentLinkedQueue {
  int tailOffset;
  int headOffset;
  int UNSAFE;
  class Itr {
    int lastRet;
    int nextItem;
    int nextNode;
  }
  int tail;
  int head;
  class Node {
    int itemOffset;
    int nextOffset;
    int UNSAFE;
    int next;
    int item;
  }
  int serialVersionUID;
}
class ConcurrentLinkedDeque {
  int tailOffset;
  int headOffset;
  int UNSAFE;
  class DescendingItr {
  }
  class Itr {
  }
  class AbstractItr {
    int lastRet;
    int nextItem;
    int nextNode;
  }
  int HOPS;
  class Node {
    int nextOffset;
    int itemOffset;
    int prevOffset;
    int UNSAFE;
    int next;
    int item;
    int prev;
  }
  int NEXT_TERMINATOR;
  int PREV_TERMINATOR;
  int tail;
  int head;
  int serialVersionUID;
}
class ConcurrentHashMap {
  class EntrySet {
  }
  class Values {
  }
  class KeySet {
  }
  class EntryIterator {
  }
  class WriteThroughEntry {
  }
  class ValueIterator {
  }
  class KeyIterator {
  }
  class HashIterator {
    int lastReturned;
    int nextEntry;
    int currentTable;
    int nextTableIndex;
    int nextSegmentIndex;
  }
  class Segment {
    int loadFactor;
    int table;
    int threshold;
    int modCount;
    int count;
    int serialVersionUID;
  }
  class HashEntry {
    int next;
    int value;
    int hash;
    int key;
  }
  int values;
  int entrySet;
  int keySet;
  int segments;
  int segmentShift;
  int segmentMask;
  int RETRIES_BEFORE_LOCK;
  int MAX_SEGMENTS;
  int MAXIMUM_CAPACITY;
  int DEFAULT_CONCURRENCY_LEVEL;
  int DEFAULT_LOAD_FACTOR;
  int DEFAULT_INITIAL_CAPACITY;
  int serialVersionUID;
}
class CompletionService {
}
class CancellationException {
  int serialVersionUID;
}
class Callable {
}
class BrokenBarrierException {
  int serialVersionUID;
}
class BlockingQueue {
}
class BlockingDeque {
}
class AbstractExecutorService {
}
