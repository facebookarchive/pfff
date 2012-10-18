package java.util.concurrent;
class CopyOnWriteArrayList {
  class RandomAccessSubList {
  }
  class SubList {
    int data;
    int size;
    int offset;
    int backingList;
  }
  int data;
  int serialVersionUID;
}
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
  class Worker {
    int thread;
    int completedTasks;
    int firstTask;
    int runLock;
  }
  int defaultHandler;
  int completedTaskCount;
  int largestPoolSize;
  int threadFactory;
  int handler;
  int TERMINATED;
  int STOP;
  int SHUTDOWN;
  int RUNNING;
  int runState;
  int poolSize;
  int maximumPoolSize;
  int corePoolSize;
  int allowCoreThreadTimeOut;
  int keepAliveTime;
  int workers;
  int termination;
  int mainLock;
  int workQueue;
  int shutdownPerm;
  int EMPTY_RUNNABLE_ARRAY;
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
  class EmptyIterator {
  }
  int transferer;
  class TransferQueue {
    int cleanMeUpdater;
    int tailUpdater;
    int headUpdater;
    int cleanMe;
    int tail;
    int head;
    class QNode {
      int itemUpdater;
      int nextUpdater;
      int isData;
      int waiter;
      int item;
      int next;
    }
  }
  class TransferStack {
    int headUpdater;
    int head;
    class SNode {
      int matchUpdater;
      int nextUpdater;
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
    int dq;
  }
  class ScheduledFutureTask {
    int period;
    int time;
    int sequenceNumber;
  }
  int NANO_ORIGIN;
  int sequencer;
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
  class Itr {
    int lastRet;
    int cursor;
    int array;
  }
  int notEmpty;
  int lock;
  int q;
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
    int acc;
    int ccl;
  }
  class DefaultThreadFactory {
    int namePrefix;
    int threadNumber;
    int group;
    int poolNumber;
  }
  class PrivilegedCallableUsingCurrentClassLoader {
    int exception;
    int result;
    int task;
    int acc;
    int ccl;
  }
  class PrivilegedCallable {
    int exception;
    int result;
    int task;
    int acc;
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
  int q;
  int available;
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
class ConcurrentSkipListSet {
  int mapOffset;
  int unsafe;
  int m;
  int serialVersionUID;
}
class ConcurrentSkipListMap {
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
    int rightUpdater;
    int right;
    int down;
    int node;
  }
  class Node {
    int valueUpdater;
    int nextUpdater;
    int next;
    int value;
    int key;
  }
  int headUpdater;
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
  class Itr {
    int lastRet;
    int nextItem;
    int nextNode;
  }
  int tail;
  int head;
  int headUpdater;
  int tailUpdater;
  class Node {
    int itemUpdater;
    int nextUpdater;
    int next;
    int item;
  }
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
class ArrayBlockingQueue {
  class Itr {
    int lastRet;
    int nextItem;
    int nextIndex;
  }
  int notFull;
  int notEmpty;
  int lock;
  int count;
  int putIndex;
  int takeIndex;
  int items;
  int serialVersionUID;
}
class AbstractExecutorService {
}
