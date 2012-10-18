package java.lang.management;
class VMManagementFactory {
}
class ThreadMXBean {
}
class ThreadInfo {
  int seType;
  int bean;
  int lockedSynchronizers;
  int lockedMonitors;
  int trace;
  int isSuspended;
  int isInNative;
  int waitedTime;
  int waitedCount;
  int lockOwnerName;
  int lockOwnerId;
  int lockName;
  int blockedTime;
  int blockedCount;
  int threadState;
  int threadName;
  int threadId;
}
class RuntimeMXBean {
}
class OperatingSystemMXBean {
}
class MonitorInfo {
  int stackFrame;
  int stackDepth;
}
class MemoryUsage {
  int maximum;
  int committed;
  int used;
  int init;
}
class MemoryType {
  int NON_HEAP;
  int HEAP;
}
class MemoryPoolMXBean {
}
class MemoryNotificationInfo {
  int count;
  int usage;
  int poolName;
  int MEMORY_COLLECTION_THRESHOLD_EXCEEDED;
  int MEMORY_THRESHOLD_EXCEEDED;
}
class MemoryManagerMXBean {
}
class MemoryMXBean {
}
class ManagementPermission {
  int serialVersionUID;
}
class ManagementFactory {
  class ManagementInvocationHandler {
    int bean;
    int conn;
  }
  int platformServer;
  int compilationBean;
  int memoryBean;
  int threadBean;
  int classLoadingBean;
  int runtimeBean;
  int osBean;
  int THREAD_MXBEAN_NAME;
  int RUNTIME_MXBEAN_NAME;
  int OPERATING_SYSTEM_MXBEAN_NAME;
  int MEMORY_POOL_MXBEAN_DOMAIN_TYPE;
  int MEMORY_MXBEAN_NAME;
  int MEMORY_MANAGER_MXBEAN_DOMAIN_TYPE;
  int GARBAGE_COLLECTOR_MXBEAN_DOMAIN_TYPE;
  int COMPILATION_MXBEAN_NAME;
  int CLASS_LOADING_MXBEAN_NAME;
}
class LockInfo {
  int identityHashCode;
  int className;
}
class GarbageCollectorMXBean {
}
class CompilationMXBean {
}
class ClassLoadingMXBean {
}
