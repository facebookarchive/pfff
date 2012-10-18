package java.nio.channels.spi;
class SelectorProvider {
  int provider;
}
class AbstractSelector {
  int wakeupRunnable;
  int cancelledKeysSet;
  int provider;
  int isOpen;
}
class AbstractSelectionKey {
  int isValid;
}
class AbstractSelectableChannel {
  int isBlocking;
  int blockingLock;
  int keyList;
  int provider;
}
class AbstractInterruptibleChannel {
  int interruptAndCloseRunnable;
  int interrupted;
  int closed;
}
