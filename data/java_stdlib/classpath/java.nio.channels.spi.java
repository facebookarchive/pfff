package java.nio.channels.spi;
class SelectorProvider {
  int systemDefaultProvider;
}
class AbstractSelector {
  int cancelledKeys;
  int provider;
  int closed;
}
class AbstractSelectionKey {
  int cancelled;
}
class AbstractSelectableChannel {
  int keys;
  int provider;
  int LOCK;
  int blocking;
}
class AbstractInterruptibleChannel {
  int closed;
}
