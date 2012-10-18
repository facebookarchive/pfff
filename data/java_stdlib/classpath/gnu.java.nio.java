package gnu.java.nio;
class VMSelector {
}
class VMPipe {
}
class VMChannel {
  class Kind {
    int OTHER;
    int FILE;
    int SOCK_DGRAM;
    int SOCK_STREAM;
  }
  class State {
    int closed;
    int valid;
    int native_fd;
  }
  int kind;
  int nfd;
}
class VMChannelOwner {
}
class SocketChannelSelectionKeyImpl {
  int ch;
}
class SocketChannelSelectionKey {
}
class SocketChannelImpl {
  int connectAddress;
  int connected;
  int connectionPending;
  int socket;
  int channel;
}
class ServerSocketChannelSelectionKey {
}
class ServerSocketChannelImpl {
  int connected;
  int serverSocket;
  int channel;
}
class SelectorProviderImpl {
  int epoll_failed;
  int SELECTOR_IMPL;
  int SELECTOR_IMPL_EPOLL;
  int SELECTOR_IMPL_KQUEUE;
}
class SelectorImpl {
  int unhandledWakeup;
  int selectThread;
  int selectThreadMutex;
  int selected;
  int keys;
}
class SelectionKeyImpl {
  int ch;
  int impl;
  int interestOps;
  int readyOps;
}
class PipeImpl {
  int source;
  int sink;
  class SinkChannelImpl {
    int vmch;
  }
  class SourceChannelImpl {
    int vmch;
  }
}
class OutputStreamChannel {
  int out;
  int closed;
}
class NIOSocketImpl {
  int channel;
}
class NIOSocket {
  int channel;
}
class NIOServerSocket {
  int channel;
}
class NIODatagramSocket {
  int channel;
  int impl;
}
class NIOConstants {
  int DEFAULT_TIMEOUT;
}
class KqueueSelectorImpl {
  int OP_WRITE;
  int OP_READ;
  int OP_CONNECT;
  int OP_ACCEPT;
  int events;
  int blockedThread;
  int selected;
  int keys;
  int kq;
  int INITIAL_CAPACITY;
  int CAP_INCREMENT;
  int MAX_DOUBLING_CAPACITY;
  int _sizeof_struct_kevent;
}
class KqueueSelectionKeyImpl {
  int natChannel;
  int channel;
  int selector;
  int fd;
  int key;
  int activeOps;
  int readyOps;
  int interestOps;
}
class InputStreamChannel {
  int in;
  int closed;
}
class FileLockImpl {
  int valid;
}
class FileChannelImpl {
  int description;
  int mode;
  int ch;
  int err;
  int out;
  int in;
  int DSYNC;
  int SYNC;
  int EXCL;
  int APPEND;
  int WRITE;
  int READ;
}
class EpollSelectorImpl {
  int CAPACITY_INCREMENT;
  int MAX_DOUBLING_CAPACITY;
  int INITIAL_CAPACITY;
  int events;
  int waitingThread;
  int selectedKeys;
  int keys;
  int epoll_fd;
  int OP_WRITE;
  int OP_READ;
  int OP_CONNECT;
  int OP_ACCEPT;
  int sizeof_struct_epoll_event;
  int DEFAULT_EPOLL_SIZE;
}
class EpollSelectionKeyImpl {
  int cancelled;
  int valid;
  int key;
  int selectedOps;
  int interestOps;
  int channel;
  int selector;
  int fd;
}
class DatagramChannelSelectionKey {
}
class DatagramChannelImpl {
  int inChannelOperation;
  int channel;
  int socket;
}
class ChannelWriter {
  int charBuffer;
  int byteBuffer;
  int enc;
  int byteChannel;
  int DEFAULT_BUFFER_CAP;
}
class ChannelReader {
  int charBuffer;
  int byteBuffer;
  int decoder;
  int channel;
  int DEFAULT_BUFFER_CAP;
}
class ChannelOutputStream {
  int ch;
}
class ChannelInputStream {
  int ch;
}
