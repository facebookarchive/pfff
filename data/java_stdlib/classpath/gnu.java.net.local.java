package gnu.java.net.local;
class LocalSocketImpl {
  class LocalOutputStream {
    int impl;
  }
  class LocalInputStream {
    int impl;
  }
  int remote;
  int local;
  int socket_fd;
  int out;
  int in;
  int created;
}
class LocalSocketAddress {
  int path;
}
class LocalSocket {
  int localConnected;
  int localClosed;
  int localimpl;
}
class LocalServerSocket {
  int closed;
  int myImpl;
}
