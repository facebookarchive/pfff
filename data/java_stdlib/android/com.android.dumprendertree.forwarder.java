package com.android.dumprendertree.forwarder;
class Forwarder {
  class SocketPipe {
    int out;
    int in;
  }
  int BUFFER_SIZE;
  int LOGTAG;
  int to;
  int from;
  int server;
}
class ForwardService {
  int FORWARD_HOST_CONF;
  int DEFAULT_TEST_HOST;
  int LOGTAG;
  int inst;
  int fs8443;
  int fs8080;
  int fs8000;
}
class ForwardServer {
  class ServerRunner {
    int socket;
  }
  int forwarders;
  int started;
  int serverSocket;
  int localPort;
  int remoteAddress;
  int remotePort;
  int LOGTAG;
}
class AdbUtils {
  int LOGTAG;
  int ADB_RESPONSE_SIZE;
  int ADB_HOST;
  int ADB_PORT;
  int ADB_OK;
}
