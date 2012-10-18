package org.apache.harmony.luni.tests.internal.net.www.protocol.http;
class HttpURLConnectionTest {
  class TestProxySelector {
    int server_port;
    int proxy_port;
  }
  class MockProxyServer {
    int acceptedAuthorizedRequest;
  }
  class MockServer {
    int started;
    int accepted;
    int serverSocket;
  }
  int bound;
}
