package org.apache.harmony.luni.tests.internal.net.www.protocol.https;
class HttpsURLConnectionTest {
  class ClientConnectionWork {
    int connection;
  }
  class ServerWork {
    int peerSocket;
    int responseCode;
    int needProxyAuthentication;
    int actAsProxy;
    int serverSocket;
  }
  class Work {
    int out;
    int clientsData;
    int respAuthenticationRequired;
    int httpsResponseTail;
    int httpsResponseContent;
    int proxyResponse;
    int responseHead;
  }
  class TestHostnameVerifier {
    int verified;
  }
  int store;
  int AUTHENTICATION_REQUIRED_CODE;
  int NOT_FOUND_CODE;
  int OK_CODE;
  int TIMEOUT;
  int DO_LOG;
  int KS_PASSWORD;
}
