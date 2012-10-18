package android.core;
class TestWebServer {
  class Worker {
    int headers;
    int running;
    int hasContent;
    int readStarted;
    int testNum;
    int testID;
    int requestMethod;
    int s;
    int buf;
  }
  int EOL;
  int BUF_SIZE;
  class AcceptThread {
    int running;
    int ss;
  }
  int acceptedConnections;
  int acceptLimit;
  int redirectCode;
  int redirectHost;
  int chunked;
  int keepAlive;
  int mLog;
  int mPort;
  int mTimeout;
  int acceptT;
  int http11;
  int HTTP_VERSION_STRING;
  int DEFAULT_TIMEOUT;
  int DEFAULT_PORT;
  int workers;
  int timeout;
  int activeThreads;
  int threads;
  int LOGTAG;
}
class TestWebData {
  int testDir;
  int testType;
  int testName;
  int testLastModified;
  int testLength;
  int REDIRECT_307;
  int REDIRECT_303;
  int REDIRECT_302;
  int REDIRECT_301;
  int testServerResponse;
  int testParams;
  int tests;
  int postContent;
  int test2;
  int test1;
}
class TestHttpClient {
  int context;
  int connStrategy;
  int httpexecutor;
  int httpproc;
  int params;
}
class TestEventHandler {
  int LOGTAG;
  int netRunner;
  int mRequestHandle;
  int expectDetails;
  int expectHeaders;
  class TestHeader {
    int value;
    int name;
  }
  int expectCertificate;
  int expectSslErrors;
  int expectErrorId;
  int expectDataLength;
  int expectData;
  int expectPermanent;
  int expectLocation;
  int eventsReceived;
  int notExpecting;
  int expects;
  int expectCode;
  int expectMinor;
  int expectMajor;
  int TEST_NUM_EXPECTS;
  int TEST_SSL_CERTIFICATE_ERROR;
  int TEST_ERROR;
  int TEST_ENDDATA;
  int TEST_DATA;
  int TEST_LOCATION_CHANGED;
  int TEST_HEADERS;
  int TEST_STATUS;
  int TEST_REQUEST_SENT;
  int delayResponse;
  int useLowLevel;
  int syncObj;
  int headerMap;
  int reasonPhrase;
  int responseCode;
  int minorVersion;
  int majorVersion;
}
class SocketTest {
  int connected;
  int error;
  int client;
  int serverError;
  int PACKAGE_DROPPING_ADDRESS;
  int KNOW_GOOD_ADDRESS;
  int NON_EXISTING_ADDRESS;
}
class Sha1Test {
  int mTestData;
  class TestData {
    int result;
    int input;
  }
}
class SSLSocketTest {
  class ClientSessionCacheProxy {
    int ops;
    int delegate;
  }
  class FakeClientSessionCache {
    int sessions;
    int ops;
  }
  int handshakeException;
  int handshakeSocket;
  class TestClient {
    int provideKeys;
    int port;
    int exception;
    int trustManager;
  }
  class TestServer {
    int provideKeys;
    int clientAuth;
    int port;
    int exception;
    int trustManager;
    int CLIENT_AUTH_NEEDED;
    int CLIENT_AUTH_WANTED;
    int CLIENT_AUTH_NONE;
  }
  class TestTrustManager {
    int authType;
    int chain;
  }
  int PASSWORD;
  int CLIENT_KEYS_BKS;
  int CLIENT_KEYS_JKS;
  int SERVER_KEYS_BKS;
  int SERVER_KEYS_JKS;
  int multithreadedFetchRandom;
  int multithreadedFetchWins;
  int multithreadedFetchRuns;
  int clientFactory;
}
class RequestAPITest {
  int mTestWebServer;
  int mRequestQueue;
  int syncObj;
  int LOGTAG;
}
class ProxyTest {
  int mHttpHost;
  int mContext;
}
class MiscRegressionTest {
}
class LowLevelNetRunner {
  int count;
}
class JniLibTest {
}
class InetAddrTest {
  int HOSTS;
}
class HttpHeaderTest {
  int CACHE_CONTROL_COMPOUND2;
  int CACHE_CONTROL_COMPOUND;
  int CACHE_CONTROL_PRIVATE;
  int CACHE_CONTROL_MAX_AGE;
  int LAST_MODIFIED;
}
class HttpConstants {
  int REQ_SERVER;
  int REQ_LOCATION;
  int REQ_LAST_MODIFIED;
  int REQ_EXPIRES;
  int REQ_CONTENT_TYPE;
  int REQ_CONTENT_RANGE;
  int REQ_CONTENT_MD5;
  int REQ_CONTENT_LOCATION;
  int REQ_CONTENT_LENGTH;
  int REQ_CONTENT_LANGUAGE;
  int REQ_CONTENT_ENCODING;
  int REQ_ALLOW;
  int REQ_KEEP_ALIVE;
  int REQ_USER_AGENT;
  int REQ_TE;
  int REQ_REFERER;
  int REQ_RANGE;
  int REQ_PROXY_AUTHENTICATION;
  int REQ_MAX_FORWARDS;
  int REQ_IF_UNMODIFIED_SINCE;
  int REQ_IF_RANGE;
  int REQ_IF_NONE_MATCH;
  int REQ_IF_MODIFIED_SINCE;
  int REQ_IF_MATCH;
  int REQ_HOST;
  int REQ_FROM;
  int REQ_EXPECT;
  int REQ_AUTHORIZATION;
  int REQ_ACCEPT_LANGUAGE;
  int REQ_ACCEPT_ENCODING;
  int REQ_ACCEPT_CHARSET;
  int REQ_ACCEPT;
  int REQ_WARNING;
  int REQ_VIA;
  int REQ_UPGRADE;
  int REQ_TRANSFER_ENCODING;
  int REQ_TRAILER;
  int REQ_PRAGMA;
  int REQ_DATE;
  int REQ_CONNECTION;
  int REQ_CACHE_CONTROL;
  int REQ_UNKNOWN;
  int requestHeaders;
  int POST_METHOD;
  int HEAD_METHOD;
  int GET_METHOD;
  int UNKNOWN_METHOD;
  int HTTP_VERSION;
  int HTTP_GATEWAY_TIMEOUT;
  int HTTP_UNAVAILABLE;
  int HTTP_BAD_GATEWAY;
  int HTTP_INTERNAL_ERROR;
  int HTTP_SERVER_ERROR;
  int HTTP_UNSUPPORTED_TYPE;
  int HTTP_REQ_TOO_LONG;
  int HTTP_ENTITY_TOO_LARGE;
  int HTTP_PRECON_FAILED;
  int HTTP_LENGTH_REQUIRED;
  int HTTP_GONE;
  int HTTP_CONFLICT;
  int HTTP_CLIENT_TIMEOUT;
  int HTTP_PROXY_AUTH;
  int HTTP_NOT_ACCEPTABLE;
  int HTTP_BAD_METHOD;
  int HTTP_NOT_FOUND;
  int HTTP_FORBIDDEN;
  int HTTP_PAYMENT_REQUIRED;
  int HTTP_UNAUTHORIZED;
  int HTTP_BAD_REQUEST;
  int HTTP_USE_PROXY;
  int HTTP_NOT_MODIFIED;
  int HTTP_SEE_OTHER;
  int HTTP_MOVED_TEMP;
  int HTTP_MOVED_PERM;
  int HTTP_MULT_CHOICE;
  int HTTP_PARTIAL;
  int HTTP_RESET;
  int HTTP_NO_CONTENT;
  int HTTP_NOT_AUTHORITATIVE;
  int HTTP_ACCEPTED;
  int HTTP_CREATED;
  int HTTP_OK;
}
class HeapTest {
  class FinalizableObject {
  }
  int sLock;
  int sNumFinalized;
  int TAG;
}
