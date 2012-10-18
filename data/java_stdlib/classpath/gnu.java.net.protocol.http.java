package gnu.java.net.protocol.http;
class SimpleCookieManager {
  int cookies;
}
class ResponseHeaderHandler {
}
class Response {
  int body;
  int headers;
  int message;
  int code;
  int minorVersion;
  int majorVersion;
}
class RequestBodyWriter {
}
class Request {
  int nonce;
  int dispatched;
  int authenticator;
  int responseHeaderHandlers;
  int requestBodyWriter;
  int requestHeaders;
  int path;
  int method;
  int connection;
}
class LimitedLengthInputStream {
  int doClose;
  int in;
  int eof;
  int connection;
  int restrictLen;
  int remainingLen;
}
class Headers {
  class HeaderElement {
    int value;
    int name;
  }
  int dateFormat;
  int headers;
}
class Handler {
}
class HTTPURLConnection {
  int handshakeEvent;
  int errorSink;
  int responseSink;
  int response;
  int requestMethodSetExplicitly;
  int requestSink;
  int requestHeaders;
  int request;
  int keepAlive;
  int agent;
  int proxyPort;
  int proxyHostname;
  int connection;
}
class HTTPDateFormat {
  int MONTHS;
  int DAYS_OF_WEEK;
}
class HTTPConnection {
  int timeLastUsed;
  int useCount;
  class Pool {
    int reaper;
    class Reaper {
    }
    int connectionTTL;
    int maxConnections;
    int connectionPool;
    int instance;
  }
  int pool;
  int cookieManager;
  int nonceCounts;
  int out;
  int in;
  int sslSocketFactory;
  int socket;
  int handshakeCompletedListeners;
  int minorVersion;
  int majorVersion;
  int proxyPort;
  int proxyHostname;
  int timeout;
  int connectionTimeout;
  int secure;
  int port;
  int hostname;
  int userAgent;
  int HTTPS_PORT;
  int HTTP_PORT;
}
class Credentials {
  int password;
  int username;
}
class CookieManager {
}
class Cookie {
  int expires;
  int secure;
  int path;
  int domain;
  int comment;
  int value;
  int name;
}
class ChunkedInputStream {
  int eof;
  int meta;
  int count;
  int size;
  int in;
  int headers;
}
class ByteArrayRequestBodyWriter {
  int pos;
  int content;
}
class Authenticator {
}
