package android.net.http;
class Timer {
  int mLast;
  int mStart;
}
class SslError {
  int mUrl;
  int mCertificate;
  int mErrors;
  int SSL_MAX_ERROR;
  int SSL_INVALID;
  int SSL_DATE_INVALID;
  int SSL_UNTRUSTED;
  int SSL_IDMISMATCH;
  int SSL_EXPIRED;
  int SSL_NOTYETVALID;
}
class SslCertificateTest {
  int Issue1597Certificate;
}
class SslCertificate {
  class DName {
    int mUName;
    int mOName;
    int mCName;
    int mDName;
  }
  int X509_CERTIFICATE;
  int VALID_NOT_AFTER;
  int VALID_NOT_BEFORE;
  int ISSUED_BY;
  int ISSUED_TO;
  int mX509Certificate;
  int mValidNotAfter;
  int mValidNotBefore;
  int mIssuedBy;
  int mIssuedTo;
  int ISO_8601_DATE_FORMAT;
}
class RequestQueue {
  class ConnectionManager {
  }
  class SyncFeeder {
    int mRequest;
  }
  class ActivePool {
    int mConnectionCount;
    int mTotalConnection;
    int mTotalRequest;
    int mIdleCache;
    int mThreads;
  }
  int CONNECTION_COUNT;
  int mProxyChangeReceiver;
  int mProxyHost;
  int mConnectivityManager;
  int mActivePool;
  int mContext;
  int mPending;
}
class RequestHandle {
  int MAX_REDIRECT_COUNT;
  int PROXY_AUTHORIZATION_HEADER;
  int AUTHORIZATION_HEADER;
  int mConnection;
  int mRedirectCount;
  int mBodyLength;
  int mBodyProvider;
  int mRequest;
  int mRequestQueue;
  int mHeaders;
  int mMethod;
  int mUri;
  int mUrl;
}
class RequestFeeder {
}
class Request {
  int requestContentProcessor;
  int mLoadingPaused;
  int mClientResource;
  int CONTENT_LENGTH_HEADER;
  int ACCEPT_ENCODING_HEADER;
  int HOST_HEADER;
  int mBodyLength;
  int mBodyProvider;
  int mReceivedBytes;
  int mFailCount;
  int mCancelled;
  int mProxyHost;
  int mHost;
  int mPath;
  int mHttpRequest;
  int mConnection;
  int mEventHandler;
}
class LoggingEventHandler {
}
class IdleCache {
  class IdleReaper {
  }
  int mReused;
  int mCached;
  int mThread;
  int mCount;
  int mEntries;
  int CHECK_INTERVAL;
  int TIMEOUT;
  int EMPTY_CHECK_MAX;
  int IDLE_CACHE_MAX;
  class Entry {
    int mTimeout;
    int mConnection;
    int mHost;
  }
}
class SSLConnectionClosedByUserException {
}
class HttpsConnection {
  int mProxyHost;
  int mAborted;
  int mSuspended;
  int mSuspendLock;
  int mSslSocketFactory;
}
class HttpResponseCacheTest {
  int server;
  int cacheDir;
}
class HttpResponseCache {
  int delegate;
}
class HttpLog {
  int LOGV;
  int DEBUG;
  int LOGTAG;
}
class HttpConnection {
}
class HttpAuthHeader {
  int mPassword;
  int mUsername;
  int mIsProxy;
  int mAlgorithm;
  int mQop;
  int mOpaque;
  int mNonce;
  int mRealm;
  int mStale;
  int DIGEST;
  int BASIC;
  int UNKNOWN;
  int mScheme;
  int ALGORITHM_TOKEN;
  int QOP_TOKEN;
  int OPAQUE_TOKEN;
  int STALE_TOKEN;
  int NONCE_TOKEN;
  int REALM_TOKEN;
  int DIGEST_TOKEN;
  int BASIC_TOKEN;
}
class Headers {
  class HeaderCallback {
  }
  int mExtraHeaderValues;
  int mExtraHeaderNames;
  int sHeaderNames;
  int mHeaders;
  int cookies;
  int connectionType;
  int contentLength;
  int transferEncoding;
  int HEADER_COUNT;
  int IDX_X_PERMITTED_CROSS_DOMAIN_POLICIES;
  int IDX_REFRESH;
  int IDX_PRAGMA;
  int IDX_SET_COOKIE;
  int IDX_ETAG;
  int IDX_LAST_MODIFIED;
  int IDX_CACHE_CONTROL;
  int IDX_EXPIRES;
  int IDX_ACCEPT_RANGES;
  int IDX_CONTENT_DISPOSITION;
  int IDX_PROXY_AUTHENTICATE;
  int IDX_WWW_AUTHENTICATE;
  int IDX_PROXY_CONNECTION;
  int IDX_LOCATION;
  int IDX_CONN_DIRECTIVE;
  int IDX_CONTENT_ENCODING;
  int IDX_CONTENT_TYPE;
  int IDX_CONTENT_LEN;
  int IDX_TRANSFER_ENCODING;
  int HASH_X_PERMITTED_CROSS_DOMAIN_POLICIES;
  int HASH_REFRESH;
  int HASH_PRAGMA;
  int HASH_SET_COOKIE;
  int HASH_ETAG;
  int HASH_LAST_MODIFIED;
  int HASH_CACHE_CONTROL;
  int HASH_EXPIRES;
  int HASH_ACCEPT_RANGES;
  int HASH_CONTENT_DISPOSITION;
  int HASH_PROXY_AUTHENTICATE;
  int HASH_WWW_AUTHENTICATE;
  int HASH_PROXY_CONNECTION;
  int HASH_LOCATION;
  int HASH_CONN_DIRECTIVE;
  int HASH_CONTENT_ENCODING;
  int HASH_CONTENT_TYPE;
  int HASH_CONTENT_LEN;
  int HASH_TRANSFER_ENCODING;
  int X_PERMITTED_CROSS_DOMAIN_POLICIES;
  int REFRESH;
  int PRAGMA;
  int SET_COOKIE;
  int ETAG;
  int LAST_MODIFIED;
  int CACHE_CONTROL;
  int EXPIRES;
  int ACCEPT_RANGES;
  int CONTENT_DISPOSITION;
  int PROXY_AUTHENTICATE;
  int WWW_AUTHENTICATE;
  int PROXY_CONNECTION;
  int LOCATION;
  int CONN_DIRECTIVE;
  int CONTENT_ENCODING;
  int CONTENT_TYPE;
  int CONTENT_LEN;
  int TRANSFER_ENCODING;
  int NO_CONTENT_LENGTH;
  int NO_TRANSFER_ENCODING;
  int NO_CONN_TYPE;
  int CONN_KEEP_ALIVE;
  int CONN_CLOSE;
  int LOGTAG;
}
class EventHandler {
  int TOO_MANY_REQUESTS_ERROR;
  int FILE_NOT_FOUND_ERROR;
  int FILE_ERROR;
  int ERROR_BAD_URL;
  int ERROR_FAILED_SSL_HANDSHAKE;
  int ERROR_UNSUPPORTED_SCHEME;
  int ERROR_REDIRECT_LOOP;
  int ERROR_TIMEOUT;
  int ERROR_IO;
  int ERROR_CONNECT;
  int ERROR_PROXYAUTH;
  int ERROR_AUTH;
  int ERROR_UNSUPPORTED_AUTH_SCHEME;
  int ERROR_LOOKUP;
  int ERROR;
  int OK;
}
class ErrorStrings {
  int LOGTAG;
}
class DefaultHttpClientTest {
  int server;
}
class DefaultHttpClientProxyTest {
}
class CookiesTest {
  int server;
}
class ConnectionThread {
  int mConnection;
  int mId;
  int mRequestFeeder;
  int mConnectionManager;
  int mContext;
  int mRunning;
  int mWaiting;
  int mTotalThreadTime;
  int mCurrentThreadTime;
  int WAIT_TICK;
  int WAIT_TIMEOUT;
}
class Connection {
  int mBuf;
  int mRequestFeeder;
  int HTTP_CONNECTION;
  int MAX_PIPE;
  int MIN_PIPE;
  int RETRY_REQUEST_LIMIT;
  int mActive;
  int STATE_CANCEL_REQUESTED;
  int STATE_NORMAL;
  int mHttpContext;
  int mCanPersist;
  int mHost;
  int mCertificate;
  int mHttpClientConnection;
  int mContext;
  int states;
  int DONE;
  int DRAIN;
  int READ;
  int SEND;
  int SOCKET_TIMEOUT;
}
class CharArrayBuffers {
  int uppercaseAddon;
}
class CertificateChainValidator {
  int sVerifier;
  int sInstance;
}
class AndroidHttpClientProxyTest {
}
class AndroidHttpClientConnection {
  int socket;
  int open;
  int metrics;
  int requestWriter;
  int entityserializer;
  int maxLineLength;
  int maxHeaderCount;
  int outbuffer;
  int inbuffer;
}
class AndroidHttpClient {
  class CurlLogger {
  }
  int curlConfiguration;
  class LoggingConfiguration {
    int level;
    int tag;
  }
  int mLeakedException;
  int delegate;
  int sThreadCheckInterceptor;
  int textContentTypes;
  int TAG;
  int SOCKET_OPERATION_TIMEOUT;
  int DEFAULT_SYNC_MIN_GZIP_BYTES;
}
