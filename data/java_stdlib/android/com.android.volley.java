package com.android.volley;
class VolleyLog {
  class MarkerLog {
    int mFinished;
    int mMarkers;
    class Marker {
      int time;
      int thread;
      int name;
    }
    int MIN_DURATION_FOR_LOGGING_MS;
    int ENABLED;
  }
  int DEBUG;
  int TAG;
}
class VolleyError {
  int networkResponse;
}
class TimeoutError {
}
class ServerError {
}
class RetryPolicy {
}
class ResponseDeliveryTest {
  int mSuccessResponse;
  int mRequest;
  int mDelivery;
}
class ResponseDelivery {
}
class Response {
  int intermediate;
  int error;
  int cacheEntry;
  int result;
  class ErrorListener {
  }
  class Listener {
  }
}
class RequestTest {
  class TestRequest {
    int mPriority;
  }
}
class RequestQueueTest {
  class DelayedRequest {
    int mDeliveredCount;
    int mParsedCount;
    int mDelayMillis;
  }
  class OrderCheckingNetwork {
    int mSemaphore;
    int mLastSequence;
    int mLastPriority;
  }
  int mDelivery;
}
class RequestQueue {
  class RequestFilter {
  }
  int mCacheDispatcher;
  int mDispatchers;
  int mDelivery;
  int mNetwork;
  int mCache;
  int DEFAULT_NETWORK_THREAD_POOL_SIZE;
  int mNetworkQueue;
  int mCacheQueue;
  int mCurrentRequests;
  int mWaitingRequests;
  int sSequenceGenerator;
}
class Request {
  class Priority {
    int IMMEDIATE;
    int HIGH;
    int NORMAL;
    int LOW;
  }
  int mTag;
  int mCacheEntry;
  int mRetryPolicy;
  int SLOW_REQUEST_THRESHOLD_MS;
  int mRequestBirthTime;
  int mResponseDelivered;
  int mDrainable;
  int mCanceled;
  int mShouldCache;
  int mRequestQueue;
  int mSequence;
  int mErrorListener;
  int mUrl;
  int mEventLog;
  int DEFAULT_POST_PARAMS_ENCODING;
}
class ParseError {
}
class NoConnectionError {
}
class NetworkResponse {
  int notModified;
  int headers;
  int data;
  int statusCode;
}
class NetworkError {
}
class NetworkDispatcherTest {
  int TIMEOUT_MILLIS;
  int CANNED_DATA;
  int mRequest;
  int mCache;
  int mNetwork;
  int mNetworkQueue;
  int mDelivery;
  int mDispatcher;
}
class NetworkDispatcher {
  int mQuit;
  int mDelivery;
  int mCache;
  int mNetwork;
  int mQueue;
}
class Network {
}
class ExecutorDelivery {
  class ResponseDeliveryRunnable {
    int mRunnable;
    int mResponse;
    int mRequest;
  }
  int mDiscardBefore;
  int mResponsePoster;
}
class DefaultRetryPolicy {
  int DEFAULT_BACKOFF_MULT;
  int DEFAULT_MAX_RETRIES;
  int DEFAULT_TIMEOUT_MS;
  int mBackoffMultiplier;
  int mMaxNumRetries;
  int mCurrentRetryCount;
  int mCurrentTimeoutMs;
}
class CacheDispatcherTest {
  int TIMEOUT_MILLIS;
  int mRequest;
  int mDelivery;
  int mCache;
  int mNetworkQueue;
  int mCacheQueue;
  int mDispatcher;
}
class CacheDispatcher {
  int mQuit;
  int mDelivery;
  int mCache;
  int mNetworkQueue;
  int mCacheQueue;
  int DEBUG;
}
class Cache {
  class Entry {
    int softTtl;
    int ttl;
    int serverDate;
    int etag;
    int data;
  }
}
class AuthFailureError {
  int mResolutionIntent;
}
