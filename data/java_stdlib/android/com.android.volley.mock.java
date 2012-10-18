package com.android.volley.mock;
class WaitableQueue {
  class MagicStopRequest {
  }
  int mStopEvent;
  int mStopRequest;
}
class MockResponseDelivery {
  int responsePosted;
  int postError_called;
  int postResponse_called;
}
class MockRequest {
  int mPriority;
  int cancel_called;
  int deliverError_called;
  int parseResponse_called;
  int deliverResponse_called;
  int mCacheKey;
  int mPostParams;
}
class MockNetwork {
  int requestHandled;
  int mDataToReturn;
  int mNumExceptionsToThrow;
  int ALWAYS_THROW_EXCEPTIONS;
}
class MockHttpStack {
  int mLastPostBody;
  int mLastHeaders;
  int mLastUrl;
  int mResponseToReturn;
}
class MockHttpClient {
  int requestExecuted;
  int mResponseEntity;
  int mStatusCode;
}
class MockCache {
  int entryPut;
  int keyPut;
  int putCalled;
  int mFakeEntry;
  int getCalled;
  int clearCalled;
}
