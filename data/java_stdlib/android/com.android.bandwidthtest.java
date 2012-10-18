package com.android.bandwidthtest;
class NetworkState {
  int mReason;
  int mTransitionDirection;
  int mTransitionTarget;
  int mStateDepository;
  int LOG_TAG;
  class StateTransitionDirection {
    int DO_NOTHING;
    int TO_CONNECTION;
    int TO_DISCONNECTION;
  }
}
class BandwidthTestRunner {
  int mTestServer;
  int mSsid;
}
class BandwidthTest {
  int mRunner;
  int mDeviceId;
  int mTestServer;
  int mSsid;
  int mUid;
  int mTManager;
  int mConnectionUtil;
  int mContext;
  int FILE_SIZE;
  int TMP_FILENAME;
  int BASE_DIR;
  int INSTRUMENTATION_IN_PROGRESS;
  int PROC_LABEL;
  int PROF_LABEL;
  int LOG_TAG;
}
