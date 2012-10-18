package com.android.server.sip;
class SipWakeupTimer {
  class MyEventComparator {
  }
  class MyEvent {
    int mCallback;
    int mLastTriggerTime;
    int mTriggerTime;
    int mMaxPeriod;
    int mPeriod;
  }
  int mExecutor;
  int mPendingIntent;
  int mEventQueue;
  int mAlarmManager;
  int mContext;
  int DEBUG_TIMER;
  int TRIGGER_TIME;
  int TAG;
}
class SipWakeLock {
  int mHolders;
  int mTimerWakeLock;
  int mWakeLock;
  int mPowerManager;
  int TAG;
  int DEBUG;
}
class SipSessionListenerProxy {
  int mListener;
  int TAG;
}
class SipSessionGroup {
  class KeepAliveProcessCallbackProxy {
    int mCallback;
  }
  class MakeCallCommand {
    int mTimeout;
    int mSessionDescription;
  }
  class RegisterCommand {
    int mDuration;
  }
  class SipSessionImpl {
    class KeepAliveProcess {
      int mInterval;
      int mRPort;
      int mPortChanged;
      int mCallback;
      int mRunning;
      int TAG;
    }
    class SessionTimer {
      int mRunning;
    }
    int mReplaces;
    int mReferredBy;
    int mReferSession;
    int mKeepAliveSession;
    int mKeepAliveProcess;
    int mAuthenticationRetryCount;
    int mSessionTimer;
    int mInCall;
    int mPeerSessionDescription;
    int mClientTransaction;
    int mServerTransaction;
    int mDialog;
    int mInviteReceived;
    int mState;
    int mProxy;
    int mPeerProfile;
  }
  class KeepAliveProcessCallback {
  }
  class SipSessionCallReceiverImpl {
  }
  int mExternalPort;
  int mExternalIp;
  int mSessionMap;
  int mWakeLock;
  int mWakeupTimer;
  int mLocalIp;
  int mCallReceiverSession;
  int mSipHelper;
  int mSipStack;
  int mPassword;
  int mLocalProfile;
  int CONTINUE_CALL;
  int HOLD_CALL;
  int END_CALL;
  int DEREGISTER;
  int WAKE_LOCK_HOLDING_TIME;
  int INCALL_KEEPALIVE_INTERVAL;
  int KEEPALIVE_TIMEOUT;
  int END_CALL_TIMER;
  int CANCEL_CALL_TIMER;
  int EXPIRY_TIME;
  int THREAD_POOL_SIZE;
  int ANONYMOUS;
  int DEBUG_PING;
  int DEBUG;
  int TAG;
}
class SipService {
  class MyExecutor {
  }
  class ConnectivityReceiver {
  }
  class AutoRegistrationProcess {
    int mKeepAliveSuccessCount;
    int mRunning;
    int mErrorMessage;
    int mErrorCode;
    int mExpiryTime;
    int mRegistered;
    int mBackoff;
    int mProxy;
    int mKeepAliveSession;
    int mSession;
    int TAG;
    int MIN_KEEPALIVE_SUCCESS_COUNT;
  }
  class IntervalMeasurementProcess {
    int mPassCount;
    int mInterval;
    int mMaxInterval;
    int mMinInterval;
    int mSession;
    int mGroup;
    int mLocalProfile;
    int NAT_MEASUREMENT_RETRY_INTERVAL;
    int MAX_RETRY_COUNT;
    int PASS_THRESHOLD;
    int MIN_INTERVAL;
    int TAG;
  }
  class SipSessionGroupExt {
    int mAutoRegistration;
    int mOpenedToReceiveCalls;
    int mIncomingCallPendingIntent;
    int mSipGroup;
  }
  int mLastGoodKeepAliveInterval;
  int mKeepAliveInterval;
  int mMyWakeLock;
  int mConnectivityReceiver;
  int mPendingSessions;
  int mSipGroups;
  int mExecutor;
  int mIntervalMeasurementProcess;
  int mSipOnWifiOnly;
  int mWifiLock;
  int mTimer;
  int mNetworkType;
  int mLocalIp;
  int mContext;
  int DEFAULT_MAX_KEEPALIVE_INTERVAL;
  int DEFAULT_KEEPALIVE_INTERVAL;
  int MIN_EXPIRY_TIME;
  int SHORT_EXPIRY_TIME;
  int EXPIRY_TIME;
  int DEBUG;
  int TAG;
}
class SipHelper {
  int mMessageFactory;
  int mHeaderFactory;
  int mAddressFactory;
  int mSipProvider;
  int mSipStack;
  int DEBUG_PING;
  int DEBUG;
  int TAG;
}
