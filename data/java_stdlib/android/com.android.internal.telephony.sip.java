package com.android.internal.telephony.sip;
class SipPhoneFactory {
}
class SipPhoneBase {
  int state;
  int mRingbackRegistrants;
  int LOG_TAG;
}
class SipPhone {
  class SipAudioCallAdapter {
  }
  class SipConnection {
    int mAdapter;
    int mIncoming;
    int mOriginalNumber;
    int mPeer;
    int mState;
    int mSipAudioCall;
    int mOwner;
  }
  class SipCall {
  }
  int mProfile;
  int mSipManager;
  int backgroundCall;
  int foregroundCall;
  int ringingCall;
  int TIMEOUT_HOLD_CALL;
  int TIMEOUT_ANSWER_CALL;
  int TIMEOUT_MAKE_CALL;
  int DEBUG;
  int LOG_TAG;
}
class SipConnectionBase {
  int postDialState;
  int mCause;
  int holdingStartTime;
  int duration;
  int connectTimeReal;
  int disconnectTime;
  int connectTime;
  int createTime;
  int isIncoming;
  int nextPostDialChar;
  int postDialString;
  int dialString;
  int mSipAudioCall;
  int LOG_TAG;
}
class SipCommandInterface {
}
class SipCallBase {
  int connections;
}
