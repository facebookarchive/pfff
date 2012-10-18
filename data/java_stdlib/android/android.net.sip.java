package android.net.sip;
class SipSessionAdapter {
}
class SipSession {
  int mListener;
  int mSession;
  class Listener {
  }
  class State {
    int NOT_DEFINED;
    int ENDING_CALL;
    int PINGING;
    int IN_CALL;
    int OUTGOING_CALL_CANCELING;
    int OUTGOING_CALL_RING_BACK;
    int OUTGOING_CALL;
    int INCOMING_CALL_ANSWERING;
    int INCOMING_CALL;
    int DEREGISTERING;
    int REGISTERING;
    int READY_TO_CALL;
  }
  int TAG;
}
class SipRegistrationListener {
}
class SipProfile {
  class Builder {
    int mProxyAddress;
    int mDisplayName;
    int mUri;
    int mProfile;
    int mAddressFactory;
  }
  int CREATOR;
  int mCallingUid;
  int mAutoRegistration;
  int mSendKeepAlive;
  int mPort;
  int mAuthUserName;
  int mProfileName;
  int mProtocol;
  int mDomain;
  int mPassword;
  int mProxyAddress;
  int mAddress;
  int UDP;
  int TCP;
  int DEFAULT_PORT;
  int serialVersionUID;
}
class SipManager {
  class ListenerRelay {
    int mUri;
    int mListener;
  }
  int mContext;
  int mSipService;
  int TAG;
  int EXTRA_LOCAL_URI;
  int ACTION_SIP_REMOVE_PHONE;
  int ACTION_SIP_ADD_PHONE;
  int ACTION_SIP_INCOMING_CALL;
  int ACTION_SIP_SERVICE_UP;
  int EXTRA_OFFER_SD;
  int EXTRA_CALL_ID;
  int INCOMING_CALL_RESULT_CODE;
}
class SipException {
}
class SipErrorCode {
  int SERVER_UNREACHABLE;
  int CROSS_DOMAIN_AUTHENTICATION;
  int DATA_CONNECTION_LOST;
  int IN_PROGRESS;
  int INVALID_CREDENTIALS;
  int PEER_NOT_REACHABLE;
  int INVALID_REMOTE_URI;
  int TIME_OUT;
  int CLIENT_ERROR;
  int TRANSACTION_TERMINTED;
  int SERVER_ERROR;
  int SOCKET_ERROR;
  int NO_ERROR;
}
class SipAudioCall {
  int mErrorMessage;
  int mErrorCode;
  int mWifiHighPerfLock;
  int mWm;
  int mPendingCallRequest;
  int mHold;
  int mMuted;
  int mInCall;
  int mAudioGroup;
  int mAudioStream;
  int mPeerSd;
  int mSessionId;
  int mTransferringSession;
  int mSipSession;
  int mListener;
  int mLocalProfile;
  int mContext;
  class Listener {
  }
  int TRANSFER_TIMEOUT;
  int SESSION_TIMEOUT;
  int DONT_RELEASE_SOCKET;
  int RELEASE_SOCKET;
  int TAG;
}
class SimpleSessionDescription {
  class Fields {
    int mLines;
    int mOrder;
  }
  class Media {
    int mFormats;
    int mProtocol;
    int mPortCount;
    int mPort;
    int mType;
  }
  int mMedia;
  int mFields;
}
