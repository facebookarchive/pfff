package com.android.internal.telephony.test;
class SimulatedRadioControl {
}
class SimulatedGsmCallState {
  int CONNECTING_PAUSE_MSEC;
  int MAX_CALLS;
  int EVENT_PROGRESS_CALL_STATE;
  int nextDialFailImmediately;
  int autoProgressConnecting;
  int calls;
}
class InvalidStateEx {
}
class CallInfo {
  int TOA;
  int number;
  int isMpty;
  int state;
  int isMT;
  class State {
    int WAITING;
    int INCOMING;
    int ALERTING;
    int DIALING;
    int HOLDING;
    int ACTIVE;
    int value;
  }
}
class SimulatedCommands {
  int nextCallFailCause;
  int pausedResponses;
  int pausedResponseCount;
  int mSsnNotifyOn;
  int mPin2Code;
  int mNetworkType;
  int mPuk2UnlockAttempts;
  int mPin2UnlockAttempts;
  int mSimFdnEnabled;
  int mSimFdnEnabledState;
  int mPinCode;
  int mPukUnlockAttempts;
  int mPinUnlockAttempts;
  int mSimLockEnabled;
  int mSimLockedState;
  int mHandlerThread;
  int simulatedCallState;
  int SIM_PUK2_CODE;
  int DEFAULT_SIM_PIN2_CODE;
  int INITIAL_FDN_STATE;
  int SIM_PUK_CODE;
  int DEFAULT_SIM_PIN_CODE;
  int INITIAL_LOCK_STATE;
  class SimFdnState {
    int SIM_PERM_LOCKED;
    int REQUIRE_PUK2;
    int REQUIRE_PIN2;
    int NONE;
  }
  class SimLockState {
    int SIM_PERM_LOCKED;
    int REQUIRE_PUK;
    int REQUIRE_PIN;
    int NONE;
  }
  int LOG_TAG;
}
class ModelInterpreter {
  int sDefaultResponses;
  int PROGRESS_CALL_STATE;
  int pausedResponseMonitor;
  int pausedResponseCount;
  int mHandlerThread;
  int simulatedCallState;
  int finalResponse;
  int ss;
  int lineReader;
  int out;
  int in;
  int LOG_TAG;
  int CONNECTING_PAUSE_MSEC;
  int MAX_CALLS;
}
class InterpreterEx {
  int result;
}
class LineReader {
  int inStream;
  int buffer;
  int BUFFER_SIZE;
}
