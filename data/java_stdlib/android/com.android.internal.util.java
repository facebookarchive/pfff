package com.android.internal.util;
class XmlUtils_Delegate {
}
class XmlUtils {
}
class WithFramework {
}
class TypedProperties {
  int STRING_SET;
  int STRING_NULL;
  int STRING_NOT_SET;
  int STRING_TYPE_MISMATCH;
  class TypeException {
  }
  int TYPE_ERROR;
  int TYPE_STRING;
  int TYPE_DOUBLE;
  int TYPE_FLOAT;
  int TYPE_LONG;
  int TYPE_INT;
  int TYPE_SHORT;
  int TYPE_BYTE;
  int TYPE_BOOLEAN;
  int TYPE_UNSET;
  int NULL_STRING;
  class ParseException {
  }
}
class Hsm1 {
  int mP2;
  int mS2;
  int mS1;
  int mP1;
  class P2 {
  }
  class S2 {
  }
  class S1 {
  }
  class P1 {
  }
  int CMD_5;
  int CMD_4;
  int CMD_3;
  int CMD_2;
  int CMD_1;
  int TAG;
}
class StateMachineTest {
  int waitObject;
  int sharedCounter;
  class StateMachineSharedThread {
    int mS1;
    int mMaxCount;
    class S1 {
    }
  }
  class StateMachineUnhandledMessage {
    int mS1;
    int mUnhandledMessageCount;
    int mThisSm;
    class S1 {
    }
  }
  class StateMachine7 {
    int mArrivalTimeMsg3;
    int mArrivalTimeMsg2;
    int mMsgCount;
    int mS2;
    int mS1;
    int mThisSm;
    class S2 {
    }
    class S1 {
    }
    int SM7_DELAY_TIME;
  }
  class StateMachine6 {
    int mArrivalTimeMsg2;
    int mArrivalTimeMsg1;
    int mS1;
    int mThisSm;
    class S1 {
    }
  }
  class StateMachine5 {
    int mChildState5ExitCount;
    int mChildState5EnterCount;
    int mChildState4ExitCount;
    int mChildState4EnterCount;
    int mChildState3ExitCount;
    int mChildState3EnterCount;
    int mParentState2ExitCount;
    int mParentState2EnterCount;
    int mChildState2ExitCount;
    int mChildState2EnterCount;
    int mChildState1ExitCount;
    int mChildState1EnterCount;
    int mParentState1ExitCount;
    int mParentState1EnterCount;
    int mChildState5;
    int mChildState4;
    int mChildState3;
    int mParentState2;
    int mChildState2;
    int mChildState1;
    int mParentState1;
    int mThisSm;
    class ChildState5 {
    }
    class ChildState4 {
    }
    class ChildState3 {
    }
    class ParentState2 {
    }
    class ChildState2 {
    }
    class ChildState1 {
    }
    class ParentState1 {
    }
  }
  class StateMachine4 {
    int mChildState2;
    int mChildState1;
    int mParentState;
    int mThisSm;
    class ChildState2 {
    }
    class ChildState1 {
    }
    class ParentState {
    }
  }
  class StateMachine3 {
    int mChildState;
    int mParentState;
    int mThisSm;
    class ChildState {
    }
    class ParentState {
    }
  }
  class StateMachine2 {
    int mDidExit;
    int mDidEnter;
    int mS2;
    int mS1;
    int mThisSm;
    class S2 {
    }
    class S1 {
    }
  }
  class StateMachine1 {
    int mExitCount;
    int mEnterCount;
    int mS1;
    int mThisSm;
    class S1 {
    }
  }
  class StateMachine0 {
    int mS1;
    int mThisSm;
    class S1 {
    }
  }
  class StateMachineEnterExitTransitionToTest {
    int mS4ExitCount;
    int mS4EnterCount;
    int mS3ExitCount;
    int mS3EnterCount;
    int mS2ExitCount;
    int mS2EnterCount;
    int mS1ExitCount;
    int mS1EnterCount;
    int mS4;
    int mS3;
    int mS2;
    int mS1;
    int mThisSm;
    class S4 {
    }
    class S3 {
    }
    class S2 {
    }
    class S1 {
    }
  }
  class StateMachineQuitTest {
    int mS1;
    int mThisSm;
    class S1 {
    }
    int mQuitCount;
  }
  int TAG;
  int WAIT_FOR_DEBUGGER;
  int DBG;
  int TEST_CMD_6;
  int TEST_CMD_5;
  int TEST_CMD_4;
  int TEST_CMD_3;
  int TEST_CMD_2;
  int TEST_CMD_1;
}
class StateMachine {
  int mSmThread;
  int mSmHandler;
  class SmHandler {
    class QuittingState {
    }
    class HaltingState {
    }
    int mDeferredMessages;
    int mDestState;
    int mInitialState;
    int mStateInfo;
    class StateInfo {
      int active;
      int parentStateInfo;
      int state;
    }
    int mSm;
    int mQuittingState;
    int mHaltingState;
    int mTempStateStackCount;
    int mTempStateStack;
    int mStateStackTopIndex;
    int mStateStack;
    int mIsConstructionCompleted;
    int mProcessedMessages;
    int mMsg;
    int mSmHandlerObj;
    int mDbg;
  }
  class ProcessedMessages {
    int mCount;
    int mOldestIndex;
    int mMaxSize;
    int mMessages;
    int DEFAULT_SIZE;
  }
  class ProcessedMessageInfo {
    int mOrgState;
    int mState;
    int mInfo;
    int mWhat;
    int mTime;
  }
  int NOT_HANDLED;
  int HANDLED;
  int SM_INIT_CMD;
  int SM_QUIT_CMD;
  int mName;
  int TAG;
}
class State {
}
class Protocol {
  int BASE_NSD_MANAGER;
  int BASE_DNS_PINGER;
  int BASE_DATA_CONNECTION_TRACKER;
  int BASE_DATA_CONNECTION_AC;
  int BASE_DATA_CONNECTION;
  int BASE_DHCP;
  int BASE_WIFI_MANAGER;
  int BASE_WIFI_MONITOR;
  int BASE_WIFI_P2P_SERVICE;
  int BASE_WIFI_P2P_MANAGER;
  int BASE_WIFI_WATCHDOG;
  int BASE_WIFI;
  int BASE_SYSTEM_ASYNC_CHANNEL;
  int BASE_SYSTEM_RESERVED;
  int MAX_MESSAGE;
}
class ProcFileReaderTest {
}
class ProcFileReader {
  int mLineFinished;
  int mTail;
  int mBuffer;
  int mStream;
}
class PredicatesTest {
  int FALSE;
  int TRUE;
}
class Predicates {
  class NotPredicate {
    int predicate;
  }
  class OrPredicate {
    int components;
  }
  class AndPredicate {
    int components;
  }
}
class Predicate {
}
class Preconditions {
}
class Objects {
}
class MemInfoReader {
  int mCachedSize;
  int mFreeSize;
  int mTotalSize;
  int mBuffer;
}
class JournaledFile {
  int mWriting;
  int mTemp;
  int mReal;
}
class IndentingPrintWriter {
  int mEmptyLine;
  int mCurrent;
  int mBuilder;
  int mIndent;
}
class IState {
  int NOT_HANDLED;
  int HANDLED;
}
class HexDump {
  int HEX_DIGITS;
}
class FileRotatorTest {
  class RecordingReader {
    int mActual;
  }
  int YELLOW;
  int BLUE;
  int GREEN;
  int RED;
  int TEST_TIME;
  int ANOTHER_PREFIX;
  int PREFIX;
  int mBasePath;
  int TAG;
}
class FileRotator {
  class FileInfo {
    int endMillis;
    int startMillis;
    int prefix;
  }
  class Rewriter {
  }
  class Writer {
  }
  class Reader {
  }
  int SUFFIX_NO_BACKUP;
  int SUFFIX_BACKUP;
  int mDeleteAgeMillis;
  int mRotateAgeMillis;
  int mPrefix;
  int mBasePath;
  int LOGD;
  int TAG;
}
class FastXmlSerializer {
  int mInTag;
  int mBytes;
  int mCharset;
  int mOutputStream;
  int mWriter;
  int mPos;
  int mText;
  int BUFFER_LEN;
  int ESCAPE_TABLE;
}
class FastMath {
}
class CharSequencesTest {
}
class CharSequences {
}
class BitwiseStreamsTest {
  int LOG_TAG;
}
class BitwiseOutputStream {
  class AccessException {
  }
  int mEnd;
  int mPos;
  int mBuf;
}
class BitwiseInputStream {
  class AccessException {
  }
  int mEnd;
  int mPos;
  int mBuf;
}
class AsyncService {
  int mAsyncServiceInfo;
  class AsyncServiceInfo {
    int mRestartFlags;
    int mHandler;
  }
  int mHandler;
  int mMessenger;
  int CMD_ASYNC_SERVICE_DESTROY;
  int CMD_ASYNC_SERVICE_ON_START_INTENT;
  int DBG;
  int TAG;
}
class AsyncChannelTest {
  int TAG;
  int WAIT_FOR_DEBUGGER;
  int DBG;
}
class AsyncChannel {
  class AsyncChannelConnection {
  }
  class SyncMessenger {
    class SyncHandler {
      int mResultMsg;
      int mLockObject;
    }
    int mMessenger;
    int mHandler;
    int mHandlerThread;
    int sCount;
    int sStack;
  }
  int mDstMessenger;
  int mSrcMessenger;
  int mSrcHandler;
  int mSrcContext;
  int mConnection;
  int STATUS_FULL_CONNECTION_REFUSED_ALREADY_CONNECTED;
  int STATUS_SEND_UNSUCCESSFUL;
  int STATUS_BINDING_UNSUCCESSFUL;
  int STATUS_SUCCESSFUL;
  int sCmdToString;
  int CMD_TO_STRING_COUNT;
  int CMD_CHANNEL_DISCONNECTED;
  int CMD_CHANNEL_DISCONNECT;
  int CMD_CHANNEL_FULLY_CONNECTED;
  int CMD_CHANNEL_FULL_CONNECTION;
  int CMD_CHANNEL_HALF_CONNECTED;
  int BASE;
  int DBG;
  int TAG;
}
class ArrayUtils {
  int sCache;
  int CACHE_SIZE;
  int EMPTY;
}
