package com.android.frameworkperf;
class TestService {
  class ReadFileOp {
    int mBuffer;
    int mRAF;
    int mFile;
  }
  class WriteFileOp {
    int mBuffer;
    int mRAF;
    int mFile;
  }
  class CreateWriteSyncFileOp {
    int mFile;
  }
  class CreateWriteFileOp {
    int mFile;
  }
  class CreateFileOp {
    int mFile;
  }
  class LoadLargeScaledBitmapOp {
    int mContext;
  }
  class LoadSmallScaledBitmapOp {
    int mContext;
  }
  class LoadRecycleLargeBitmapOp {
    int mContext;
  }
  class LoadLargeBitmapOp {
    int mContext;
  }
  class LoadRecycleSmallBitmapOp {
    int mContext;
  }
  class LoadSmallBitmapOp {
    int mContext;
  }
  class CreateRecycleBitmapOp {
    int mContext;
  }
  class CreateBitmapOp {
    int mContext;
  }
  class LayoutInflaterImageButtonOp {
    int mContext;
  }
  class LayoutInflaterButtonOp {
    int mContext;
  }
  class LayoutInflaterViewOp {
    int mContext;
  }
  class LayoutInflaterLargeOp {
    int mContext;
  }
  class LayoutInflaterOp {
    int mContext;
  }
  class ParseLargeXmlResOp {
    int mContext;
  }
  class ParseXmlResOp {
    int mContext;
  }
  class ReadXmlAttrsOp {
    int mAttrs;
    int mParser;
    int mContext;
  }
  class OpenXmlResOp {
    int mContext;
  }
  class IpcOp {
    int mProcessName;
    int mPm;
  }
  class MethodCallOp {
  }
  class PaintGcOp {
  }
  class FinalizingGcOp {
    class Finalizable {
    }
  }
  class ObjectGcOp {
  }
  class GcOp {
  }
  class SchedulerOp {
  }
  class CpuOp {
  }
  class NoOp {
  }
  class Op {
    int mLongName;
    int mName;
  }
  class RunnerThread {
    int mPriority;
    int mOp;
  }
  class TestRunner {
    int mForegroundOps;
    int mForegroundEndTime;
    int mBackgroundOps;
    int mBackgroundEndTime;
    int mForegroundRunning;
    int mBackgroundRunning;
    int mStartTime;
    int mForegroundThread;
    int mBackgroundThread;
    int mDoneCallback;
    int mBackgroundOp;
    int mForegroundOp;
    int mMaxOps;
    int mMaxRunTime;
    int mHandler;
  }
  class BackgroundMode {
    int SCHEDULER;
    int CPU;
    int NOTHING;
  }
  int mRunner;
  int mHandler;
  int RES_TERMINATED;
  int RES_TEST_FINISHED;
  int MSG_REALLY_TERMINATE;
  int MSG_REALLY_START;
  int CMD_TERMINATE;
  int CMD_START_TEST;
  int mAvailOps;
  int mOpPairs;
  int TAG;
}
class TestArgs {
  int CREATOR;
  int bgOp;
  int fgOp;
  int combOp;
  int maxOps;
  int maxTime;
}
class SimpleInflater {
  int mContext;
  int XML_ITEM;
  int XML_GROUP;
  int XML_MENU;
}
class SchedulerService {
}
class RunResult {
  int CREATOR;
  int bgOps;
  int bgTime;
  int fgOps;
  int fgTime;
  int bgLongName;
  int fgLongName;
  int name;
}
class Receiver {
}
class LocalTestService {
}
class FrameworkPerfTest {
  int TEST_TIMEOUT;
}
class FrameworkPerfActivity {
  int mMessenger;
  int mHandler;
  int MSG_DO_NEXT_TEST;
  class TestConnection {
    int mLinked;
    int mService;
  }
  int mResultNotifier;
  int mResults;
  int mConnectionBound;
  int mCurConnection;
  int mCurOpIndex;
  int mBgTest;
  int mFgTest;
  int mBgTestIndex;
  int mFgTestIndex;
  int mLimitLabels;
  int mAvailOpDescriptions;
  int mAvailOpLabels;
  int mStarted;
  int mLimitIsIterations;
  int mMaxRunTime;
  int mPartialWakeLock;
  int mLog;
  int mLocalCheckBox;
  int mStopButton;
  int mStartButton;
  int mTestTime;
  int mLimitLabel;
  int mLimitSpinner;
  int mBgSpinner;
  int mFgSpinner;
  int DEBUG;
  int TAG;
}
