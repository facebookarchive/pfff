package com.android.commands.am;
class Am {
  class InstrumentationWatcher {
    int mRawMode;
    int mFinished;
  }
  class IntentReceiver {
    int mFinished;
  }
  class MyActivityController {
    int mGotGdbPrint;
    int mGdbThread;
    int mGdbProcess;
    int mResult;
    int RESULT_ANR_WAIT;
    int RESULT_ANR_KILL;
    int RESULT_ANR_DIALOG;
    int RESULT_EARLY_ANR_KILL;
    int RESULT_EARLY_ANR_CONTINUE;
    int RESULT_CRASH_KILL;
    int RESULT_CRASH_DIALOG;
    int RESULT_DEFAULT;
    int mState;
    int STATE_ANR;
    int STATE_EARLY_ANR;
    int STATE_CRASHED;
    int STATE_NORMAL;
    int mGdbPort;
  }
  int NO_CLASS_ERROR_CODE;
  int NO_SYSTEM_ERROR_CODE;
  int FATAL_ERROR_CODE;
  int mProfileFile;
  int mUserId;
  int mRepeat;
  int mStopOption;
  int mWaitOption;
  int mStartFlags;
  int mCurArgData;
  int mNextArg;
  int mArgs;
  int mAm;
}
