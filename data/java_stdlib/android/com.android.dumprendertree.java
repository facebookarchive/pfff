package com.android.dumprendertree;
class WebViewEventSender {
  int mTouchMetaState;
  int mTouchPoints;
  class TouchPoint {
    int mCancelled;
    int mMoved;
    int mReleased;
    int mDownTime;
    int mY;
    int mX;
    int mId;
  }
  int mouseY;
  int mouseX;
  int mWebView;
  int LOGTAG;
}
class TestShellCallback {
}
class TestShellActivity {
  int mPendingGeolocationPermissionCallbacks;
  int mGeolocationPermission;
  int mIsGeolocationPermissionSet;
  int DRAW_TIME_LOG;
  int DRAW_RUNS;
  int STOP_ON_REF_ERROR;
  int CURRENT_TEST_NUMBER;
  int TOTAL_TEST_COUNT;
  int SAVE_IMAGE;
  int GET_DRAW_TIME;
  int UI_AUTO_TEST;
  int TIMEOUT_IN_MILLIS;
  int RESULT_FILE;
  int TEST_URL;
  int LOGTAG;
  int MSG_DUMP_TIMEOUT;
  int MSG_WEBKIT_DATA;
  int MSG_TIMEOUT;
  int DUMP_TIMEOUT_MS;
  int TIMEOUT_STR;
  int mDumpWebKitData;
  int mPageFinished;
  int mCanOpenWindows;
  int mConsoleMessages;
  int mDatabaseCallbackStrings;
  int mDumpDatabaseCallbacks;
  int mWebHistory;
  int mKeepWebHistory;
  int mDialogStrings;
  int mTitleChanges;
  int mDumpTitleChanges;
  int mWaitUntilDone;
  int mDumpChildFramesAsText;
  int mDumpTopFrameAsText;
  int mDefaultDumpDataType;
  int mDumpDataType;
  int mFinishedRunning;
  int mRequestedWebKitData;
  int mTimedOut;
  int mStopOnRefError;
  int mCurrentTestNumber;
  int mTotalTestCount;
  int mTestListReader;
  int mUiAutoTestPath;
  int mTimeoutInMillis;
  int mResultFile;
  int mTestUrl;
  int mCallbackProxy;
  int mCallback;
  int mHandler;
  int mEventSender;
  int mWebView;
  int mWebViewClassic;
  class NewWindowWebView {
  }
  int mChromeClient;
  int mViewClient;
  class AsyncHandler {
  }
  int WEBKIT_USES_PAGE_CACHE_PREFERENCE_KEY;
  int WEBKIT_OFFLINE_WEB_APPLICATION_CACHE_ENABLED;
  class DumpDataType {
    int NO_OP;
    int EXT_REPR;
    int DUMP_AS_TEXT;
  }
}
class ReliabilityTestActivity {
  class PageDoneRunner {
  }
  class WebViewStatusChecker {
    int initialStartCount;
  }
  class SimpleChromeClient {
    int timeoutCounter;
  }
  class SimpleWebViewClient {
  }
  int pageDoneRunner;
  int pageLoadTime;
  int startTime;
  int manualDelay;
  int pageStartCount;
  int pageDoneLock;
  int pageDone;
  int logTime;
  int timeoutFlag;
  int handler;
  int chromeClient;
  int webViewClient;
  int webView;
  int LOGTAG;
  int MSG_NAV_LOGTIME;
  int MSG_NAV_URL;
  int MSG_NAVIGATE;
  int MSG_TIMEOUT;
  int RESULT_TIMEOUT;
  int PARAM_TIMEOUT;
  int PARAM_URL;
  int TEST_URL_ACTION;
}
class Menu {
  int LAYOUT_TESTS_LIST_FILE;
  int LOGTAG;
  int MENU_START;
}
class LoadTestsAutoTest {
  int mForwardServer;
  int LOAD_TEST_RUNNER_FILES;
  int mFinished;
  int LOCAL_PORT;
  int MAX_GC_WAIT_SEC;
  int LOAD_TEST_RESULT;
  int LOGTAG;
}
class LayoutTestsAutoTest {
  int mResumeIndex;
  int mTestCount;
  int mFinished;
  int mTestPathPrefix;
  int mJsEngine;
  int mRebaselineResults;
  int mTestListIgnoreResult;
  int mTestList;
  int mResultRecorder;
  int LAYOUT_TESTS_RUNNER;
  int LAYOUT_RESULTS_CRASHED_RESULT_FILE;
  int LAYOUT_RESULTS_NONTEXT_RESULT_FILE;
  int LAYOUT_RESULTS_FAILED_RESULT_FILE;
  int LAYOUT_TESTS_RESULTS_REFERENCE_FILES;
  int TEST_STATUS_FILE;
  int LAYOUT_TESTS_LIST_FILE;
  int ANDROID_EXPECTED_RESULT_DIR;
  int LAYOUT_TESTS_RESULT_DIR;
  int LAYOUT_TESTS_ROOT;
  int EXTERNAL_DIR;
  int DEFAULT_TIMEOUT_IN_MILLIS;
  int LOGTAG;
}
class MyTestRecorder {
  int mBufferedOutputNoResultStream;
  int mBufferedOutputIgnoreResultStream;
  int mBufferedOutputFailedStream;
  int mBufferedOutputPassedStream;
}
class LayoutTestsAutoRunner {
  int mJsEngine;
  int mRebaseline;
  int mTimeoutInMillis;
  int mTestPath;
  int mPageCyclerIteration;
  int mPageCyclerForwardHost;
  int mPageCyclerSuite;
}
class LayoutTestController {
}
class HTMLHostApp {
}
class FsUtils {
  int HTTP_WML_TESTS_PREFIX;
  int HTTP_MEDIA_TESTS_PREFIX;
  int HTTP_LOCAL_TESTS_PREFIX;
  int HTTPS_TESTS_PREFIX;
  int HTTP_TESTS_PREFIX;
  int EXTERNAL_DIR;
  int LOGTAG;
}
class FileList {
  int RUN_TESTS;
  int OPEN_DIRECTORY;
  int mFocusIndex;
  int mFocusFile;
  int mBaseLength;
  int mPath;
}
class FileFilter {
  int ignoreTestList;
  int nonTestDirs;
  int ignoreResultList;
  int LOGTAG;
}
class EventSender {
}
class CallbackProxy {
  int SET_XSS_AUDITOR_ENABLED;
  int LAYOUT_DUMP_CHILD_FRAMES_TEXT;
  int OVERRIDE_PREFERENCE;
  int SET_GEOLOCATION_PERMISSION;
  int LAYOUT_SET_CAN_OPEN_WINDOWS;
  int LAYOUT_DUMP_DATABASE_CALLBACKS;
  int LAYOUT_WAIT_UNTIL_DONE;
  int LAYOUT_TEST_REPAINT;
  int LAYOUT_SET_WINDOW_KEY;
  int LAYOUT_MAIN_FIRST_RESP;
  int LAYOUT_SET_ACCEPT_EDIT;
  int LAYOUT_REPAINT_HORZ;
  int LAYOUT_QUEUE_SCRIPT;
  int LAYOUT_QUEUE_RELOAD;
  int LAYOUT_QUEUE_LOAD;
  int LAYOUT_QUEUE_FWD_NAV;
  int LAYOUT_QUEUE_BACK_NAV;
  int LAYOUT_NOTIFY_DONE;
  int LAYOUT_KEEP_WEB_HISTORY;
  int LAYOUT_DUMP_TITLE_CHANGES;
  int LAYOUT_DUMP_SEL_RECT;
  int LAYOUT_DUMP_EDIT_CB;
  int LAYOUT_DUMP_CHILD_SCROLL;
  int LAYOUT_DUMP_HISTORY;
  int LAYOUT_DUMP_TEXT;
  int LAYOUT_DISPLAY;
  int LAYOUT_CLEAR_LIST;
  int EVENT_SET_TOUCH_MODIFIER;
  int EVENT_CANCEL_TOUCH_POINT;
  int EVENT_CLEAR_TOUCH_POINTS;
  int EVENT_RELEASE_TOUCH_POINT;
  int EVENT_UPDATE_TOUCH_POINT;
  int EVENT_ADD_TOUCH_POINT;
  int EVENT_TOUCH_CANCEL;
  int EVENT_TOUCH_END;
  int EVENT_TOUCH_MOVE;
  int EVENT_TOUCH_START;
  int EVENT_MOUSE_UP;
  int EVENT_MOUSE_MOVE;
  int EVENT_MOUSE_DOWN;
  int EVENT_MOUSE_CLICK;
  int EVENT_LEAP;
  int EVENT_KEY_DOWN_2;
  int EVENT_KEY_DOWN_1;
  int EVENT_FIRE_KBD;
  int EVENT_DOM_LOG;
  int mLayoutTestController;
  int mEventSender;
}
