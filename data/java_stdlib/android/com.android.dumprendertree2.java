package com.android.dumprendertree2;
class VisualDiffUtils {
  int DONT_PRINT_LINE_NUMBER;
}
class TextResult {
  int mHandler;
  int mDumpChildFramesAsText;
  int mResultObtainedMsg;
  int mResultCode;
  int mDidTimeOut;
  int mRelativePath;
  int mActualResult;
  int mExpectedResultPath;
  int mExpectedResult;
  int MSG_DOCUMENT_AS_TEXT;
}
class TestsListPreloaderThread {
  int mDoneMsg;
  int mRelativePath;
  int mFileFilter;
  int mTestsList;
  int LOG_TAG;
}
class TestsListActivity {
  int mEverythingFinished;
  int mOnEverythingFinishedCallback;
  int mTotalTestCount;
  int mTestsList;
  int mHandler;
  int sProgressDialog;
  int EXTRA_TEST_PATH;
  int MSG_TEST_LIST_PRELOADER_DONE;
}
class SummarizerDBHelper {
  int mContext;
  int mDb;
  int mDbHelper;
  class DatabaseHelper {
  }
  int TABLES_NAMES;
  int UNEXPECTED_PASSES_TABLE;
  int EXPECTED_PASSES_TABLE;
  int UNEXPECTED_FAILURES_TABLE;
  int EXPECTED_FAILURES_TABLE;
  int DATABASE_VERSION;
  int DATABASE_NAME;
  int KEY_BYTES;
  int KEY_PATH;
  int KEY_ID;
}
class Summarizer {
  int mDbHelper;
  int mResultsSinceLastDbAccess;
  int mResultsSinceLastHtmlDump;
  int mDate;
  int mTestsRelativePath;
  int mResultsRootDirPath;
  int mFileFilter;
  int mExpectedPassesCursor;
  int mUnexpectedPassesCursor;
  int mExpectedFailuresCursor;
  int mUnexpectedFailuresCursor;
  int mUnexpectedPasses;
  int mExpectedPasses;
  int mExpectedFailures;
  int mUnexpectedFailures;
  int mCrashedTestsCount;
  int RESULTS_PER_DB_ACCESS;
  int RESULTS_PER_DUMP;
  int TXT_SUMMARY_RELATIVE_PATH;
  int HTML_DETAILS_RELATIVE_PATH;
  int SCRIPT;
  int CSS;
  int LOG_TAG;
}
class ManagerService {
  int mAllTestsRelativePath;
  int mLastExpectedResultPathFetched;
  int mLastExpectedResultPathRequested;
  int mCurrentlyRunningTestIndex;
  int mCurrentlyRunningTest;
  int mSummarizer;
  int mInternalMessagesHandler;
  int mMessenger;
  int mIncomingHandler;
  int MSG_RESET;
  int MSG_CURRENT_TEST_CRASHED;
  int MSG_FIRST_TEST;
  int MSG_ALL_TESTS_FINISHED;
  int MSG_PROCESS_ACTUAL_RESULTS;
  int IMAGE_RESULT_EXTENSION;
  int TEXT_RESULT_EXTENSION;
  int EXPECTED_RESULT_LOCATION_RELATIVE_DIR_PREFIXES;
  int RESULTS_ROOT_DIR_PATH;
  int CRASH_TIMEOUT_MS;
  int MSG_SUMMARIZER_DONE;
  int MSG_CRASH_TIMEOUT_EXPIRED;
  int LOG_TAG;
}
class LayoutTestsExecutor {
  int mLayoutTestControllerHandler;
  int WEBKIT_USES_PAGE_CACHE_PREFERENCE_KEY;
  int WEBKIT_OFFLINE_WEB_APPLICATION_CACHE_ENABLED;
  int MSG_SET_XSS_AUDITOR_ENABLED;
  int MSG_OVERRIDE_PREFERENCE;
  int MSG_SET_GEOLOCATION_PERMISSION;
  int MSG_DUMP_DATABASE_CALLBACKS;
  int MSG_SET_CAN_OPEN_WINDOWS;
  int MSG_DUMP_CHILD_FRAMES_AS_TEXT;
  int MSG_DUMP_AS_TEXT;
  int MSG_NOTIFY_DONE;
  int MSG_WAIT_UNTIL_DONE;
  class WebViewWithJavascriptInterfaces {
  }
  int mWebChromeClient;
  int mWebViewClient;
  int mResultHandler;
  int mServiceConnection;
  int mManagerServiceMessenger;
  int mScreenDimLock;
  int mEventSender;
  int mPendingGeolocationPermissionCallbacks;
  int mGeolocationPermission;
  int mIsGeolocationPermissionSet;
  int mDumpDatabaseCallbacks;
  int mCanOpenWindows;
  int mLayoutTestController;
  int mCurrentAdditionalTextOutput;
  int mCurrentResult;
  int mCurrentTestTimedOut;
  int mCurrentState;
  int mCurrentTestUri;
  int mCurrentTestRelativePath;
  int mCurrentWebView;
  int mTotalTestCount;
  int mCurrentTestIndex;
  int mTestsList;
  int DEFAULT_TIME_OUT_MS;
  int MSG_TEST_TIMED_OUT;
  int MSG_ACTUAL_RESULT_OBTAINED;
  int EXTRA_TEST_INDEX;
  int EXTRA_TESTS_FILE;
  int LOG_TAG;
  class CurrentState {
    int OBTAINING_RESULT;
    int WAITING_FOR_ASYNCHRONOUS_TEST;
    int RENDERING_PAGE;
    int IDLE;
  }
}
class LayoutTestController {
  int mLayoutTestsExecutor;
  int LOG_TAG;
}
class FsUtils {
  class UrlDataGetter {
    int mGetComplete;
    int mBytes;
    int mUrl;
  }
  int sHttpClient;
  int HTTP_TIMEOUT_MS;
  int SCRIPT_URL;
  int LOG_TAG;
}
class FileFilter {
  int mSlowList;
  int mFailList;
  int mCrashList;
  int TOKEN_SLOW;
  int TOKEN_FAIL;
  int TOKEN_CRASH;
  int SSL_PATH;
  int HTTP_TESTS_PATH;
  int TEST_EXPECTATIONS_TXT_PATH;
  int LOG_TAG;
}
class EventSenderImpl {
  int mEventSenderHandler;
  int mWebView;
  int mMousePoint;
  int mTouchMetaState;
  int mTouchPoints;
  class TouchPoint {
    int mCancelled;
    int mMoved;
    int mReleased;
    int mDownTime;
    int mPoint;
    int mId;
  }
  class Point {
    int mY;
    int mX;
  }
  int MSG_CANCEL_TOUCH_POINT;
  int MSG_SET_TOUCH_MODIFIER;
  int MSG_TOUCH_END;
  int MSG_RELEASE_TOUCH_POINT;
  int MSG_TOUCH_CANCEL;
  int MSG_CLEAR_TOUCH_POINTS;
  int MSG_TOUCH_MOVE;
  int MSG_UPDATE_TOUCH_POINT;
  int MSG_TOUCH_START;
  int MSG_ADD_TOUCH_POINT;
  int MSG_MOUSE_MOVE_TO;
  int MSG_MOUSE_CLICK;
  int MSG_MOUSE_UP;
  int MSG_MOUSE_DOWN;
  int MSG_KEY_DOWN;
  int MSG_LEAP_FORWARD;
  int MSG_FIRE_KEYBOARD_EVENTS_TO_ELEMENT;
  int MSG_ENABLE_DOM_UI_EVENT_LOGGING;
  int LOG_TAG;
}
class EventSender {
  int mEventSenderImpl;
}
class CrashedDummyResult {
  int mRelativePath;
}
class AdditionalTextOutput {
  int mOutputs;
  class OutputType {
    int CONSOLE_MESSAGE;
    int EXCEEDED_DB_QUOTA_MESSAGE;
    int JS_DIALOG;
  }
  int LOG_TAG;
}
class AbstractResult {
  int mAdditionalTextOutputString;
  class ResultCode {
    int NO_ACTUAL_RESULT;
    int NO_EXPECTED_RESULT;
    int RESULTS_DIFFER;
    int RESULTS_MATCH;
    int mTitle;
  }
  class TestType {
    int RENDER_TREE;
    int TEXT;
  }
  int LOG_TAG;
}
