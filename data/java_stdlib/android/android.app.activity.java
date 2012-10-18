package android.app.activity;
class TestedScreen {
  class Idler {
  }
  int mHandler;
  int CLEAR_TASK;
  int DELIVER_RESULT;
  int WAIT_BEFORE_FINISH;
}
class TestedActivity {
  class Idler {
  }
  int mHandler;
}
class SubActivityTest {
}
class SubActivityScreen {
  int mMode;
  int CHILD_OFFSET;
  int FINISH_SUB_MODE;
  int PENDING_RESULT_MODE;
  int RESULT_MODE;
  int NO_RESULT_MODE;
}
class SetTimeZonePermissionsTest {
  int mAlarm;
  int mCurrentZone;
  int mZones;
}
class ServiceTest {
  class TestConnection {
    int mCount;
    int mMonitor;
    int mSetReporter;
    int mExpectDisconnect;
  }
  class EmptyConnection {
  }
  int mStartReceiver;
  int mStartState;
  int STATE_UNBIND_ONLY;
  int STATE_REBIND;
  int STATE_DESTROY;
  int STATE_UNBIND;
  int STATE_START_2;
  int STATE_START_1;
  int REBIND_CODE;
  int UNBIND_CODE;
  int SET_REPORTER_CODE;
  int DESTROYED_CODE;
  int STARTED_CODE;
  int REPORT_OBJ_NAME;
  int SERVICE_LOCAL_DENIED;
  int SERVICE_LOCAL_GRANTED;
  int SERVICE_LOCAL;
}
class SearchableActivity {
}
class ResultReceiver {
}
class RemoteSubActivityScreen {
  int mFirst;
  int mHandler;
}
class RemoteReceiver {
}
class RemoteGrantedReceiver {
}
class RemoteDeniedReceiver {
}
class MetaDataTest {
}
class LocalService {
  int mStartCount;
  int mReportObject;
  int mBinder;
}
class LocalScreen {
}
class LocalReceiver {
}
class LocalProvider {
  class DatabaseHelper {
    int DATABASE_VERSION;
    int DATABASE_NAME;
  }
  int sURLMatcher;
  int DATA_ID;
  int DATA;
  int mOpenHelper;
  int TAG;
}
class LocalGrantedService {
}
class LocalGrantedReceiver {
}
class LocalDialog {
}
class LocalDeniedService {
}
class LocalDeniedReceiver {
}
class LocalActivity {
}
class LifecycleTest {
  int mTabIntent;
  int mTopIntent;
}
class LaunchpadTabActivity {
}
class LaunchpadActivity {
  int mReceiver;
  int mTimeout;
  int mUnregister;
  int mCallTarget;
  int ERROR_TRANSACTION;
  int GOT_RECEIVE_TRANSACTION;
  int mHandler;
  int sCallingTest;
  int mReceiverRegistered;
  int mReceivedData;
  int mExpectedData;
  int mNextReceiver;
  int mExpectedReceivers;
  int mNextLifecycle;
  int mExpectedLifecycle;
  int mResultStack;
  int mData;
  int mResultCode;
  int mStartTime;
  int mStarted;
  int mBadParcelable;
  int DO_LOCAL_DIALOG;
  int DO_LOCAL_SCREEN;
  int DO_FINISH;
  int ON_DESTROY;
  int ON_STOP;
  int ON_PAUSE;
  int ON_FREEZE;
  int ON_RESUME;
  int ON_RESTART;
  int ON_START;
  int DATA_2;
  int DATA_1;
  int RECEIVER_ABORT;
  int RECEIVER_REMOTE;
  int RECEIVER_LOCAL;
  int RECEIVER_REG;
  int BROADCAST_STICKY2;
  int BROADCAST_STICKY1;
  int BROADCAST_ABORT;
  int BROADCAST_MULTI;
  int BROADCAST_REPEAT;
  int BROADCAST_ALL;
  int BROADCAST_REMOTE;
  int BROADCAST_LOCAL;
  int BROADCAST_REGISTERED;
  int LIFECYCLE_FINISH_START;
  int LIFECYCLE_FINISH_CREATE;
  int LIFECYCLE_DIALOG;
  int LIFECYCLE_SCREEN;
  int LIFECYCLE_BASIC;
  int FORWARDED_RESULT;
  int LAUNCHED_RESULT;
  int BAD_PARCELABLE;
  int RETURNED_RESULT;
  int FORWARD_RESULT;
  int LAUNCH;
  class CallingTest {
  }
}
class MyBadParcelable {
  int CREATOR;
}
class LaunchTest {
}
class IntentSenderTest {
}
class ClearTop {
  int WAIT_CLEAR_TASK;
}
class BroadcastTest {
  class TestBroadcastReceiver {
    int mHaveResult;
  }
  int mReceiver;
  int mCallTarget;
  int mReceiverRegistered;
  int mReceivedData;
  int mExpectedData;
  int mNextReceiver;
  int mExpectedReceivers;
  int ERROR_TRANSACTION;
  int GOT_RECEIVE_TRANSACTION;
  int DATA_2;
  int DATA_1;
  int RECEIVER_RESULTS;
  int RECEIVER_ABORT;
  int RECEIVER_REMOTE;
  int RECEIVER_LOCAL;
  int RECEIVER_REG;
  int BROADCAST_FAIL_BIND;
  int BROADCAST_FAIL_REGISTER;
  int BROADCAST_STICKY2;
  int BROADCAST_STICKY1;
  int BROADCAST_ABORT;
  int BROADCAST_MULTI;
  int BROADCAST_ALL;
  int BROADCAST_REMOTE_DENIED;
  int BROADCAST_REMOTE_GRANTED;
  int BROADCAST_REMOTE;
  int BROADCAST_LOCAL_DENIED;
  int BROADCAST_LOCAL_GRANTED;
  int BROADCAST_LOCAL;
  int BROADCAST_REGISTERED;
  int BROADCAST_TIMEOUT;
}
class ActivityTestsBase {
  int mResultStack;
  int mData;
  int mResultCode;
  int mFinished;
  int mExpecting;
  int mIntermediates;
  int mIntent;
  int PERMISSION_DENIED;
  int PERMISSION_GRANTED;
}
class ActivityTests {
  int DEBUG_LIFECYCLE;
}
class ActivityManagerTest {
  int mActivityManager;
  int mContext;
}
class AbortReceiver {
}
