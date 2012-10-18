package android.os;
class WorkSource {
  int CREATOR;
  int sGoneWork;
  int sNewbWork;
  int sTmpWorkSource;
  int mUids;
  int mNum;
}
class Vibrator {
}
class UserId {
  int MU_ENABLED;
  int USER_ALL;
  int PER_USER_RANGE;
}
class UpdateLock {
  int TIMESTAMP;
  int NOW_IS_CONVENIENT;
  int UPDATE_LOCK_CHANGED;
  int mTag;
  int mHeld;
  int mRefCounted;
  int mCount;
  int mToken;
  int sService;
  int TAG;
  int DEBUG;
}
class UEventObserver {
  class UEventThread {
    int mObservers;
  }
  int sThreadStarted;
  int sThread;
  class UEvent {
    int mMap;
  }
  int TAG;
}
class TransactionTooLargeException {
}
class TraceTest {
  class aThread {
  }
  int gMethodCalls;
  int fMethodCalls;
  int eMethodCalls;
  int TAG;
}
class Trace {
  int sEnabledTags;
  int PROPERTY_TRACE_TAG_ENABLEFLAGS;
  int TRACE_TAGS;
  int TRACE_FLAGS_START_BIT;
  int TRACE_TAG_VIDEO;
  int TRACE_TAG_AUDIO;
  int TRACE_TAG_SYNC_MANAGER;
  int TRACE_TAG_ACTIVITY_MANAGER;
  int TRACE_TAG_WINDOW_MANAGER;
  int TRACE_TAG_WEBVIEW;
  int TRACE_TAG_VIEW;
  int TRACE_TAG_INPUT;
  int TRACE_TAG_GRAPHICS;
  int TRACE_TAG_ALWAYS;
  int TRACE_TAG_NEVER;
}
class TokenWatcher {
  int mAcquired;
  int mNotificationQueue;
  int mTag;
  int mHandler;
  int mTokens;
  class Death {
    int tag;
    int token;
  }
  int mNotificationTask;
}
class TestHandlerThread {
  class LooperThread {
  }
  int mLooper;
  int mFailure;
  int mSuccess;
  int mDone;
}
class SystemVibrator {
  int mToken;
  int mService;
  int TAG;
}
class SystemService {
}
class SystemPropertiesTest {
  int KEY;
}
class SystemProperties {
  int sChangeCallbacks;
  int PROP_VALUE_MAX;
  int PROP_NAME_MAX;
}
class SystemClock_Delegate {
  int sBootTime;
}
class SystemClock {
}
class StrictMode {
  class InstanceTracker {
    int mKlass;
    int sInstanceCounts;
  }
  class InstanceCountViolation {
    int FAKE_STACK;
    int mLimit;
    int mInstances;
    int mClass;
  }
  class ViolationInfo {
    int numInstances;
    int broadcastIntentAction;
    int violationUptimeMillis;
    int violationNumThisLoop;
    int tags;
    int numAnimationsRunning;
    int durationMillis;
    int policy;
    int crashInfo;
  }
  int sExpectedActivityInstanceCount;
  int sWindowManager;
  int sThisThreadSpanState;
  class ThreadSpanState {
    int mFreeListSize;
    int mFreeListHead;
    int mActiveSize;
    int mActiveHead;
  }
  int NO_OP_SPAN;
  class Span {
    int mContainerState;
    int mPrev;
    int mNext;
    int mCreateMillis;
    int mName;
  }
  class LogStackTrace {
  }
  int sLastVmViolationTime;
  int sProcessIdleHandler;
  int sIsIdlerRegistered;
  int sLastInstanceCountCheckMillis;
  class AndroidCloseGuardReporter {
  }
  class AndroidBlockGuardPolicy {
    int mLastViolationTime;
    int mPolicyMask;
  }
  int threadHandler;
  int violationsBeingTimed;
  class StrictModeCustomViolation {
  }
  class StrictModeDiskWriteViolation {
  }
  class StrictModeDiskReadViolation {
  }
  class StrictModeNetworkViolation {
  }
  class StrictModeViolation {
  }
  int gatheredViolations;
  class VmPolicy {
    class Builder {
      int mClassInstanceLimitNeedCow;
      int mClassInstanceLimit;
      int mMask;
    }
    int classInstanceLimit;
    int mask;
    int LAX;
  }
  class ThreadPolicy {
    class Builder {
      int mMask;
    }
    int mask;
    int LAX;
  }
  int sDropboxCallsInFlight;
  int sVmPolicy;
  int sVmPolicyMask;
  int EMPTY_CLASS_LIMIT_MAP;
  int VM_PENALTY_MASK;
  int THREAD_PENALTY_MASK;
  int PENALTY_GATHER;
  int PENALTY_DROPBOX;
  int PENALTY_FLASH;
  int PENALTY_DEATH_ON_NETWORK;
  int PENALTY_DEATH;
  int PENALTY_DIALOG;
  int PENALTY_LOG;
  int ALL_VM_DETECT_BITS;
  int DETECT_VM_REGISTRATION_LEAKS;
  int DETECT_VM_INSTANCE_LEAKS;
  int DETECT_VM_ACTIVITY_LEAKS;
  int DETECT_VM_CLOSABLE_LEAKS;
  int DETECT_VM_CURSOR_LEAKS;
  int ALL_THREAD_DETECT_BITS;
  int DETECT_CUSTOM;
  int DETECT_NETWORK;
  int DETECT_DISK_READ;
  int DETECT_DISK_WRITE;
  int MAX_OFFENSES_PER_LOOP;
  int MAX_SPAN_TAGS;
  int MIN_DIALOG_INTERVAL_MS;
  int MIN_LOG_INTERVAL_MS;
  int VISUAL_PROPERTY;
  int DISABLE_PROPERTY;
  int IS_ENG_BUILD;
  int IS_USER_BUILD;
  int LOG_V;
  int TAG;
}
class StatFs {
  int mNativeContext;
}
class ServiceManagerProxy {
  int mRemote;
}
class ServiceManagerNative {
}
class ServiceManager {
}
class SchedulingPolicyService {
  int PRIORITY_MAX;
  int PRIORITY_MIN;
  int TAG;
}
class ResultReceiver {
  int CREATOR;
  class MyResultReceiver {
  }
  class MyRunnable {
    int mResultData;
    int mResultCode;
  }
  int mReceiver;
  int mHandler;
  int mLocal;
}
class RemoteMailException {
}
class RemoteException {
}
class RemoteCallbackList {
  class Callback {
    int mCookie;
    int mCallback;
  }
  int mKilled;
  int mBroadcastCount;
  int mActiveBroadcast;
  int mCallbacks;
}
class RemoteCallback {
  int CREATOR;
  class RemoteCallbackProxy {
  }
  class LocalCallback {
  }
  class DeliverResult {
    int mResult;
  }
  int mTarget;
  int mHandler;
}
class RegistrantList {
  int registrants;
}
class Registrant {
  int userObj;
  int what;
  int refH;
}
class RecoverySystem {
  class ProgressListener {
  }
  int LOG_FILE_MAX_LENGTH;
  int LAST_PREFIX;
  int LOG_FILE;
  int COMMAND_FILE;
  int RECOVERY_DIR;
  int PUBLISH_PROGRESS_INTERVAL_MS;
  int DEFAULT_KEYSTORE;
  int TAG;
}
class Process {
  class ProcessStartResult {
    int usingWrapper;
    int pid;
  }
  int PROC_OUT_FLOAT;
  int PROC_OUT_LONG;
  int PROC_OUT_STRING;
  int PROC_PARENS;
  int PROC_COMBINE;
  int PROC_TAB_TERM;
  int PROC_SPACE_TERM;
  int PROC_ZERO_TERM;
  int PROC_TERM_MASK;
  int ZYGOTE_RETRY_MILLIS;
  int sPreviousZygoteOpenFailed;
  int sZygoteWriter;
  int sZygoteInputStream;
  int sZygoteSocket;
  int SIGNAL_USR1;
  int SIGNAL_KILL;
  int SIGNAL_QUIT;
  int THREAD_GROUP_AUDIO_SYS;
  int THREAD_GROUP_AUDIO_APP;
  int THREAD_GROUP_SYSTEM;
  int THREAD_GROUP_FOREGROUND;
  int THREAD_GROUP_BG_NONINTERACTIVE;
  int THREAD_GROUP_DEFAULT;
  int SCHED_IDLE;
  int SCHED_BATCH;
  int SCHED_RR;
  int SCHED_FIFO;
  int SCHED_OTHER;
  int THREAD_PRIORITY_LESS_FAVORABLE;
  int THREAD_PRIORITY_MORE_FAVORABLE;
  int THREAD_PRIORITY_URGENT_AUDIO;
  int THREAD_PRIORITY_AUDIO;
  int THREAD_PRIORITY_URGENT_DISPLAY;
  int THREAD_PRIORITY_DISPLAY;
  int THREAD_PRIORITY_FOREGROUND;
  int THREAD_PRIORITY_BACKGROUND;
  int THREAD_PRIORITY_LOWEST;
  int THREAD_PRIORITY_DEFAULT;
  int BLUETOOTH_GID;
  int LAST_ISOLATED_UID;
  int FIRST_ISOLATED_UID;
  int LAST_APPLICATION_UID;
  int FIRST_APPLICATION_UID;
  int MEDIA_RW_GID;
  int NFC_UID;
  int VPN_UID;
  int SDCARD_RW_GID;
  int DRM_UID;
  int MEDIA_UID;
  int WIFI_UID;
  int LOG_UID;
  int SHELL_UID;
  int PHONE_UID;
  int SYSTEM_UID;
  int GOOGLE_SHARED_APP_CONTENT;
  int ANDROID_SHARED_MEDIA;
  int ZYGOTE_SOCKET;
  int LOG_TAG;
}
class ZygoteStartFailedEx {
}
class PowerManagerTest {
  int mPm;
}
class PowerManager {
  int mHandler;
  int mService;
  class WakeLock {
    int mWorkSource;
    int mHeld;
    int mRefCounted;
    int mCount;
    int mToken;
    int mTag;
    int mFlags;
    int mReleaser;
    int RELEASE_WAKE_LOCK;
  }
  int BRIGHTNESS_OFF;
  int BRIGHTNESS_DIM;
  int BRIGHTNESS_ON;
  int BRIGHTNESS_LOW_BATTERY;
  int ON_AFTER_RELEASE;
  int ACQUIRE_CAUSES_WAKEUP;
  int WAIT_FOR_PROXIMITY_NEGATIVE;
  int PROXIMITY_SCREEN_OFF_WAKE_LOCK;
  int SCREEN_DIM_WAKE_LOCK;
  int SCREEN_BRIGHT_WAKE_LOCK;
  int FULL_WAKE_LOCK;
  int PARTIAL_WAKE_LOCK;
  int LOCK_MASK;
  int WAKE_BIT_PROXIMITY_SCREEN_OFF;
  int WAKE_BIT_KEYBOARD_BRIGHT;
  int WAKE_BIT_SCREEN_BRIGHT;
  int WAKE_BIT_SCREEN_DIM;
  int WAKE_BIT_CPU_WEAK;
  int WAKE_BIT_CPU_STRONG;
  int TAG;
}
class PerformanceCollectorTest {
  class MockPerformanceResultsWriter {
    int timingResults;
    int timingLabel;
    int snapshotResults;
    int snapshotLabel;
  }
  int mPerfCollector;
}
class PerformanceCollector {
  int mExecTime;
  int mCpuTime;
  int mSnapshotExecTime;
  int mSnapshotCpuTime;
  int mPerfMeasurement;
  int mPerfSnapshot;
  int mPerfWriter;
  int METRIC_KEY_OTHER_SHARED_DIRTY;
  int METRIC_KEY_OTHER_PSS;
  int METRIC_KEY_OTHER_PRIVATE_DIRTY;
  int METRIC_KEY_GLOBAL_FREED_SIZE;
  int METRIC_KEY_GLOBAL_FREED_COUNT;
  int METRIC_KEY_GLOBAL_ALLOC_SIZE;
  int METRIC_KEY_GLOBAL_ALLOC_COUNT;
  int METRIC_KEY_NATIVE_SIZE;
  int METRIC_KEY_NATIVE_SHARED_DIRTY;
  int METRIC_KEY_NATIVE_PSS;
  int METRIC_KEY_NATIVE_PRIVATE_DIRTY;
  int METRIC_KEY_NATIVE_FREE;
  int METRIC_KEY_NATIVE_ALLOCATED;
  int METRIC_KEY_JAVA_SIZE;
  int METRIC_KEY_JAVA_SHARED_DIRTY;
  int METRIC_KEY_JAVA_PSS;
  int METRIC_KEY_JAVA_PRIVATE_DIRTY;
  int METRIC_KEY_JAVA_FREE;
  int METRIC_KEY_JAVA_ALLOCATED;
  int METRIC_KEY_GC_INVOCATION_COUNT;
  int METRIC_KEY_SENT_TRANSACTIONS;
  int METRIC_KEY_RECEIVED_TRANSACTIONS;
  int METRIC_KEY_PRE_SENT_TRANSACTIONS;
  int METRIC_KEY_PRE_RECEIVED_TRANSACTIONS;
  int METRIC_KEY_EXECUTION_TIME;
  int METRIC_KEY_CPU_TIME;
  int METRIC_KEY_LABEL;
  int METRIC_KEY_ITERATIONS;
  class PerformanceResultsWriter {
  }
}
class PatternMatcher {
  int CREATOR;
  int mType;
  int mPattern;
  int PATTERN_SIMPLE_GLOB;
  int PATTERN_PREFIX;
  int PATTERN_LITERAL;
}
class Parcelable {
  class ClassLoaderCreator {
  }
  class Creator {
  }
  int CONTENTS_FILE_DESCRIPTOR;
  int PARCELABLE_WRITE_RETURN_VALUE;
}
class ParcelUuid {
  int CREATOR;
  int mUuid;
}
class ParcelFormatException {
}
class ParcelFileDescriptor {
  int CREATOR;
  class AutoCloseOutputStream {
    int mFd;
  }
  class AutoCloseInputStream {
    int mFd;
  }
  int MODE_APPEND;
  int MODE_TRUNCATE;
  int MODE_CREATE;
  int MODE_READ_WRITE;
  int MODE_WRITE_ONLY;
  int MODE_READ_ONLY;
  int MODE_WORLD_WRITEABLE;
  int MODE_WORLD_READABLE;
  int mParcelDescriptor;
  int mClosed;
  int mFileDescriptor;
}
class ParcelBenchmark {
  int mParcel;
}
class ParcelArrayBenchmark {
  int mLongParcel;
  int mIntParcel;
  int mByteParcel;
  int mLongArray;
  int mIntArray;
  int mByteArray;
  int mWriteParcel;
  int mSize;
}
class Parcel {
  int mCreators;
  int STRING_CREATOR;
  int EX_HAS_REPLY_HEADER;
  int EX_ILLEGAL_STATE;
  int EX_NULL_POINTER;
  int EX_ILLEGAL_ARGUMENT;
  int EX_BAD_PARCELABLE;
  int EX_SECURITY;
  int VAL_CHARSEQUENCEARRAY;
  int VAL_BOOLEANARRAY;
  int VAL_SPARSEBOOLEANARRAY;
  int VAL_SERIALIZABLE;
  int VAL_BYTE;
  int VAL_LONGARRAY;
  int VAL_INTARRAY;
  int VAL_OBJECTARRAY;
  int VAL_PARCELABLEARRAY;
  int VAL_IBINDER;
  int VAL_STRINGARRAY;
  int VAL_BYTEARRAY;
  int VAL_SPARSEARRAY;
  int VAL_LIST;
  int VAL_CHARSEQUENCE;
  int VAL_BOOLEAN;
  int VAL_DOUBLE;
  int VAL_FLOAT;
  int VAL_LONG;
  int VAL_SHORT;
  int VAL_PARCELABLE;
  int VAL_BUNDLE;
  int VAL_MAP;
  int VAL_INTEGER;
  int VAL_STRING;
  int VAL_NULL;
  int sHolderPool;
  int sOwnedPool;
  int POOL_SIZE;
  int mStack;
  int mOwnsNativeParcelObject;
  int mNativePtr;
  int TAG;
  int DEBUG_RECYCLE;
}
class OsTests {
}
class OperationCanceledException {
}
class NullVibrator {
  int sInstance;
}
class NetworkOnMainThreadException {
}
class MessengerTest {
  class TestThread {
    int mTestMessenger;
    int mTestHandler;
  }
  int mConnection;
  int mServiceMessenger;
}
class MessengerService {
  int mMessenger;
  int mHandler;
}
class Messenger {
  int CREATOR;
  int mTarget;
}
class MessageQueueTest {
  class TestFieldIntegrityHandler {
    int mCount;
    int mLastMessage;
    int mHandler;
  }
  class BaseTestHandler {
    int mCount;
    int mLastMessage;
    int mHandler;
  }
}
class MessageQueue {
  class IdleHandler {
  }
  int mNextBarrierToken;
  int mBlocked;
  int mQuiting;
  int mPendingIdleHandlers;
  int mIdleHandlers;
  int mMessages;
  int mPtr;
  int mQuitAllowed;
}
class Message {
  int CREATOR;
  int MAX_POOL_SIZE;
  int sPoolSize;
  int sPool;
  int sPoolSync;
  int next;
  int callback;
  int target;
  int data;
  int when;
  int flags;
  int FLAGS_TO_CLEAR_ON_COPY_FROM;
  int FLAG_ASYNCHRONOUS;
  int FLAG_IN_USE;
  int replyTo;
  int obj;
  int arg2;
  int arg1;
  int what;
}
class MemoryFileTest {
  int testString;
}
class MemoryFile {
  class MemoryOutputStream {
    int mSingleByte;
    int mOffset;
  }
  class MemoryInputStream {
    int mSingleByte;
    int mOffset;
    int mMark;
  }
  int mAllowPurging;
  int mLength;
  int mAddress;
  int mFD;
  int PROT_WRITE;
  int PROT_READ;
  int TAG;
}
class Looper_Accessor {
}
class Looper {
  int mLogging;
  int mRun;
  int mThread;
  int mQueue;
  int sMainLooper;
  int sThreadLocal;
  int TAG;
}
class LocalPowerManager {
  int POKE_LOCK_TIMEOUT_MASK;
  int POKE_LOCK_MEDIUM_TIMEOUT;
  int POKE_LOCK_SHORT_TIMEOUT;
  int POKE_LOCK_IGNORE_TOUCH_EVENTS;
  int TOUCH_EVENT;
  int BUTTON_EVENT;
  int OTHER_EVENT;
}
class LatencyTimer {
  int store;
  int mScaleFactor;
  int mSampleSize;
  int TAG;
}
class IdleHandlerTest {
  class BaseTestHandler {
    int mHandler;
  }
}
class IServiceManager {
  int SET_PERMISSION_CONTROLLER_TRANSACTION;
  int CHECK_SERVICES_TRANSACTION;
  int LIST_SERVICES_TRANSACTION;
  int ADD_SERVICE_TRANSACTION;
  int CHECK_SERVICE_TRANSACTION;
  int GET_SERVICE_TRANSACTION;
  int descriptor;
}
class IInterface {
}
class IBinder {
  class DeathRecipient {
  }
  int FLAG_ONEWAY;
  int SYSPROPS_TRANSACTION;
  int LIKE_TRANSACTION;
  int TWEET_TRANSACTION;
  int INTERFACE_TRANSACTION;
  int DUMP_TRANSACTION;
  int PING_TRANSACTION;
  int LAST_CALL_TRANSACTION;
  int FIRST_CALL_TRANSACTION;
}
class Handler_Delegate {
  int sCallbacks;
  class IHandlerCallback {
  }
}
class HandlerThread_Delegate {
  int sThreads;
}
class HandlerThreadTest {
  int mLooperTid;
  int mDidSetup;
  int mGotMessageWhat;
  int mGotMessage;
  int TEST_WHAT;
}
class HandlerThread {
  int mLooper;
  int mTid;
  int mPriority;
}
class HandlerTester {
  int mLooper;
  int mSuccess;
  int mDone;
  class H {
  }
}
class Handler {
  int mMessenger;
  int mCallback;
  int mLooper;
  int mQueue;
  class MessengerImpl {
  }
  class Callback {
  }
  int TAG;
  int FIND_POTENTIAL_LEAKS;
}
class FileUtilsTest {
  int mCopyFile;
  int mTestFile;
  int TEST_DATA;
}
class FileUtils {
  int SAFE_FILENAME_PATTERN;
  class FileStatus {
    int ctime;
    int mtime;
    int atime;
    int blocks;
    int blksize;
    int size;
    int rdev;
    int gid;
    int uid;
    int nlink;
    int mode;
    int ino;
    int dev;
  }
  int S_IXOTH;
  int S_IWOTH;
  int S_IROTH;
  int S_IRWXO;
  int S_IXGRP;
  int S_IWGRP;
  int S_IRGRP;
  int S_IRWXG;
  int S_IXUSR;
  int S_IWUSR;
  int S_IRUSR;
  int S_IRWXU;
}
class FileObserverTest {
  class Observer {
    int totalEvents;
    int events;
  }
  int mTestFile;
  int mObserver;
}
class FileObserver {
  int m_mask;
  int m_descriptor;
  int m_path;
  int s_observerThread;
  class ObserverThread {
    int m_fd;
    int m_observers;
  }
  int LOG_TAG;
  int ALL_EVENTS;
  int MOVE_SELF;
  int DELETE_SELF;
  int DELETE;
  int CREATE;
  int MOVED_TO;
  int MOVED_FROM;
  int OPEN;
  int CLOSE_NOWRITE;
  int CLOSE_WRITE;
  int ATTRIB;
  int MODIFY;
  int ACCESS;
}
class Environment {
  int MEDIA_UNMOUNTABLE;
  int MEDIA_BAD_REMOVAL;
  int MEDIA_SHARED;
  int MEDIA_MOUNTED_READ_ONLY;
  int MEDIA_MOUNTED;
  int MEDIA_NOFS;
  int MEDIA_CHECKING;
  int MEDIA_UNMOUNTED;
  int MEDIA_REMOVED;
  int DIRECTORY_DCIM;
  int DIRECTORY_DOWNLOADS;
  int DIRECTORY_MOVIES;
  int DIRECTORY_PICTURES;
  int DIRECTORY_NOTIFICATIONS;
  int DIRECTORY_ALARMS;
  int DIRECTORY_RINGTONES;
  int DIRECTORY_PODCASTS;
  int DIRECTORY_MUSIC;
  int DOWNLOAD_CACHE_DIRECTORY;
  int EXTERNAL_STORAGE_ANDROID_OBB_DIRECTORY;
  int EXTERNAL_STORAGE_ANDROID_MEDIA_DIRECTORY;
  int EXTERNAL_STORAGE_ANDROID_DATA_DIRECTORY;
  int EXTERNAL_STORAGE_DIRECTORY;
  int MEDIA_STORAGE_DIRECTORY;
  int SECURE_DATA_DIRECTORY;
  int DATA_DIRECTORY;
  int mPrimaryVolume;
  int mLock;
  int SYSTEM_PROPERTY_EFS_ENABLED;
  int ROOT_DIRECTORY;
  int TAG;
}
class DropBoxManager {
  class Entry {
    int CREATOR;
    int mFlags;
    int mFileDescriptor;
    int mData;
    int mTimeMillis;
    int mTag;
  }
  int EXTRA_TIME;
  int EXTRA_TAG;
  int ACTION_DROPBOX_ENTRY_ADDED;
  int HAS_BYTE_ARRAY;
  int IS_GZIPPED;
  int IS_TEXT;
  int IS_EMPTY;
  int mService;
  int TAG;
}
class Debug {
  int debugProperties;
  class InstructionCount {
    int mCounts;
    int NUM_INSTR;
  }
  int SYSFS_QEMU_TRACE_STATE;
  class MemoryInfo {
    int CREATOR;
    int otherStats;
    int NUM_OTHER_STATS;
    int otherSharedDirty;
    int otherPrivateDirty;
    int otherPss;
    int nativeSharedDirty;
    int nativePrivateDirty;
    int nativePss;
    int dalvikSharedDirty;
    int dalvikPrivateDirty;
    int dalvikPss;
  }
  int DEFAULT_TRACE_FILE_PATH;
  int DEFAULT_TRACE_EXTENSION;
  int DEFAULT_TRACE_BODY;
  int DEFAULT_TRACE_PATH_PREFIX;
  int SPIN_DELAY;
  int MIN_DEBUGGER_IDLE;
  int mWaiting;
  int SHOW_INITIALIZED;
  int SHOW_CLASSLOADER;
  int SHOW_FULL_DETAIL;
  int TRACE_COUNT_ALLOCS;
  int TAG;
}
class DeadObjectException {
}
class CountDownTimer {
  int mHandler;
  int MSG;
  int mStopTimeInFuture;
  int mCountdownInterval;
  int mMillisInFuture;
}
class ConditionVariable {
  int mCondition;
}
class CommonTimeUtils {
  int mInterfaceDesc;
  int mRemote;
  int ERROR_DEAD_OBJECT;
  int ERROR_BAD_VALUE;
  int ERROR;
  int SUCCESS;
}
class CommonTimeConfig {
  int METHOD_FORCE_NETWORKLESS_MASTER_MODE;
  int METHOD_SET_AUTO_DISABLE;
  int METHOD_GET_AUTO_DISABLE;
  int METHOD_SET_PANIC_THRESHOLD;
  int METHOD_GET_PANIC_THRESHOLD;
  int METHOD_SET_CLIENT_SYNC_INTERVAL;
  int METHOD_GET_CLIENT_SYNC_INTERVAL;
  int METHOD_SET_MASTER_ANNOUNCE_INTERVAL;
  int METHOD_GET_MASTER_ANNOUNCE_INTERVAL;
  int METHOD_SET_INTERFACE_BINDING;
  int METHOD_GET_INTERFACE_BINDING;
  int METHOD_SET_MASTER_ELECTION_GROUP_ID;
  int METHOD_GET_MASTER_ELECTION_GROUP_ID;
  int METHOD_SET_MASTER_ELECTION_ENDPOINT;
  int METHOD_GET_MASTER_ELECTION_ENDPOINT;
  int METHOD_SET_MASTER_ELECTION_PRIORITY;
  int METHOD_GET_MASTER_ELECTION_PRIORITY;
  int mDeathHandler;
  int mUtils;
  int mInterfaceDesc;
  int mRemote;
  int mServerDiedListener;
  int mListenerLock;
  class OnServerDiedListener {
  }
  int SERVICE_NAME;
  int INVALID_GROUP_ID;
  int ERROR_DEAD_OBJECT;
  int ERROR_BAD_VALUE;
  int ERROR;
  int SUCCESS;
}
class CommonClock {
  int METHOD_CBK_ON_TIMELINE_CHANGED;
  int METHOD_UNREGISTER_LISTENER;
  int METHOD_REGISTER_LISTENER;
  int METHOD_GET_MASTER_ADDRESS;
  int METHOD_GET_STATE;
  int METHOD_GET_TIMELINE_ID;
  int METHOD_GET_ESTIMATED_ERROR;
  int METHOD_GET_LOCAL_FREQ;
  int METHOD_GET_LOCAL_TIME;
  int METHOD_GET_COMMON_FREQ;
  int METHOD_GET_COMMON_TIME;
  int METHOD_LOCAL_TIME_TO_COMMON_TIME;
  int METHOD_COMMON_TIME_TO_LOCAL_TIME;
  int METHOD_IS_COMMON_TIME_VALID;
  int mCallbackTgt;
  class TimelineChangedListener {
    int DESCRIPTOR;
  }
  int mDeathHandler;
  int mUtils;
  int mInterfaceDesc;
  int mRemote;
  int mServerDiedListener;
  int mTimelineChangedListener;
  int mListenerLock;
  class OnServerDiedListener {
  }
  class OnTimelineChangedListener {
  }
  int SERVICE_NAME;
  int STATE_WAIT_FOR_ELECTION;
  int STATE_RONIN;
  int STATE_MASTER;
  int STATE_CLIENT;
  int STATE_INITIAL;
  int STATE_INVALID;
  int ERROR_ESTIMATE_UNKNOWN;
  int INVALID_TIMELINE_ID;
  int TIME_NOT_SYNCED;
}
class CancellationSignal {
  class Transport {
    int mCancellationSignal;
  }
  class OnCancelListener {
  }
  int mCancelInProgress;
  int mRemote;
  int mOnCancelListener;
  int mIsCanceled;
}
class Bundle {
  int CREATOR;
  int mClassLoader;
  int mAllowFds;
  int mFdsKnown;
  int mHasFds;
  int mParcelledData;
  int mMap;
  int EMPTY;
  int LOG_TAG;
}
class Build_Delegate {
}
class BuildTest {
  int TAG;
}
class Build {
  int IS_DEBUGGABLE;
  int HOST;
  int USER;
  int TIME;
  int FINGERPRINT;
  int TAGS;
  int TYPE;
  class VERSION_CODES {
    int JELLY_BEAN;
    int ICE_CREAM_SANDWICH_MR1;
    int ICE_CREAM_SANDWICH;
    int HONEYCOMB_MR2;
    int HONEYCOMB_MR1;
    int HONEYCOMB;
    int GINGERBREAD_MR1;
    int GINGERBREAD;
    int FROYO;
    int ECLAIR_MR1;
    int ECLAIR_0_1;
    int ECLAIR;
    int DONUT;
    int CUPCAKE;
    int BASE_1_1;
    int BASE;
    int CUR_DEVELOPMENT;
  }
  class VERSION {
    int RESOURCES_SDK_INT;
    int CODENAME;
    int SDK_INT;
    int SDK;
    int RELEASE;
    int INCREMENTAL;
  }
  int SERIAL;
  int HARDWARE;
  int RADIO;
  int BOOTLOADER;
  int MODEL;
  int BRAND;
  int MANUFACTURER;
  int CPU_ABI2;
  int CPU_ABI;
  int BOARD;
  int DEVICE;
  int PRODUCT;
  int DISPLAY;
  int ID;
  int UNKNOWN;
}
class BroadcasterTest {
  class Tests2and3 {
    int mSuccess;
    int mHandlers;
    int N;
  }
  int MESSAGE_D;
  int MESSAGE_C;
  int MESSAGE_B;
  int MESSAGE_A;
}
class Broadcaster {
  int mReg;
  class Registration {
    int targetWhats;
    int targets;
    int senderWhat;
    int prev;
    int next;
  }
}
class BrightnessLimit {
}
class BinderThreadPriorityTest {
  class ServiceStub {
  }
  int mConnection;
  int mSavedPriority;
  int mService;
  int TAG;
}
class BinderThreadPriorityService {
  int mBinder;
  int TAG;
}
class BinderProxy {
  int mOrgue;
  int mObject;
  int mSelf;
}
class Binder {
  int mDescriptor;
  int mOwner;
  int mObject;
  int TAG;
  int FIND_POTENTIAL_LEAKS;
}
class BatteryStats {
  class HistoryPrinter {
    int oldVolt;
    int oldTemp;
    int oldPlug;
    int oldHealth;
    int oldStatus;
    int oldState;
  }
  int HISTORY_STATE_DESCRIPTIONS;
  int NUM_DATA_CONNECTION_TYPES;
  int DATA_CONNECTION_NAMES;
  int DATA_CONNECTION_OTHER;
  int DATA_CONNECTION_EHRPD;
  int DATA_CONNECTION_LTE;
  int DATA_CONNECTION_EVDO_B;
  int DATA_CONNECTION_IDEN;
  int DATA_CONNECTION_HSPA;
  int DATA_CONNECTION_HSUPA;
  int DATA_CONNECTION_HSDPA;
  int DATA_CONNECTION_1xRTT;
  int DATA_CONNECTION_EVDO_A;
  int DATA_CONNECTION_EVDO_0;
  int DATA_CONNECTION_CDMA;
  int DATA_CONNECTION_UMTS;
  int DATA_CONNECTION_EDGE;
  int DATA_CONNECTION_GPRS;
  int DATA_CONNECTION_NONE;
  int NUM_SCREEN_BRIGHTNESS_BINS;
  int SCREEN_BRIGHTNESS_NAMES;
  int SCREEN_BRIGHTNESS_BRIGHT;
  int SCREEN_BRIGHTNESS_LIGHT;
  int SCREEN_BRIGHTNESS_MEDIUM;
  int SCREEN_BRIGHTNESS_DIM;
  int SCREEN_BRIGHTNESS_DARK;
  class BitDescription {
    int values;
    int name;
    int shift;
    int mask;
  }
  class HistoryItem {
    int DELTA_STATE_MASK;
    int DELTA_STATE_FLAG;
    int DELTA_BATTERY_LEVEL_FLAG;
    int DELTA_CMD_SHIFT;
    int DELTA_CMD_MASK;
    int DELTA_TIME_LONG;
    int DELTA_TIME_INT;
    int DELTA_TIME_ABS;
    int DELTA_TIME_MASK;
    int states;
    int MOST_INTERESTING_STATES;
    int STATE_BLUETOOTH_ON_FLAG;
    int STATE_WIFI_ON_FLAG;
    int STATE_PHONE_IN_CALL_FLAG;
    int STATE_BATTERY_PLUGGED_FLAG;
    int STATE_SCREEN_ON_FLAG;
    int STATE_VIDEO_ON_FLAG;
    int STATE_AUDIO_ON_FLAG;
    int STATE_WIFI_MULTICAST_ON_FLAG;
    int STATE_WIFI_SCAN_LOCK_FLAG;
    int STATE_WIFI_FULL_LOCK_FLAG;
    int STATE_WIFI_RUNNING_FLAG;
    int STATE_PHONE_SCANNING_FLAG;
    int STATE_GPS_ON_FLAG;
    int STATE_SENSOR_ON_FLAG;
    int STATE_WAKE_LOCK_FLAG;
    int STATE_DATA_CONNECTION_SHIFT;
    int STATE_DATA_CONNECTION_MASK;
    int STATE_PHONE_STATE_SHIFT;
    int STATE_PHONE_STATE_MASK;
    int STATE_SIGNAL_STRENGTH_SHIFT;
    int STATE_SIGNAL_STRENGTH_MASK;
    int STATE_BRIGHTNESS_SHIFT;
    int STATE_BRIGHTNESS_MASK;
    int batteryVoltage;
    int batteryTemperature;
    int batteryPlugType;
    int batteryHealth;
    int batteryStatus;
    int batteryLevel;
    int cmd;
    int CMD_OVERFLOW;
    int CMD_START;
    int CMD_UPDATE;
    int CMD_NULL;
    int time;
    int next;
    int DEBUG;
    int TAG;
  }
  class Uid {
    class Pkg {
      class Serv {
      }
    }
    class Proc {
      class ExcessivePower {
        int usedTime;
        int overTime;
        int type;
        int TYPE_CPU;
        int TYPE_WAKE;
      }
    }
    class Pid {
      int mWakeStart;
      int mWakeSum;
    }
    class Sensor {
      int GPS;
    }
    int NUM_USER_ACTIVITY_TYPES;
    int USER_ACTIVITY_TYPES;
    class Wakelock {
    }
  }
  class Timer {
  }
  class Counter {
  }
  int mFormatter;
  int mFormatBuilder;
  int DATA_CONNECTION_COUNT_DATA;
  int DATA_CONNECTION_TIME_DATA;
  int SIGNAL_STRENGTH_COUNT_DATA;
  int SIGNAL_SCANNING_TIME_DATA;
  int SIGNAL_STRENGTH_TIME_DATA;
  int SCREEN_BRIGHTNESS_DATA;
  int MISC_DATA;
  int WIFI_LOCK_DATA;
  int BATTERY_LEVEL_DATA;
  int BATTERY_DISCHARGE_DATA;
  int BATTERY_DATA;
  int USER_ACTIVITY_DATA;
  int NETWORK_DATA;
  int KERNEL_WAKELOCK_DATA;
  int WAKELOCK_DATA;
  int SENSOR_DATA;
  int PROCESS_DATA;
  int APK_DATA;
  int UID_DATA;
  int BYTES_PER_GB;
  int BYTES_PER_MB;
  int BYTES_PER_KB;
  int BATTERY_STATS_CHECKIN_VERSION;
  int STAT_NAMES;
  int STATS_SINCE_UNPLUGGED;
  int STATS_CURRENT;
  int STATS_LAST;
  int STATS_SINCE_CHARGED;
  int VIDEO_TURNED_ON;
  int AUDIO_TURNED_ON;
  int WIFI_MULTICAST_ENABLED;
  int SCAN_WIFI_LOCK;
  int FULL_WIFI_LOCK;
  int WIFI_RUNNING;
  int SENSOR;
  int WAKE_TYPE_WINDOW;
  int WAKE_TYPE_FULL;
  int WAKE_TYPE_PARTIAL;
  int LOCAL_LOGV;
}
class BatteryManager {
  int BATTERY_PLUGGED_USB;
  int BATTERY_PLUGGED_AC;
  int BATTERY_HEALTH_COLD;
  int BATTERY_HEALTH_UNSPECIFIED_FAILURE;
  int BATTERY_HEALTH_OVER_VOLTAGE;
  int BATTERY_HEALTH_DEAD;
  int BATTERY_HEALTH_OVERHEAT;
  int BATTERY_HEALTH_GOOD;
  int BATTERY_HEALTH_UNKNOWN;
  int BATTERY_STATUS_FULL;
  int BATTERY_STATUS_NOT_CHARGING;
  int BATTERY_STATUS_DISCHARGING;
  int BATTERY_STATUS_CHARGING;
  int BATTERY_STATUS_UNKNOWN;
  int EXTRA_INVALID_CHARGER;
  int EXTRA_TECHNOLOGY;
  int EXTRA_TEMPERATURE;
  int EXTRA_VOLTAGE;
  int EXTRA_PLUGGED;
  int EXTRA_ICON_SMALL;
  int EXTRA_SCALE;
  int EXTRA_LEVEL;
  int EXTRA_PRESENT;
  int EXTRA_HEALTH;
  int EXTRA_STATUS;
}
class BadParcelableException {
}
class AsyncTask {
  class AsyncTaskResult {
    int mData;
    int mTask;
  }
  class WorkerRunnable {
    int mParams;
  }
  class InternalHandler {
  }
  class Status {
    int FINISHED;
    int RUNNING;
    int PENDING;
  }
  class SerialExecutor {
    int mActive;
    int mTasks;
  }
  int mTaskInvoked;
  int mCancelled;
  int mStatus;
  int mFuture;
  int mWorker;
  int sDefaultExecutor;
  int sHandler;
  int MESSAGE_POST_PROGRESS;
  int MESSAGE_POST_RESULT;
  int SERIAL_EXECUTOR;
  int THREAD_POOL_EXECUTOR;
  int sPoolWorkQueue;
  int sThreadFactory;
  int KEEP_ALIVE;
  int MAXIMUM_POOL_SIZE;
  int CORE_POOL_SIZE;
  int LOG_TAG;
}
class AsyncResult {
  int result;
  int exception;
  int userObj;
}
class AidlTest {
  class AidlObject {
  }
  class TestParcelable {
    int CREATOR;
    int mAString;
    int mAnInt;
  }
  int mRemote;
}
