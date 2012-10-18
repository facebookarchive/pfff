package com.android.server.am;
class UsageStatsService {
  class PkgUsageStatsExtended {
    int mResumedTime;
    int mPausedTime;
    int mUsageTime;
    int mLaunchCount;
    int mLaunchTimes;
  }
  class TimeStats {
    int times;
    int count;
  }
  int mUnforcedDiskWriteRunning;
  int mLastWriteElapsedTime;
  int mLastWriteDay;
  int mCal;
  int mDir;
  int mFileLeaf;
  int mHistoryFile;
  int mFile;
  int mIsResumed;
  int mLastResumedComp;
  int mLastResumedPkg;
  int mFileLock;
  int mStatsLock;
  int mPackageMonitor;
  int mLastResumeTimes;
  int mStats;
  int mContext;
  int sService;
  int LAUNCH_TIME_BINS;
  int NUM_LAUNCH_TIME_BINS;
  int MAX_NUM_FILES;
  int FILE_WRITE_INTERVAL;
  int FILE_HISTORY;
  int FILE_PREFIX;
  int CHECKIN_VERSION;
  int VERSION;
  int TAG;
  int REPORT_UNEXPECTED;
  int localLOGV;
  int SERVICE_NAME;
}
class UriPermissionOwner {
  class ExternalToken {
  }
  int writeUriPermissions;
  int readUriPermissions;
  int externalToken;
  int owner;
  int service;
}
class UriPermission {
  int stringName;
  int writeOwners;
  int readOwners;
  int globalModeFlags;
  int modeFlags;
  int uri;
  int uid;
}
class TransferPipe {
  class Caller {
  }
  int mBufferPrefix;
  int mComplete;
  int mFailure;
  int mEndTime;
  int mOutFd;
  int mFds;
  int mThread;
  int DEFAULT_TIMEOUT;
  int DEBUG;
  int TAG;
}
class ThumbnailHolder {
  int lastDescription;
  int lastThumbnail;
}
class TaskRecord {
  int userId;
  int stringName;
  int askedCompatMode;
  int rootWasReset;
  int lastActiveTime;
  int numActivities;
  int realActivity;
  int origActivity;
  int affinityIntent;
  int intent;
  int affinity;
  int taskId;
}
class TaskAccessInfo {
  int subtasks;
  int rootIndex;
  int root;
  class SubTask {
    int index;
    int activity;
    int thumbnail;
  }
}
class StrictModeViolationDialog {
  int mHandler;
  int DISMISS_TIMEOUT;
  int ACTION_OK_AND_REPORT;
  int ACTION_OK;
  int mProc;
  int mResult;
  int TAG;
}
class ServiceRecord {
  int pendingStarts;
  int deliveredStarts;
  class StartItem {
    int stringName;
    int uriPermissions;
    int doneExecutingCount;
    int deliveryCount;
    int deliveredTime;
    int neededGrants;
    int intent;
    int id;
    int taskRemoved;
    int sr;
  }
  int lastStartId;
  int stringName;
  int nextRestartTime;
  int restartTime;
  int restartDelay;
  int restartCount;
  int totalRestartCount;
  int crashCount;
  int executingStart;
  int executeNesting;
  int callStart;
  int stopIfKilled;
  int startRequested;
  int lastActivity;
  int foregroundNoti;
  int foregroundId;
  int isForeground;
  int isolatedProc;
  int app;
  int connections;
  int bindings;
  int createTime;
  int restarter;
  int exported;
  int dataDir;
  int resDir;
  int baseDir;
  int permission;
  int processName;
  int packageName;
  int userId;
  int appInfo;
  int serviceInfo;
  int intent;
  int shortName;
  int name;
  int stats;
  int ams;
  int MAX_DONE_EXECUTING_COUNT;
  int MAX_DELIVERY_COUNT;
}
class ReceiverList {
  int stringName;
  int linkedToDeath;
  int curBroadcast;
  int uid;
  int pid;
  int app;
  int receiver;
  int owner;
}
class ProviderMap {
  int mProvidersByClassPerUser;
  int mProvidersByNamePerUser;
  int mGlobalByClass;
  int mGlobalByName;
  int DBG;
  int TAG;
}
class ProcessRecord {
  int errorReportReceiver;
  int notRespondingReport;
  int crashingReport;
  int stringName;
  int shortStringName;
  int waitDialog;
  int waitedForDebugger;
  int debugging;
  int removed;
  int anrDialog;
  int notResponding;
  int crashDialog;
  int crashing;
  int persistent;
  int conProviders;
  int pubProviders;
  int receivers;
  int connections;
  int executingServices;
  int services;
  int activities;
  int adjTarget;
  int adjSourceOom;
  int adjSource;
  int adjTypeCode;
  int adjType;
  int lastPss;
  int hidden;
  int empty;
  int reportLowMemory;
  int lastLowMemory;
  int lastRequestedGc;
  int curCpuTime;
  int lastCpuTime;
  int lastWakeTime;
  int curReceiver;
  int usingWrapper;
  int instrumentationResultClass;
  int instrumentationArguments;
  int instrumentationWatcher;
  int instrumentationProfileFile;
  int instrumentationInfo;
  int instrumentationClass;
  int deathRecipient;
  int compat;
  int lruSeq;
  int adjSeq;
  int forcingToForeground;
  int waitingToKill;
  int killedBackground;
  int bad;
  int hasAboveClient;
  int pendingUiClean;
  int hasShownUi;
  int systemNoUi;
  int foregroundActivities;
  int foregroundServices;
  int setIsForeground;
  int keeping;
  int serviceb;
  int memImportance;
  int trimMemoryLevel;
  int setSchedGroup;
  int curSchedGroup;
  int setAdj;
  int curAdj;
  int nonStoppingAdj;
  int setRawAdj;
  int curRawAdj;
  int hiddenAdj;
  int maxAdj;
  int lruWeight;
  int lastActivityTime;
  int starting;
  int pid;
  int thread;
  int pkgList;
  int processName;
  int userId;
  int uid;
  int isolated;
  int info;
  int batteryStats;
}
class ProcessList {
  int mHaveDisplaySize;
  int mTotalMemMb;
  int mOomMinFree;
  int mOomMinFreeHigh;
  int mOomMinFreeLow;
  int mOomAdj;
  int EMPTY_APP_IDLE_OFFSET;
  int CONTENT_APP_IDLE_OFFSET;
  int MAX_HIDDEN_APPS;
  int MIN_HIDDEN_APPS;
  int PAGE_SIZE;
  int SYSTEM_ADJ;
  int PERSISTENT_PROC_ADJ;
  int FOREGROUND_APP_ADJ;
  int VISIBLE_APP_ADJ;
  int PERCEPTIBLE_APP_ADJ;
  int HEAVY_WEIGHT_APP_ADJ;
  int BACKUP_APP_ADJ;
  int SERVICE_ADJ;
  int HOME_APP_ADJ;
  int PREVIOUS_APP_ADJ;
  int SERVICE_B_ADJ;
  int HIDDEN_APP_MIN_ADJ;
  int HIDDEN_APP_MAX_ADJ;
  int MIN_CRASH_INTERVAL;
}
class PendingThumbnailsRecord {
  int finished;
  int pendingRecords;
  int receiver;
}
class PendingIntentRecord {
  class Key {
    int ODD_PRIME_NUMBER;
    int hashCode;
    int flags;
    int allResolvedTypes;
    int allIntents;
    int options;
    int requestResolvedType;
    int requestIntent;
    int requestCode;
    int who;
    int activity;
    int packageName;
    int type;
  }
  int stringName;
  int canceled;
  int sent;
  int ref;
  int uid;
  int key;
  int owner;
}
class LaunchWarningWindow {
}
class IntentBindRecord {
  int stringName;
  int doRebind;
  int hasBound;
  int received;
  int requested;
  int binder;
  int apps;
  int intent;
  int service;
}
class FactoryErrorDialog {
  int mHandler;
}
class DeviceMonitor {
  int instance;
  int PATHS;
  int BASE;
  int PROC;
  int running;
  int buffer;
  int MAX_FILES;
  int INTERVAL;
  int SAMPLE_COUNT;
  int LOG_TAG;
}
class CoreSettingsObserver {
  int mActivityManagerService;
  int mCoreSettings;
  int sCoreSettingToTypeMap;
  int LOG_TAG;
}
class ContentProviderRecord {
  class ExternalProcessHandle {
    int mAcquisitionCount;
    int mToken;
    int LOG_TAG;
  }
  int shortStringName;
  int stringName;
  int launchingApp;
  int proc;
  int externalProcessNoHandleCount;
  int externalProcessTokenToHandle;
  int connections;
  int noReleaseNeeded;
  int provider;
  int name;
  int appInfo;
  int uid;
  int info;
  int service;
}
class ContentProviderConnection {
  int numUnstableIncs;
  int numStableIncs;
  int dead;
  int waiting;
  int unstableCount;
  int stableCount;
  int createTime;
  int client;
  int provider;
}
class ConnectionRecord {
  int serviceDead;
  int stringName;
  int clientIntent;
  int clientLabel;
  int flags;
  int conn;
  int activity;
  int binding;
}
class CompatModePackages {
  int mHandler;
  int MSG_WRITE;
  int mPackages;
  int COMPAT_FLAG_ENABLED;
  int COMPAT_FLAG_DONT_ASK;
  int mFile;
  int mService;
  int DEBUG_CONFIGURATION;
  int TAG;
}
class CompatModeDialog {
  int mHint;
  int mAlwaysShow;
  int mCompatEnabled;
  int mAppInfo;
  int mService;
}
class BroadcastRecord {
  int curReceiver;
  int curComponent;
  int curApp;
  int curFilter;
  int CALL_DONE_RECEIVE;
  int CALL_IN_RECEIVE;
  int APP_RECEIVE;
  int IDLE;
  int queue;
  int anrCount;
  int state;
  int receiver;
  int nextReceiver;
  int resultAbort;
  int resultExtras;
  int resultData;
  int resultCode;
  int finishTime;
  int receiverTime;
  int dispatchClockTime;
  int dispatchTime;
  int resultTo;
  int receivers;
  int requiredPermission;
  int initialSticky;
  int sticky;
  int ordered;
  int callingUid;
  int callingPid;
  int callerPackage;
  int callerApp;
  int intent;
}
class BroadcastQueue {
  class AppNotResponding {
    int mAnnotation;
    int mApp;
  }
  int mHandler;
  int BROADCAST_TIMEOUT_MSG;
  int BROADCAST_INTENT_MSG;
  int mPendingBroadcastRecvIndex;
  int mPendingBroadcast;
  int mPendingBroadcastTimeoutMessage;
  int mBroadcastsScheduled;
  int mBroadcastHistory;
  int mOrderedBroadcasts;
  int mParallelBroadcasts;
  int mTimeoutPeriod;
  int mQueueName;
  int mService;
  int MAX_BROADCAST_HISTORY;
  int DEBUG_MU;
  int DEBUG_BROADCAST_LIGHT;
  int DEBUG_BROADCAST;
  int TAG_MU;
  int TAG;
}
class BroadcastFilter {
  int requiredPermission;
  int packageName;
  int receiverList;
}
class BatteryStatsService {
  int mBluetoothProfileServiceListener;
  int mBluetoothHeadset;
  int mBluetoothPendingStats;
  int mContext;
  int mStats;
  int sService;
}
class BaseErrorDialog {
  int mConsuming;
  int mHandler;
}
class BackupRecord {
  int app;
  int backupMode;
  int appInfo;
  int stringName;
  int stats;
  int RESTORE_FULL;
  int RESTORE;
  int BACKUP_FULL;
  int BACKUP_NORMAL;
}
class AppWaitingForDebuggerDialog {
  int mHandler;
  int mAppName;
  int mProc;
  int mService;
}
class AppNotRespondingDialog {
  int mHandler;
  int mProc;
  int mService;
  int WAIT_AND_REPORT;
  int WAIT;
  int FORCE_CLOSE;
  int TAG;
}
class AppErrorResult {
  int mResult;
  int mHasResult;
}
class AppErrorDialog {
  int mHandler;
  int DISMISS_TIMEOUT;
  int FORCE_QUIT_AND_REPORT;
  int FORCE_QUIT;
  int mProc;
  int mResult;
  int TAG;
}
class AppBindRecord {
  int connections;
  int client;
  int intent;
  int service;
}
class ActivityStack {
  int FINISH_AFTER_VISIBLE;
  int FINISH_AFTER_PAUSE;
  int FINISH_IMMEDIATELY;
  int mHandler;
  class ScheduleDestroyArgs {
    int mReason;
    int mOomAdj;
    int mOwner;
  }
  int DESTROY_ACTIVITIES_MSG;
  int STOP_TIMEOUT_MSG;
  int LAUNCH_TICK_MSG;
  int RESUME_TOP_ACTIVITY_MSG;
  int DESTROY_TIMEOUT_MSG;
  int LAUNCH_TIMEOUT_MSG;
  int IDLE_NOW_MSG;
  int IDLE_TIMEOUT_MSG;
  int PAUSE_TIMEOUT_MSG;
  int SLEEP_TIMEOUT_MSG;
  int mCurrentUser;
  int mThumbnailHeight;
  int mThumbnailWidth;
  int mDismissKeyguardOnNextActivity;
  int mSleepTimeout;
  int mInitialStartTime;
  int mUserLeaving;
  int mConfigWillChange;
  int mLastStartedActivity;
  int mResumedActivity;
  int mLastPausedActivity;
  int mPausingActivity;
  int mLaunchingActivity;
  int mGoingToSleep;
  int mWaitingActivityVisible;
  int mWaitingActivityLaunched;
  int mFinishingActivities;
  int mNoAnimActivities;
  int mGoingToSleepActivities;
  int mStoppingActivities;
  int mWaitingVisibleActivities;
  int mLRUActivities;
  int mValidateAppTokens;
  int mHistory;
  int mContext;
  int mMainStack;
  int mService;
  class ActivityState {
    int DESTROYED;
    int DESTROYING;
    int FINISHING;
    int STOPPED;
    int STOPPING;
    int PAUSED;
    int PAUSING;
    int RESUMED;
    int INITIALIZING;
  }
  int SHOW_APP_STARTING_PREVIEW;
  int START_WARN_TIME;
  int ACTIVITY_INACTIVE_RESET_TIME;
  int DESTROY_TIMEOUT;
  int LAUNCH_TIMEOUT;
  int SLEEP_TIMEOUT;
  int STOP_TIMEOUT;
  int PAUSE_TIMEOUT;
  int LAUNCH_TICK;
  int IDLE_TIMEOUT;
  int VALIDATE_TOKENS;
  int DEBUG_SAVED_STATE;
  int DEBUG_ADD_REMOVE;
  int DEBUG_STATES;
  int DEBUG_TASKS;
  int DEBUG_CONFIGURATION;
  int DEBUG_RESULTS;
  int DEBUG_TRANSITION;
  int DEBUG_USER_LEAVING;
  int DEBUG_VISBILITY;
  int DEBUG_PAUSE;
  int DEBUG_SWITCH;
  int localLOGV;
  int TAG;
}
class ActivityResult {
  int mFrom;
}
class ActivityRecord {
  class Token {
    int weakActivity;
  }
  int inHistory;
  int stringName;
  int forceNewConfig;
  int immersive;
  int frozenBeforeDestroy;
  int hasBeenLaunched;
  int idle;
  int thumbnailNeeded;
  int nowVisible;
  int waitingVisible;
  int sleeping;
  int visible;
  int launchMode;
  int keysPaused;
  int configChangeFlags;
  int configDestroy;
  int finishing;
  int delayedResume;
  int stopped;
  int haveState;
  int launchFailed;
  int frontOfTask;
  int icicle;
  int state;
  int app;
  int uriPermissions;
  int connections;
  int pendingOptions;
  int newIntents;
  int pendingResults;
  int results;
  int requestCode;
  int resultWho;
  int resultTo;
  int compat;
  int configuration;
  int launchTickTime;
  int pauseTime;
  int cpuTimeAtResume;
  int lastVisibleTime;
  int startTime;
  int launchTime;
  int thumbHolder;
  int task;
  int windowFlags;
  int realTheme;
  int theme;
  int icon;
  int labelRes;
  int nonLocalizedLabel;
  int dataDir;
  int resDir;
  int baseDir;
  int isHomeActivity;
  int componentSpecified;
  int noDisplay;
  int fullscreen;
  int stateNotNeeded;
  int taskAffinity;
  int processName;
  int packageName;
  int resolvedType;
  int shortComponentName;
  int realActivity;
  int intent;
  int userId;
  int launchedFromUid;
  int info;
  int appToken;
  int stack;
  int service;
}
