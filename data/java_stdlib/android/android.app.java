package android.app;
class WallpaperManager {
  int sGlobals;
  int sSync;
  class Globals {
    int mHandler;
    int MSG_CLEAR_WALLPAPER;
    int mDefaultWallpaper;
    int mWallpaper;
    int mService;
  }
  class FastBitmapDrawable {
    int mPaint;
    int mDrawTop;
    int mDrawLeft;
    int mHeight;
    int mWidth;
    int mBitmap;
  }
  int mContext;
  int COMMAND_DROP;
  int COMMAND_SECONDARY_TAP;
  int COMMAND_TAP;
  int WALLPAPER_PREVIEW_META_DATA;
  int EXTRA_LIVE_WALLPAPER_COMPONENT;
  int ACTION_CHANGE_LIVE_WALLPAPER;
  int ACTION_LIVE_WALLPAPER_CHOOSER;
  int mWallpaperYStep;
  int mWallpaperXStep;
  int DEBUG;
  int TAG;
}
class WallpaperInfo {
  int CREATOR;
  int mDescriptionResource;
  int mAuthorResource;
  int mThumbnailResource;
  int mSettingsActivityName;
  int mService;
  int TAG;
}
class UiModeManager {
  int DISABLE_CAR_MODE_GO_HOME;
  int ENABLE_CAR_MODE_GO_CAR_HOME;
  int mService;
  int MODE_NIGHT_YES;
  int MODE_NIGHT_NO;
  int MODE_NIGHT_AUTO;
  int ACTION_EXIT_DESK_MODE;
  int ACTION_ENTER_DESK_MODE;
  int ACTION_EXIT_CAR_MODE;
  int ACTION_ENTER_CAR_MODE;
  int TAG;
}
class TranslucentFancyActivity {
}
class TimePickerDialog {
  int mIs24HourView;
  int mInitialMinute;
  int mInitialHourOfDay;
  int mCallback;
  int mTimePicker;
  int IS_24_HOUR;
  int MINUTE;
  int HOUR;
  class OnTimeSetListener {
  }
}
class TaskStackBuilder {
  int mSourceContext;
  int mIntents;
  int TAG;
}
class TabActivity {
  int mDefaultTabIndex;
  int mDefaultTab;
  int mTabHost;
}
class SuggestionProvider {
  int COLUMNS;
  int sURLMatcher;
  int SEARCH_SUGGESTIONS;
  int TAG;
}
class StatusBarManager {
  int mToken;
  int mService;
  int mContext;
  int NAVIGATION_HINT_BACK_ALT;
  int NAVIGATION_HINT_RECENT_NOP;
  int NAVIGATION_HINT_HOME_NOP;
  int NAVIGATION_HINT_BACK_NOP;
  int DISABLE_MASK;
  int DISABLE_NONE;
  int DISABLE_NAVIGATION;
  int DISABLE_CLOCK;
  int DISABLE_BACK;
  int DISABLE_RECENT;
  int DISABLE_HOME;
  int DISABLE_SYSTEM_INFO;
  int DISABLE_NOTIFICATION_TICKER;
  int DISABLE_NOTIFICATION_ALERTS;
  int DISABLE_NOTIFICATION_ICONS;
  int DISABLE_EXPAND;
}
class SharedPreferencesImpl {
  class EditorImpl {
    int mClear;
    int mModified;
  }
  class MemoryCommitResult {
    int writeToDiskResult;
    int writtenToDiskLatch;
    int mapToWriteToDisk;
    int listeners;
    int keysModified;
    int changesMade;
  }
  int mListeners;
  int mContent;
  int mWritingToDiskLock;
  int mStatSize;
  int mStatTimestamp;
  int mLoaded;
  int mDiskWritesInFlight;
  int mMap;
  int mMode;
  int mBackupFile;
  int mFile;
  int DEBUG;
  int TAG;
}
class Service {
  int mStartCompatibility;
  int mActivityManager;
  int mApplication;
  int mToken;
  int mClassName;
  int mThread;
  int START_FLAG_RETRY;
  int START_FLAG_REDELIVERY;
  int START_TASK_REMOVED_COMPLETE;
  int START_REDELIVER_INTENT;
  int START_NOT_STICKY;
  int START_STICKY;
  int START_STICKY_COMPATIBILITY;
  int START_CONTINUATION_MASK;
  int TAG;
}
class SearchablesTest {
  class MyMockPackageManager {
    int mSearchablesMode;
    int mRealPackageManager;
    int SEARCHABLES_MOCK_ONEGOOD_ONEBAD;
    int SEARCHABLES_MOCK_ONEGOOD;
    int SEARCHABLES_MOCK_ZERO;
    int SEARCHABLES_PASSTHROUGH;
  }
  class MyMockContext {
    int mPackageManager;
    int mRealContext;
  }
}
class SearchableInfo {
  int CREATOR;
  class ActionKeyInfo {
    int mSuggestActionMsgColumn;
    int mSuggestActionMsg;
    int mQueryActionMsg;
    int mKeyCode;
  }
  int mVoiceMaxResults;
  int mVoiceLanguageId;
  int mVoicePromptTextId;
  int mVoiceLanguageModeId;
  int mVoiceSearchMode;
  int VOICE_SEARCH_LAUNCH_RECOGNIZER;
  int VOICE_SEARCH_LAUNCH_WEB_SEARCH;
  int VOICE_SEARCH_SHOW_BUTTON;
  int mSuggestProviderPackage;
  int mActionKeys;
  int mSuggestThreshold;
  int mSuggestIntentData;
  int mSuggestIntentAction;
  int mSuggestSelection;
  int mSuggestPath;
  int mSuggestAuthority;
  int mSettingsDescriptionId;
  int mAutoUrlDetect;
  int mQueryAfterZeroResults;
  int mIncludeInGlobalSearch;
  int mSearchImeOptions;
  int mSearchInputType;
  int mSearchButtonText;
  int mIconId;
  int mSearchMode;
  int mHintId;
  int mSearchActivity;
  int mLabelId;
  int SEARCH_MODE_QUERY_REWRITE_FROM_TEXT;
  int SEARCH_MODE_QUERY_REWRITE_FROM_DATA;
  int SEARCH_MODE_BADGE_ICON;
  int SEARCH_MODE_BADGE_LABEL;
  int MD_XML_ELEMENT_SEARCHABLE_ACTION_KEY;
  int MD_XML_ELEMENT_SEARCHABLE;
  int MD_LABEL_SEARCHABLE;
  int LOG_TAG;
  int DBG;
}
class SearchManagerTest {
  int mContext;
  int SEARCHABLE_ACTIVITY;
}
class SearchManager {
  class OnCancelListener {
  }
  class OnDismissListener {
  }
  int mSearchDialog;
  int mCancelListener;
  int mDismissListener;
  int mHandler;
  int mAssociatedPackage;
  int mContext;
  int mService;
  int DISABLE_VOICE_SEARCH;
  int CONTEXT_IS_VOICE;
  int INTENT_ACTION_SEARCH_SETTINGS_CHANGED;
  int INTENT_GLOBAL_SEARCH_ACTIVITY_CHANGED;
  int INTENT_ACTION_SEARCHABLES_CHANGED;
  int INTENT_ACTION_WEB_SEARCH_SETTINGS;
  int INTENT_ACTION_SEARCH_SETTINGS;
  int INTENT_ACTION_GLOBAL_SEARCH;
  int SUGGEST_PARAMETER_LIMIT;
  int SUGGEST_NEVER_MAKE_SHORTCUT;
  int SUGGEST_COLUMN_LAST_ACCESS_HINT;
  int SUGGEST_COLUMN_FLAGS;
  int SUGGEST_COLUMN_SPINNER_WHILE_REFRESHING;
  int SUGGEST_COLUMN_SHORTCUT_ID;
  int SUGGEST_COLUMN_QUERY;
  int SUGGEST_COLUMN_INTENT_DATA_ID;
  int SUGGEST_COLUMN_INTENT_EXTRA_DATA;
  int SUGGEST_COLUMN_INTENT_DATA;
  int SUGGEST_COLUMN_INTENT_ACTION;
  int SUGGEST_COLUMN_ICON_2;
  int SUGGEST_COLUMN_ICON_1;
  int SUGGEST_COLUMN_TEXT_2_URL;
  int SUGGEST_COLUMN_TEXT_2;
  int SUGGEST_COLUMN_TEXT_1;
  int SUGGEST_COLUMN_FORMAT;
  int SHORTCUT_MIME_TYPE;
  int SUGGEST_URI_PATH_SHORTCUT;
  int SUGGEST_MIME_TYPE;
  int SUGGEST_URI_PATH_QUERY;
  int FLAG_QUERY_REFINEMENT;
  int ACTION_MSG;
  int CURSOR_EXTRA_KEY_IN_PROGRESS;
  int EXTRA_WEB_SEARCH_PENDINGINTENT;
  int EXTRA_NEW_SEARCH;
  int EXTRA_SELECT_QUERY;
  int EXTRA_DATA_KEY;
  int ACTION_KEY;
  int SEARCH_MODE;
  int APP_DATA;
  int USER_QUERY;
  int QUERY;
  int MENU_KEYCODE;
  int MENU_KEY;
  int TAG;
  int DBG;
}
class SearchDialog {
  int mOnSuggestionSelectionListener;
  int mOnQueryChangeListener;
  int mOnCloseListener;
  class SearchBar {
    int mSearchDialog;
  }
  int mConfChangeListener;
  int mSearchAutoCompleteImeOptions;
  int mUserQuery;
  int mVoiceAppSearchIntent;
  int mVoiceWebSearchIntent;
  int mActivityContext;
  int mAppSearchData;
  int mLaunchComponent;
  int mSearchable;
  int mCloseSearch;
  int mWorkingSpinner;
  int mSearchView;
  int mSearchPlate;
  int mSearchAutoComplete;
  int mAppIcon;
  int mBadgeLabel;
  int SEARCH_PLATE_LEFT_PADDING_NON_GLOBAL;
  int IME_OPTION_NO_MICROPHONE;
  int INSTANCE_KEY_USER_QUERY;
  int INSTANCE_KEY_APPDATA;
  int INSTANCE_KEY_COMPONENT;
  int LOG_TAG;
  int DBG;
}
class ResultInfo {
  int CREATOR;
  int mData;
  int mResultCode;
  int mRequestCode;
  int mResultWho;
}
class QueuedWork {
  int sSingleThreadExecutor;
  int sPendingWorkFinishers;
}
class ProgressDialog {
  int mViewUpdateHandler;
  int mHasStarted;
  int mIndeterminate;
  int mMessage;
  int mIndeterminateDrawable;
  int mProgressDrawable;
  int mIncrementSecondaryBy;
  int mIncrementBy;
  int mSecondaryProgressVal;
  int mProgressVal;
  int mMax;
  int mProgressPercentFormat;
  int mProgressPercent;
  int mProgressNumberFormat;
  int mProgressNumber;
  int mProgressStyle;
  int mMessageView;
  int mProgress;
  int STYLE_HORIZONTAL;
  int STYLE_SPINNER;
}
class PendingIntent {
  int CREATOR;
  class FinishedDispatcher {
    int mResultExtras;
    int mResultData;
    int mResultCode;
    int mIntent;
    int mHandler;
    int mWho;
    int mPendingIntent;
  }
  class OnFinished {
  }
  class CanceledException {
  }
  int FLAG_UPDATE_CURRENT;
  int FLAG_CANCEL_CURRENT;
  int FLAG_NO_CREATE;
  int FLAG_ONE_SHOT;
  int mTarget;
}
class OnActivityPausedListener {
}
class NotificationStressTest {
  int notifyId;
  int mNotificationManager;
  int mContext;
  int mRandom;
  int ICONS;
  int NUM_ITERATIONS;
}
class NotificationManager {
  int mContext;
  int sService;
  int localLOGV;
  int TAG;
}
class Notification {
  class InboxStyle {
    int mTexts;
  }
  class BigTextStyle {
    int mBigText;
  }
  class BigPictureStyle {
    int mBigLargeIconSet;
    int mBigLargeIcon;
    int mPicture;
  }
  class Style {
    int mBuilder;
    int mSummaryTextSet;
    int mSummaryText;
    int mBigContentTitle;
  }
  class Builder {
    int mStyle;
    int mUseChronometer;
    int mActions;
    int mPriority;
    int mExtras;
    int mKindList;
    int mProgressIndeterminate;
    int mProgress;
    int mProgressMax;
    int mFlags;
    int mDefaults;
    int mLedOffMs;
    int mLedOnMs;
    int mLedArgb;
    int mVibrate;
    int mAudioStreamType;
    int mSound;
    int mLargeIcon;
    int mTickerView;
    int mTickerText;
    int mFullScreenIntent;
    int mDeleteIntent;
    int mContentView;
    int mContentIntent;
    int mSubText;
    int mContentInfo;
    int mContentText;
    int mContentTitle;
    int mNumber;
    int mSmallIconLevel;
    int mSmallIcon;
    int mWhen;
    int mContext;
    int MAX_ACTION_BUTTONS;
  }
  int CREATOR;
  int actions;
  class Action {
    int CREATOR;
    int actionIntent;
    int title;
    int icon;
  }
  int extras;
  int EXTRA_PEOPLE;
  int kind;
  int KIND_PROMO;
  int KIND_EVENT;
  int KIND_EMAIL;
  int KIND_MESSAGE;
  int KIND_CALL;
  int priority;
  int PRIORITY_MAX;
  int PRIORITY_HIGH;
  int PRIORITY_MIN;
  int PRIORITY_LOW;
  int PRIORITY_DEFAULT;
  int flags;
  int FLAG_HIGH_PRIORITY;
  int FLAG_FOREGROUND_SERVICE;
  int FLAG_NO_CLEAR;
  int FLAG_AUTO_CANCEL;
  int FLAG_ONLY_ALERT_ONCE;
  int FLAG_INSISTENT;
  int FLAG_ONGOING_EVENT;
  int FLAG_SHOW_LIGHTS;
  int defaults;
  int ledOffMS;
  int ledOnMS;
  int ledARGB;
  int vibrate;
  int audioStreamType;
  int STREAM_DEFAULT;
  int sound;
  int largeIcon;
  int bigContentView;
  int contentView;
  int tickerView;
  int tickerText;
  int fullScreenIntent;
  int deleteIntent;
  int contentIntent;
  int number;
  int iconLevel;
  int icon;
  int when;
  int DEFAULT_LIGHTS;
  int DEFAULT_VIBRATE;
  int DEFAULT_SOUND;
  int DEFAULT_ALL;
}
class NativeActivity {
  class InputMethodCallback {
    int mNa;
  }
  class NativeContentView {
    int mActivity;
  }
  int mDestroyed;
  int mDispatchingUnhandledKey;
  int mLastContentHeight;
  int mLastContentWidth;
  int mLastContentY;
  int mLastContentX;
  int mLocation;
  int mCurSurfaceHolder;
  int mCurInputQueue;
  int mNativeHandle;
  int mInputMethodCallback;
  int mIMM;
  int mNativeContentView;
  int KEY_NATIVE_SAVED_STATE;
  int META_DATA_FUNC_NAME;
  int META_DATA_LIB_NAME;
}
class MediaRouteButton {
  class MediaRouteCallback {
  }
  int ACTIVATED_STATE_SET;
  int mDialogFragment;
  int mExtendedSettingsClickListener;
  int mMinHeight;
  int mMinWidth;
  int mToggleMode;
  int mRemoteActive;
  int mRemoteIndicator;
  int mAttachedToWindow;
  int mRouteTypes;
  int mRouterCallback;
  int mRouter;
  int TAG;
}
class MediaRouteActionProvider {
  class RouterCallback {
    int mAp;
  }
  int mCallback;
  int mExtendedSettingsListener;
  int mRouteTypes;
  int mView;
  int mMenuItem;
  int mRouter;
  int mContext;
  int TAG;
}
class LocalActivityManager {
  int mCurState;
  int mFinishing;
  int mSingleMode;
  int mActivityArray;
  int mActivities;
  int mResumed;
  int mParent;
  int mActivityThread;
  int DESTROYED;
  int RESUMED;
  int STARTED;
  int CREATED;
  int INITIALIZING;
  int RESTORED;
  class LocalActivityRecord {
    int curState;
    int instanceState;
    int window;
    int activity;
    int activityInfo;
    int intent;
    int id;
  }
  int localLOGV;
  int TAG;
}
class LoaderManagerImpl {
  class LoaderInfo {
    int mPendingLoader;
    int mListenerRegistered;
    int mDestroyed;
    int mReportNextStart;
    int mRetainingStarted;
    int mRetaining;
    int mStarted;
    int mData;
    int mDeliveredData;
    int mHaveData;
    int mLoader;
    int mCallbacks;
    int mArgs;
    int mId;
  }
  int mCreatingLoader;
  int mRetainingStarted;
  int mRetaining;
  int mStarted;
  int mActivity;
  int mInactiveLoaders;
  int mLoaders;
  int DEBUG;
  int TAG;
}
class LoaderManager {
  class LoaderCallbacks {
  }
}
class LoadedApk {
  class ServiceDispatcher {
    class DeathMonitor {
      int mService;
      int mName;
    }
    class RunConnection {
      int mCommand;
      int mService;
      int mName;
    }
    int mActiveConnections;
    class InnerConnection {
      int mDispatcher;
    }
    class ConnectionInfo {
      int deathMonitor;
      int binder;
    }
    int mForgotten;
    int mDied;
    int mUnbindLocation;
    int mFlags;
    int mLocation;
    int mActivityThread;
    int mContext;
    int mConnection;
    int mIServiceConnection;
  }
  class ReceiverDispatcher {
    class Args {
      int mOrdered;
      int mCurIntent;
    }
    int mForgotten;
    int mUnregisterLocation;
    int mLocation;
    int mRegistered;
    int mInstrumentation;
    int mActivityThread;
    int mContext;
    int mReceiver;
    int mIIntentReceiver;
    class InnerReceiver {
      int mStrongRef;
      int mDispatcher;
    }
  }
  class WarningContextClassLoader {
    int warned;
  }
  int mClientCount;
  int mUnboundServices;
  int mServices;
  int mUnregisteredReceivers;
  int mReceivers;
  int mApplication;
  int mClassLoader;
  int mResources;
  int mCompatibilityInfo;
  int mIncludeCode;
  int mSecurityViolation;
  int mBaseClassLoader;
  int mDataDirFile;
  int mLibDir;
  int mDataDir;
  int mSharedLibraries;
  int mResDir;
  int mAppDir;
  int mPackageName;
  int mApplicationInfo;
  int mActivityThread;
  int TAG;
}
class ServiceConnectionLeaked {
}
class IntentReceiverLeaked {
}
class ListFragment {
  int mListShown;
  int mEmptyText;
  int mListContainer;
  int mProgressContainer;
  int mStandardEmptyView;
  int mEmptyView;
  int mList;
  int mAdapter;
  int mOnClickListener;
  int mRequestFocus;
  int mHandler;
}
class ListActivity {
  int mOnClickListener;
  int mRequestFocus;
  int mFinishedStart;
  int mHandler;
  int mList;
  int mAdapter;
}
class LauncherActivity {
  class IconResizer {
    int mCanvas;
    int mOldBounds;
    int mIconHeight;
    int mIconWidth;
  }
  class ActivityAdapter {
    class ArrayFilter {
    }
    int mShowIcons;
    int mFilter;
    int mActivitiesList;
    int mInflater;
    int mIconResizer;
    int mOriginalValues;
    int lock;
  }
  class ListItem {
    int extras;
    int className;
    int packageName;
    int icon;
    int label;
    int resolveInfo;
  }
  int mIconResizer;
  int mPackageManager;
  int mIntent;
}
class KeyguardManager {
  class OnKeyguardExitResult {
  }
  class KeyguardLock {
    int mTag;
    int mToken;
  }
  int mWM;
}
class IntentService {
  class ServiceHandler {
  }
  int mRedelivery;
  int mName;
  int mServiceHandler;
  int mServiceLooper;
}
class Instrumentation {
  class Idler {
    int mIdle;
    int mCallback;
  }
  class ActivityGoing {
    int mWaiter;
  }
  class ActivityWaiter {
    int activity;
    int intent;
  }
  class SyncRunnable {
    int mComplete;
    int mTarget;
  }
  class EmptyRunnable {
  }
  class InstrumentationThread {
  }
  class ActivityResult {
    int mResultData;
    int mResultCode;
  }
  class ActivityMonitor {
    int mLastActivity;
    int mHits;
    int mBlock;
    int mResult;
    int mClass;
    int mWhich;
  }
  int mPerfMetrics;
  int mPerformanceCollector;
  int mAutomaticPerformanceSnapshots;
  int mWatcher;
  int mActivityMonitors;
  int mWaitingActivities;
  int mRunner;
  int mComponent;
  int mAppContext;
  int mInstrContext;
  int mMessageQueue;
  int mThread;
  int mSync;
  int TAG;
  int REPORT_KEY_STREAMRESULT;
  int REPORT_KEY_IDENTIFIER;
}
class IApplicationThread {
  int UNSTABLE_PROVIDER_DIED_TRANSACTION;
  int DUMP_DB_INFO_TRANSACTION;
  int DUMP_PROVIDER_TRANSACTION;
  int DUMP_GFX_INFO_TRANSACTION;
  int DUMP_MEM_INFO_TRANSACTION;
  int SCHEDULE_TRIM_MEMORY_TRANSACTION;
  int UPDATE_PACKAGE_COMPATIBILITY_INFO_TRANSACTION;
  int SET_CORE_SETTINGS_TRANSACTION;
  int SET_HTTP_PROXY_TRANSACTION;
  int CLEAR_DNS_CACHE_TRANSACTION;
  int DUMP_ACTIVITY_TRANSACTION;
  int DUMP_HEAP_TRANSACTION;
  int SCHEDULE_CRASH_TRANSACTION;
  int DISPATCH_PACKAGE_BROADCAST_TRANSACTION;
  int SCHEDULE_SUICIDE_TRANSACTION;
  int GET_MEMORY_INFO_TRANSACTION;
  int SCHEDULE_DESTROY_BACKUP_AGENT_TRANSACTION;
  int SCHEDULE_CREATE_BACKUP_AGENT_TRANSACTION;
  int SET_SCHEDULING_GROUP_TRANSACTION;
  int PROFILER_CONTROL_TRANSACTION;
  int SCHEDULE_SLEEPING_TRANSACTION;
  int SCHEDULE_RELAUNCH_ACTIVITY_TRANSACTION;
  int SCHEDULE_ACTIVITY_CONFIGURATION_CHANGED_TRANSACTION;
  int SCHEDULE_LOW_MEMORY_TRANSACTION;
  int SCHEDULE_REGISTERED_RECEIVER_TRANSACTION;
  int DUMP_SERVICE_TRANSACTION;
  int SCHEDULE_UNBIND_SERVICE_TRANSACTION;
  int SCHEDULE_BIND_SERVICE_TRANSACTION;
  int PROCESS_IN_BACKGROUND_TRANSACTION;
  int UPDATE_TIME_ZONE_TRANSACTION;
  int SCHEDULE_SERVICE_ARGS_TRANSACTION;
  int SCHEDULE_CONFIGURATION_CHANGED_TRANSACTION;
  int REQUEST_THUMBNAIL_TRANSACTION;
  int SCHEDULE_EXIT_TRANSACTION;
  int BIND_APPLICATION_TRANSACTION;
  int SCHEDULE_STOP_SERVICE_TRANSACTION;
  int SCHEDULE_CREATE_SERVICE_TRANSACTION;
  int SCHEDULE_RECEIVER_TRANSACTION;
  int SCHEDULE_FINISH_ACTIVITY_TRANSACTION;
  int SCHEDULE_NEW_INTENT_TRANSACTION;
  int SCHEDULE_LAUNCH_ACTIVITY_TRANSACTION;
  int SCHEDULE_SEND_RESULT_TRANSACTION;
  int SCHEDULE_RESUME_ACTIVITY_TRANSACTION;
  int SCHEDULE_WINDOW_VISIBILITY_TRANSACTION;
  int SCHEDULE_STOP_ACTIVITY_TRANSACTION;
  int SCHEDULE_PAUSE_ACTIVITY_TRANSACTION;
  int descriptor;
  int EXTERNAL_STORAGE_UNAVAILABLE;
  int PACKAGE_REMOVED;
  int DEBUG_WAIT;
  int DEBUG_ON;
  int DEBUG_OFF;
  int BACKUP_MODE_RESTORE_FULL;
  int BACKUP_MODE_RESTORE;
  int BACKUP_MODE_FULL;
  int BACKUP_MODE_INCREMENTAL;
}
class IActivityManager {
  int IS_INTENT_SENDER_AN_ACTIVITY_TRANSACTION;
  int UNSTABLE_PROVIDER_DIED_TRANSACTION;
  int GET_LAUNCHED_FROM_UID_TRANSACTION;
  int FINISH_ACTIVITY_AFFINITY_TRANSACTION;
  int SET_LOCK_SCREEN_SHOWN_TRANSACTION;
  int NAVIGATE_UP_TO_TRANSACTION;
  int TARGET_TASK_AFFINITY_MATCHES_ACTIVITY_TRANSACTION;
  int GET_CURRENT_USER_TRANSACTION;
  int KILL_PROCESSES_BELOW_FOREGROUND_TRANSACTION;
  int GET_MY_MEMORY_STATE_TRANSACTION;
  int REMOVE_CONTENT_PROVIDER_EXTERNAL_TRANSACTION;
  int GET_CONTENT_PROVIDER_EXTERNAL_TRANSACTION;
  int KILL_ALL_BACKGROUND_PROCESSES_TRANSACTION;
  int DISMISS_KEYGUARD_ON_NEXT_ACTIVITY_TRANSACTION;
  int SHOW_BOOT_MESSAGE_TRANSACTION;
  int GET_PROCESS_PSS_TRANSACTION;
  int UPDATE_PERSISTENT_CONFIGURATION_TRANSACTION;
  int IS_INTENT_SENDER_TARGETED_TO_PACKAGE_TRANSACTION;
  int UNREGISTER_PROCESS_OBSERVER_TRANSACTION;
  int REGISTER_PROCESS_OBSERVER_TRANSACTION;
  int REMOVE_TASK_TRANSACTION;
  int REMOVE_SUB_TASK_TRANSACTION;
  int SWITCH_USER_TRANSACTION;
  int SET_PACKAGE_ASK_SCREEN_COMPAT_TRANSACTION;
  int GET_PACKAGE_ASK_SCREEN_COMPAT_TRANSACTION;
  int SET_PACKAGE_SCREEN_COMPAT_MODE_TRANSACTION;
  int GET_PACKAGE_SCREEN_COMPAT_MODE_TRANSACTION;
  int SET_FRONT_ACTIVITY_SCREEN_COMPAT_MODE_TRANSACTION;
  int GET_FRONT_ACTIVITY_SCREEN_COMPAT_MODE_TRANSACTION;
  int ACTIVITY_SLEPT_TRANSACTION;
  int START_ACTIVITIES_IN_PACKAGE_TRANSACTION;
  int START_ACTIVITIES_TRANSACTION;
  int DUMP_HEAP_TRANSACTION;
  int CHECK_GRANT_URI_PERMISSION_TRANSACTION;
  int REVOKE_URI_PERMISSION_FROM_OWNER_TRANSACTION;
  int GRANT_URI_PERMISSION_FROM_OWNER_TRANSACTION;
  int NEW_URI_PERMISSION_OWNER_TRANSACTION;
  int GET_PROVIDER_MIME_TYPE_TRANSACTION;
  int CRASH_APPLICATION_TRANSACTION;
  int IS_TOP_ACTIVITY_IMMERSIVE_TRANSACTION;
  int SET_IMMERSIVE_TRANSACTION;
  int IS_IMMERSIVE_TRANSACTION;
  int HANDLE_APPLICATION_STRICT_MODE_VIOLATION_TRANSACTION;
  int FINISH_HEAVY_WEIGHT_APP_TRANSACTION;
  int GET_RUNNING_EXTERNAL_APPLICATIONS_TRANSACTION;
  int START_ACTIVITY_WITH_CONFIG_TRANSACTION;
  int WILL_ACTIVITY_BE_VISIBLE_TRANSACTION;
  int START_ACTIVITY_AND_WAIT_TRANSACTION;
  int IS_USER_A_MONKEY_TRANSACTION;
  int KILL_BACKGROUND_PROCESSES_TRANSACTION;
  int HANDLE_APPLICATION_WTF_TRANSACTION;
  int OVERRIDE_PENDING_TRANSITION_TRANSACTION;
  int START_ACTIVITY_INTENT_SENDER_TRANSACTION;
  int KILL_APPLICATION_PROCESS_TRANSACTION;
  int GET_PROCESS_MEMORY_INFO_TRANSACTION;
  int CLOSE_SYSTEM_DIALOGS_TRANSACTION;
  int KILL_APPLICATION_WITH_UID_TRANSACTION;
  int START_ACTIVITY_IN_PACKAGE_TRANSACTION;
  int GET_UID_FOR_INTENT_SENDER_TRANSACTION;
  int UNBIND_BACKUP_AGENT_TRANSACTION;
  int BACKUP_AGENT_CREATED_TRANSACTION;
  int START_BACKUP_AGENT_TRANSACTION;
  int RESUME_APP_SWITCHES_TRANSACTION;
  int STOP_APP_SWITCHES_TRANSACTION;
  int SHUTDOWN_TRANSACTION;
  int PROFILE_CONTROL_TRANSACTION;
  int PEEK_SERVICE_TRANSACTION;
  int GET_DEVICE_CONFIGURATION_TRANSACTION;
  int GET_RUNNING_APP_PROCESSES_TRANSACTION;
  int GET_TASK_THUMBNAILS_TRANSACTION;
  int GET_SERVICES_TRANSACTION;
  int KILL_PIDS_TRANSACTION;
  int FORCE_STOP_PACKAGE_TRANSACTION;
  int CLEAR_APP_DATA_TRANSACTION;
  int GET_PROCESSES_IN_ERROR_STATE_TRANSACTION;
  int GET_MEMORY_INFO_TRANSACTION;
  int MOVE_ACTIVITY_TASK_TO_BACK_TRANSACTION;
  int SET_SERVICE_FOREGROUND_TRANSACTION;
  int SET_PROCESS_FOREGROUND_TRANSACTION;
  int UNBIND_FINISHED_TRANSACTION;
  int GET_REQUESTED_ORIENTATION_TRANSACTION;
  int SET_REQUESTED_ORIENTATION_TRANSACTION;
  int REMOVE_CONTENT_PROVIDER_TRANSACTION;
  int NOTE_WAKEUP_ALARM_TRANSACTION;
  int START_NEXT_MATCHING_ACTIVITY_TRANSACTION;
  int ENTER_SAFE_MODE_TRANSACTION;
  int GET_PACKAGE_FOR_INTENT_SENDER_TRANSACTION;
  int CANCEL_INTENT_SENDER_TRANSACTION;
  int GET_INTENT_SENDER_TRANSACTION;
  int ACTIVITY_DESTROYED_TRANSACTION;
  int SERVICE_DONE_EXECUTING_TRANSACTION;
  int GET_RECENT_TASKS_TRANSACTION;
  int SIGNAL_PERSISTENT_PROCESSES_TRANSACTION;
  int SHOW_WAITING_FOR_DEBUGGER_TRANSACTION;
  int SET_ACTIVITY_CONTROLLER_TRANSACTION;
  int REVOKE_URI_PERMISSION_TRANSACTION;
  int GRANT_URI_PERMISSION_TRANSACTION;
  int CHECK_URI_PERMISSION_TRANSACTION;
  int CHECK_PERMISSION_TRANSACTION;
  int GET_PROCESS_LIMIT_TRANSACTION;
  int SET_PROCESS_LIMIT_TRANSACTION;
  int GET_PACKAGE_FOR_TOKEN_TRANSACTION;
  int GET_ACTIVITY_CLASS_FOR_TOKEN_TRANSACTION;
  int STOP_SERVICE_TOKEN_TRANSACTION;
  int UPDATE_CONFIGURATION_TRANSACTION;
  int GET_CONFIGURATION_TRANSACTION;
  int FINISH_INSTRUMENTATION_TRANSACTION;
  int START_INSTRUMENTATION_TRANSACTION;
  int SET_ALWAYS_FINISH_TRANSACTION;
  int SET_DEBUG_APP_TRANSACTION;
  int WAKING_UP_TRANSACTION;
  int GOING_TO_SLEEP_TRANSACTION;
  int PUBLISH_SERVICE_TRANSACTION;
  int UNBIND_SERVICE_TRANSACTION;
  int BIND_SERVICE_TRANSACTION;
  int STOP_SERVICE_TRANSACTION;
  int START_SERVICE_TRANSACTION;
  int GET_RUNNING_SERVICE_CONTROL_PANEL_TRANSACTION;
  int FINISH_SUB_ACTIVITY_TRANSACTION;
  int REF_CONTENT_PROVIDER_TRANSACTION;
  int PUBLISH_CONTENT_PROVIDERS_TRANSACTION;
  int GET_CONTENT_PROVIDER_TRANSACTION;
  int REPORT_THUMBNAIL_TRANSACTION;
  int GET_TASK_FOR_ACTIVITY_TRANSACTION;
  int MOVE_TASK_BACKWARDS_TRANSACTION;
  int MOVE_TASK_TO_BACK_TRANSACTION;
  int MOVE_TASK_TO_FRONT_TRANSACTION;
  int GET_TASKS_TRANSACTION;
  int GET_CALLING_ACTIVITY_TRANSACTION;
  int GET_CALLING_PACKAGE_TRANSACTION;
  int ACTIVITY_STOPPED_TRANSACTION;
  int ACTIVITY_PAUSED_TRANSACTION;
  int ACTIVITY_IDLE_TRANSACTION;
  int ATTACH_APPLICATION_TRANSACTION;
  int FINISH_RECEIVER_TRANSACTION;
  int UNBROADCAST_INTENT_TRANSACTION;
  int BROADCAST_INTENT_TRANSACTION;
  int UNREGISTER_RECEIVER_TRANSACTION;
  int REGISTER_RECEIVER_TRANSACTION;
  int FINISH_ACTIVITY_TRANSACTION;
  int OPEN_CONTENT_URI_TRANSACTION;
  int UNHANDLED_BACK_TRANSACTION;
  int START_ACTIVITY_TRANSACTION;
  int HANDLE_APPLICATION_CRASH_TRANSACTION;
  int START_RUNNING_TRANSACTION;
  int descriptor;
  class WaitResult {
    int CREATOR;
    int totalTime;
    int thisTime;
    int who;
    int timeout;
    int result;
  }
  class ContentProviderHolder {
    int CREATOR;
    int noReleaseNeeded;
    int connection;
    int provider;
    int info;
  }
}
class Fragment_Delegate {
  int sProjectCallback;
}
class FragmentTransaction {
  int TRANSIT_FRAGMENT_FADE;
  int TRANSIT_FRAGMENT_CLOSE;
  int TRANSIT_FRAGMENT_OPEN;
  int TRANSIT_NONE;
  int TRANSIT_UNSET;
  int TRANSIT_EXIT_MASK;
  int TRANSIT_ENTER_MASK;
}
class FragmentManagerImpl {
  int mExecCommit;
  int mStateArray;
  int mStateBundle;
  int mHavePendingDeferredStart;
  int mNoTransactionsBecause;
  int mDestroyed;
  int mStateSaved;
  int mNeedMenuInvalidate;
  int mActivity;
  int mCurState;
  int mBackStackChangeListeners;
  int mAvailBackStackIndices;
  int mBackStackIndices;
  int mCreatedMenus;
  int mBackStack;
  int mAvailIndices;
  int mAdded;
  int mActive;
  int mExecutingActions;
  int mTmpActions;
  int mPendingActions;
  int USER_VISIBLE_HINT_TAG;
  int VIEW_STATE_TAG;
  int TARGET_STATE_TAG;
  int TARGET_REQUEST_CODE_STATE_TAG;
  int TAG;
  int DEBUG;
}
class FragmentManagerState {
  int CREATOR;
  int mBackStack;
  int mAdded;
  int mActive;
}
class FragmentManager {
  int POP_BACK_STACK_INCLUSIVE;
  class OnBackStackChangedListener {
  }
  class BackStackEntry {
  }
}
class FragmentBreadCrumbs {
  int mOnClickListener;
  class OnBreadCrumbClickListener {
  }
  int mOnBreadCrumbClickListener;
  int mParentClickListener;
  int mParentEntry;
  int mTopEntry;
  int mMaxVisible;
  int mContainer;
  int mInflater;
  int mActivity;
}
class Fragment {
  class InstantiationException {
  }
  class SavedState {
    int CREATOR;
    int mState;
  }
  int mCheckedForLoaderManager;
  int mLoadersStarted;
  int mLoaderManager;
  int mUserVisibleHint;
  int mDeferStart;
  int mView;
  int mContainer;
  int mNextAnim;
  int mCalled;
  int mMenuVisible;
  int mHasMenu;
  int mRetaining;
  int mRetainInstance;
  int mDetached;
  int mHidden;
  int mTag;
  int mContainerId;
  int mFragmentId;
  int mActivity;
  int mFragmentManager;
  int mBackStackNesting;
  int mRestored;
  int mInLayout;
  int mFromLayout;
  int mResumed;
  int mRemoving;
  int mAdded;
  int mTargetRequestCode;
  int mTargetIndex;
  int mTarget;
  int mArguments;
  int mWho;
  int mIndex;
  int mSavedViewState;
  int mSavedFragmentState;
  int mStateAfterAnimating;
  int mAnimatingAway;
  int mState;
  int RESUMED;
  int STARTED;
  int STOPPED;
  int ACTIVITY_CREATED;
  int CREATED;
  int INITIALIZING;
  int INVALID_STATE;
  int sClassMap;
}
class FragmentState {
  int CREATOR;
  int mInstance;
  int mSavedFragmentState;
  int mArguments;
  int mDetached;
  int mRetainInstance;
  int mTag;
  int mContainerId;
  int mFragmentId;
  int mFromLayout;
  int mIndex;
  int mClassName;
}
class ExpandableListActivity {
  int mFinishedStart;
  int mList;
  int mAdapter;
}
class DownloadManagerStressTest {
  int CACHE_DIR;
  int TAG;
}
class DownloadManagerFunctionalTest {
  int PROHIBITED_DIRECTORY;
  int CACHE_DIR;
  int TAG;
}
class DownloadManagerBaseTest {
  class WiFiChangedReceiver {
    int mContext;
  }
  class MultipleDownloadsCompletedReceiver {
    int downloadIds;
    int mNumDownloadsCompleted;
  }
  class LoggingRng {
  }
  class DataType {
    int BINARY;
    int TEXT;
  }
  class DownloadFileType {
    int ZIP;
    int UNRECOGNIZED;
    int GARBAGE;
    int GIF;
    int APK;
    int PLAINTEXT;
  }
  int DOWNLOAD_TO_DOWNLOAD_CACHE_DIR;
  int DOWNLOAD_TO_SYSTEM_CACHE;
  int MAX_WAIT_FOR_LARGE_DOWNLOAD_TIME;
  int MAX_WAIT_FOR_DOWNLOAD_TIME;
  int WAIT_FOR_DOWNLOAD_POLL_TIME;
  int DEFAULT_WAIT_POLL_TIME;
  int DEFAULT_MAX_WAIT_TIME;
  int DEFAULT_FILENAME;
  int HTTP_SERVICE_UNAVAILABLE;
  int HTTP_NOT_FOUND;
  int HTTP_PARTIAL_CONTENT;
  int HTTP_REDIRECT;
  int HTTP_OK;
  int LOG_TAG;
  int FILE_BLOCK_READ_SIZE;
  int DEFAULT_FILE_SIZE;
  int mReceiver;
  int mContext;
  int mFileType;
  int mServer;
  int mDownloadManager;
  int TAG;
}
class DownloadManager {
  class CursorTranslator {
    int mBaseUri;
  }
  int NON_DOWNLOADMANAGER_DOWNLOAD;
  int mBaseUri;
  int mPackageName;
  int mResolver;
  class Query {
    int mOnlyIncludeVisibleInDownloadsUi;
    int mOrderDirection;
    int mOrderByColumn;
    int mStatusFlags;
    int mIds;
    int ORDER_DESCENDING;
    int ORDER_ASCENDING;
  }
  class Request {
    int mNotificationVisibility;
    int VISIBILITY_VISIBLE_NOTIFY_ONLY_COMPLETION;
    int VISIBILITY_HIDDEN;
    int VISIBILITY_VISIBLE_NOTIFY_COMPLETED;
    int VISIBILITY_VISIBLE;
    int SCANNABLE_VALUE_NO;
    int SCANNABLE_VALUE_YES;
    int mUseSystemCache;
    int mScannable;
    int mIsVisibleInDownloadsUi;
    int mMeteredAllowed;
    int mRoamingAllowed;
    int mAllowedNetworkTypes;
    int mMimeType;
    int mDescription;
    int mTitle;
    int mRequestHeaders;
    int mDestinationUri;
    int mUri;
    int NETWORK_WIFI;
    int NETWORK_MOBILE;
  }
  int UNDERLYING_COLUMNS;
  int EXTRA_NOTIFICATION_CLICK_DOWNLOAD_IDS;
  int EXTRA_DOWNLOAD_ID;
  int INTENT_EXTRAS_SORT_BY_SIZE;
  int ACTION_VIEW_DOWNLOADS;
  int ACTION_NOTIFICATION_CLICKED;
  int ACTION_DOWNLOAD_COMPLETE;
  int PAUSED_UNKNOWN;
  int PAUSED_QUEUED_FOR_WIFI;
  int PAUSED_WAITING_FOR_NETWORK;
  int PAUSED_WAITING_TO_RETRY;
  int ERROR_BLOCKED;
  int ERROR_FILE_ALREADY_EXISTS;
  int ERROR_CANNOT_RESUME;
  int ERROR_DEVICE_NOT_FOUND;
  int ERROR_INSUFFICIENT_SPACE;
  int ERROR_TOO_MANY_REDIRECTS;
  int ERROR_HTTP_DATA_ERROR;
  int ERROR_UNHANDLED_HTTP_CODE;
  int ERROR_FILE_ERROR;
  int ERROR_UNKNOWN;
  int STATUS_FAILED;
  int STATUS_SUCCESSFUL;
  int STATUS_PAUSED;
  int STATUS_RUNNING;
  int STATUS_PENDING;
  int COLUMN_MEDIAPROVIDER_URI;
  int COLUMN_LAST_MODIFIED_TIMESTAMP;
  int COLUMN_BYTES_DOWNLOADED_SO_FAR;
  int COLUMN_REASON;
  int COLUMN_STATUS;
  int COLUMN_LOCAL_FILENAME;
  int COLUMN_LOCAL_URI;
  int COLUMN_TOTAL_SIZE_BYTES;
  int COLUMN_MEDIA_TYPE;
  int COLUMN_URI;
  int COLUMN_DESCRIPTION;
  int COLUMN_TITLE;
  int COLUMN_ID;
}
class DialogFragment {
  int mShownByMe;
  int mDismissed;
  int mViewDestroyed;
  int mDialog;
  int mBackStackId;
  int mShowsDialog;
  int mCancelable;
  int mTheme;
  int mStyle;
  int SAVED_BACK_STACK_ID;
  int SAVED_SHOWS_DIALOG;
  int SAVED_CANCELABLE;
  int SAVED_THEME;
  int SAVED_STYLE;
  int SAVED_DIALOG_STATE_TAG;
  int STYLE_NO_INPUT;
  int STYLE_NO_FRAME;
  int STYLE_NO_TITLE;
  int STYLE_NORMAL;
}
class Dialog {
  class ListenersHandler {
    int mDialog;
  }
  int DIALOG_HIERARCHY_TAG;
  int DIALOG_SHOWING_TAG;
  int mDismissAction;
  int mActionMode;
  int mListenersHandler;
  int SHOW;
  int CANCEL;
  int DISMISS;
  int mHandler;
  int mCanceled;
  int mShowing;
  int mCreated;
  int mOnKeyListener;
  int mShowMessage;
  int mDismissMessage;
  int mCancelMessage;
  int mCancelAndDismissTaken;
  int mCancelable;
  int mActionBar;
  int mDecor;
  int mWindow;
  int mWindowManager;
  int mContext;
  int mOwnerActivity;
  int TAG;
}
class DatePickerDialog {
  class OnDateSetListener {
  }
  int mTitleNeedsUpdate;
  int mCalendar;
  int mCallBack;
  int mDatePicker;
  int DAY;
  int MONTH;
  int YEAR;
}
class ContextImpl {
  class ApplicationContentResolver {
    int mMainThread;
  }
  int mServiceCache;
  int WALLPAPER_FETCHER;
  int sNextPerContextServiceCacheIndex;
  int SYSTEM_SERVICE_MAP;
  class StaticServiceFetcher {
    int mCachedInstance;
  }
  class ServiceFetcher {
    int mContextCacheIndex;
  }
  int EMPTY_FILE_LIST;
  int mExternalCacheDir;
  int mExternalFilesDir;
  int mObbDir;
  int mCacheDir;
  int mFilesDir;
  int mPreferencesDir;
  int mDatabasesDir;
  int mSync;
  int mRestricted;
  int mReceiverRestrictedContext;
  int mPackageManager;
  int mTheme;
  int mThemeResource;
  int mContentResolver;
  int mActivityToken;
  int mOuterContext;
  int mMainThread;
  int mResources;
  int mBasePackageName;
  int mPackageInfo;
  int sSharedPrefs;
  int DEBUG;
  int TAG;
}
class ReceiverRestrictedContext {
}
class BackStackRecord {
  int mBreadCrumbShortTitleText;
  int mBreadCrumbShortTitleRes;
  int mBreadCrumbTitleText;
  int mBreadCrumbTitleRes;
  int mIndex;
  int mCommitted;
  int mName;
  int mAllowAddToBackStack;
  int mAddToBackStack;
  int mTransitionStyle;
  int mTransition;
  int mPopExitAnim;
  int mPopEnterAnim;
  int mExitAnim;
  int mEnterAnim;
  int mNumOp;
  int mTail;
  int mHead;
  class Op {
    int removed;
    int popExitAnim;
    int popEnterAnim;
    int exitAnim;
    int enterAnim;
    int fragment;
    int cmd;
    int prev;
    int next;
  }
  int OP_ATTACH;
  int OP_DETACH;
  int OP_SHOW;
  int OP_HIDE;
  int OP_REMOVE;
  int OP_REPLACE;
  int OP_ADD;
  int OP_NULL;
  int mManager;
  int TAG;
}
class BackStackState {
  int CREATOR;
  int mBreadCrumbShortTitleText;
  int mBreadCrumbShortTitleRes;
  int mBreadCrumbTitleText;
  int mBreadCrumbTitleRes;
  int mIndex;
  int mName;
  int mTransitionStyle;
  int mTransition;
  int mOps;
}
class ApplicationThreadProxy {
  int mRemote;
}
class ApplicationThreadNative {
}
class ApplicationPackageManager {
  int sStringCache;
  int sIconCache;
  int sSync;
  int mPM;
  int mContext;
  class ResourceName {
    int iconId;
    int packageName;
  }
  int mCachedSafeMode;
  int DEBUG_ICONS;
  int DEBUG;
  int TAG;
}
class ApplicationLoaders {
  int gApplicationLoaders;
  int mLoaders;
}
class ApplicationErrorReport {
  int CREATOR;
  class RunningServiceInfo {
    int serviceDetails;
    int durationMillis;
  }
  class BatteryInfo {
    int checkinDetails;
    int usageDetails;
    int durationMicros;
    int usagePercent;
  }
  class AnrInfo {
    int info;
    int cause;
    int activity;
  }
  class CrashInfo {
    int stackTrace;
    int throwLineNumber;
    int throwMethodName;
    int throwClassName;
    int throwFileName;
    int exceptionMessage;
    int exceptionClassName;
  }
  int runningServiceInfo;
  int batteryInfo;
  int anrInfo;
  int crashInfo;
  int systemApp;
  int time;
  int processName;
  int installerPackageName;
  int packageName;
  int type;
  int TYPE_RUNNING_SERVICE;
  int TYPE_BATTERY;
  int TYPE_ANR;
  int TYPE_CRASH;
  int TYPE_NONE;
  int DEFAULT_ERROR_RECEIVER_PROPERTY;
  int SYSTEM_APPS_ERROR_RECEIVER_PROPERTY;
}
class Application {
  class ActivityLifecycleCallbacks {
  }
  int mLoadedApk;
  int mActivityLifecycleCallbacks;
  int mComponentCallbacks;
}
class AppGlobals {
}
class AliasActivity {
  int ALIAS_META_DATA;
}
class AlertDialog {
  class Builder {
    int mTheme;
    int P;
  }
  int THEME_DEVICE_DEFAULT_LIGHT;
  int THEME_DEVICE_DEFAULT_DARK;
  int THEME_HOLO_LIGHT;
  int THEME_HOLO_DARK;
  int THEME_TRADITIONAL;
  int mAlert;
}
class AlarmManager {
  int INTERVAL_DAY;
  int INTERVAL_HALF_DAY;
  int INTERVAL_HOUR;
  int INTERVAL_HALF_HOUR;
  int INTERVAL_FIFTEEN_MINUTES;
  int mService;
  int ELAPSED_REALTIME;
  int ELAPSED_REALTIME_WAKEUP;
  int RTC;
  int RTC_WAKEUP;
}
class ActivityThread {
  class ProviderRefCount {
    int removePending;
    int unstableCount;
    int stableCount;
    int client;
    int holder;
  }
  class StopInfo {
    int description;
    int thumbnail;
    int state;
    int activity;
  }
  int mThumbnailCanvas;
  int mAvailThumbnailBitmap;
  int mThumbnailHeight;
  int mThumbnailWidth;
  int sCurrentBroadcastIntent;
  int mMainThreadConfig;
  class ResourcesKey {
    int mHash;
    int mScale;
    int mResDir;
  }
  class GcIdler {
  }
  class Idler {
  }
  class H {
    int UNSTABLE_PROVIDER_DIED;
    int DUMP_PROVIDER;
    int TRIM_MEMORY;
    int UPDATE_PACKAGE_COMPATIBILITY_INFO;
    int SET_CORE_SETTINGS;
    int SLEEPING;
    int DUMP_ACTIVITY;
    int DUMP_HEAP;
    int SCHEDULE_CRASH;
    int DISPATCH_PACKAGE_BROADCAST;
    int ENABLE_JIT;
    int REMOVE_PROVIDER;
    int SUICIDE;
    int DESTROY_BACKUP_AGENT;
    int CREATE_BACKUP_AGENT;
    int PROFILER_CONTROL;
    int RELAUNCH_ACTIVITY;
    int ACTIVITY_CONFIGURATION_CHANGED;
    int LOW_MEMORY;
    int DUMP_SERVICE;
    int UNBIND_SERVICE;
    int BIND_SERVICE;
    int GC_WHEN_IDLE;
    int CLEAN_UP_CONTEXT;
    int CONFIGURATION_CHANGED;
    int REQUEST_THUMBNAIL;
    int STOP_SERVICE;
    int SERVICE_ARGS;
    int CREATE_SERVICE;
    int RECEIVER;
    int NEW_INTENT;
    int EXIT_APPLICATION;
    int BIND_APPLICATION;
    int DESTROY_ACTIVITY;
    int SEND_RESULT;
    int RESUME_ACTIVITY;
    int HIDE_WINDOW;
    int SHOW_WINDOW;
    int STOP_ACTIVITY_HIDE;
    int STOP_ACTIVITY_SHOW;
    int PAUSE_ACTIVITY_FINISHING;
    int PAUSE_ACTIVITY;
    int LAUNCH_ACTIVITY;
  }
  class ApplicationThread {
    int ACTIVITY_THREAD_CHECKIN_VERSION;
    int DB_INFO_FORMAT;
    int TWO_COUNT_COLUMNS;
    int ONE_COUNT_COLUMN;
    int HEAP_COLUMN;
  }
  class UpdateCompatibilityData {
    int info;
    int pkg;
  }
  class DumpHeapData {
    int fd;
    int path;
  }
  class ProfilerControlData {
    int fd;
    int path;
  }
  class ContextCleanupInfo {
    int who;
    int what;
    int context;
  }
  class ResultData {
    int results;
    int token;
  }
  class DumpComponentInfo {
    int args;
    int prefix;
    int token;
    int fd;
  }
  class Profiler {
    int handlingProfiling;
    int profiling;
    int autoStopProfiler;
    int profileFd;
    int profileFile;
  }
  class AppBindData {
    int initAutoStopProfiler;
    int initProfileFd;
    int initProfileFile;
    int compatInfo;
    int config;
    int persistent;
    int restrictedBackupMode;
    int enableOpenGlTrace;
    int debugMode;
    int instrumentationWatcher;
    int instrumentationArgs;
    int instrumentationName;
    int providers;
    int appInfo;
    int processName;
    int info;
  }
  class ServiceArgsData {
    int args;
    int flags;
    int startId;
    int taskRemoved;
    int token;
  }
  class BindServiceData {
    int rebind;
    int intent;
    int token;
  }
  class CreateServiceData {
    int intent;
    int compatInfo;
    int info;
    int token;
  }
  class CreateBackupAgentData {
    int backupMode;
    int compatInfo;
    int appInfo;
  }
  class ReceiverData {
    int compatInfo;
    int info;
    int intent;
  }
  class NewIntentData {
    int token;
    int intents;
  }
  class ProviderClientRecord {
    int mHolder;
    int mLocalProvider;
    int mProvider;
    int mNames;
  }
  class ActivityClientRecord {
    int mPendingRemoveWindowManager;
    int mPendingRemoveWindow;
    int onlyLocalRequest;
    int pendingConfigChanges;
    int isForward;
    int startsNotResumed;
    int pendingIntents;
    int pendingResults;
    int packageInfo;
    int compatInfo;
    int activityInfo;
    int autoStopProfiler;
    int profileFd;
    int profileFile;
    int nextIdle;
    int createdConfig;
    int newConfig;
    int hideForNow;
    int stopped;
    int paused;
    int lastNonConfigurationInstances;
    int embeddedID;
    int parent;
    int window;
    int activity;
    int state;
    int intent;
    int ident;
    int token;
  }
  int mCoreSettings;
  int sMainThreadHandler;
  int mGcIdlerScheduled;
  int mGcIdler;
  int mOnPauseListeners;
  int mLocalProvidersByName;
  int mLocalProviders;
  int mProviderRefCountMap;
  int mProviderMap;
  int mPendingConfiguration;
  int mRelaunchingActivities;
  int mActiveResources;
  int mDisplayMetrics;
  int mResourcePackages;
  int mPackages;
  int mJitEnabled;
  int mSystemThread;
  int mInstrumentedAppLibraryDir;
  int mInstrumentedAppDir;
  int mInstrumentationAppPackage;
  int mInstrumentationAppLibraryDir;
  int mInstrumentationAppDir;
  int mInstrumentation;
  int sThreadLocal;
  int mBackupAgents;
  int mAllApplications;
  int mInitialApplication;
  int mResCompatibilityInfo;
  int mResConfiguration;
  int mCompatConfiguration;
  int mConfiguration;
  int mProfiler;
  int mBoundApplication;
  int mServices;
  int mNumVisibleActivities;
  int mNewActivities;
  int mActivities;
  int mH;
  int mLooper;
  int mAppThread;
  int sPackageManager;
  int mSystemContext;
  int LOG_ON_RESUME_CALLED;
  int LOG_ON_PAUSE_CALLED;
  int SQLITE_MEM_RELEASED_EVENT_LOG_TAG;
  int PATTERN_SEMICOLON;
  int MIN_TIME_BETWEEN_GCS;
  int DEBUG_PROVIDER;
  int DEBUG_MEMORY_TRIM;
  int DEBUG_SERVICE;
  int DEBUG_CONFIGURATION;
  int DEBUG_BACKUP;
  int DEBUG_RESULTS;
  int DEBUG_BROADCAST;
  int DEBUG_MESSAGES;
  int localLOGV;
  int THUMBNAIL_FORMAT;
  int TAG;
}
class RemoteServiceException {
}
class SuperNotCalledException {
}
class ActivityOptions {
  class OnAnimationStartedListener {
  }
  int mAnimationStartedListener;
  int mStartHeight;
  int mStartWidth;
  int mStartY;
  int mStartX;
  int mThumbnail;
  int mCustomExitResId;
  int mCustomEnterResId;
  int mAnimationType;
  int mPackageName;
  int ANIM_THUMBNAIL_DELAYED;
  int ANIM_THUMBNAIL;
  int ANIM_SCALE_UP;
  int ANIM_CUSTOM;
  int ANIM_NONE;
  int KEY_ANIM_START_LISTENER;
  int KEY_ANIM_START_HEIGHT;
  int KEY_ANIM_START_WIDTH;
  int KEY_ANIM_START_Y;
  int KEY_ANIM_START_X;
  int KEY_ANIM_THUMBNAIL;
  int KEY_ANIM_EXIT_RES_ID;
  int KEY_ANIM_ENTER_RES_ID;
  int KEY_ANIM_TYPE;
  int KEY_PACKAGE_NAME;
}
class ActivityManagerProxy {
  int mRemote;
}
class ActivityManagerNative {
  int gDefault;
  int sSystemReady;
}
class ActivityManager {
  class RunningAppProcessInfo {
    int CREATOR;
    int importanceReasonImportance;
    int importanceReasonComponent;
    int importanceReasonPid;
    int importanceReasonCode;
    int REASON_SERVICE_IN_USE;
    int REASON_PROVIDER_IN_USE;
    int REASON_UNKNOWN;
    int lru;
    int importance;
    int IMPORTANCE_EMPTY;
    int IMPORTANCE_BACKGROUND;
    int IMPORTANCE_SERVICE;
    int IMPORTANCE_CANT_SAVE_STATE;
    int IMPORTANCE_PERCEPTIBLE;
    int IMPORTANCE_VISIBLE;
    int IMPORTANCE_FOREGROUND;
    int IMPORTANCE_PERSISTENT;
    int lastTrimLevel;
    int flags;
    int FLAG_PERSISTENT;
    int FLAG_CANT_SAVE_STATE;
    int pkgList;
    int uid;
    int pid;
    int processName;
  }
  class ProcessErrorStateInfo {
    int CREATOR;
    int crashData;
    int stackTrace;
    int longMsg;
    int shortMsg;
    int tag;
    int uid;
    int pid;
    int processName;
    int condition;
    int NOT_RESPONDING;
    int CRASHED;
    int NO_ERROR;
  }
  class MemoryInfo {
    int CREATOR;
    int foregroundAppThreshold;
    int visibleAppThreshold;
    int secondaryServerThreshold;
    int hiddenAppThreshold;
    int lowMemory;
    int threshold;
    int totalMem;
    int availMem;
  }
  class RunningServiceInfo {
    int CREATOR;
    int clientLabel;
    int clientPackage;
    int flags;
    int FLAG_PERSISTENT_PROCESS;
    int FLAG_SYSTEM_PROCESS;
    int FLAG_FOREGROUND;
    int FLAG_STARTED;
    int restarting;
    int lastActivityTime;
    int crashCount;
    int clientCount;
    int started;
    int activeSince;
    int foreground;
    int process;
    int uid;
    int pid;
    int service;
  }
  int MOVE_TASK_NO_USER_ACTION;
  int MOVE_TASK_WITH_HOME;
  class TaskThumbnails {
    int CREATOR;
    int retriever;
    int numSubThumbbails;
    int mainThumbnail;
  }
  int REMOVE_TASK_KILL_PROCESS;
  class RunningTaskInfo {
    int CREATOR;
    int numRunning;
    int numActivities;
    int description;
    int thumbnail;
    int topActivity;
    int baseActivity;
    int id;
  }
  int RECENT_IGNORE_UNAVAILABLE;
  int RECENT_WITH_EXCLUDED;
  class RecentTaskInfo {
    int CREATOR;
    int description;
    int origActivity;
    int baseIntent;
    int persistentId;
    int id;
  }
  int COMPAT_MODE_TOGGLE;
  int COMPAT_MODE_ENABLED;
  int COMPAT_MODE_DISABLED;
  int COMPAT_MODE_UNKNOWN;
  int COMPAT_MODE_NEVER;
  int COMPAT_MODE_ALWAYS;
  int INTENT_SENDER_SERVICE;
  int INTENT_SENDER_ACTIVITY_RESULT;
  int INTENT_SENDER_ACTIVITY;
  int INTENT_SENDER_BROADCAST;
  int BROADCAST_STICKY_CANT_HAVE_PERMISSION;
  int BROADCAST_SUCCESS;
  int START_FLAG_AUTO_STOP_PROFILER;
  int START_FLAG_OPENGL_TRACES;
  int START_FLAG_DEBUG;
  int START_FLAG_ONLY_IF_NEEDED;
  int START_SWITCHES_CANCELED;
  int START_DELIVERED_TO_TOP;
  int START_TASK_TO_FRONT;
  int START_RETURN_INTENT_TO_CALLER;
  int START_SUCCESS;
  int START_INTENT_NOT_RESOLVED;
  int START_CLASS_NOT_FOUND;
  int START_FORWARD_AND_REQUEST_CONFLICT;
  int START_PERMISSION_DENIED;
  int START_NOT_ACTIVITY;
  int START_CANCELED;
  int mHandler;
  int mContext;
  int localLOGV;
  int TAG;
}
class ActivityGroup {
  int mLocalActivityManager;
  int PARENT_NON_CONFIG_INSTANCE_KEY;
  int STATES_KEY;
}
class Activity {
  int DEFAULT_KEYS_SEARCH_GLOBAL;
  int DEFAULT_KEYS_SEARCH_LOCAL;
  int DEFAULT_KEYS_SHORTCUT;
  int DEFAULT_KEYS_DIALER;
  int DEFAULT_KEYS_DISABLE;
  int mHandler;
  int mUiThread;
  int mInstanceTracker;
  int FOCUSED_STATE_SET;
  int mDefaultKeySsb;
  int mDefaultKeyMode;
  int mTitleReady;
  int mResultData;
  int mResultCode;
  int mManagedCursors;
  class ManagedCursor {
    int mUpdated;
    int mReleased;
    int mCursor;
  }
  int mLoaderManager;
  int mAllLoaderManagers;
  int mFragments;
  int mTitleColor;
  int mTitle;
  int mEnableDefaultActionBarUp;
  int mActionBar;
  int mVisibleFromClient;
  int mVisibleFromServer;
  int mWindowAdded;
  int mDecor;
  int mWindowManager;
  int mWindow;
  int mLastNonConfigurationInstances;
  class NonConfigurationInstances {
    int loaders;
    int fragments;
    int children;
    int activity;
  }
  int mMenuInflater;
  int mSearchManager;
  int mCurrentConfig;
  int mConfigChangeFlags;
  int mChangingConfigurations;
  int mTemporaryPause;
  int mStartedActivity;
  int mFinished;
  int mStopped;
  int mResumed;
  int mLoadersStarted;
  int mCheckedForLoaderManager;
  int mCalled;
  int mParent;
  int mMainThread;
  int mActivityInfo;
  int mComponent;
  int mIntent;
  int mApplication;
  int mEmbeddedID;
  int mIdent;
  int mToken;
  int mInstrumentation;
  int mManagedDialogs;
  class ManagedDialog {
    int mArgs;
    int mDialog;
  }
  int SAVED_DIALOG_ARGS_KEY_PREFIX;
  int SAVED_DIALOG_KEY_PREFIX;
  int SAVED_DIALOGS_TAG;
  int SAVED_DIALOG_IDS_KEY;
  int FRAGMENTS_TAG;
  int WINDOW_HIERARCHY_TAG;
  int RESULT_FIRST_USER;
  int RESULT_OK;
  int RESULT_CANCELED;
  int DEBUG_LIFECYCLE;
  int TAG;
}
