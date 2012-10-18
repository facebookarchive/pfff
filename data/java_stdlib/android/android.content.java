package android.content;
class UriMatcher {
  int mChildren;
  int mText;
  int mWhich;
  int mCode;
  int TEXT;
  int NUMBER;
  int EXACT;
  int PATH_SPLIT_PATTERN;
  int NO_MATCH;
}
class TestContext {
  int mRealContext;
  int mResolver;
}
class SyncStorageEngineTest {
}
class SyncStorageEngine {
  int STATISTICS_FILE_ITEM;
  int STATISTICS_FILE_ITEM_OLD;
  int STATISTICS_FILE_END;
  int PENDING_OPERATION_VERSION;
  int STATUS_FILE_ITEM;
  int STATUS_FILE_END;
  int mSyncRequestListener;
  int mMasterSyncAutomatically;
  int mNextHistoryId;
  int mNumPendingFinished;
  int PENDING_FINISH_TO_WRITE;
  int mPendingFile;
  int mStatisticsFile;
  int mStatusFile;
  int mAccountInfoFile;
  int mSyncRandomOffset;
  int sSyncStorageEngine;
  int mContext;
  int mYearInDays;
  int mYear;
  int mCal;
  int mDayStats;
  int mNextAuthorityId;
  int mChangeListeners;
  int mSyncHistory;
  int mSyncStatus;
  int mCurrentSyncs;
  int mPendingOperations;
  int mAccounts;
  int mAuthorities;
  class OnSyncRequestListener {
  }
  class DayStats {
    int failureTime;
    int failureCount;
    int successTime;
    int successCount;
    int day;
  }
  class SyncHistoryItem {
    int initialization;
    int mesg;
    int downstreamActivity;
    int upstreamActivity;
    int event;
    int source;
    int elapsedTime;
    int eventTime;
    int historyId;
    int authorityId;
  }
  class AuthorityInfo {
    int periodicSyncs;
    int delayUntil;
    int backoffDelay;
    int backoffTime;
    int syncable;
    int enabled;
    int ident;
    int authority;
    int userId;
    int account;
  }
  class AccountInfo {
    int authorities;
    int accountAndUser;
  }
  class PendingOperation {
    int flatExtras;
    int authorityId;
    int expedited;
    int extras;
    int authority;
    int syncSource;
    int userId;
    int account;
  }
  int sAuthorityRenames;
  int ACCOUNTS_VERSION;
  int SYNC_ENABLED_DEFAULT;
  int WRITE_STATISTICS_DELAY;
  int MSG_WRITE_STATISTICS;
  int WRITE_STATUS_DELAY;
  int MSG_WRITE_STATUS;
  int MAX_HISTORY;
  int MESG_CANCELED;
  int MESG_SUCCESS;
  int SOURCES;
  int SYNC_CONNECTION_SETTING_CHANGED_INTENT;
  int NOT_IN_BACKOFF_MODE;
  int SOURCE_PERIODIC;
  int SOURCE_USER;
  int SOURCE_POLL;
  int SOURCE_LOCAL;
  int SOURCE_SERVER;
  int EVENTS;
  int EVENT_STOP;
  int EVENT_START;
  int MILLIS_IN_4WEEKS;
  int DEFAULT_POLL_FREQUENCY_SECONDS;
  int XML_TAG_LISTEN_FOR_TICKLES;
  int XML_ATTR_USER;
  int XML_ATTR_ENABLED;
  int XML_ATTR_SYNC_RANDOM_OFFSET;
  int XML_ATTR_LISTEN_FOR_TICKLES;
  int XML_ATTR_NEXT_AUTHORITY_ID;
  int DEBUG_FILE;
  int TAG;
}
class SyncStatusObserver {
}
class SyncStatusInfo {
  int CREATOR;
  int TAG;
  int periodicSyncTimes;
  int initialize;
  int pending;
  int initialFailureTime;
  int lastFailureMesg;
  int lastFailureSource;
  int lastFailureTime;
  int lastSuccessSource;
  int lastSuccessTime;
  int numSourcePeriodic;
  int numSourceUser;
  int numSourceLocal;
  int numSourceServer;
  int numSourcePoll;
  int numSyncs;
  int totalElapsedTime;
  int authorityId;
  int VERSION;
}
class SyncStats {
  int CREATOR;
  int numSkippedEntries;
  int numEntries;
  int numDeletes;
  int numUpdates;
  int numInserts;
  int numConflictDetectedExceptions;
  int numParseExceptions;
  int numIoExceptions;
  int numAuthExceptions;
}
class SyncResult {
  int CREATOR;
  int ALREADY_IN_PROGRESS;
  int stats;
  int delayUntil;
  int moreRecordsToGet;
  int partialSyncUnavailable;
  int fullSyncRequested;
  int databaseError;
  int tooManyRetries;
  int tooManyDeletions;
  int syncAlreadyInProgress;
}
class SyncQueue {
  int mOperationsMap;
  int mSyncStorageEngine;
  int TAG;
}
class SyncOperationTest {
}
class SyncOperation {
  int effectiveRunTime;
  int delayUntil;
  int backoff;
  int pendingOperation;
  int expedited;
  int earliestRunTime;
  int key;
  int extras;
  int allowParallelSyncs;
  int authority;
  int syncSource;
  int userId;
  int account;
}
class SyncManager {
  class SyncHandler {
    class SyncNotificationInfo {
      int startTime;
      int isActive;
    }
    int mReadyToRunLatch;
    int mWakeLocks;
    int mSyncTimeTracker;
    int mAlarmScheduleTime;
    int mSyncNotificationInfo;
    int MESSAGE_CANCEL;
    int MESSAGE_SERVICE_DISCONNECTED;
    int MESSAGE_SERVICE_CONNECTED;
    int MESSAGE_CHECK_ALARMS;
    int MESSAGE_SYNC_ALARM;
    int MESSAGE_SYNC_FINISHED;
  }
  class ServiceConnectionData {
    int syncAdapter;
    int activeSyncContext;
  }
  class SyncTimeTracker {
    int mTimeSpentSyncing;
    int mWhenSyncStarted;
    int mLastWasSyncing;
  }
  class AccountSyncStats {
    int times;
    int elapsedTime;
    int name;
  }
  class AuthoritySyncStats {
    int accountMap;
    int times;
    int elapsedTime;
    int name;
  }
  class ActiveSyncContext {
    int mIsLinkedToDeath;
    int mSyncInfo;
    int mSyncAdapterUid;
    int mSyncWakeLock;
    int mBound;
    int mTimeoutStartTime;
    int mStartTime;
    int mSyncAdapter;
    int mHistoryRowId;
    int mSyncOperation;
  }
  class SyncAlarmIntentReceiver {
  }
  class SyncHandlerMessagePayload {
    int syncResult;
    int activeSyncContext;
  }
  int mBootCompleted;
  int mSyncHandler;
  int ACTION_SYNC_ALARM;
  int mUserIntentReceiver;
  int mShutdownIntentReceiver;
  int mConnectivityIntentReceiver;
  int SYNC_ALARM_TIMEOUT_MAX;
  int SYNC_ALARM_TIMEOUT_MIN;
  int mSyncRandomOffsetMillis;
  int mPowerManager;
  int mBackgroundDataSettingChanged;
  int mBootCompletedReceiver;
  int mStorageIntentReceiver;
  int mSyncAdapters;
  int mConnManagerDoNotUseDirectly;
  int mSyncAlarmIntent;
  int mNeedSyncActiveNotification;
  int mActiveSyncContexts;
  int mSyncQueue;
  int mSyncStorageEngine;
  int mAlarmService;
  int mNotificationMgr;
  int mStorageIsLow;
  int mDataConnectionIsConnected;
  int mSyncManagerWakeLock;
  int mHandleAlarmWakeLock;
  int mAccounts;
  int INITIAL_ACCOUNTS_ARRAY;
  int mContext;
  int MAX_SIMULTANEOUS_INITIALIZATION_SYNCS;
  int MAX_SIMULTANEOUS_REGULAR_SYNCS;
  int SYNC_LOOP_WAKE_LOCK;
  int HANDLE_SYNC_ALARM_WAKE_LOCK;
  int SYNC_WAKE_LOCK_PREFIX;
  int INITIALIZATION_UNBIND_DELAY_MS;
  int DELAY_RETRY_SYNC_IN_PROGRESS_IN_SECONDS;
  int DEFAULT_MAX_SYNC_RETRY_TIME_IN_SECONDS;
  int INITIAL_SYNC_RETRY_TIME_IN_MS;
  int SYNC_NOTIFICATION_DELAY;
  int MAX_TIME_PER_SYNC;
  int LOCAL_SYNC_DELAY;
  int TAG;
}
class SyncInfo {
  int CREATOR;
  int startTime;
  int authority;
  int account;
  int authorityId;
}
class SyncContext {
  int HEARTBEAT_SEND_INTERVAL_IN_MS;
  int mLastHeartbeatSendTime;
  int mSyncContext;
}
class SyncAdaptersCache {
  class MySerializer {
  }
  int sSerializer;
  int ATTRIBUTES_NAME;
  int SERVICE_META_DATA;
  int SERVICE_INTERFACE;
  int TAG;
}
class SyncAdapterType {
  int CREATOR;
  int settingsActivity;
  int allowParallelSyncs;
  int isAlwaysSyncable;
  int supportsUploading;
  int userVisible;
  int isKey;
  int accountType;
  int authority;
}
class SyncActivityTooManyDeletes {
  int mProvider;
  int mAuthority;
  int mAccount;
  int mNumDeletes;
}
class SharedPreferences {
  class Editor {
  }
  class OnSharedPreferenceChangeListener {
  }
}
class ServiceConnection {
}
class SearchRecentSuggestionsProvider {
  class DatabaseHelper {
    int mNewVersion;
  }
  int mSuggestionProjection;
  int mSuggestSuggestionClause;
  int mUriMatcher;
  int mSuggestionsUri;
  int URI_MATCH_SUGGEST;
  int DATABASE_MODE_2LINES;
  int DATABASE_MODE_QUERIES;
  int DATABASE_VERSION;
  int NULL_COLUMN;
  int ORDER_BY;
  int sSuggestions;
  int sDatabaseName;
  int mOpenHelper;
  int mTwoLineDisplay;
  int mMode;
  int mAuthority;
  int TAG;
}
class ReceiverCallNotAllowedException {
}
class PeriodicSync {
  int CREATOR;
  int period;
  int extras;
  int authority;
  int account;
}
class OperationApplicationException {
  int mNumSuccessfulYieldPoints;
}
class ObserverNodeTest {
  class TestObserver {
  }
}
class MutableContextWrapper {
}
class MemoryFileProviderTest {
}
class MemoryFileProvider {
  class DatabaseHelper {
    int DATABASE_VERSION;
    int DATABASE_NAME;
  }
  int sURLMatcher;
  int FILE;
  int HUGE;
  int DATA_ID_BLOB;
  int mOpenHelper;
  int TEST_BLOB;
  int DATA_FILE;
  int TAG;
}
class Loader {
  class OnLoadCanceledListener {
  }
  class OnLoadCompleteListener {
  }
  class ForceLoadContentObserver {
  }
  int mContentChanged;
  int mReset;
  int mAbandoned;
  int mStarted;
  int mContext;
  int mOnLoadCanceledListener;
  int mListener;
  int mId;
}
class IntentSender {
  int CREATOR;
  class FinishedDispatcher {
    int mResultExtras;
    int mResultData;
    int mResultCode;
    int mIntent;
    int mHandler;
    int mWho;
    int mIntentSender;
  }
  class OnFinished {
  }
  class SendIntentException {
  }
  int mTarget;
}
class IntentFilter {
  int CREATOR;
  class AuthorityEntry {
    int mPort;
    int mWild;
    int mHost;
    int mOrigHost;
  }
  class MalformedMimeTypeException {
  }
  int mHasPartialTypes;
  int mDataTypes;
  int mDataPaths;
  int mDataAuthorities;
  int mDataSchemes;
  int mCategories;
  int mActions;
  int mPriority;
  int NO_MATCH_CATEGORY;
  int NO_MATCH_ACTION;
  int NO_MATCH_DATA;
  int NO_MATCH_TYPE;
  int MATCH_CATEGORY_TYPE;
  int MATCH_CATEGORY_PATH;
  int MATCH_CATEGORY_PORT;
  int MATCH_CATEGORY_HOST;
  int MATCH_CATEGORY_SCHEME;
  int MATCH_CATEGORY_EMPTY;
  int MATCH_ADJUSTMENT_NORMAL;
  int MATCH_ADJUSTMENT_MASK;
  int MATCH_CATEGORY_MASK;
  int SYSTEM_LOW_PRIORITY;
  int SYSTEM_HIGH_PRIORITY;
  int ACTION_STR;
  int NAME_STR;
  int CAT_STR;
  int TYPE_STR;
  int SCHEME_STR;
  int AUTH_STR;
  int HOST_STR;
  int PORT_STR;
  int PATH_STR;
  int LITERAL_STR;
  int PREFIX_STR;
  int SGLOB_STR;
}
class Intent {
  int CREATOR;
  class FilterComparison {
    int mHashCode;
    int mIntent;
  }
  int FILL_IN_CLIP_DATA;
  int FILL_IN_SELECTOR;
  int FILL_IN_SOURCE_BOUNDS;
  int FILL_IN_PACKAGE;
  int FILL_IN_COMPONENT;
  int FILL_IN_CATEGORIES;
  int FILL_IN_DATA;
  int FILL_IN_ACTION;
  int mClipData;
  int mSelector;
  int mSourceBounds;
  int mExtras;
  int mCategories;
  int mFlags;
  int mComponent;
  int mPackage;
  int mType;
  int mData;
  int mAction;
  int URI_INTENT_SCHEME;
  int IMMUTABLE_FLAGS;
  int FLAG_RECEIVER_BOOT_UPGRADE;
  int FLAG_RECEIVER_REGISTERED_ONLY_BEFORE_BOOT;
  int FLAG_RECEIVER_FOREGROUND;
  int FLAG_RECEIVER_REPLACE_PENDING;
  int FLAG_RECEIVER_REGISTERED_ONLY;
  int FLAG_ACTIVITY_TASK_ON_HOME;
  int FLAG_ACTIVITY_CLEAR_TASK;
  int FLAG_ACTIVITY_NO_ANIMATION;
  int FLAG_ACTIVITY_REORDER_TO_FRONT;
  int FLAG_ACTIVITY_NO_USER_ACTION;
  int FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET;
  int FLAG_ACTIVITY_LAUNCHED_FROM_HISTORY;
  int FLAG_ACTIVITY_RESET_TASK_IF_NEEDED;
  int FLAG_ACTIVITY_BROUGHT_TO_FRONT;
  int FLAG_ACTIVITY_EXCLUDE_FROM_RECENTS;
  int FLAG_ACTIVITY_PREVIOUS_IS_TOP;
  int FLAG_ACTIVITY_FORWARD_RESULT;
  int FLAG_ACTIVITY_CLEAR_TOP;
  int FLAG_ACTIVITY_MULTIPLE_TASK;
  int FLAG_ACTIVITY_NEW_TASK;
  int FLAG_ACTIVITY_SINGLE_TOP;
  int FLAG_ACTIVITY_NO_HISTORY;
  int FLAG_INCLUDE_STOPPED_PACKAGES;
  int FLAG_EXCLUDE_STOPPED_PACKAGES;
  int FLAG_DEBUG_LOG_RESOLUTION;
  int FLAG_FROM_BACKGROUND;
  int FLAG_GRANT_WRITE_URI_PERMISSION;
  int FLAG_GRANT_READ_URI_PERMISSION;
  int EXTRA_USERID;
  int EXTRA_LOCAL_ONLY;
  int EXTRA_CLIENT_INTENT;
  int EXTRA_CLIENT_LABEL;
  int EXTRA_CHANGED_UID_LIST;
  int EXTRA_CHANGED_PACKAGE_LIST;
  int EXTRA_CHANGED_COMPONENT_NAME_LIST;
  int EXTRA_CHANGED_COMPONENT_NAME;
  int EXTRA_REMOTE_INTENT_TOKEN;
  int EXTRA_BUG_REPORT;
  int METADATA_DOCK_HOME;
  int EXTRA_DOCK_STATE_HE_DESK;
  int EXTRA_DOCK_STATE_LE_DESK;
  int EXTRA_DOCK_STATE_CAR;
  int EXTRA_DOCK_STATE_DESK;
  int EXTRA_DOCK_STATE_UNDOCKED;
  int EXTRA_DOCK_STATE;
  int EXTRA_ALARM_COUNT;
  int EXTRA_REPLACING;
  int EXTRA_DATA_REMOVED;
  int EXTRA_PACKAGES;
  int EXTRA_UID;
  int EXTRA_PHONE_NUMBER;
  int EXTRA_DONT_KILL_APP;
  int EXTRA_KEY_CONFIRM;
  int EXTRA_KEY_EVENT;
  int EXTRA_INITIAL_INTENTS;
  int EXTRA_TITLE;
  int EXTRA_INTENT;
  int EXTRA_SUBJECT;
  int EXTRA_BCC;
  int EXTRA_CC;
  int EXTRA_EMAIL;
  int EXTRA_STREAM;
  int EXTRA_HTML_TEXT;
  int EXTRA_TEXT;
  int EXTRA_TEMPLATE;
  int CATEGORY_APP_MUSIC;
  int CATEGORY_APP_MESSAGING;
  int CATEGORY_APP_MAPS;
  int CATEGORY_APP_GALLERY;
  int CATEGORY_APP_EMAIL;
  int CATEGORY_APP_CONTACTS;
  int CATEGORY_APP_CALENDAR;
  int CATEGORY_APP_CALCULATOR;
  int CATEGORY_APP_BROWSER;
  int CATEGORY_CAR_MODE;
  int CATEGORY_HE_DESK_DOCK;
  int CATEGORY_LE_DESK_DOCK;
  int CATEGORY_DESK_DOCK;
  int CATEGORY_CAR_DOCK;
  int CATEGORY_FRAMEWORK_INSTRUMENTATION_TEST;
  int CATEGORY_OPENABLE;
  int CATEGORY_SAMPLE_CODE;
  int CATEGORY_UNIT_TEST;
  int CATEGORY_TEST;
  int CATEGORY_MONKEY;
  int CATEGORY_APP_MARKET;
  int CATEGORY_EMBED;
  int CATEGORY_DEVELOPMENT_PREFERENCE;
  int CATEGORY_PREFERENCE;
  int CATEGORY_HOME;
  int CATEGORY_INFO;
  int CATEGORY_LAUNCHER;
  int CATEGORY_TAB;
  int CATEGORY_SELECTED_ALTERNATIVE;
  int CATEGORY_ALTERNATIVE;
  int CATEGORY_BROWSABLE;
  int CATEGORY_DEFAULT;
  int ACTION_USER_SWITCHED;
  int ACTION_USER_REMOVED;
  int ACTION_USER_ADDED;
  int ACTION_PRE_BOOT_COMPLETED;
  int ACTION_REMOTE_INTENT;
  int ACTION_DOCK_EVENT;
  int ACTION_REBOOT;
  int ACTION_NEW_OUTGOING_CALL;
  int ACTION_ADVANCED_SETTINGS_CHANGED;
  int ACTION_USB_AUDIO_DEVICE_PLUG;
  int ACTION_USB_AUDIO_ACCESSORY_PLUG;
  int ACTION_HDMI_AUDIO_PLUG;
  int ACTION_DIGITAL_AUDIO_DOCK_PLUG;
  int ACTION_ANALOG_AUDIO_DOCK_PLUG;
  int ACTION_HEADSET_PLUG;
  int ACTION_PROVIDER_CHANGED;
  int ACTION_AIRPLANE_MODE_CHANGED;
  int ACTION_INPUT_METHOD_CHANGED;
  int ACTION_GTALK_SERVICE_DISCONNECTED;
  int ACTION_GTALK_SERVICE_CONNECTED;
  int ACTION_CAMERA_BUTTON;
  int ACTION_MEDIA_BUTTON;
  int ACTION_MEDIA_SCANNER_SCAN_FILE;
  int ACTION_MEDIA_SCANNER_FINISHED;
  int ACTION_MEDIA_SCANNER_STARTED;
  int ACTION_MEDIA_EJECT;
  int ACTION_MEDIA_UNMOUNTABLE;
  int ACTION_MEDIA_BAD_REMOVAL;
  int ACTION_MEDIA_UNSHARED;
  int ACTION_MEDIA_SHARED;
  int ACTION_MEDIA_MOUNTED;
  int ACTION_MEDIA_NOFS;
  int ACTION_MEDIA_CHECKING;
  int ACTION_MEDIA_UNMOUNTED;
  int ACTION_MEDIA_REMOVED;
  int ACTION_UMS_DISCONNECTED;
  int ACTION_UMS_CONNECTED;
  int ACTION_MANAGE_PACKAGE_STORAGE;
  int ACTION_DEVICE_STORAGE_NOT_FULL;
  int ACTION_DEVICE_STORAGE_FULL;
  int ACTION_DEVICE_STORAGE_OK;
  int ACTION_DEVICE_STORAGE_LOW;
  int ACTION_REQUEST_SHUTDOWN;
  int ACTION_SHUTDOWN;
  int ACTION_POWER_DISCONNECTED;
  int ACTION_POWER_CONNECTED;
  int ACTION_BATTERY_OKAY;
  int ACTION_BATTERY_LOW;
  int ACTION_BATTERY_CHANGED;
  int ACTION_LOCALE_CHANGED;
  int ACTION_CONFIGURATION_CHANGED;
  int ACTION_WALLPAPER_CHANGED;
  int ACTION_EXTERNAL_APPLICATIONS_UNAVAILABLE;
  int ACTION_EXTERNAL_APPLICATIONS_AVAILABLE;
  int ACTION_PACKAGE_NEEDS_VERIFICATION;
  int ACTION_PACKAGE_FIRST_LAUNCH;
  int ACTION_UID_REMOVED;
  int ACTION_PACKAGE_DATA_CLEARED;
  int ACTION_PACKAGE_RESTARTED;
  int ACTION_QUERY_PACKAGE_RESTART;
  int ACTION_PACKAGE_CHANGED;
  int ACTION_PACKAGE_FULLY_REMOVED;
  int ACTION_PACKAGE_REMOVED;
  int ACTION_MY_PACKAGE_REPLACED;
  int ACTION_PACKAGE_REPLACED;
  int ACTION_PACKAGE_ADDED;
  int ACTION_PACKAGE_INSTALL;
  int ACTION_CLOSE_SYSTEM_DIALOGS;
  int ACTION_BOOT_COMPLETED;
  int ACTION_SYNC_STATE_CHANGED;
  int ACTION_ALARM_CHANGED;
  int ACTION_CLEAR_DNS_CACHE;
  int ACTION_TIMEZONE_CHANGED;
  int ACTION_DATE_CHANGED;
  int ACTION_TIME_CHANGED;
  int ACTION_TIME_TICK;
  int ACTION_USER_PRESENT;
  int ACTION_SCREEN_ON;
  int ACTION_SCREEN_OFF;
  int METADATA_SETUP_VERSION;
  int ACTION_UNINSTALL_PACKAGE;
  int EXTRA_INSTALL_RESULT;
  int EXTRA_RETURN_RESULT;
  int EXTRA_ALLOW_REPLACE;
  int EXTRA_NOT_UNKNOWN_SOURCE;
  int EXTRA_INSTALLER_PACKAGE_NAME;
  int ACTION_INSTALL_PACKAGE;
  int ACTION_MANAGE_NETWORK_USAGE;
  int ACTION_UPGRADE_SETUP;
  int ACTION_POWER_USAGE_SUMMARY;
  int ACTION_APP_ERROR;
  int ACTION_SEARCH_LONG_PRESS;
  int ACTION_VOICE_COMMAND;
  int ACTION_CALL_BUTTON;
  int ACTION_FACTORY_TEST;
  int ACTION_BUG_REPORT;
  int ACTION_SET_WALLPAPER;
  int ACTION_ALL_APPS;
  int ACTION_ASSIST;
  int ACTION_WEB_SEARCH;
  int ACTION_SYSTEM_TUTORIAL;
  int ACTION_SEARCH;
  int ACTION_PICK_ACTIVITY;
  int ACTION_SYNC;
  int ACTION_RUN;
  int ACTION_DELETE;
  int ACTION_PASTE;
  int ACTION_INSERT;
  int ACTION_ANSWER;
  int ACTION_SEND_MULTIPLE;
  int ACTION_SEND;
  int ACTION_SENDTO;
  int ACTION_CALL_PRIVILEGED;
  int ACTION_CALL_EMERGENCY;
  int ACTION_CALL;
  int ACTION_DIAL;
  int ACTION_GET_CONTENT;
  int ACTION_CHOOSER;
  class ShortcutIconResource {
    int CREATOR;
    int resourceName;
    int packageName;
  }
  int EXTRA_SHORTCUT_ICON_RESOURCE;
  int EXTRA_SHORTCUT_ICON;
  int EXTRA_SHORTCUT_NAME;
  int EXTRA_SHORTCUT_INTENT;
  int ACTION_CREATE_SHORTCUT;
  int ACTION_PICK;
  int ACTION_INSERT_OR_EDIT;
  int ACTION_EDIT;
  int ACTION_ATTACH_DATA;
  int ACTION_DEFAULT;
  int ACTION_VIEW;
  int ACTION_MAIN;
}
class IContentProvider {
  int CREATE_CANCELATION_SIGNAL_TRANSACTION;
  int OPEN_TYPED_ASSET_FILE_TRANSACTION;
  int GET_STREAM_TYPES_TRANSACTION;
  int CALL_TRANSACTION;
  int APPLY_BATCH_TRANSACTION;
  int OPEN_ASSET_FILE_TRANSACTION;
  int OPEN_FILE_TRANSACTION;
  int BULK_INSERT_TRANSACTION;
  int UPDATE_TRANSACTION;
  int DELETE_TRANSACTION;
  int INSERT_TRANSACTION;
  int GET_TYPE_TRANSACTION;
  int QUERY_TRANSACTION;
  int descriptor;
}
class EntityIterator {
}
class Entity {
  class NamedContentValues {
    int values;
    int uri;
  }
  int mSubValues;
  int mValues;
}
class DialogInterface {
  class OnKeyListener {
  }
  class OnMultiChoiceClickListener {
  }
  class OnClickListener {
  }
  class OnShowListener {
  }
  class OnDismissListener {
  }
  class OnCancelListener {
  }
  int BUTTON3;
  int BUTTON2;
  int BUTTON1;
  int BUTTON_NEUTRAL;
  int BUTTON_NEGATIVE;
  int BUTTON_POSITIVE;
}
class DefaultDataHandler {
  int mContentResolver;
  int mValues;
  int mUris;
  int ARG;
  int SELECT;
  int DEL;
  int POSTFIX;
  int URI_STR;
  int COL;
  int ROW;
}
class CursorLoader {
  int mCancellationSignal;
  int mCursor;
  int mSortOrder;
  int mSelectionArgs;
  int mSelection;
  int mProjection;
  int mUri;
  int mObserver;
}
class CursorEntityIterator {
  int mIsClosed;
  int mCursor;
}
class ContextWrapper {
  int mBase;
}
class Context {
  int CONTEXT_RESTRICTED;
  int CONTEXT_IGNORE_SECURITY;
  int CONTEXT_INCLUDE_CODE;
  int SCHEDULING_POLICY_SERVICE;
  int INPUT_SERVICE;
  int SERIAL_SERVICE;
  int USB_SERVICE;
  int SIP_SERVICE;
  int NFC_SERVICE;
  int DOWNLOAD_SERVICE;
  int UI_MODE_SERVICE;
  int DEVICE_POLICY_SERVICE;
  int DROPBOX_SERVICE;
  int BACKUP_SERVICE;
  int APPWIDGET_SERVICE;
  int TEXT_SERVICES_MANAGER_SERVICE;
  int INPUT_METHOD_SERVICE;
  int CLIPBOARD_SERVICE;
  int TELEPHONY_SERVICE;
  int MEDIA_ROUTER_SERVICE;
  int AUDIO_SERVICE;
  int NSD_SERVICE;
  int WIFI_P2P_SERVICE;
  int WIFI_SERVICE;
  int NETWORK_POLICY_SERVICE;
  int NETWORK_STATS_SERVICE;
  int NETWORKMANAGEMENT_SERVICE;
  int UPDATE_LOCK_SERVICE;
  int THROTTLE_SERVICE;
  int CONNECTIVITY_SERVICE;
  int STATUS_BAR_SERVICE;
  int VIBRATOR_SERVICE;
  int WALLPAPER_SERVICE;
  int STORAGE_SERVICE;
  int SENSOR_SERVICE;
  int SEARCH_SERVICE;
  int COUNTRY_DETECTOR;
  int LOCATION_SERVICE;
  int KEYGUARD_SERVICE;
  int ACCESSIBILITY_SERVICE;
  int NOTIFICATION_SERVICE;
  int ALARM_SERVICE;
  int ACTIVITY_SERVICE;
  int ACCOUNT_SERVICE;
  int LAYOUT_INFLATER_SERVICE;
  int WINDOW_SERVICE;
  int POWER_SERVICE;
  int BIND_NOT_VISIBLE;
  int BIND_ADJUST_WITH_ACTIVITY;
  int BIND_IMPORTANT;
  int BIND_WAIVE_PRIORITY;
  int BIND_ALLOW_OOM_MANAGEMENT;
  int BIND_ABOVE_CLIENT;
  int BIND_NOT_FOREGROUND;
  int BIND_DEBUG_UNBIND;
  int BIND_AUTO_CREATE;
  int MODE_ENABLE_WRITE_AHEAD_LOGGING;
  int MODE_MULTI_PROCESS;
  int MODE_APPEND;
  int MODE_WORLD_WRITEABLE;
  int MODE_WORLD_READABLE;
  int MODE_PRIVATE;
}
class ContentValues {
  int CREATOR;
  int mValues;
  int TAG;
}
class ContentUris {
}
class ContentTests {
}
class ContentService {
  class ObserverNode {
    int mObservers;
    int mChildren;
    int mName;
    int DELETE_TYPE;
    int UPDATE_TYPE;
    int INSERT_TYPE;
    class ObserverEntry {
      int observersLock;
      int notifyForDescendents;
      int pid;
      int uid;
      int observer;
    }
  }
  class ObserverCall {
    int mSelfChange;
    int mObserver;
    int mNode;
  }
  int mSyncManagerLock;
  int mSyncManager;
  int mRootNode;
  int mFactoryTest;
  int mContext;
  int TAG;
}
class ContentResolverTest {
  int mContentResolver;
}
class ContentResolver {
  int TAG;
  int mContext;
  int sContentService;
  int CONTENT_SERVICE_NAME;
  class ParcelFileDescriptorInner {
    int mReleaseProviderFlag;
    int mContentProvider;
  }
  class CursorWrapperInner {
    int mProviderReleased;
    int mCloseGuard;
    int TAG;
    int mContentProvider;
  }
  class OpenResourceIdResult {
    int id;
    int r;
  }
  int mRandom;
  int SLOW_THRESHOLD_MILLIS;
  int SYNC_OBSERVER_TYPE_ALL;
  int SYNC_OBSERVER_TYPE_STATUS;
  int SYNC_OBSERVER_TYPE_ACTIVE;
  int SYNC_OBSERVER_TYPE_PENDING;
  int SYNC_OBSERVER_TYPE_SETTINGS;
  int SYNC_ERROR_INTERNAL;
  int SYNC_ERROR_TOO_MANY_RETRIES;
  int SYNC_ERROR_TOO_MANY_DELETIONS;
  int SYNC_ERROR_CONFLICT;
  int SYNC_ERROR_PARSE;
  int SYNC_ERROR_IO;
  int SYNC_ERROR_AUTHENTICATION;
  int SYNC_ERROR_SYNC_ALREADY_IN_PROGRESS;
  int CURSOR_DIR_BASE_TYPE;
  int CURSOR_ITEM_BASE_TYPE;
  int SCHEME_FILE;
  int SCHEME_ANDROID_RESOURCE;
  int SCHEME_CONTENT;
  int SYNC_EXTRAS_INITIALIZE;
  int SYNC_EXTRAS_DISCARD_LOCAL_DELETIONS;
  int SYNC_EXTRAS_OVERRIDE_TOO_MANY_DELETIONS;
  int SYNC_EXTRAS_UPLOAD;
  int SYNC_EXTRAS_MANUAL;
  int SYNC_EXTRAS_DO_NOT_RETRY;
  int SYNC_EXTRAS_IGNORE_BACKOFF;
  int SYNC_EXTRAS_IGNORE_SETTINGS;
  int SYNC_EXTRAS_FORCE;
  int SYNC_EXTRAS_EXPEDITED;
  int SYNC_EXTRAS_ACCOUNT;
}
class ContentQueryMapTest {
  class LooperThread {
    int mSuccess;
    int mError;
  }
}
class ContentQueryMap {
  int mDirty;
  int mContentObserver;
  int mValues;
  int mKeepUpdated;
  int mHandlerForUpdateNotifications;
  int mKeyColumn;
  int mColumnNames;
  int mCursor;
}
class ContentProviderResult {
  int CREATOR;
  int count;
  int uri;
}
class ContentProviderOperationTest {
  class TestContentProvider {
  }
  int CLASS_OPERATION;
  int CLASS_BUILDER;
  int sTestValues1;
  int sTestUri1;
}
class ContentProviderOperation {
  class Builder {
    int mYieldAllowed;
    int mSelectionArgsBackReferences;
    int mValuesBackReferences;
    int mExpectedCount;
    int mValues;
    int mSelectionArgs;
    int mSelection;
    int mUri;
    int mType;
  }
  int CREATOR;
  int TAG;
  int mYieldAllowed;
  int mSelectionArgsBackReferences;
  int mValuesBackReferences;
  int mExpectedCount;
  int mValues;
  int mSelectionArgs;
  int mSelection;
  int mUri;
  int mType;
  int TYPE_ASSERT;
  int TYPE_DELETE;
  int TYPE_UPDATE;
  int TYPE_INSERT;
}
class ContentProviderProxy {
  int mRemote;
}
class ContentProviderNative {
}
class ContentProviderClient {
  int mReleased;
  int mStable;
  int mContentResolver;
  int mContentProvider;
}
class ContentProvider {
  class PipeDataWriter {
  }
  class Transport {
  }
  int mTransport;
  int mExported;
  int mPathPermissions;
  int mWritePermission;
  int mReadPermission;
  int mMyUid;
  int mContext;
  int TAG;
}
class ContentInsertHandler {
}
class ComponentName {
  int CREATOR;
  int mClass;
  int mPackage;
}
class ComponentCallbacks2 {
  int TRIM_MEMORY_RUNNING_MODERATE;
  int TRIM_MEMORY_RUNNING_LOW;
  int TRIM_MEMORY_RUNNING_CRITICAL;
  int TRIM_MEMORY_UI_HIDDEN;
  int TRIM_MEMORY_BACKGROUND;
  int TRIM_MEMORY_MODERATE;
  int TRIM_MEMORY_COMPLETE;
}
class ComponentCallbacks {
}
class ClipboardManager {
  class OnPrimaryClipChangedListener {
  }
  int mHandler;
  int MSG_REPORT_PRIMARY_CLIP_CHANGED;
  int mPrimaryClipChangedServiceListener;
  int mPrimaryClipChangedListeners;
  int mContext;
  int sService;
  int sStaticLock;
}
class ClipDescription {
  int CREATOR;
  int mMimeTypes;
  int mLabel;
  int MIMETYPE_TEXT_INTENT;
  int MIMETYPE_TEXT_URILIST;
  int MIMETYPE_TEXT_HTML;
  int MIMETYPE_TEXT_PLAIN;
}
class ClipData {
  int CREATOR;
  class Item {
    int mUri;
    int mIntent;
    int mHtmlText;
    int mText;
  }
  int mItems;
  int mIcon;
  int mClipDescription;
  int MIMETYPES_TEXT_INTENT;
  int MIMETYPES_TEXT_URILIST;
  int MIMETYPES_TEXT_HTML;
  int MIMETYPES_TEXT_PLAIN;
}
class BroadcastReceiver {
  class PendingResult {
    int mFinished;
    int mAbortBroadcast;
    int mResultExtras;
    int mResultData;
    int mResultCode;
    int mToken;
    int mInitialStickyHint;
    int mOrderedHint;
    int mType;
    int TYPE_UNREGISTERED;
    int TYPE_REGISTERED;
    int TYPE_COMPONENT;
  }
  int mDebugUnregister;
  int mPendingResult;
}
class BrickDeniedTest {
}
class AsyncTaskLoader {
  int mHandler;
  int mLastLoadCompleteTime;
  int mUpdateThrottle;
  int mCancellingTask;
  int mTask;
  class LoadTask {
    int waiting;
    int mDone;
  }
  int DEBUG;
  int TAG;
}
class AsyncQueryHandler {
  class WorkerHandler {
  }
  class WorkerArgs {
    int values;
    int cookie;
    int result;
    int orderBy;
    int selectionArgs;
    int selection;
    int projection;
    int handler;
    int uri;
  }
  int mWorkerThreadHandler;
  int sLooper;
  int mResolver;
  int EVENT_ARG_DELETE;
  int EVENT_ARG_UPDATE;
  int EVENT_ARG_INSERT;
  int EVENT_ARG_QUERY;
  int localLOGV;
  int TAG;
}
class AssetTest {
  int mAssets;
}
class ActivityNotFoundException {
}
class AbstractThreadedSyncAdapter {
  class SyncThread {
    int mThreadsKey;
    int mExtras;
    int mAccount;
    int mAuthority;
    int mSyncContext;
  }
  class ISyncAdapterImpl {
  }
  int mAllowParallelSyncs;
  int mAutoInitialize;
  int mSyncThreadLock;
  int mSyncThreads;
  int mISyncAdapterImpl;
  int mNumSyncStarts;
  int mContext;
  int LOG_SYNC_DETAILS;
}
