package com.android.server;
class WiredAccessoryObserver {
  int mHandler;
  class BootCompletedReceiver {
  }
  int mAudioManager;
  int mWakeLock;
  int mContext;
  int mHeadsetName;
  int mPrevHeadsetState;
  int mHeadsetState;
  int uEventInfo;
  class UEventInfo {
    int mState2Bits;
    int mState1Bits;
    int mDevName;
  }
  int HEADSETS_WITH_MIC;
  int SUPPORTED_HEADSETS;
  int BIT_HDMI_AUDIO;
  int BIT_USB_HEADSET_DGTL;
  int BIT_USB_HEADSET_ANLG;
  int BIT_HEADSET_NO_MIC;
  int BIT_HEADSET;
  int LOG;
  int TAG;
}
class WifiService {
  class NotificationEnabledSettingObserver {
  }
  class Multicaster {
  }
  class DeathRecipient {
    int mWorkSource;
    int mBinder;
    int mMode;
    int mTag;
  }
  class LockList {
    int mList;
  }
  class WifiLock {
  }
  int mReceiver;
  int mWifiWatchdogStateMachine;
  int mTmpWorkSource;
  int mWifiStateMachineHandler;
  class WifiStateMachineHandler {
    int mWsmChannel;
  }
  int mAsyncServiceHandler;
  class AsyncServiceHandler {
  }
  int mClients;
  int mWifiStateMachineChannel;
  int mNumScansSinceNetworkStateChange;
  int NUM_SCANS_BEFORE_ACTUALLY_SCANNING;
  int mNotificationShown;
  int mNotification;
  int mNotificationRepeatTime;
  int mNotificationEnabledSettingObserver;
  int mNotificationEnabled;
  int NOTIFICATION_REPEAT_DELAY_MS;
  int ICON_NETWORKS_AVAILABLE;
  int mNetworkInfo;
  int mIsReceiverRegistered;
  int mWifiEnabled;
  int mAirplaneModeOn;
  int mPersistWifiState;
  int WIFI_DISABLED_AIRPLANE_ON;
  int WIFI_ENABLED_AIRPLANE_OVERRIDE;
  int WIFI_ENABLED;
  int WIFI_DISABLED;
  int ACTION_DEVICE_IDLE;
  int DEFAULT_IDLE_MS;
  int POLL_TRAFFIC_STATS_INTERVAL_MSECS;
  int mInterfaceName;
  int mDataActivity;
  int mRxPkts;
  int mTxPkts;
  int mTrafficStatsPollToken;
  int mEnableTrafficStatsPoll;
  int mBatteryStats;
  int mMulticastDisabled;
  int mMulticastEnabled;
  int mMulticasters;
  int mScanCount;
  int mScanLocksReleased;
  int mScanLocksAcquired;
  int mFullLocksReleased;
  int mFullLocksAcquired;
  int mFullHighPerfLocksReleased;
  int mFullHighPerfLocksAcquired;
  int mLocks;
  int mPluggedType;
  int mEmergencyCallbackMode;
  int mDeviceIdle;
  int mScreenOff;
  int IDLE_REQUEST;
  int mIdleIntent;
  int mAlarmManager;
  int mContext;
  int mWifiStateMachine;
  int DBG;
  int TAG;
}
class Watchdog {
  class Monitor {
  }
  class RebootRequestReceiver {
  }
  class RebootReceiver {
  }
  class HeartbeatHandler {
  }
  int mReqRecheckInterval;
  int mReqMinNextAlarm;
  int mReqMinScreenOff;
  int mReqRebootWindow;
  int mReqRebootStartTime;
  int mReqRebootInterval;
  int mReqRebootNoWait;
  int mRebootInterval;
  int mBootTime;
  int mRebootIntent;
  int mCheckupIntent;
  int mNeedScheduledCheck;
  int mMinAlarm;
  int mMinScreenOff;
  int mCalendar;
  int mPhonePid;
  int mCurrentMonitor;
  int mForceKillSystem;
  int mCompleted;
  int mActivity;
  int mAlarm;
  int mPower;
  int mBattery;
  int mResolver;
  int mMonitors;
  int mHandler;
  int sWatchdog;
  int NATIVE_STACKS_OF_INTEREST;
  int REBOOT_ACTION;
  int REBOOT_DEFAULT_WINDOW;
  int REBOOT_DEFAULT_START_TIME;
  int REBOOT_DEFAULT_INTERVAL;
  int MEMCHECK_DEFAULT_RECHECK_INTERVAL;
  int MEMCHECK_DEFAULT_MIN_ALARM;
  int MEMCHECK_DEFAULT_MIN_SCREEN_OFF;
  int TIME_TO_WAIT;
  int TIME_TO_RESTART;
  int MONITOR;
  int RECORD_KERNEL_THREADS;
  int DB;
  int localLOGV;
  int TAG;
}
class WallpaperManagerService {
  class MyPackageMonitor {
  }
  class WallpaperConnection {
    int mWallpaper;
    int mEngine;
    int mService;
    int mToken;
    int mInfo;
  }
  class WallpaperData {
    int height;
    int width;
    int callbacks;
    int wallpaperObserver;
    int wallpaperUpdating;
    int lastDiedTime;
    int connection;
    int imageWallpaperComponent;
    int nextWallpaperComponent;
    int wallpaperComponent;
    int name;
    int imageWallpaperPending;
    int wallpaperFile;
    int userId;
  }
  int mCurrentUserId;
  int mWallpaperMap;
  int mLastWallpaper;
  int mMonitor;
  int mIWindowManager;
  int mContext;
  class WallpaperObserver {
    int mWallpaperFile;
    int mWallpaperDir;
    int mWallpaper;
  }
  int WALLPAPER_INFO;
  int WALLPAPER;
  int WALLPAPER_BASE_DIR;
  int MIN_WALLPAPER_CRASH_TIME;
  int mLock;
  int DEBUG;
  int TAG;
}
class VibratorService {
  int mIntentReceiver;
  class VibrateThread {
    int mDone;
    int mVibration;
  }
  int mVibrationRunnable;
  class Vibration {
    int mUid;
    int mRepeat;
    int mPattern;
    int mStartTime;
    int mTimeout;
    int mToken;
  }
  int mInputDeviceListenerRegistered;
  int mVibrateInputDevicesSetting;
  int mInputDeviceVibrators;
  int mThread;
  int mIm;
  int mWakeLock;
  int mContext;
  int mH;
  int mTmpWorkSource;
  int mCurrentVibration;
  int mVibrations;
  int TAG;
}
class UpdateLockService {
  class LockWatcher {
  }
  int mLocks;
  int mContext;
  int PERMISSION;
  int TAG;
  int DEBUG;
}
class UiModeManagerService {
  int mHandler;
  int mLocationListener;
  int mEmptyLocationListener;
  int mUpdateLocationReceiver;
  int mBatteryReceiver;
  int mDockModeReceiver;
  int mTwilightUpdateReceiver;
  int mResultReceiver;
  int mWakeLock;
  int mStatusBarManager;
  int mLocation;
  int mLocationManager;
  int mAlarmManager;
  int mNotificationManager;
  int mSystemReady;
  int mConfiguration;
  int mHoldingConfiguration;
  int mSetUiMode;
  int mCurUiMode;
  int mComputedNightMode;
  int mTelevision;
  int mDeskModeKeepsScreenOn;
  int mCarModeKeepsScreenOn;
  int mDefaultUiModeType;
  int mCharging;
  int mCarModeEnabled;
  int mNightMode;
  int mLastBroadcastState;
  int mDockState;
  int mLock;
  int mContext;
  int ACTION_UPDATE_NIGHT_MODE;
  int FACTOR_GMT_OFFSET_LONGITUDE;
  int LOCATION_UPDATE_ENABLE_INTERVAL_MAX;
  int LOCATION_UPDATE_ENABLE_INTERVAL_MIN;
  int LOCATION_UPDATE_DISTANCE_METER;
  int MIN_LOCATION_UPDATE_MS;
  int LOCATION_UPDATE_MS;
  int MSG_GET_NEW_LOCATION_UPDATE;
  int MSG_ENABLE_LOCATION_UPDATES;
  int MSG_UPDATE_TWILIGHT;
  int ENABLE_LAUNCH_DESK_DOCK_APP;
  int ENABLE_LAUNCH_CAR_DOCK_APP;
  int KEY_LAST_UPDATE_INTERVAL;
  int LOG;
  int TAG;
}
class TwilightCalculator {
  int mState;
  int mSunrise;
  int mSunset;
  int UTC_2000;
  int OBLIQUITY;
  int C3;
  int C2;
  int C1;
  int ALTIDUTE_CORRECTION_CIVIL_TWILIGHT;
  int J0;
  int DEGREES_TO_RADIANS;
  int NIGHT;
  int DAY;
}
class ThrottleServiceTest {
  int mThrottleService;
  int mMockTime;
  int mMockNMService;
  int mWatchingContext;
  int TEST_IFACE;
  int TEST_RESET_DAY;
  int TEST_KBITPS;
  int MB_IN_BYTES;
  int TAG;
}
class ThrottleService {
  class DataRecorder {
    int DATA_FILE_VERSION;
    int MAX_SIMS_SUPPORTED;
    int mTelephonyManager;
    int mImsi;
    int mContext;
    int mParent;
    int mPeriodEnd;
    int mPeriodStart;
    int mPeriodCount;
    int mCurrentPeriod;
    int mPeriodTxData;
    int mPeriodRxData;
  }
  class MyHandler {
  }
  int EVENT_IFACE_UP;
  int EVENT_RESET_ALARM;
  int EVENT_POLL_ALARM;
  int EVENT_POLICY_CHANGED;
  int EVENT_REBOOT_RECOVERY;
  class SettingsObserver {
    int mHandler;
    int mMsg;
  }
  class InterfaceObserver {
    int mIface;
    int mHandler;
    int mMsg;
  }
  int mTime;
  int mPollStickyBroadcast;
  int THROTTLE_INDEX_UNTHROTTLED;
  int THROTTLE_INDEX_UNINITIALIZED;
  int mThrottleIndex;
  int mSettingsObserver;
  int mInterfaceObserver;
  int mWarningNotificationSent;
  int mThrottlingNotification;
  int NOTIFICATION_WARNING;
  int mIface;
  int mRecorder;
  int mNotificationManager;
  int mAlarmManager;
  int mNMService;
  int mPendingResetIntent;
  int RESET_REQUEST;
  int ACTION_RESET;
  int mPendingPollIntent;
  int POLL_REQUEST;
  int ACTION_POLL;
  int mLastWrite;
  int mLastRead;
  int mPolicyNotificationsAllowedMask;
  int mPolicyResetDay;
  int mPolicyThrottleValue;
  int mPolicyThreshold;
  int mPolicyPollPeriodSec;
  int mMaxNtpCacheAge;
  int MAX_NTP_CACHE_AGE;
  int TESTING_THRESHOLD;
  int TESTING_RESET_PERIOD_SEC;
  int TESTING_POLLING_PERIOD_SEC;
  int INITIAL_POLL_DELAY_SEC;
  int mContext;
  int mThread;
  int mHandler;
  int VDBG;
  int DBG;
  int TAG;
  int TESTING_ENABLED_PROPERTY;
}
class TextServicesManagerService {
  class InternalDeathRecipient {
    int mBundle;
    int mUid;
    int mGroup;
    int mScLocale;
    int mScListener;
    int mTsListener;
  }
  class InternalServiceConnection {
    int mBundle;
    int mLocale;
    int mSciId;
  }
  class SpellCheckerBindGroup {
    int mConnected;
    int mSpellChecker;
    int mBound;
    int mListeners;
    int mInternalConnection;
    int TAG;
  }
  class TextServicesMonitor {
  }
  int mSpellCheckerBindGroups;
  int mSpellCheckerList;
  int mSpellCheckerMap;
  int mMonitor;
  int mSystemReady;
  int mContext;
  int DBG;
  int TAG;
}
class TelephonyRegistry {
  int PHONE_STATE_PERMISSION_MASK;
  int mCellInfo;
  int mOtaspMode;
  int mDataConnectionNetworkType;
  int mCellLocation;
  int mDataConnectionLinkCapabilities;
  int mDataConnectionLinkProperties;
  int mConnectedApns;
  int mDataConnectionApn;
  int mDataConnectionReason;
  int mDataConnectionPossible;
  int mDataConnectionState;
  int mDataActivity;
  int mCallForwarding;
  int mMessageWaiting;
  int mSignalStrength;
  int mServiceState;
  int mCallIncomingNumber;
  int mCallState;
  int mBatteryStats;
  int mRecords;
  int mRemoveList;
  int mContext;
  class Record {
    int events;
    int callback;
    int binder;
    int pkgForDebug;
  }
  int DBG;
  int TAG;
}
class SystemServer {
  int EARLIEST_SUPPORTED_TIME;
  int SNAPSHOT_INTERVAL;
  int timer;
  int FACTORY_TEST_HIGH_LEVEL;
  int FACTORY_TEST_LOW_LEVEL;
  int FACTORY_TEST_OFF;
  int TAG;
}
class ServerThread {
  int mContentResolver;
  int ENCRYPTED_STATE;
  int ENCRYPTING_STATE;
  int TAG;
}
class SystemBackupAgent {
  int WALLPAPER_INFO_KEY;
  int WALLPAPER_IMAGE_KEY;
  int WALLPAPER_INFO;
  int WALLPAPER_INFO_DIR;
  int WALLPAPER_IMAGE;
  int WALLPAPER_IMAGE_DIR;
  int WALLPAPER_INFO_FILENAME;
  int WALLPAPER_IMAGE_FILENAME;
  int TAG;
}
class StatusBarManagerService {
  int mBroadcastReceiver;
  class NotificationCallbacks {
  }
  class DisableRecord {
    int token;
    int what;
    int pkg;
  }
  int mImeToken;
  int mImeBackDisposition;
  int mImeWindowVis;
  int mMenuVisible;
  int mSystemUiVisibility;
  int mLock;
  int mDisabled;
  int mSysUiVisToken;
  int mDisableRecords;
  int mNotifications;
  int mIcons;
  int mBar;
  int mNotificationCallbacks;
  int mHandler;
  int mWindowManager;
  int mContext;
  int SPEW;
  int TAG;
}
class ShutdownActivity {
  int mConfirm;
  int mReboot;
  int TAG;
}
class SerialService {
  int mSerialPorts;
  int mContext;
}
class SamplingProfilerService {
  class SamplingProfilerSettingsObserver {
    int mContentResolver;
  }
  int snapshotObserver;
  int mContext;
  int SNAPSHOT_DIR;
  int LOCAL_LOGV;
  int TAG;
}
class ResettableTimeout {
  int mThread;
  int mOffCalled;
  int mOffAt;
  int mLock;
  class T {
  }
}
class RecognitionManagerService {
  class MyPackageMonitor {
  }
  int mMonitor;
  int mContext;
  int TAG;
}
class RandomBlock {
  int block;
  int BLOCK_SIZE;
  int DEBUG;
  int TAG;
}
class ProcessMap {
  int mMap;
}
class PreferredComponent {
  class Callbacks {
  }
  int mCallbacks;
  int mParseError;
  int mShortComponent;
  int mSetComponents;
  int mSetClasses;
  int mSetPackages;
  int mComponent;
  int mMatch;
}
class PowerManagerService {
  int mLightListener;
  int mProximityListener;
  class LockList {
  }
  int mInitialAnimation;
  int mAutoBrightnessTask;
  int mProximityTask;
  class ScreenBrightnessAnimator {
    int prefix;
    int startTimeMillis;
    int duration;
    int currentMask;
    int currentValue;
    int endSensorValue;
    int startSensorValue;
    int endValue;
    int startValue;
    int ANIMATE_POWER_OFF;
    int ANIMATE_LIGHTS;
  }
  int mForceReenableScreenTask;
  int mScreenOffBroadcastDone;
  int mScreenOffStart;
  int mScreenOnBroadcastDone;
  int mScreenOnStart;
  int mNotificationTask;
  int mScreenOnListener;
  class TimeoutTask {
    int remainingTimeoutOverride;
    int nextState;
  }
  class PokeLock {
    int awakeOnSet;
    int tag;
    int binder;
    int pokey;
  }
  class WakeLock {
    int minState;
    int activated;
    int ws;
    int monitorType;
    int pid;
    int uid;
    int tag;
    int binder;
    int flags;
  }
  int mSettings;
  class SettingsObserver {
  }
  class DockReceiver {
  }
  class BootCompletedReceiver {
  }
  class BatteryReceiver {
  }
  class UnsynchronizedWakeLock {
    int mHeld;
    int mRefCounted;
    int mCount;
    int mToken;
    int mTag;
    int mFlags;
  }
  int mDebugLightAnimation;
  int mDebugLightSensor;
  int mDebugProximitySensor;
  int mSpew;
  int mTouchCycles;
  int mLastTouchDown;
  int mTotalTouchDownTime;
  int ANIM_SETTING_OFF;
  int ANIM_SETTING_ON;
  int mWindowScaleAnimation;
  int mAnimationSetting;
  int mWarningSpewThrottleTime;
  int mWarningSpewThrottleCount;
  int mUnplugTurnsOnScreen;
  int mLightSensorWarmupTime;
  int mKeyboardBacklightValues;
  int mButtonBacklightValues;
  int mLcdBacklightValues;
  int mAutoBrightnessLevels;
  int mAutoBrightessEnabled;
  int mUseSoftwareAutoBrightness;
  int mScreenBrightnessDim;
  int mButtonBrightnessOverride;
  int mScreenBrightnessOverride;
  int mScreenBrightnessSetting;
  int mPreventScreenOn;
  int mLastScreenOnTime;
  int mPokeLocks;
  int mInitComplete;
  int mPokeAwakeOnSet;
  int mPokey;
  int mNextTimeout;
  int mIsDocked;
  int mDimScreen;
  int mLightSensorKeyboardBrightness;
  int mLightSensorButtonBrightness;
  int mLightSensorScreenBrightness;
  int mLightSensorAdjustSetting;
  int mLightSensorPendingValue;
  int mLightSensorPendingIncrease;
  int mLightSensorPendingDecrease;
  int mHighestLightSensorValue;
  int mProxIgnoredBecauseScreenTurnedOff;
  int mLightSensorValue;
  int mLightSensorEnabled;
  int mLightSensor;
  int mProximitySensor;
  int mSensorManager;
  int mBatteryService;
  int mBatteryStats;
  int mActivityService;
  int mIsPowered;
  int mStillNeedSleepNotification;
  int mWaitingForFirstLightSensor;
  int mScreenBrightnessAnimator;
  int mTimeoutTask;
  int mHandler;
  int mScreenBrightnessHandler;
  int mScreenOffHandler;
  int mHandlerThread;
  int mProximityPartialLock;
  int mPreventScreenOnPartialLock;
  int mStayOnWhilePluggedInPartialLock;
  int mStayOnWhilePluggedInScreenDimLock;
  int mBroadcastWakeLock;
  int mAttentionLight;
  int mKeyboardLight;
  int mButtonLight;
  int mLcdLight;
  int mContext;
  int mLightsService;
  int mScreenOnIntent;
  int mScreenOffIntent;
  int mLocks;
  int mPolicy;
  int mScreenOffTime;
  int mLastEventTime;
  int mWakeLockState;
  int mScreenOffDelay;
  int mDimDelay;
  int mKeylightDelay;
  int mMaximumScreenOffTimeout;
  int mScreenOffTimeoutSetting;
  int mLastProximityEventTime;
  int mProximityPendingValue;
  int mProximitySensorActive;
  int mProximitySensorEnabled;
  int mProximityWakeLockCount;
  int mUserActivityAllowed;
  int mKeyboardVisible;
  int mUserState;
  int mScreenOffReason;
  int mPowerState;
  int mPartialCount;
  int mInitialized;
  int mSkippedScreenOn;
  int mPreparingForScreenOn;
  int mBroadcastWhy;
  int mBroadcastQueue;
  int mStayOnConditions;
  int mHeadless;
  int mBootCompleted;
  int mDoneBooting;
  int MY_PID;
  int MY_UID;
  int INITIAL_KEYBOARD_BRIGHTNESS;
  int INITIAL_BUTTON_BRIGHTNESS;
  int INITIAL_SCREEN_BRIGHTNESS;
  int IMMEDIATE_ANIM_STEPS;
  int AUTODIMNESS_ANIM_STEPS;
  int AUTOBRIGHTNESS_ANIM_STEPS;
  int ANIM_STEPS;
  int mAnimateScreenLights;
  int LIGHTS_MASK;
  int ALL_BRIGHT;
  int SCREEN_BUTTON_BRIGHT;
  int SCREEN_BRIGHT;
  int SCREEN_DIM;
  int SCREEN_OFF;
  int BATTERY_LOW_BIT;
  int KEYBOARD_BRIGHT_BIT;
  int BUTTON_BRIGHT_BIT;
  int SCREEN_BRIGHT_BIT;
  int SCREEN_ON_BIT;
  int ALL_LIGHTS_OFF;
  int LOW_BATTERY_THRESHOLD;
  int DEFAULT_SCREEN_BRIGHTNESS;
  int DEFAULT_SCREEN_OFF_TIMEOUT;
  int mShortKeylightDelay;
  int PROXIMITY_THRESHOLD;
  int PROXIMITY_SENSOR_DELAY;
  int LIGHT_SENSOR_OFFSET_SCALE;
  int LIGHT_SENSOR_RANGE_EXPANSION;
  int LIGHT_SENSOR_RATE;
  int LIGHT_SENSOR_DELAY;
  int LONG_DIM_TIME;
  int LONG_KEYLIGHT_DELAY;
  int MEDIUM_KEYLIGHT_DELAY;
  int SHORT_KEYLIGHT_DELAY_DEFAULT;
  int LOCK_MASK;
  int LOG_TOUCH_DOWNS;
  int LOG_PARTIAL_WL;
  int DEBUG_SCREEN_ON;
  int FULL_WAKE_LOCK_ID;
  int PARTIAL_WAKE_LOCK_ID;
  int PARTIAL_NAME;
  int TAG;
  int NOMINAL_FRAME_TIME_MS;
}
class PackageManagerBackupAgent {
  class Metadata {
    int signatures;
    int versionCode;
  }
  int mHasMetadata;
  int mStoredIncrementalVersion;
  int mStoredSdkVersion;
  int mExisting;
  int mStateVersions;
  int mRestoredSignatures;
  int mPackageManager;
  int mAllPackages;
  int GLOBAL_METADATA_KEY;
  int DEBUG;
  int TAG;
}
class NsdService {
  class ClientInfo {
    int mClientIds;
    int mResolvedService;
    int mMessenger;
    int mChannel;
    int MAX_LIMIT;
  }
  class NativeCallbackReceiver {
  }
  class NativeEvent {
    int raw;
    int code;
  }
  class NativeResponseCode {
    int SERVICE_GET_ADDR_SUCCESS;
    int SERVICE_GET_ADDR_FAILED;
    int SERVICE_UPDATE_FAILED;
    int SERVICE_UPDATED;
    int SERVICE_RESOLVED;
    int SERVICE_RESOLUTION_FAILED;
    int SERVICE_REGISTERED;
    int SERVICE_REGISTRATION_FAILED;
    int SERVICE_LOST;
    int SERVICE_FOUND;
    int SERVICE_DISCOVERY_FAILED;
  }
  int mNativeDaemonConnected;
  int mNativeConnector;
  class NsdStateMachine {
    class EnabledState {
    }
    class DisabledState {
    }
    class DefaultState {
    }
    int mEnabledState;
    int mDisabledState;
    int mDefaultState;
  }
  int sCmdToString;
  int CMD_TO_STRING_COUNT;
  int BASE;
  int mUniqueId;
  int INVALID_ID;
  int mReplyChannel;
  int mIdToClientInfoMap;
  int mClients;
  int mNsdStateMachine;
  int mContentResolver;
  int mContext;
  int DBG;
  int MDNS_TAG;
  int TAG;
}
class NotificationManagerService {
  class WorkerHandler {
  }
  class SettingsObserver {
  }
  int mIntentReceiver;
  int mNotificationCallbacks;
  class ToastRecord {
    int duration;
    int callback;
    int pkg;
    int pid;
  }
  class NotificationRecord {
    int statusBarKey;
    int score;
    int notification;
    int initialPid;
    int uid;
    int id;
    int tag;
    int pkg;
  }
  int ATTR_NAME;
  int TAG_PACKAGE;
  int TAG_BLOCKED_PKGS;
  int ATTR_VERSION;
  int TAG_BODY;
  int DB_VERSION;
  int mBlockedPackages;
  int mPolicyFile;
  int mLedNotification;
  int mLights;
  int mToastQueue;
  int mNotificationList;
  int mNotificationPulseEnabled;
  int mInCall;
  int mScreenOn;
  int mVibrator;
  int mAudioService;
  int mVibrateNotification;
  int mSoundNotification;
  int mDisabledNotifications;
  int mSystemReady;
  int mDefaultNotificationLedOff;
  int mDefaultNotificationLedOn;
  int mDefaultNotificationColor;
  int mAttentionLight;
  int mNotificationLight;
  int mStatusBar;
  int mHandler;
  int mForegroundToken;
  int mAm;
  int mContext;
  int ENABLE_BLOCKED_TOASTS;
  int ENABLE_BLOCKED_NOTIFICATIONS;
  int SCORE_DISPLAY_THRESHOLD;
  int NOTIFICATION_PRIORITY_MULTIPLIER;
  int JUNK_SCORE;
  int SCORE_ONGOING_HIGHER;
  int DEFAULT_STREAM_TYPE;
  int DEFAULT_VIBRATE_PATTERN;
  int SHORT_DELAY;
  int LONG_DELAY;
  int MESSAGE_TIMEOUT;
  int MAX_PACKAGE_NOTIFICATIONS;
  int DBG;
  int TAG;
}
class NetworkTimeUpdateService {
  class SettingsObserver {
    int mHandler;
    int mMsg;
  }
  class MyHandler {
  }
  int mConnectivityReceiver;
  int mNitzReceiver;
  int mTryAgainCounter;
  int mLastNtpFetchTime;
  int mSettingsObserver;
  int mPendingPollIntent;
  int mAlarmManager;
  int mThread;
  int mHandler;
  int mTime;
  int mContext;
  int mNitzZoneSetTime;
  int mNitzTimeSetTime;
  int NOT_SET;
  int POLL_REQUEST;
  int ACTION_POLL;
  int TIME_ERROR_THRESHOLD_MS;
  int TRY_AGAIN_TIMES_MAX;
  int POLLING_INTERVAL_SHORTER_MS;
  int POLLING_INTERVAL_MS;
  int EVENT_NETWORK_CONNECTED;
  int EVENT_POLL_NETWORK_TIME;
  int EVENT_AUTO_TIME_CHANGED;
  int DBG;
  int TAG;
}
class NetworkStatsServiceTest {
  int mNetworkObserver;
  int mSession;
  int mService;
  int mConnManager;
  int mSettings;
  int mTime;
  int mAlarmManager;
  int mNetManager;
  int mStatsDir;
  int mServiceContext;
  int mElapsedRealtime;
  int UID_GREEN;
  int UID_BLUE;
  int UID_RED;
  int sTemplateImsi2;
  int sTemplateImsi1;
  int sTemplateWifi;
  int TEST_SSID;
  int IMSI_2;
  int IMSI_1;
  int TEST_START;
  int TEST_IFACE2;
  int TEST_IFACE;
  int TAG;
}
class NetworkPolicyManagerServiceTest {
  class IdleFuture {
  }
  class FutureCapture {
    int capture;
  }
  class FutureAnswer {
  }
  class TestAbstractFuture {
  }
  int PID_3;
  int PID_2;
  int PID_1;
  int UID_B_GUEST;
  int UID_A_GUEST;
  int UID_B;
  int UID_A;
  int APP_ID_B;
  int APP_ID_A;
  int USER_ID_GUEST;
  int USER_ID;
  int mElapsedRealtime;
  int mStartTime;
  int mStubBinder;
  int mNetworkObserver;
  int mProcessObserver;
  int mService;
  int mNotifManager;
  int mConnManager;
  int mTime;
  int mPolicyListener;
  int mNetworkManager;
  int mStatsService;
  int mPowerManager;
  int mActivityManager;
  int mPolicyDir;
  int mServiceContext;
  int sTemplateWifi;
  int TEST_SSID;
  int TEST_IFACE;
  int TEST_START;
  int TAG;
}
class NetworkManagementSocketTagger {
  class SocketTags {
    int statsUid;
    int statsTag;
  }
  int threadSocketTags;
  int PROP_QTAGUID_ENABLED;
  int LOGD;
  int TAG;
}
class NetworkManagementService {
  class NetdCallbackReceiver {
  }
  int mBandwidthControlEnabled;
  int mUidRejectOnQuota;
  int mActiveAlerts;
  int mActiveQuotas;
  int mQuotaLock;
  int mStatsFactory;
  int mObservers;
  int mConnectedSignal;
  int mThread;
  int mMainHandler;
  int mConnector;
  int mContext;
  class NetdResponseCode {
    int BandwidthControl;
    int InterfaceChange;
    int DnsProxyQueryResult;
    int TetheringStatsResult;
    int QuotaCounterResult;
    int InterfaceTxThrottleResult;
    int InterfaceRxThrottleResult;
    int InterfaceTxCounterResult;
    int InterfaceRxCounterResult;
    int SoftapStatusResult;
    int InterfaceGetCfgResult;
    int IpFwdStatusResult;
    int TetherStatusResult;
    int TtyListResult;
    int TetherDnsFwdTgtListResult;
    int TetherInterfaceListResult;
    int InterfaceListResult;
  }
  int LIMIT_GLOBAL_ALERT;
  int SECONDARY;
  int DEFAULT;
  int REMOVE;
  int ADD;
  int NETD_TAG;
  int DBG;
  int TAG;
}
class NativeDaemonEvent {
  int mParsed;
  int mRawEvent;
  int mMessage;
  int mCode;
  int mCmdNumber;
}
class NativeDaemonConnectorTest {
  int TAG;
}
class NativeDaemonConnectorException {
  int mEvent;
  int mCmd;
}
class NativeDaemonConnector {
  class ResponseQueue {
    int mMaxCount;
    int mResponses;
    class Response {
      int request;
      int responses;
      int cmdNum;
    }
  }
  class Command {
    int mArguments;
    int mCmd;
  }
  class NativeDaemonFailureException {
  }
  class NativeDaemonArgumentException {
  }
  int BUFFER_SIZE;
  int mDaemonLock;
  int WARN_EXECUTE_DELAY_MS;
  int DEFAULT_TIMEOUT;
  int mSequenceNumber;
  int mCallbackHandler;
  int mCallbacks;
  int mResponseQueue;
  int mLocalLog;
  int mOutputStream;
  int mSocket;
  int TAG;
  int LOGD;
}
class MountServiceTests {
  class ObbObserver {
    int done;
    int state;
    int path;
  }
  int OBB_MOUNT_PREFIX;
  int WAIT_TIME_INCR;
  int MAX_WAIT_TIME;
  int TAG;
}
class MountService {
  class UnmountObbAction {
    int mForceUnmount;
  }
  class MountObbAction {
    int mKey;
  }
  class ObbAction {
    int mObbState;
    int mRetries;
    int MAX_RETRIES;
  }
  class ObbActionHandler {
    int mActions;
    int mBound;
  }
  int TAG_STORAGE;
  int TAG_STORAGE_LIST;
  class MountServiceBinderListener {
    int mListener;
  }
  int mBroadcastReceiver;
  int mHandler;
  int mHandlerThread;
  class MountServiceHandler {
    int mUpdatingStatus;
    int mForceUnmounts;
  }
  class ShutdownCallBack {
    int observer;
  }
  class UmsEnableCallBack {
    int method;
  }
  class UnmountCallBack {
    int retries;
    int removeEncryption;
    int force;
    int path;
  }
  int MAX_UNMOUNT_RETRIES;
  int RETRY_UNMOUNT_DELAY;
  int H_UNMOUNT_MS;
  int H_UNMOUNT_PM_DONE;
  int H_UNMOUNT_PM_UPDATE;
  int mContainerService;
  class DefaultContainerConnection {
  }
  int mDefContainerConn;
  int DEFAULT_CONTAINER_COMPONENT;
  int OBB_FLUSH_MOUNT_STATE;
  int OBB_MCS_RECONNECT;
  int OBB_MCS_UNBIND;
  int OBB_MCS_BOUND;
  int OBB_RUN_ACTION;
  int mObbActionHandler;
  class ObbState {
    int nonce;
    int token;
    int callerUid;
    int filename;
  }
  int mObbPathToStateMap;
  int mObbMounts;
  int PBKDF2_HASH_ROUNDS;
  int CRYPTO_ALGORITHM_KEY_SIZE;
  int mAsecMountSet;
  int mEmulateExternalStorage;
  int mSendUmsConnectedOnBoot;
  int mAsecsScanned;
  int mConnectedSignal;
  int mBooted;
  int mListeners;
  int mUmsAvailable;
  int mUmsEnabling;
  int mPms;
  int mExternalStoragePath;
  int mVolumeMap;
  int mVolumeStates;
  int mPrimaryVolume;
  int mVolumes;
  int mConnector;
  int mContext;
  class VoldResponseCode {
    int VolumeBadRemoval;
    int VolumeDiskRemoved;
    int VolumeDiskInserted;
    int VolumeStateChange;
    int OpFailedStorageNotFound;
    int OpFailedStorageBusy;
    int OpFailedVolNotMounted;
    int OpFailedMediaCorrupt;
    int OpFailedMediaBlank;
    int OpFailedNoMedia;
    int ShareEnabledResult;
    int AsecPathResult;
    int ShareStatusResult;
    int StorageUsersListResult;
    int AsecListResult;
    int VolumeListResult;
  }
  class VolumeState {
    int SharedMnt;
    int Shared;
    int Formatting;
    int Unmounting;
    int Mounted;
    int Checking;
    int Pending;
    int Idle;
    int NoMedia;
    int Init;
  }
  int MAX_CONTAINERS;
  int VOLD_TAG;
  int TAG;
  int WATCHDOG_ENABLE;
  int DEBUG_OBB;
  int DEBUG_EVENTS;
  int DEBUG_UNMOUNT;
  int LOCAL_LOGD;
}
class MockAccessibilityService {
  int mIsSystemBoundAsClient;
  int mReplaying;
  int mExpectedInterrupt;
  int mExpectedEvents;
}
class MasterClearReceiver {
  int TAG;
}
class LocationManagerService {
  int mPackageMonitor;
  int mBroadcastReceiver;
  class LocationWorkerHandler {
  }
  class ProximityListener {
    int isGpsAvailable;
  }
  class ProximityAlert {
    int mLocation;
    int mIntent;
    int mExpiration;
    int mRadius;
    int mLongitude;
    int mLatitude;
    int mUid;
  }
  class UpdateRecord {
    int mLastStatusBroadcast;
    int mLastFixBroadcast;
    int mUid;
    int mSingleShot;
    int mMinDistance;
    int mMinTime;
    int mReceiver;
    int mProvider;
  }
  class LpCapabilityComparator {
    int SPEED_SCORE;
    int BEARING_SCORE;
    int ALTITUDE_SCORE;
  }
  class LpAccuracyComparator {
  }
  class LpPowerComparator {
  }
  class SettingsObserver {
  }
  class Receiver {
    int mRequiredPermissions;
    int mPendingBroadcasts;
    int mUpdateRecords;
    int mKey;
    int mPendingIntent;
    int mListener;
  }
  int mSettings;
  int mNetworkState;
  int mLastKnownLocation;
  int mProximitiesEntered;
  int mProximityAlerts;
  int mProximityListener;
  int mProximityReceiver;
  int mTmpWorkSource;
  int mRecordsByProvider;
  int mLock;
  int mProvidersByName;
  int mProviders;
  int mReceivers;
  int mPendingBroadcasts;
  int mWakeLock;
  int WAKELOCK_KEY;
  int MESSAGE_PACKAGE_UPDATED;
  int MESSAGE_LOCATION_CHANGED;
  int mGpsLocationProvider;
  int mNetworkLocationProvider;
  int mLocationHandler;
  int mNetInitiatedListener;
  int mGpsStatusProvider;
  int mGeocodeProvider;
  int mGeocodeProviderPackageName;
  int mNetworkLocationProviderPackageName;
  int mPackageManager;
  int mContext;
  int sProvidersLoaded;
  int mMockProviders;
  int mDisabledProviders;
  int mEnabledProviders;
  int MAX_PROVIDER_SCHEDULING_JITTER;
  int INSTALL_LOCATION_PROVIDER;
  int ACCESS_LOCATION_EXTRA_COMMANDS;
  int ACCESS_MOCK_LOCATION;
  int ACCESS_COARSE_LOCATION;
  int ACCESS_FINE_LOCATION;
  int mLastWriteTime;
  int LOCAL_LOGV;
  int TAG;
}
class LightsService {
  int mNativePointer;
  int mContext;
  int mH;
  int mLegacyFlashlightHack;
  class Light {
    int mFlashing;
    int mOffMS;
    int mOnMS;
    int mMode;
    int mColor;
    int mId;
  }
  int mLights;
  int BRIGHTNESS_MODE_SENSOR;
  int BRIGHTNESS_MODE_USER;
  int LIGHT_FLASH_HARDWARE;
  int LIGHT_FLASH_TIMED;
  int LIGHT_FLASH_NONE;
  int LIGHT_ID_COUNT;
  int LIGHT_ID_WIFI;
  int LIGHT_ID_BLUETOOTH;
  int LIGHT_ID_ATTENTION;
  int LIGHT_ID_NOTIFICATIONS;
  int LIGHT_ID_BATTERY;
  int LIGHT_ID_BUTTONS;
  int LIGHT_ID_KEYBOARD;
  int LIGHT_ID_BACKLIGHT;
  int DEBUG;
  int TAG;
}
class IntentResolver {
  int mTypedActionToFilter;
  int mActionToFilter;
  int mSchemeToFilter;
  int mWildTypeToFilter;
  int mBaseTypeToFilter;
  int mTypeToFilter;
  int mFilters;
  int mResolvePrioritySorter;
  class IteratorWrapper {
    int mCur;
    int mI;
  }
  int localLOGV;
  int DEBUG;
  int TAG;
}
class InputMethodManagerService {
  class InputMethodFileManager {
    int mSubtypesMap;
    int mMethodMap;
    int mAdditionalInputMethodSubtypeFile;
    int ATTR_IS_AUXILIARY;
    int ATTR_IME_SUBTYPE_EXTRA_VALUE;
    int ATTR_IME_SUBTYPE_MODE;
    int ATTR_IME_SUBTYPE_LOCALE;
    int ATTR_ICON;
    int ATTR_LABEL;
    int ATTR_ID;
    int NODE_IMI;
    int NODE_SUBTYPE;
    int NODE_SUBTYPES;
    int ADDITIONAL_SUBTYPES_FILE_NAME;
    int INPUT_METHOD_PATH;
    int SYSTEM_PATH;
  }
  class InputMethodSettings {
    int mEnabledInputMethodsStrCache;
    int mMethodList;
    int mMethodMap;
    int mResolver;
    int mRes;
    int mSubtypeSplitter;
    int mInputMethodSplitter;
    int INPUT_METHOD_SUBTYPE_SEPARATER;
    int INPUT_METHOD_SEPARATER;
  }
  class InputMethodAndSubtypeListManager {
    int mSortedImmis;
    int mSystemLocaleStr;
    int mImms;
    int mPm;
    int mContext;
  }
  class ImeSubtypeListAdapter {
    int mCheckedItem;
    int mItemsList;
    int mTextViewResourceId;
    int mInflater;
  }
  class ImeSubtypeListItem {
    int mIsSystemLanguage;
    int mIsSystemLocale;
    int mSubtypeId;
    int mImi;
    int mSubtypeName;
    int mImeName;
  }
  class HardKeyboardListener {
  }
  class MethodCallback {
    int mParentIMMS;
    int mMethod;
  }
  class MyPackageMonitor {
  }
  class ScreenOnOffReceiver {
  }
  class SettingsObserver {
  }
  int mLastSystemLocale;
  int mSubtypeIds;
  int mIms;
  int mSwitchingDialogTitleView;
  int mSwitchingDialog;
  int mDialogBuilder;
  int mImeWindowVis;
  int mBackDisposition;
  int mScreenOn;
  int mEnabledSession;
  int mBoundToMethod;
  int mLastBindTime;
  int mCurMethod;
  int mCurToken;
  int mCurIntent;
  int mInputShown;
  int mShowForced;
  int mShowExplicitlyRequested;
  int mShowRequested;
  int mHaveConnection;
  int mShortcutInputMethodsAndSubtypes;
  int mCurrentSubtype;
  int mCurId;
  int mCurAttribute;
  int mCurInputContext;
  int mCurFocusedWindow;
  int mCurClient;
  int mCurSeq;
  int mCurMethodId;
  int mSystemReady;
  int mClients;
  class ClientState {
    int curSession;
    int sessionRequested;
    int binding;
    int pid;
    int uid;
    int inputContext;
    int client;
  }
  class SessionState {
    int session;
    int method;
    int client;
  }
  int mImeSelectedOnBoot;
  int mNotificationShown;
  int mShowOngoingImeSwitcherForPhones;
  int mImeSwitchPendingIntent;
  int mImeSwitcherNotification;
  int mStatusBar;
  int mKeyguardManager;
  int mNotificationManager;
  int mVisibleBound;
  int mVisibleConnection;
  int mSecureSuggestionSpans;
  int mMethodMap;
  int mMethodList;
  int mNoBinding;
  int mWindowManagerService;
  int mHardKeyboardListener;
  int mImListManager;
  int mFileManager;
  int mCaller;
  int mIWindowManager;
  int mSettingsObserver;
  int mSettings;
  int mHandler;
  int mRes;
  int mContext;
  int ENGLISH_LOCALE;
  int TAG_ASCII_CAPABLE;
  int TAG_ENABLED_WHEN_DEFAULT_IS_NOT_ASCII_CAPABLE;
  int TAG_TRY_SUPPRESSING_IME_SWITCHER;
  int SUBTYPE_MODE_VOICE;
  int SUBTYPE_MODE_KEYBOARD;
  int NOT_A_SUBTYPE_ID_STR;
  int NOT_A_SUBTYPE_ID;
  int SECURE_SUGGESTION_SPANS_MAX_SIZE;
  int TIME_TO_RECONNECT;
  int MSG_HARD_KEYBOARD_SWITCH_CHANGED;
  int MSG_SET_ACTIVE;
  int MSG_BIND_METHOD;
  int MSG_UNBIND_METHOD;
  int MSG_RESTART_INPUT;
  int MSG_START_INPUT;
  int MSG_CREATE_SESSION;
  int MSG_ATTACH_TOKEN;
  int MSG_HIDE_SOFT_INPUT;
  int MSG_SHOW_SOFT_INPUT;
  int MSG_BIND_INPUT;
  int MSG_UNBIND_INPUT;
  int MSG_SHOW_IM_CONFIG;
  int MSG_SHOW_IM_SUBTYPE_ENABLER;
  int MSG_SHOW_IM_SUBTYPE_PICKER;
  int MSG_SHOW_IM_PICKER;
  int TAG;
  int DEBUG;
}
class INativeDaemonConnectorCallbacks {
}
class EntropyMixerTest {
}
class EntropyMixer {
  int mHandler;
  int entropyFile;
  int randomDevice;
  int START_NANOTIME;
  int START_TIME;
  int ENTROPY_WRITE_PERIOD;
  int ENTROPY_WHAT;
  int TAG;
}
class DropBoxTest {
}
class DropBoxManagerService {
  class EntryFile {
    int blocks;
    int file;
    int flags;
    int timestampMillis;
    int tag;
  }
  class FileList {
    int contents;
    int blocks;
  }
  int mReceiver;
  int mHandler;
  int mBooted;
  int mCachedQuotaUptimeMillis;
  int mCachedQuotaBlocks;
  int mBlockSize;
  int mStatFs;
  int mFilesByTag;
  int mAllFiles;
  int mDropBoxDir;
  int mContentResolver;
  int mContext;
  int PROFILE_DUMP;
  int MSG_SEND_BROADCAST;
  int QUOTA_RESCAN_MILLIS;
  int DEFAULT_RESERVE_PERCENT;
  int DEFAULT_QUOTA_PERCENT;
  int DEFAULT_QUOTA_KB;
  int DEFAULT_MAX_FILES;
  int DEFAULT_AGE_SECONDS;
  int TAG;
}
class DockObserver {
  int mHandler;
  int mPowerManager;
  int mContext;
  int mSystemReady;
  int mPreviousDockState;
  int mDockState;
  int MSG_DOCK_STATE;
  int DOCK_STATE_PATH;
  int DOCK_UEVENT_MATCH;
  int LOG;
  int TAG;
}
class DiskStatsService {
  int mContext;
  int TAG;
}
class DeviceStorageMonitorService {
  class CacheFileDeletedObserver {
  }
  class CachePackageDataObserver {
  }
  int mHandler;
  int SERVICE;
  int mMemFullThreshold;
  int mMemLowThreshold;
  int _FALSE;
  int _TRUE;
  int mCacheFileDeletedObserver;
  int mClearCacheObserver;
  int mStorageNotFullIntent;
  int mStorageFullIntent;
  int mStorageOkIntent;
  int mStorageLowIntent;
  int mClearingCache;
  int mClearSucceeded;
  int mThreadStartTime;
  int CACHE_PATH;
  int SYSTEM_PATH;
  int DATA_PATH;
  int mCacheFileStats;
  int mSystemFileStats;
  int mDataFileStats;
  int mTotalMemory;
  int mContentResolver;
  int mContext;
  int mMemFullFlag;
  int mLowMemFlag;
  int mLastReportedFreeMemTime;
  int mLastReportedFreeMem;
  int mFreeMem;
  int DEFAULT_FULL_THRESHOLD_BYTES;
  int DEFAULT_CHECK_INTERVAL;
  int DEFAULT_DISK_FREE_CHANGE_REPORTING_THRESHOLD;
  int DEFAULT_FREE_STORAGE_LOG_INTERVAL_IN_MINUTES;
  int DEFAULT_THRESHOLD_MAX_BYTES;
  int DEFAULT_THRESHOLD_PERCENTAGE;
  int LOW_MEMORY_NOTIFICATION_ID;
  int MONITOR_INTERVAL;
  int DEVICE_MEMORY_WHAT;
  int localLOGV;
  int DEBUG;
  int TAG;
}
class DevicePolicyManagerService {
  int SYSTEM_PROP_DISABLE_CAMERA;
  class MyPackageMonitor {
  }
  class ActiveAdmin {
    int globalProxyExclusionList;
    int globalProxySpec;
    int specifiesGlobalProxy;
    int disableCamera;
    int encryptionRequested;
    int passwordExpirationDate;
    int DEF_PASSWORD_EXPIRATION_DATE;
    int passwordExpirationTimeout;
    int DEF_PASSWORD_EXPIRATION_TIMEOUT;
    int maximumFailedPasswordsForWipe;
    int DEF_MAXIMUM_FAILED_PASSWORDS_FOR_WIPE;
    int maximumTimeToUnlock;
    int DEF_MAXIMUM_TIME_TO_UNLOCK;
    int minimumPasswordNonLetter;
    int DEF_MINIMUM_PASSWORD_NON_LETTER;
    int minimumPasswordSymbols;
    int DEF_MINIMUM_PASSWORD_SYMBOLS;
    int minimumPasswordNumeric;
    int DEF_MINIMUM_PASSWORD_NUMERIC;
    int minimumPasswordLetters;
    int DEF_MINIMUM_PASSWORD_LETTERS;
    int minimumPasswordLowerCase;
    int DEF_MINIMUM_PASSWORD_LOWER_CASE;
    int minimumPasswordUpperCase;
    int DEF_MINIMUM_PASSWORD_UPPER_CASE;
    int passwordHistoryLength;
    int DEF_PASSWORD_HISTORY_LENGTH;
    int minimumPasswordLength;
    int DEF_MINIMUM_PASSWORD_LENGTH;
    int passwordQuality;
    int info;
  }
  int mReceiver;
  int mAdminList;
  int mAdminMap;
  int mLastMaximumTimeToLock;
  int mHandler;
  int mPasswordOwner;
  int mFailedPasswordAttempts;
  int mActivePasswordNonLetter;
  int mActivePasswordSymbols;
  int mActivePasswordNumeric;
  int mActivePasswordLetters;
  int mActivePasswordLowerCase;
  int mActivePasswordUpperCase;
  int mActivePasswordLength;
  int mActivePasswordQuality;
  int mIWindowManager;
  int mIPowerManager;
  int mWakeLock;
  int mMonitor;
  int mContext;
  int MS_PER_DAY;
  int ACTION_EXPIRED_PASSWORD_NOTIFICATION;
  int EXPIRATION_GRACE_PERIOD_MS;
  int REQUEST_EXPIRE_PASSWORD;
  int TAG;
}
class CountryDetectorServiceTest {
  class CountryDetectorServiceTester {
    int mListener;
  }
  class CountryListenerTester {
    int mCountry;
  }
}
class CountryDetectorService {
  int mLocationBasedDetectorListener;
  int mHandler;
  int mSystemReady;
  int mCountryDetector;
  int mContext;
  int mReceivers;
  int DEBUG;
  int TAG;
  class Receiver {
    int mKey;
    int mListener;
  }
}
class ConnectivityService {
  class VpnCallback {
  }
  class SettingsObserver {
    int mHandler;
    int mWhat;
  }
  class MyHandler {
  }
  int mPolicyListener;
  class FeatureUser {
    int mCreateTime;
    int mUid;
    int mPid;
    int mBinder;
    int mFeature;
    int mNetworkType;
  }
  int mProtectedNetworks;
  int mRadioAttributes;
  class RadioAttributes {
    int mType;
    int mSimultaneity;
  }
  int mNetworksDefined;
  int mNetConfigs;
  int mSettingsObserver;
  int mGlobalProxyLock;
  int mGlobalProxy;
  int mDefaultProxyDisabled;
  int mDefaultProxyLock;
  int mDefaultProxy;
  int mInetLog;
  int INET_CONDITION_LOG_MAX_SIZE;
  int mAddedRoutes;
  int mDefaultDns;
  int mNetTransitionWakeLockTimeout;
  int mNetTransitionWakeLockSerialNumber;
  int mNetTransitionWakeLockCausedBy;
  int mNetTransitionWakeLock;
  int mInitialBroadcast;
  int mSystemReady;
  int mFeatureUsers;
  int mHandler;
  int EVENT_SET_POLICY_DATA_ENABLE;
  int EVENT_SEND_STICKY_BROADCAST_INTENT;
  int EVENT_RESTORE_DNS;
  int EVENT_SET_DEPENDENCY_MET;
  int EVENT_APPLY_GLOBAL_HTTP_PROXY;
  int EVENT_CLEAR_NET_TRANSITION_WAKELOCK;
  int EVENT_SET_MOBILE_DATA;
  int EVENT_INET_CONDITION_HOLD_END;
  int EVENT_INET_CONDITION_CHANGE;
  int EVENT_SET_NETWORK_PREFERENCE;
  int EVENT_CHANGE_MOBILE_DATA_ENABLED;
  int EVENT_RESTORE_DEFAULT_NETWORK;
  int MAX_NETWORK_STATE_TRACKER_EVENT;
  int MIN_NETWORK_STATE_TRACKER_EVENT;
  int TO_SECONDARY_TABLE;
  int TO_DEFAULT_TABLE;
  int REMOVE;
  int ADD;
  int DISABLED;
  int ENABLED;
  int mPolicyManager;
  int mNetd;
  int sServiceInstance;
  int mTestMode;
  int mDnsOverridden;
  int mNumDnsEntries;
  int mDnsLock;
  int mDefaultConnectionSequence;
  int mInetConditionChangeInFlight;
  int mDefaultInetConditionPublished;
  int mDefaultInetCondition;
  int mActiveDefaultNetwork;
  int mNetworkPreference;
  int mContext;
  int mPriorityList;
  int mNetRequestersPids;
  int mCurrentLinkProperties;
  int mNetTrackers;
  int mMeteredIfaces;
  int mUidRules;
  int mRulesLock;
  int mVpn;
  int mTetheringConfigValid;
  int mTethering;
  int MAX_HOSTROUTE_CYCLE_COUNT;
  int NETWORK_RESTORE_DELAY_PROP_NAME;
  int RESTORE_DEFAULT_NETWORK_DELAY;
  int LOGD_RULES;
  int TAG;
  int VDBG;
  int DBG;
}
class CommonTimeManagementService {
  class InterfaceScoreRule {
    int mScore;
    int mPrefix;
  }
  int mNoInterfaceRunnable;
  int mReconnectRunnable;
  int mCTServerDiedListener;
  int mConnectivityMangerObserver;
  int mIfaceObserver;
  int mEffectivePrio;
  int mDetectedAtStartup;
  int mLock;
  int mNoInterfaceHandler;
  int mReconnectHandler;
  int mCurIface;
  int mCTConfig;
  int mNetMgr;
  int mContext;
  int IFACE_SCORE_RULES;
  int NO_INTERFACE_TIMEOUT;
  int BASE_SERVER_PRIO;
  int ALLOW_WIFI;
  int AUTO_DISABLE;
  int NO_INTERFACE_TIMEOUT_PROP;
  int SERVER_PRIO_PROP;
  int ALLOW_WIFI_PROP;
  int AUTO_DISABLE_PROP;
  int NATIVE_SERVICE_RECONNECT_TIMEOUT;
  int TAG;
}
class ClipboardService {
  int mClipboards;
  class PerUserClipboard {
    int activePermissionOwners;
    int primaryClip;
    int primaryClipListeners;
    int userId;
  }
  int mPermissionOwner;
  int mPm;
  int mAm;
  int mContext;
  int TAG;
}
class CertBlacklisterTest {
  int SERIAL_KEY;
  int PUBKEY_KEY;
  int SERIAL_PATH;
  int PUBKEY_PATH;
  int BLACKLIST_ROOT;
}
class CertBlacklister {
  class BlacklistObserver {
    int mContentResolver;
    int mTmpDir;
    int mPath;
    int mName;
    int mKey;
  }
  int SERIAL_BLACKLIST_KEY;
  int PUBKEY_BLACKLIST_KEY;
  int SERIAL_PATH;
  int PUBKEY_PATH;
  int BLACKLIST_ROOT;
  int TAG;
}
class BroadcastInterceptingContext {
  class BroadcastInterceptor {
    int mFilter;
    int mReceiver;
  }
  int mInterceptors;
  int TAG;
}
class BrickReceiver {
}
class BootReceiver {
  int sTombstoneObserver;
  int OLD_UPDATER_CLASS;
  int OLD_UPDATER_PACKAGE;
  int TOMBSTONE_DIR;
  int LOG_SIZE;
  int TAG;
}
class BatteryService {
  class Led {
    int mBatteryFull;
    int mBatteryLow;
    int mBatteryCharging;
    int mBatteryLedOff;
    int mBatteryLedOn;
    int mBatteryFullARGB;
    int mBatteryMediumARGB;
    int mBatteryLowARGB;
    int mBatteryLight;
    int mLightsService;
  }
  int mInvalidChargerObserver;
  int mPowerSupplyObserver;
  int mSentLowBatteryBroadcast;
  int mLed;
  int mDischargeStartLevel;
  int mDischargeStartTime;
  int mLastPlugType;
  int mPlugType;
  int mLowBatteryCloseWarningLevel;
  int mLowBatteryWarningLevel;
  int mLastInvalidCharger;
  int mLastBatteryLevelCritical;
  int mLastBatteryTemperature;
  int mLastBatteryVoltage;
  int mLastBatteryLevel;
  int mLastBatteryPresent;
  int mLastBatteryHealth;
  int mLastBatteryStatus;
  int mInvalidCharger;
  int mBatteryLevelCritical;
  int mBatteryTechnology;
  int mBatteryTemperature;
  int mBatteryVoltage;
  int mBatteryLevel;
  int mBatteryPresent;
  int mBatteryHealth;
  int mBatteryStatus;
  int mUsbOnline;
  int mAcOnline;
  int mBatteryStats;
  int mContext;
  int BATTERY_PLUGGED_NONE;
  int DUMPSYS_DATA_PATH;
  int BATTERY_STATS_SERVICE_NAME;
  int DUMPSYS_ARGS;
  int DUMP_MAX_LENGTH;
  int mCriticalBatteryLevel;
  int BATTERY_SCALE;
  int LOCAL_LOGV;
  int TAG;
}
class BackupManagerService {
  class ActiveRestoreSession {
    class EndRestoreRunnable {
      int mSession;
      int mBackupManager;
    }
    int mEnded;
    int mRestoreSets;
    int mRestoreTransport;
    int mPackageName;
    int TAG;
  }
  class PerformInitializeTask {
    int mQueue;
  }
  class PerformClearTask {
    int mPackage;
    int mTransport;
  }
  class PerformRestoreTask {
    class RestoreRequest {
      int storedAppVersion;
      int app;
    }
    int mCurrentPackage;
    int mNewState;
    int mBackupData;
    int mSavedStateName;
    int mNewStateName;
    int mBackupDataName;
    int mStatus;
    int mFinished;
    int mCount;
    int mCurrentState;
    int mRestorePackages;
    int mAgentPackages;
    int mPmAgent;
    int mStartRealtime;
    int mFilterSet;
    int mNeedFullBackup;
    int mPmToken;
    int mStateDir;
    int mTargetPackage;
    int mToken;
    int mObserver;
    int mTransport;
  }
  class RestoreState {
    int FINAL;
    int RUNNING_QUEUE;
    int PM_METADATA;
    int DOWNLOAD_DATA;
    int INITIAL;
  }
  class PerformFullRestoreTask {
    int mDeleteObserver;
    int mInstallObserver;
    class RestoreDeleteObserver {
      int mResult;
      int mDone;
    }
    class RestoreInstallObserver {
      int mResult;
      int mPackageName;
      int mDone;
    }
    class RestoreFileRunnable {
      int mToken;
      int mSocket;
      int mInfo;
      int mAgent;
    }
    int mClearedPackages;
    int mManifestSignatures;
    int mPackageInstallers;
    int mPackagePolicies;
    int mBytes;
    int mPipes;
    int mTargetApp;
    int mAgentPackage;
    int mAgent;
    int mLatchObject;
    int mObserver;
    int mDecryptPassword;
    int mCurrentPassword;
    int mInputFile;
  }
  class RestorePolicy {
    int ACCEPT_IF_APK;
    int ACCEPT;
    int IGNORE;
  }
  class FileMetadata {
    int size;
    int mtime;
    int mode;
    int path;
    int domain;
    int type;
    int installerPackageName;
    int packageName;
  }
  class PerformFullBackupTask {
    class FullBackupRunner {
      int mWriteManifest;
      int mSendApk;
      int mToken;
      int mPipe;
      int mAgent;
      int mPackage;
    }
    int mManifestFile;
    int mFilesDir;
    int mLatchObject;
    int mEncryptPassword;
    int mCurrentPassword;
    int mPackages;
    int mIncludeSystem;
    int mAllApps;
    int mIncludeShared;
    int mIncludeApks;
    int mObserver;
    int mDeflater;
    int mOutputFile;
  }
  class PerformBackupTask {
    int mFinished;
    int mStatus;
    int mNewState;
    int mBackupData;
    int mSavedState;
    int mNewStateName;
    int mBackupDataName;
    int mSavedStateName;
    int mCurrentPackage;
    int mCurrentState;
    int mJournal;
    int mStateDir;
    int mOriginalQueue;
    int mQueue;
    int mTransport;
    int TAG;
  }
  class BackupState {
    int FINAL;
    int RUNNING_QUEUE;
    int INITIAL;
  }
  class BackupRestoreTask {
  }
  class ClearDataObserver {
  }
  int mGoogleConnection;
  int mBroadcastReceiver;
  class RunInitializeReceiver {
  }
  class RunBackupReceiver {
  }
  class BackupHandler {
  }
  int mPendingInits;
  int INIT_SENTINEL_FILE_NAME;
  int mCurrentToken;
  int mAncestralToken;
  int mAncestralPackages;
  int mTokenFile;
  int CURRENT_ANCESTRAL_RECORD_VERSION;
  int mEverStoredApps;
  int mEverStored;
  int ENCRYPTION_ALGORITHM_NAME;
  int PBKDF2_SALT_SIZE;
  int PBKDF2_KEY_SIZE;
  int PBKDF2_HASH_ROUNDS;
  int mPasswordSalt;
  int mPasswordHashFile;
  int mPasswordHash;
  int mRng;
  int mJournal;
  int mJournalDir;
  int mDataDir;
  int mBaseStateDir;
  int mFullConfirmations;
  int mTokenGenerator;
  int mCurrentOpLock;
  int mCurrentOperations;
  class Operation {
    int callback;
    int state;
  }
  int OP_TIMEOUT;
  int OP_ACKNOWLEDGED;
  int OP_PENDING;
  class FullRestoreParams {
  }
  class FullBackupParams {
    int packages;
    int includeSystem;
    int allApps;
    int includeShared;
    int includeApks;
  }
  class FullParams {
    int encryptPassword;
    int curPassword;
    int observer;
    int latch;
    int fd;
  }
  class ClearParams {
    int packageInfo;
    int transport;
  }
  class RestoreParams {
    int filterSet;
    int needFullBackup;
    int pmToken;
    int pkgInfo;
    int token;
    int observer;
    int transport;
  }
  class RestoreGetSetsParams {
    int observer;
    int session;
    int transport;
  }
  class ProvisionedObserver {
  }
  int mProvisionedObserver;
  int mActiveRestoreSession;
  int mGoogleTransport;
  int mLocalTransport;
  int mCurrentTransport;
  int mTransports;
  int mClearingData;
  int mClearDataLock;
  int mBackupTrace;
  int DEBUG_BACKUP_TRACE;
  int mNextBackupPass;
  int mLastBackupPass;
  int mConnecting;
  int mBackupRunning;
  int mConnectedAgent;
  int mAgentConnectLock;
  int mQueueLock;
  int PACKAGE_MANAGER_SENTINEL;
  int mPendingBackups;
  class BackupRequest {
    int packageName;
  }
  int mBackupParticipants;
  int mRunInitReceiver;
  int mRunBackupReceiver;
  int mRunInitIntent;
  int mRunBackupIntent;
  int mBackupHandler;
  int mHandlerThread;
  int mWakelock;
  int mAutoRestore;
  int mProvisioned;
  int mEnabled;
  int mBackupManagerBinder;
  int mMountService;
  int mAlarmManager;
  int mPowerManager;
  int mActivityManager;
  int mPackageManagerBinder;
  int mPackageManager;
  int mContext;
  int TIMEOUT_FULL_CONFIRMATION;
  int TIMEOUT_RESTORE_INTERVAL;
  int TIMEOUT_SHARED_BACKUP_INTERVAL;
  int TIMEOUT_FULL_BACKUP_INTERVAL;
  int TIMEOUT_BACKUP_INTERVAL;
  int TIMEOUT_INTERVAL;
  int MSG_OP_COMPLETE;
  int MSG_BACKUP_RESTORE_STEP;
  int MSG_RUN_FULL_RESTORE;
  int MSG_FULL_CONFIRMATION_TIMEOUT;
  int MSG_RESTORE_TIMEOUT;
  int MSG_TIMEOUT;
  int MSG_RUN_GET_RESTORE_SETS;
  int MSG_RUN_INITIALIZE;
  int MSG_RUN_CLEAR;
  int MSG_RUN_RESTORE;
  int MSG_RUN_FULL_BACKUP;
  int MSG_RUN_BACKUP;
  int RUN_CLEAR_ACTION;
  int RUN_INITIALIZE_ACTION;
  int RUN_BACKUP_ACTION;
  int FIRST_BACKUP_INTERVAL;
  int FUZZ_MILLIS;
  int BACKUP_INTERVAL;
  int SHARED_BACKUP_AGENT_PACKAGE;
  int COMPRESS_FULL_BACKUPS;
  int BACKUP_FILE_VERSION;
  int BACKUP_FILE_HEADER_MAGIC;
  int BACKUP_MANIFEST_VERSION;
  int BACKUP_MANIFEST_FILENAME;
  int MORE_DEBUG;
  int DEBUG;
  int TAG;
}
class AttributeCache {
  class Entry {
    int array;
    int context;
  }
  class Package {
    int mMap;
    int context;
  }
  int mConfiguration;
  int mPackages;
  int mContext;
  int sInstance;
}
class AppWidgetServiceImpl {
  int mDeletedHosts;
  int mDeletedProviders;
  int mMaxWidgetBitmapMemory;
  int mStateLoaded;
  int mUserId;
  int mSafeMode;
  int mPackagesWithBindWidgetPermission;
  int mHosts;
  int mAppWidgetIds;
  int mNextAppWidgetId;
  int mInstalledProviders;
  int mAlarmManager;
  int mPm;
  int mLocale;
  int mContext;
  int mRemoteViewsServicesAppWidgets;
  int mBoundRemoteViewsServices;
  class ServiceConnectionProxy {
    int mConnectionCb;
  }
  class AppWidgetId {
    int host;
    int options;
    int views;
    int provider;
    int appWidgetId;
  }
  class Host {
    int tag;
    int zombie;
    int callbacks;
    int instances;
    int packageName;
    int hostId;
    int uid;
  }
  class Provider {
    int tag;
    int zombie;
    int broadcast;
    int instances;
    int info;
    int uid;
  }
  int MIN_UPDATE_PERIOD;
  int SETTINGS_FILENAME;
  int TAG;
}
class AppWidgetService {
  int mBroadcastReceiver;
  int mAppWidgetServices;
  int mSafeMode;
  int mHosts;
  int mAppWidgetIds;
  int mNextAppWidgetId;
  int mInstalledProviders;
  int mAlarmManager;
  int mPackageManager;
  int mLocale;
  int mContext;
  class ServiceConnectionProxy {
    int mConnectionCb;
  }
  class AppWidgetId {
    int host;
    int views;
    int provider;
    int appWidgetId;
  }
  class Host {
    int tag;
    int zombie;
    int callbacks;
    int instances;
    int packageName;
    int hostId;
    int uid;
  }
  class Provider {
    int tag;
    int zombie;
    int broadcast;
    int instances;
    int info;
    int uid;
  }
  int TAG;
}
class AlarmManagerService {
  class ResultReceiver {
  }
  class UninstallReceiver {
  }
  class ClockReceiver {
  }
  class AlarmHandler {
    int DATE_CHANGE_EVENT;
    int MINUTE_CHANGE_EVENT;
    int ALARM_EVENT;
  }
  class AlarmThread {
  }
  class Alarm {
    int operation;
    int repeatInterval;
    int when;
    int count;
    int type;
  }
  class IncreasingTimeOrder {
  }
  int mBroadcastStats;
  class BroadcastStats {
    int filterStats;
    int nesting;
    int startTime;
    int numWakeup;
    int aggregateTime;
  }
  class FilterStats {
    int count;
  }
  int mDateChangeSender;
  int mTimeTickSender;
  int mResultReceiver;
  int mUninstallReceiver;
  int mClockReceiver;
  int mHandler;
  int mWaitThread;
  int mInFlight;
  int mWakeLock;
  int mBroadcastRefCount;
  int mDescriptor;
  int mIncreasingTimeOrder;
  int mElapsedRealtimeAlarms;
  int mElapsedRealtimeWakeupAlarms;
  int mRtcAlarms;
  int mRtcWakeupAlarms;
  int mLock;
  int mContext;
  int mBackgroundIntent;
  int TIMEZONE_PROPERTY;
  int ALARM_EVENT;
  int localLOGV;
  int ClockReceiver_TAG;
  int TAG;
  int QUANTUM;
  int TIME_CHANGED_MASK;
  int ELAPSED_REALTIME_MASK;
  int ELAPSED_REALTIME_WAKEUP_MASK;
  int RTC_MASK;
  int RTC_WAKEUP_MASK;
  int LATE_ALARM_THRESHOLD;
}
class AccessibilityManagerTest {
  class AnyIAccessibilityManagerClientMather {
  }
  class AccessibilityEventMather {
    int mExpectedEvent;
  }
  int mMockServiceInterface;
  int TIMEOUT_BINDER_CALL;
}
class AccessibilityManagerServiceTest {
  class MySecondMockAccessibilityService {
    int sInstance;
    int sComponentName;
  }
  class MyFirstMockAccessibilityService {
    int sInstance;
    int sComponentName;
  }
  class MyMockAccessibilityManagerClient {
    int mState;
  }
  int mManagerService;
  int TIMEOUT_TEST_NOTIFICATION_TIMEOUT;
  int TIMEOUT_START_MOCK_ACCESSIBILITY_SERVICES;
  int TIMEOUT_BINDER_CALL;
}
