package com.android.internal.policy.impl;
class SimUnlockScreen {
  class TouchInput {
    int mCancelButton;
    int mNine;
    int mEight;
    int mSeven;
    int mSix;
    int mFive;
    int mFour;
    int mThree;
    int mTwo;
    int mOne;
    int mZero;
  }
  class CheckSimPin {
    int mPin;
  }
  int DIGITS;
  int mKeyguardStatusViewManager;
  int mKeyboardHidden;
  int mCreationOrientation;
  int mLockPatternUtils;
  int mSimUnlockProgressDialog;
  int mEnteredDigits;
  int mEnteredPin;
  int mBackSpaceButton;
  int mOkButton;
  int mPinText;
  int mHeaderText;
  int mCallback;
  int mUpdateMonitor;
  int DIGIT_PRESS_WAKE_MILLIS;
}
class SimPukUnlockScreen {
  class TouchInput {
    int mCancelButton;
    int mNine;
    int mEight;
    int mSeven;
    int mSix;
    int mFive;
    int mFour;
    int mThree;
    int mTwo;
    int mOne;
    int mZero;
  }
  class CheckSimPuk {
    int mPuk;
    int mPin;
  }
  int DIGITS;
  int mKeyboardHidden;
  int mCreationOrientation;
  int mLockPatternUtils;
  int mSimUnlockProgressDialog;
  int mDelPinButton;
  int mDelPukButton;
  int mOkButton;
  int mFocusedEntry;
  int mPinText;
  int mPukText;
  int mHeaderText;
  int mKeyguardStatusViewManager;
  int mCallback;
  int mUpdateMonitor;
  int DIGIT_PRESS_WAKE_MILLIS;
}
class ShortcutManager {
  int mShortcutIntents;
  int mCursor;
  int mContext;
  int sProjection;
  int COLUMN_INTENT;
  int COLUMN_SHORTCUT;
  int TAG;
}
class RecentApplicationsDialog {
  int mBroadcastReceiver;
  int mCleanup;
  int mHandler;
  class RecentTag {
    int intent;
    int info;
  }
  int mBroadcastIntentFilter;
  int mNoAppsText;
  int mIcons;
  int MAX_RECENT_TASKS;
  int NUM_BUTTONS;
  int sStatusBar;
  int DBG_FORCE_EMPTY_LIST;
}
class RecentApplicationsBackground {
  int mTmp1;
  int mTmp0;
  int mBackground;
  int mBackgroundSizeChanged;
  int TAG;
}
class Policy {
  int preload_classes;
  int TAG;
}
class PhoneWindowManager {
  int mScreenLockTimeout;
  int mBootMsgDialog;
  int mPowerReceiver;
  int mDockReceiver;
  int mScreenshotTimeout;
  int mScreenshotConnection;
  int mScreenshotLock;
  int mHideNavInputEventReceiverFactory;
  class HideNavInputEventReceiver {
  }
  int mAllowSystemUiDelay;
  int WINDOW_TYPES_WHERE_HOME_DOESNT_WORK;
  int PRINT_ANIM;
  int mScreenshotChordLongPress;
  int mPowerLongPress;
  int mOrientationListener;
  class MyOrientationListener {
  }
  class SettingsObserver {
  }
  int mHDMIObserver;
  class PolicyHandler {
  }
  int MSG_DISPATCH_MEDIA_KEY_REPEAT_WITH_WAKE_LOCK;
  int MSG_DISPATCH_MEDIA_KEY_WITH_WAKE_LOCK;
  int MSG_DISABLE_POINTER_LOCATION;
  int MSG_ENABLE_POINTER_LOCATION;
  int mFallbackActions;
  int mHavePendingMediaKeyRepeatWithWakeLock;
  int mBroadcastWakeLock;
  int mShortcutManager;
  int mPowerKeyTime;
  int mPowerKeyTriggered;
  int mVolumeUpKeyTriggered;
  int mVolumeDownKeyConsumedByScreenshotChord;
  int mVolumeDownKeyTime;
  int mVolumeDownKeyTriggered;
  int mScreenshotChordEnabled;
  int SCREENSHOT_CHORD_DEBOUNCE_DELAY_MILLIS;
  int mLongPressOnHomeBehavior;
  int mUpsideDownRotation;
  int mPortraitRotation;
  int mSeascapeRotation;
  int mLandscapeRotation;
  int mDisplay;
  int mIncallPowerBehavior;
  int mEndcallBehavior;
  int mPluggedIn;
  int mScreenSaverMayRun;
  int mScreenSaverEnabledByUser;
  int mScreenSaverTimeout;
  int mScreenSaverFeatureAvailable;
  int mLockScreenTimerActive;
  int mLockScreenTimeout;
  int mAllowLockscreenWhenOn;
  int mAssistKeyLongPressed;
  int mConsumeSearchKeyUp;
  int mSearchKeyShortcutPending;
  int mDeskDockIntent;
  int mCarDockIntent;
  int mHomeIntent;
  int mHomeLongPressed;
  int mHomePressed;
  int mDismissKeyguard;
  int mHideLockScreen;
  int mForceStatusBar;
  int mTopIsFullscreen;
  int mTopFullscreenOpaqueWindowState;
  int mTmpNavigationFrame;
  int mTmpVisibleFrame;
  int mTmpContentFrame;
  int mTmpDisplayFrame;
  int mTmpParentFrame;
  int mHideNavFakeWindow;
  int mLastFocusNeedsMenu;
  int mForceClearedSystemUiFlags;
  int mResettingSystemUiFlags;
  int mLastSystemUiFlags;
  int mStatusBarLayer;
  int mDockLayer;
  int mDockBottom;
  int mDockRight;
  int mDockTop;
  int mDockLeft;
  int mContentBottom;
  int mContentRight;
  int mContentTop;
  int mContentLeft;
  int mCurBottom;
  int mCurRight;
  int mCurTop;
  int mCurLeft;
  int mStableFullscreenBottom;
  int mStableFullscreenRight;
  int mStableFullscreenTop;
  int mStableFullscreenLeft;
  int mStableBottom;
  int mStableRight;
  int mStableTop;
  int mStableLeft;
  int mSystemBottom;
  int mSystemRight;
  int mSystemTop;
  int mSystemLeft;
  int mRestrictedScreenHeight;
  int mRestrictedScreenWidth;
  int mRestrictedScreenTop;
  int mRestrictedScreenLeft;
  int mUnrestrictedScreenHeight;
  int mUnrestrictedScreenWidth;
  int mUnrestrictedScreenTop;
  int mUnrestrictedScreenLeft;
  int mPointerLocationInputChannel;
  int mPointerLocationView;
  int mPointerLocationInputEventReceiver;
  class PointerLocationInputEventReceiver {
    int mView;
  }
  int mFocusedApp;
  int mFocusedWindow;
  int mPointerLocationMode;
  int mHasSoftInput;
  int mCurrentAppOrientation;
  int mOrientationSensorEnabled;
  int mScreenOnFully;
  int mScreenOnEarly;
  int mLongPressOnPowerBehavior;
  int mLidControlsSleep;
  int mLidNavigationAccessibility;
  int mLidKeyboardAccessibility;
  int mDeskDockEnablesAccelerometer;
  int mCarDockEnablesAccelerometer;
  int mAllowAllRotations;
  int mAccelerometerDefault;
  int mUserRotation;
  int mUserRotationMode;
  int mHdmiRotation;
  int mDeskDockRotation;
  int mCarDockRotation;
  int mLidOpenRotation;
  int mDockMode;
  int mUiMode;
  int mExternalDisplayHeight;
  int mExternalDisplayWidth;
  int mHdmiPlugged;
  int mSystemBooted;
  int mSystemReady;
  int mHaveBuiltInKeyboard;
  int mLidState;
  int mLanguageSwitchKeyPressed;
  int mRecentAppsDialogHeldModifiers;
  int mRecentAppsDialog;
  int RECENT_APPS_BEHAVIOR_DISMISS_AND_SWITCH;
  int RECENT_APPS_BEHAVIOR_DISMISS;
  int RECENT_APPS_BEHAVIOR_EXIT_TOUCH_MODE_AND_SHOW;
  int RECENT_APPS_BEHAVIOR_SHOW_OR_DISMISS;
  int mLastInputMethodTargetWindow;
  int mLastInputMethodWindow;
  int mHandler;
  int mPendingPowerKeyUpCanceled;
  int mPowerKeyHandled;
  int mGlobalActions;
  int mKeyguardMediator;
  int mKeyguard;
  int mNavigationBarWidthForRotation;
  int mNavigationBarHeightForRotation;
  int mNavigationBarOnBottom;
  int mNavigationBarCanMove;
  int mCanHideNavigationBar;
  int mHasNavigationBar;
  int mNavigationBar;
  int mStatusBarHeight;
  int mHasSystemNavBar;
  int mStatusBar;
  int mSafeMode;
  int mHeadless;
  int mEnableShiftMenuBugReports;
  int mSafeModeEnabledVibePattern;
  int mSafeModeDisabledVibePattern;
  int mKeyboardTapVibePattern;
  int mVirtualKeyVibePattern;
  int mLongPressVibePattern;
  int mSearchManager;
  int mVibrator;
  int mServiceAquireLock;
  int mStatusBarService;
  int mPowerManager;
  int mWindowManagerFuncs;
  int mWindowManager;
  int mContext;
  int mLock;
  int sApplicationLaunchKeyCategories;
  int SYSTEM_UI_CHANGING_LAYOUT;
  int SYSTEM_DIALOG_REASON_ASSIST;
  int SYSTEM_DIALOG_REASON_HOME_KEY;
  int SYSTEM_DIALOG_REASON_RECENT_APPS;
  int SYSTEM_DIALOG_REASON_GLOBAL_ACTIONS;
  int SYSTEM_DIALOG_REASON_KEY;
  int APPLICATION_SUB_PANEL_SUBLAYER;
  int APPLICATION_PANEL_SUBLAYER;
  int APPLICATION_MEDIA_OVERLAY_SUBLAYER;
  int APPLICATION_MEDIA_SUBLAYER;
  int HIDDEN_NAV_CONSUMER_LAYER;
  int POINTER_LAYER;
  int BOOT_PROGRESS_LAYER;
  int SECURE_SYSTEM_OVERLAY_LAYER;
  int DRAG_LAYER;
  int SYSTEM_ERROR_LAYER;
  int NAVIGATION_BAR_PANEL_LAYER;
  int NAVIGATION_BAR_LAYER;
  int SYSTEM_OVERLAY_LAYER;
  int VOLUME_OVERLAY_LAYER;
  int STATUS_BAR_PANEL_LAYER;
  int STATUS_BAR_LAYER;
  int STATUS_BAR_SUB_PANEL_LAYER;
  int SCREENSAVER_LAYER;
  int KEYGUARD_DIALOG_LAYER;
  int KEYGUARD_LAYER;
  int INPUT_METHOD_DIALOG_LAYER;
  int INPUT_METHOD_LAYER;
  int SYSTEM_ALERT_LAYER;
  int PRIORITY_PHONE_LAYER;
  int TOAST_LAYER;
  int SYSTEM_DIALOG_LAYER;
  int SEARCH_BAR_LAYER;
  int PHONE_LAYER;
  int APPLICATION_LAYER;
  int WALLPAPER_LAYER;
  int LONG_PRESS_HOME_RECENT_SYSTEM_UI;
  int LONG_PRESS_HOME_RECENT_DIALOG;
  int LONG_PRESS_HOME_NOTHING;
  int LONG_PRESS_POWER_SHUT_OFF;
  int LONG_PRESS_POWER_GLOBAL_ACTIONS;
  int LONG_PRESS_POWER_NOTHING;
  int SEPARATE_TIMEOUT_FOR_SCREEN_SAVER;
  int ENABLE_DESK_DOCK_HOME_CAPTURE;
  int ENABLE_CAR_DOCK_HOME_CAPTURE;
  int SHOW_PROCESSES_ON_ALT_MENU;
  int SHOW_STARTING_ANIMATIONS;
  int DEBUG_STARTING_WINDOW;
  int DEBUG_INPUT;
  int DEBUG_LAYOUT;
  int localLOGV;
  int DEBUG;
  int TAG;
}
class PhoneWindow {
  class DialogMenuCallback {
    int mSubMenuHelper;
    int mFeatureId;
  }
  class RotationWatcher {
    int mIsWatching;
    int mWindows;
    int mRotationChanged;
    int mHandler;
  }
  class PanelFeatureState {
    class SavedState {
      int CREATOR;
      int menuState;
      int isInExpandedMode;
      int isOpen;
      int featureId;
    }
    int frozenActionViewState;
    int frozenMenuState;
    int wasLastExpanded;
    int wasLastOpen;
    int refreshMenuContent;
    int refreshDecorView;
    int qwertyMode;
    int isInExpandedMode;
    int isOpen;
    int isHandled;
    int isPrepared;
    int listPresenterTheme;
    int isCompact;
    int listMenuPresenter;
    int iconMenuPresenter;
    int menu;
    int shownPanelView;
    int createdPanelView;
    int decorView;
    int windowAnimations;
    int y;
    int x;
    int gravity;
    int fullBackground;
    int background;
    int featureId;
  }
  class DrawableFeatureState {
    int curAlpha;
    int alpha;
    int cur;
    int def;
    int child;
    int local;
    int uri;
    int resid;
    int featureId;
  }
  class DecorView {
    class ActionModeCallbackWrapper {
      int mWrapped;
    }
    int mShowActionModePopup;
    int mActionModePopup;
    int mActionModeView;
    int mActionMode;
    int mDownY;
    int mWatchingForMenu;
    int mMenuBackground;
    int mChanging;
    int mFrameOffsets;
    int mFramePadding;
    int mBackgroundPadding;
    int mDrawingBounds;
    int mFeatureId;
    int mDefaultOpacity;
  }
  class ActionMenuPresenterCallback {
  }
  class PanelMenuPresenterCallback {
  }
  int ACTION_BAR_TAG;
  int PANELS_TAG;
  int VIEWS_TAG;
  int FOCUSED_ID_TAG;
  int sRotationWatcher;
  class WindowManagerHolder {
    int sWindowManager;
  }
  int mUiOptions;
  int mKeyguardManager;
  int mAudioManager;
  int mVolumeControlStreamType;
  int mClosingActionMenu;
  int mContextMenuHelper;
  int mContextMenu;
  int mAlwaysReadCloseOnTouchAttr;
  int mTitleColor;
  int mTitle;
  int mTextColor;
  int mFrameResource;
  int mBackgroundDrawable;
  int mBackgroundResource;
  int mHorizontalProgressBar;
  int mCircularProgressBar;
  int mRightIconView;
  int mLeftIconView;
  int mPanelChordingKey;
  int mPreparedPanel;
  int mPanels;
  int mDrawables;
  int mPanelMenuPresenterCallback;
  int mActionMenuPresenterCallback;
  int mActionBar;
  int mTitleView;
  int mLayoutInflater;
  int mIsFloating;
  int mTakeInputQueueCallback;
  int mTakeSurfaceCallback;
  int mContentParent;
  int mDecor;
  int mFixedHeightMinor;
  int mFixedHeightMajor;
  int mFixedWidthMinor;
  int mFixedWidthMajor;
  int mMinWidthMinor;
  int mMinWidthMajor;
  int mContextMenuCallback;
  int SWEEP_OPEN_MENU;
  int TAG;
}
class PhoneLayoutInflater {
  int sClassPrefixList;
}
class PhoneFallbackEventHandler {
  int mTelephonyManager;
  int mSearchManager;
  int mKeyguardManager;
  int mAudioManager;
  int mView;
  int mContext;
  int DEBUG;
  int TAG;
}
class PatternUnlockScreen {
  class UnlockPatternListener {
  }
  class FooterMode {
    int VerifyUnlocked;
    int ForgotLockPattern;
    int Normal;
  }
  int mCreationOrientation;
  int mForgotPatternButton;
  int mForgotPatternClick;
  int mCancelPatternRunnable;
  int mLastPokeTime;
  int mLockPatternView;
  int mKeyguardStatusViewManager;
  int mEnableFallback;
  int mCallback;
  int mUpdateMonitor;
  int mLockPatternUtils;
  int mCountdownTimer;
  int mTotalFailedPatternAttempts;
  int mFailedPatternAttemptsSinceLastTimeout;
  int MIN_PATTERN_BEFORE_POKE_WAKELOCK;
  int UNLOCK_PATTERN_WAKE_INTERVAL_FIRST_DOTS_MS;
  int UNLOCK_PATTERN_WAKE_INTERVAL_MS;
  int PATTERN_CLEAR_TIMEOUT_MS;
  int TAG;
  int DEBUG;
}
class PasswordUnlockScreen {
  int MINIMUM_PASSWORD_LENGTH_BEFORE_REPORT;
  int mResuming;
  int mUseSystemIME;
  int mStatusViewManager;
  int mCreationHardKeyboardHidden;
  int mCreationOrientation;
  int mKeyboardHelper;
  int mKeyboardView;
  int mLockPatternUtils;
  int mPasswordEntry;
  int mIsAlpha;
  int mCallback;
  int mUpdateMonitor;
  int TAG;
}
class LockScreen {
  int mOnResumePing;
  class GlowPadViewMethods {
    int mGlowPadView;
  }
  class WaveViewMethods {
    int mWaveView;
  }
  class SlidingTabMethods {
    int mSlidingTab;
  }
  class UnlockWidgetCommonMethods {
  }
  int mSimStateCallback;
  int mInfoCallback;
  int mHasVibrator;
  int mSearchDisabled;
  int mCameraDisabled;
  int mUnlockWidget;
  int mUnlockWidgetMethods;
  int mStatusViewManager;
  int mEnableMenuKeyInLockScreen;
  int mAudioManager;
  int mSilentMode;
  int mCreationOrientation;
  int mEnableRingSilenceFallback;
  int mCallback;
  int mUpdateMonitor;
  int mLockPatternUtils;
  int ASSIST_ICON_METADATA_NAME;
  int STAY_ON_WHILE_GRABBED_TIMEOUT;
  int WAIT_FOR_ANIMATION_TIMEOUT;
  int ENABLE_MENU_KEY_FILE;
  int TAG;
  int DBG;
  int ON_RESUME_PING_DELAY;
}
class LockPatternKeyguardViewTest {
  class MockKeyguardCallback {
    int mKeyguardDoneCount;
    int mPokeWakelockCount;
  }
  class TestableLockPatternKeyguardView {
    int mInjectedUnlockScreens;
    int mInjectedLockScreens;
  }
  class MockKeyguardScreen {
    int mCleanupCount;
    int mOnResumeCount;
    int mOnPauseCount;
  }
  class MockLockPatternUtils {
    int isPermanentlyLocked;
    int isLockPatternEnabled;
  }
  class MockUpdateMonitor {
    int simState;
  }
  int mKeyguardViewCallback;
  int mLPKV;
  int mLockPatternUtils;
  int mUpdateMonitor;
}
class LockPatternKeyguardViewProperties {
  int mUpdateMonitor;
  int mLockPatternUtils;
}
class LockPatternKeyguardView {
  class FastBitmapDrawable {
    int mOpacity;
    int mBitmap;
  }
  int mInfoCallback;
  class AccountAnalyzer {
    int mAccountIndex;
    int mAccounts;
    int mAccountManager;
  }
  int mKeyguardScreenCallback;
  int mWidgetCallback;
  int mRecreateRunnable;
  int mConfiguration;
  int mLockPatternUtils;
  int mIsVerifyUnlockOnly;
  int mForgotPattern;
  int mUnlockScreenMode;
  int mMode;
  class UnlockMode {
    int Unknown;
    int Password;
    int Account;
    int SimPuk;
    int SimPin;
    int Pattern;
  }
  class Mode {
    int UnlockScreen;
    int LockScreen;
  }
  int mSavedState;
  int mTransportControlView;
  int sIsFirstAppearanceAfterBoot;
  int mPluggedIn;
  int mHasDialog;
  int mSuppressBiometricUnlock;
  int mRequiresSim;
  int BIOMETRIC_AREA_EMERGENCY_DIALER_TIMEOUT;
  int mBiometricUnlockStartupLock;
  int mBiometricUnlock;
  int mShowLockBeforeUnlock;
  int mEnableFallback;
  int mWindowFocused;
  int mScreenOn;
  int mUnlockScreen;
  int mLockScreen;
  int mWindowController;
  int mUpdateMonitor;
  int TAG;
  int DEBUG;
  int ACTION_EMERGENCY_DIAL;
  int EMERGENCY_CALL_TIMEOUT;
  int DEBUG_CONFIGURATION;
  int TRANSPORT_USERACTIVITY_TIMEOUT;
}
class KeyguardWindowController {
}
class KeyguardViewProperties {
}
class KeyguardViewMediator {
  int mHandler;
  int mBroadCastReceiver;
  int mUserChangeReceiver;
  int mInfoCallback;
  int mLockSoundVolume;
  int mLockSoundStreamId;
  int mUnlockSoundId;
  int mLockSoundId;
  int mLockSounds;
  int mLockPatternUtils;
  int mWaitingUntilKeyguardVisible;
  int mUserPresentIntent;
  int mPhoneState;
  int mScreenOn;
  int mUpdateMonitor;
  int mKeyguardViewProperties;
  int mExitSecureCallback;
  int mCallback;
  int mWakelockSequence;
  int mDelayedShowingSequence;
  int mHidden;
  int mShowing;
  int mNeedToReshowWhenReenabled;
  int mExternallyEnabled;
  int mKeyguardViewManager;
  int mWakeAndHandOff;
  int mShowKeyguardWakeLock;
  int mWakeLock;
  int mPM;
  int mRealPowerManager;
  int mSuppressNextLockSound;
  int mSystemReady;
  int mShowingLockIcon;
  int mShowLockIcon;
  int mStatusBarManager;
  int mAudioManager;
  int mAlarmManager;
  int mContext;
  int mMasterStreamType;
  int ENABLE_INSECURE_STATUS_BAR_EXPAND;
  int KEYGUARD_DONE_DRAWING_TIMEOUT_MS;
  int KEYGUARD_LOCK_AFTER_DELAY_DEFAULT;
  int AWAKE_INTERVAL_DEFAULT_MS;
  int KEYGUARD_TIMEOUT;
  int SET_HIDDEN;
  int KEYGUARD_DONE_AUTHENTICATING;
  int KEYGUARD_DONE_DRAWING;
  int KEYGUARD_DONE;
  int WAKE_WHEN_READY;
  int NOTIFY_SCREEN_ON;
  int NOTIFY_SCREEN_OFF;
  int VERIFY_UNLOCK;
  int RESET;
  int HIDE;
  int SHOW;
  int TIMEOUT;
  int DELAYED_KEYGUARD_ACTION;
  int TAG;
  int DBG_WAKE;
  int DEBUG;
  int KEYGUARD_DISPLAY_TIMEOUT_DELAY_DEFAULT;
}
class KeyguardViewManager {
  class KeyguardViewHost {
    int mCallback;
  }
  class ShowListener {
  }
  int mScreenOn;
  int mKeyguardView;
  int mKeyguardHost;
  int mNeedsInput;
  int mWindowLayoutParams;
  int mUpdateMonitor;
  int mKeyguardViewProperties;
  int mCallback;
  int mViewManager;
  int mContext;
  int TAG;
  int DEBUG;
}
class KeyguardViewCallback {
}
class KeyguardViewBase {
  int mBackgroundDrawable;
  int KEYGUARD_MANAGES_VOLUME;
  int mTelephonyManager;
  int mAudioManager;
  int mCallback;
  int BACKGROUND_COLOR;
}
class KeyguardUpdateMonitor {
  class SimStateCallback {
  }
  class InfoCallbackImpl {
  }
  class InfoCallback {
  }
  class BatteryStatus {
    int health;
    int plugged;
    int level;
    int status;
  }
  class SimArgs {
    int simState;
  }
  int DEBUG_SIM_STATES;
  int MSG_USER_CHANGED;
  int MSG_DPM_STATE_CHANGED;
  int MSG_DEVICE_PROVISIONED;
  int MSG_CLOCK_VISIBILITY_CHANGED;
  int MSG_PHONE_STATE_CHANGED;
  int MSG_RINGER_MODE_CHANGED;
  int MSG_SIM_STATE_CHANGE;
  int MSG_CARRIER_INFO_UPDATE;
  int MSG_BATTERY_UPDATE;
  int MSG_TIME_UPDATE;
  int mPhoneState;
  int mRingMode;
  int mContentObserver;
  int mSimStateCallbacks;
  int mInfoCallbacks;
  int mHandler;
  int mClockVisible;
  int FAILED_BIOMETRIC_UNLOCK_ATTEMPTS_BEFORE_BACKUP;
  int mFailedBiometricUnlockAttempts;
  int mFailedAttempts;
  int mTelephonySpn;
  int mTelephonyPlmn;
  int mBatteryStatus;
  int mDeviceProvisioned;
  int mSimState;
  int mContext;
  int LOW_BATTERY_THRESHOLD;
  int DEBUG;
  int TAG;
}
class KeyguardStatusViewManager {
  int mSimStateCallback;
  int mInfoCallback;
  class StatusMode {
    int SimPermDisabled;
    int SimLocked;
    int SimPukLocked;
    int SimMissingLocked;
    int SimMissing;
    int NetworkLocked;
    int Normal;
    int mShowStatusLines;
  }
  class TransientTextManager {
    int mMessages;
    class Data {
      int text;
      int icon;
    }
    int mTextView;
  }
  int mDigitalClock;
  int mPhoneState;
  int mSpn;
  int mPlmn;
  int mEmergencyCallButtonEnabledInScreen;
  int mCallback;
  int mShowingStatus;
  int mOwnerInfoText;
  int mInstructionText;
  int mHelpMessageText;
  int mCarrierHelpText;
  int mCarrierText;
  int mEmergencyButtonEnabledBecauseSimLocked;
  int mEmergencyCallButton;
  int mUpdateMonitor;
  int mLockPatternUtils;
  int mSimState;
  int mBatteryLevel;
  int mPluggedIn;
  int mShowingBatteryInfo;
  int mContainer;
  int mTransportView;
  int mAlarmStatusView;
  int mOwnerInfoView;
  int mStatus1View;
  int mDateView;
  int mCarrierView;
  int mTransientTextManager;
  int mDateFormatString;
  int mStatus;
  int BATTERY_INFO;
  int OWNER_INFO;
  int HELP_MESSAGE_TEXT;
  int CARRIER_HELP_TEXT;
  int CARRIER_TEXT;
  int INSTRUCTION_TEXT;
  int INSTRUCTION_RESET_DELAY;
  int BATTERY_LOW_ICON;
  int CHARGING_ICON;
  int ALARM_ICON;
  int LOCK_ICON;
  int TAG;
  int DEBUG;
}
class KeyguardScreenCallback {
}
class KeyguardScreen {
}
class IconUtilities {
  int mColorIndex;
  int mDisplayMetrics;
  int mCanvas;
  int mOldBounds;
  int mGlowColorFocusedPaint;
  int mGlowColorPressedPaint;
  int mBlurPaint;
  int mPaint;
  int mIconTextureHeight;
  int mIconTextureWidth;
  int mIconHeight;
  int mIconWidth;
  int sColors;
  int TAG;
}
class GlobalActions {
  int mHandler;
  int DIALOG_DISMISS_DELAY;
  int MESSAGE_SHOW;
  int MESSAGE_REFRESH;
  int MESSAGE_DISMISS;
  int mAirplaneModeObserver;
  int mRingerModeReceiver;
  int mPhoneStateListener;
  int mBroadcastReceiver;
  class SilentModeTriStateAction {
    int mContext;
    int mHandler;
    int mAudioManager;
    int ITEM_IDS;
  }
  class SilentModeToggleAction {
  }
  class ToggleAction {
    int mDisabledStatusMessageResId;
    int mEnabledStatusMessageResId;
    int mMessageResId;
    int mDisabledIconResid;
    int mEnabledIconResId;
    int mState;
    class State {
      int On;
      int TurningOff;
      int TurningOn;
      int Off;
      int inTransition;
    }
  }
  class SinglePressAction {
    int mMessage;
    int mMessageResId;
    int mIconResId;
  }
  class Action {
  }
  class MyAdapter {
  }
  int mIWindowManager;
  int mHasVibrator;
  int mHasTelephony;
  int mIsWaitingForEcmExit;
  int mAirplaneState;
  int mDeviceProvisioned;
  int mKeyguardShowing;
  int mAdapter;
  int mAirplaneModeOn;
  int mSilentModeAction;
  int mDialog;
  int mItems;
  int mAudioManager;
  int mWindowManagerFuncs;
  int mContext;
  int SHOW_SILENT_TOGGLE;
  int TAG;
}
class FaceUnlock {
  int mFaceUnlockCallback;
  int mConnection;
  int mKeyguardScreenCallback;
  int BACKUP_LOCK_TIMEOUT;
  int SERVICE_STARTUP_VIEW_TIMEOUT;
  int mIsRunning;
  int MSG_POKE_WAKELOCK;
  int MSG_EXPOSE_FALLBACK;
  int MSG_REPORT_FAILED_ATTEMPT;
  int MSG_CANCEL;
  int MSG_UNLOCK;
  int MSG_SERVICE_DISCONNECTED;
  int MSG_SERVICE_CONNECTED;
  int MSG_HIDE_FACE_UNLOCK_VIEW;
  int MSG_SHOW_FACE_UNLOCK_VIEW;
  int mHandler;
  int mFaceUnlockView;
  int mBoundToService;
  int mService;
  int mServiceRunningLock;
  int mServiceRunning;
  int mUpdateMonitor;
  int mLockPatternUtils;
  int mContext;
  int TAG;
  int DEBUG;
}
class BiometricSensorUnlock {
}
class AccountUnlockScreen {
  int mKeyguardStatusViewManager;
  int mCheckingDialog;
  int mOk;
  int mPassword;
  int mLogin;
  int mInstructions;
  int mTopHeader;
  int mUpdateMonitor;
  int mLockPatternUtils;
  int mCallback;
  int AWAKE_POKE_MILLIS;
  int LOCK_PATTERN_CLASS;
  int LOCK_PATTERN_PACKAGE;
}
