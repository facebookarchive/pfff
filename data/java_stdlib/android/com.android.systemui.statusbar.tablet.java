package com.android.systemui.statusbar.tablet;
class TabletTicker {
  int mWindowShouldClose;
  int mLayoutTransition;
  int mBar;
  int mLargeIconHeight;
  int mQueuePos;
  int mQueue;
  int mKeys;
  int mCurrentView;
  int mCurrentNotification;
  int mCurrentKey;
  int mWindow;
  int mContext;
  int ADVANCE_DELAY;
  int MSG_ADVANCE;
  int QUEUE_LENGTH;
  int CLICKABLE_TICKER;
  int TAG;
}
class TabletStatusBarView {
  int mDelegateHelper;
  int mPos;
  int mPanels;
  int mIgnoreChildren;
  int MAX_PANELS;
  int mHandler;
}
class TabletStatusBar {
  int mBroadcastReceiver;
  class NotificationIconTouchListener {
    int mTouchSlop;
    int mInitialTouchY;
    int mInitialTouchX;
    int mPeekIndex;
    int mVT;
  }
  class NotificationTriggerTouchListener {
    int mHiliteOnR;
    int mTouchSlop;
    int mInitialTouchY;
    int mInitialTouchX;
    int mVT;
  }
  int mOnClickListener;
  class H {
  }
  int mHomeSearchActionListener;
  int mShowSearchPanel;
  int mShowSearchHoldoff;
  int mNavigationIconHints;
  int mSystemUiVisibility;
  int mCompatModePanel;
  int mInputMethodsPanel;
  int mDisabled;
  int mCompatibilityHelpDialog;
  int mSpaceBarKeyEvent;
  int mFakeSpaceBar;
  int mTicker;
  int mIconLayout;
  int mShadow;
  int mBarContents;
  int mDoNotDisturb;
  int mNetworkController;
  int mLocationController;
  int mBluetoothController;
  int mBatteryController;
  int mNotificationFlingVelocity;
  int mNotificationPeekTapDuration;
  int mNotificationPeekScrubRight;
  int mNotificationPeekScrubLeft;
  int mNotificationPeekKey;
  int mNotificationPeekIndex;
  int mNotificationPeekRow;
  int mNotificationPeekWindow;
  int mNotificationPanelParams;
  int mNotificationPanel;
  int mCompatModeButton;
  int mInputMethodSwitchButton;
  int mFeedbackIconArea;
  int mAltBackButtonEnabledForIme;
  int mRecentButton;
  int mMenuButton;
  int mHomeButton;
  int mBackButton;
  int mNotificationDNDDummyEntry;
  int mNotificationDNDMode;
  int mNavigationArea;
  int mNotificationIconArea;
  int mNotificationTrigger;
  int mNotificationArea;
  int mStatusBarView;
  int mWindowManager;
  int mMaxNotificationIcons;
  int mMenuNavIconWidth;
  int mNavIconWidth;
  int mIconHPadding;
  int mIconSize;
  int mNaturalBarHeight;
  int HIDE_ICONS_BELOW_SCORE;
  int NOTIFICATION_PRIORITY_MULTIPLIER;
  int NOTIFICATION_PEEK_FADE_DELAY;
  int NOTIFICATION_PEEK_HOLD_THRESH;
  int FAKE_SPACE_BAR;
  int MSG_STOP_TICKER;
  int MSG_CLOSE_COMPAT_MODE_PANEL;
  int MSG_OPEN_COMPAT_MODE_PANEL;
  int MSG_CLOSE_INPUT_METHODS_PANEL;
  int MSG_OPEN_INPUT_METHODS_PANEL;
  int MSG_HIDE_CHROME;
  int MSG_SHOW_CHROME;
  int MSG_CLOSE_NOTIFICATION_PEEK;
  int MSG_OPEN_NOTIFICATION_PEEK;
  int MSG_CLOSE_NOTIFICATION_PANEL;
  int MSG_OPEN_NOTIFICATION_PANEL;
  int TAG;
  int DEBUG_COMPAT_HELP;
  int DEBUG;
}
class StatusBarPanel {
}
class ShirtPocket {
  int mAnimShow;
  int mAnimHide;
  class DropZone {
    int mPocket;
  }
  int mPreviewIcon;
  int mClipping;
  int TAG;
  int DEBUG;
}
class SettingsView {
  int mRotationLockSeparator;
  int mRotationLockContainer;
  int mDoNotDisturb;
  int mBrightness;
  int mRotate;
  int mAirplane;
  int TAG;
}
class PanelBackgroundView {
}
class NotificationPeekPanel {
  int mBar;
}
class NotificationPanelTitle {
  int mSettingsButton;
  int buttons;
  int mPanel;
}
class NotificationPanel {
  class Choreographer {
    int HYPERSPACE_OFFRAMP;
    int CLOSE_DURATION;
    int OPEN_DURATION;
    int mContentAnim;
    int mPanelHeight;
    int mVisible;
  }
  int mPreDrawListener;
  int mClearButtonListener;
  int mChoreo;
  int mContentFrameMissingTranslation;
  int sDecelerateInterpolator;
  int sAccelerateInterpolator;
  int mClearButton;
  int mBar;
  int mContentParent;
  int mSettingsView;
  int mContentArea;
  int mContentFrame;
  int mNotificationScroller;
  int mNotificationButton;
  int mSettingsButton;
  int mTitleArea;
  int mNotificationCount;
  int mHasClearableNotifications;
  int mShowing;
  int PANEL_FADE_DURATION;
  int DEBUG;
  int TAG;
  int latestItems;
  int mExpandHelper;
}
class NotificationLinearLayout {
  int mTmp;
  int mInsetLeft;
  int mItemGlow;
  int TAG;
}
class NotificationIconArea {
  class IconLayout {
  }
  int mIconLayout;
  int TAG;
}
class NotificationArea {
}
class InputMethodsPanel {
  class OnHardKeyboardEnabledChangeListener {
  }
  class InputMethodComparator {
  }
  int mConfigureImeShortcut;
  int mLastSystemLocaleString;
  int mEnabledInputMethodAndSubtypesCacheStr;
  int mPackageManager;
  int mHardKeyboardSwitch;
  int mHardKeyboardSection;
  int mHardKeyboardEnabledChangeListener;
  int mHardKeyboardEnabled;
  int mHardKeyboardAvailable;
  int mInputMethodMenuList;
  int mInputMethodSwitchButton;
  int mToken;
  int mContext;
  int mPackageChanged;
  int mAttached;
  int mEnabledInputMethodAndSubtypesCache;
  int mRadioViewAndImiMap;
  int mIntentFilter;
  int mImm;
  int mBroadcastReceiver;
  int TAG;
  int DEBUG;
}
class InputMethodButton {
  int TAG_TRY_SUPPRESSING_IME_SWITCHER;
  int mHardKeyboardAvailable;
  int mScreenLocked;
  int mShowButton;
  int mToken;
  int mIcon;
  int mId;
  int mImm;
  int ID_IME_BUTTON_VISIBILITY_ALWAYS_HIDE;
  int ID_IME_BUTTON_VISIBILITY_ALWAYS_SHOW;
  int ID_IME_BUTTON_VISIBILITY_AUTO;
  int DEBUG;
  int TAG;
}
class CompatModePanel {
  int mTrigger;
  int mOffButton;
  int mOnButton;
  int mContext;
  int mAttached;
  int mAM;
  int TAG;
  int DEBUG;
}
