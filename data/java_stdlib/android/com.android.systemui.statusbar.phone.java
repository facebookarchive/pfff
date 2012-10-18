package com.android.systemui.statusbar.phone;
class TrackingPatternView {
  int mTextureHeight;
  int mTextureWidth;
  int mPaint;
  int mTexture;
}
class TickerView {
  int mTicker;
}
class Ticker {
  int mAdvanceTicker;
  class Segment {
    int first;
    int next;
    int current;
    int text;
    int icon;
    int notification;
  }
  int mIconScale;
  int mTextSwitcher;
  int mIconSwitcher;
  int mTickerView;
  int mPaint;
  int mSegments;
  int mHandler;
  int mContext;
  int TICKER_SEGMENT_DELAY;
}
class StatusBarWindowView {
  int mService;
  int latestItems;
  int mExpandHelper;
  int TAG;
}
class PhoneStatusBarView {
  int mCapturingEvents;
  int mButtonBounds;
  int mEndTime;
  int mEndAlpha;
  int mStartAlpha;
  int mNightMode;
  int mStatusIcons;
  int mNotificationIcons;
  int mStartY;
  int mStartX;
  int mTracking;
  int mService;
  int DIM_ANIM_TIME;
  int TAG;
}
class PhoneStatusBarPolicy {
  int mIntentReceiver;
  int mInetCondition;
  int mIsWifiConnected;
  int mLastWifiSignalLevel;
  int sWifiTemporarilyNotConnectedImage;
  int sWifiSignalImages;
  int mBluetoothEnabled;
  int mVolumeVisible;
  int mSimState;
  int mStorageManager;
  int mHandler;
  int mService;
  int mContext;
  int SHOW_SYNC_ICON;
  int INET_CONDITION_THRESHOLD;
  int AM_PM_STYLE;
  int AM_PM_STYLE_GONE;
  int AM_PM_STYLE_SMALL;
  int AM_PM_STYLE_NORMAL;
  int EVENT_BATTERY_CLOSE;
  int TAG;
}
class PhoneStatusBar {
  class FastColorDrawable {
    int mColor;
  }
  int mStopTracing;
  int mStartTracing;
  int mBroadcastReceiver;
  int mSettingsButtonListener;
  int mClearButtonListener;
  int mTickingDoneListener;
  class MyTicker {
  }
  class NotificationClicker {
    int mId;
    int mTag;
    int mPkg;
    int mIntent;
  }
  int mFocusChangeListener;
  int mRevealAnimationCallback;
  int mAnimationCallback;
  class H {
  }
  int mHomeSearchActionListener;
  int mShowSearchPanel;
  int mShowSearchHoldoff;
  int mRecentsClickListener;
  class ExpandedDialog {
  }
  int mPerformFling;
  int mPerformSelfExpandFling;
  int mStartRevealAnimation;
  int mMakeIconsInvisible;
  int mNavigationIconHints;
  int mDisplayMetrics;
  int mSystemUiVisibility;
  int mDisabled;
  int mLightsOnAnimation;
  int mLightsOutAnimation;
  int mPostCollapseCleanup;
  int mAbsPos;
  int mFlingY;
  int mFlingVelocity;
  int mViewDelta;
  int mAnimatingReveal;
  int mAnimLastTimeNanos;
  int mAnimAccel;
  int mAnimVel;
  int mAnimY;
  int mClosing;
  int mAnimating;
  int mChoreographer;
  int mVelocityTracker;
  int mTracking;
  int mEdgeBorder;
  int mTicking;
  int mTickerView;
  int mTicker;
  int mPanelSlightlyVisible;
  int mTrackingPosition;
  int mNavigationBarView;
  int mIntruderAlertView;
  int mDateView;
  int mExpandedVisible;
  int mExpanded;
  int mPositionTmp;
  int mCloseViewHeight;
  int mCloseView;
  int mCarrierLabelHeight;
  int mCarrierLabelVisible;
  int mCarrierLabel;
  int mRotationButton;
  int mSettingsButton;
  int mClearButton;
  int mNotificationPanelIsFullScreenWidth;
  int mNotificationPanelMinHeight;
  int mNotificationPanelGravity;
  int mNotificationPanelBackgroundPadding;
  int mNotificationPanelMarginLeftPx;
  int mNotificationPanelMarginBottomPx;
  int mExpandedContents;
  int mScrollView;
  int mNotificationPanel;
  int mStatusIcons;
  int mMoreIcon;
  int mNotificationIcons;
  int mIcons;
  int mQueueLock;
  int mPixelFormat;
  int mStatusBarView;
  int mStatusBarWindow;
  int mWindowManager;
  int mDisplay;
  int mIconHPadding;
  int mIconSize;
  int mNaturalBarHeight;
  int mNetworkController;
  int mLocationController;
  int mBatteryController;
  int mIconPolicy;
  int mFlingGestureMaxOutputVelocityPx;
  int mCollapseAccelPx;
  int mExpandAccelPx;
  int mFlingGestureMaxXVelocityPx;
  int mExpandMinDisplayFraction;
  int mCollapseMinDisplayFraction;
  int mFlingCollapseMinVelocityPx;
  int mFlingExpandMinVelocityPx;
  int mSelfCollapseVelocityPx;
  int mSelfExpandVelocityPx;
  int HIDE_ICONS_BELOW_SCORE;
  int NOTIFICATION_PRIORITY_MULTIPLIER;
  int CLOSE_PANEL_WHEN_EMPTIED;
  int INTRUDER_ALERT_DECAY_MS;
  int MSG_CLOSE_NOTIFICATION_PANEL;
  int MSG_OPEN_NOTIFICATION_PANEL;
  int SHOW_CARRIER_LABEL;
  int DIM_BEHIND_EXPANDED_PANEL;
  int ACTION_STATUSBAR_START;
  int CHATTY;
  int DUMPTRUCK;
  int SPEW;
  int DEBUG;
  int TAG;
}
class NavigationBarView {
  int mLightsOutListener;
  int mHandler;
  class H {
  }
  int MSG_CHECK_INVALID_LAYOUT;
  int WORKAROUND_INVALID_LAYOUT;
  int mDelegateHelper;
  int mBackAltLandIcon;
  int mBackAltIcon;
  int mBackLandIcon;
  int mBackIcon;
  int mNavigationIconHints;
  int mDisabledFlags;
  int mShowMenu;
  int mLowProfile;
  int mHidden;
  int mVertical;
  int mBarSize;
  int mRotatedViews;
  int mCurrentView;
  int mDisplay;
  int mBarService;
  int ANIMATE_HIDE_TRANSITION;
  int NAVBAR_ALWAYS_AT_RIGHT;
  int DEBUG_DEADZONE;
  int TAG;
  int DEBUG;
}
class IconMerger {
  int mMoreView;
  int mIconSize;
  int DEBUG;
  int TAG;
}
class CloseDragHandle {
  int mService;
}
class CarrierLabel {
  int mIntentReceiver;
  int mAttached;
}
