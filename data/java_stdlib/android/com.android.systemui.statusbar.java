package com.android.systemui.statusbar;
class StatusBarIconView {
  int mNotification;
  int mNumberText;
  int mNumberY;
  int mNumberX;
  int mNumberPain;
  int mNumberBackground;
  int mSlot;
  int mIcon;
  int TAG;
}
class SignalClusterView {
  int mSpacer;
  int mAirplane;
  int mMobileType;
  int mMobileActivity;
  int mWifiActivity;
  int mMobile;
  int mWifi;
  int mMobileGroup;
  int mWifiGroup;
  int mMobileTypeDescription;
  int mMobileDescription;
  int mWifiDescription;
  int mAirplaneIconId;
  int mIsAirplaneMode;
  int mMobileTypeId;
  int mMobileActivityId;
  int mMobileStrengthId;
  int mMobileVisible;
  int mWifiActivityId;
  int mWifiStrengthId;
  int mWifiVisible;
  int mNC;
  int TAG;
  int DEBUG;
}
class RotationToggle {
  int mRotater;
}
class NotificationData {
  int mEntryCmp;
  int mEntries;
  class Entry {
    int expandedLarge;
    int largeIcon;
    int expanded;
    int content;
    int row;
    int icon;
    int notification;
    int key;
  }
}
class LatestItemView {
}
class DoNotDisturb {
  int mDoNotDisturb;
  int mPrefs;
  int mStatusBar;
  int mContext;
}
class DelegateViewHelper {
  int mSwapXY;
  int mStarted;
  int mInitialTouch;
  int mPanelShowing;
  int mTriggerThreshhold;
  int mDownPoint;
  int mTempPoint;
  int mBar;
  int mSourceView;
  int mDelegateView;
}
class CommandQueue {
  class H {
  }
  class Callbacks {
  }
  class NotificationQueueEntry {
    int notification;
    int key;
  }
  int mHandler;
  int mCallbacks;
  int mList;
  int FLAG_EXCLUDE_COMPAT_MODE_PANEL;
  int FLAG_EXCLUDE_INPUT_METHODS_PANEL;
  int FLAG_EXCLUDE_NOTIFICATION_PANEL;
  int FLAG_EXCLUDE_RECENTS_PANEL;
  int FLAG_EXCLUDE_SEARCH_PANEL;
  int FLAG_EXCLUDE_NONE;
  int MSG_SET_NAVIGATION_ICON_HINTS;
  int MSG_CANCEL_PRELOAD_RECENT_APPS;
  int MSG_PRELOAD_RECENT_APPS;
  int MSG_TOGGLE_RECENT_APPS;
  int MSG_SET_HARD_KEYBOARD_STATUS;
  int MSG_SHOW_IME_BUTTON;
  int MSG_TOP_APP_WINDOW_CHANGED;
  int MSG_SET_SYSTEMUI_VISIBILITY;
  int OP_COLLAPSE;
  int OP_EXPAND;
  int MSG_SET_VISIBILITY;
  int MSG_DISABLE;
  int MSG_REMOVE_NOTIFICATION;
  int MSG_UPDATE_NOTIFICATION;
  int MSG_ADD_NOTIFICATION;
  int OP_REMOVE_ICON;
  int OP_SET_ICON;
  int MSG_ICON;
  int MSG_MASK;
  int MSG_SHIFT;
  int INDEX_MASK;
  int TAG;
}
class BaseStatusBar {
  class NotificationClicker {
    int mId;
    int mTag;
    int mPkg;
    int mIntent;
  }
  class TouchOutsideListener {
    int mPanel;
    int mMsg;
  }
  class H {
  }
  int mOnClickHandler;
  int mProvisioningObserver;
  int mDeviceProvisioned;
  int mWindowManager;
  int mDisplay;
  int mNotificationBlamePopup;
  int mRecentTasksLoader;
  int mRecentsPanel;
  int mSearchPanelView;
  int mPanelSlightlyVisible;
  int mCurrentlyIntrudingNotification;
  int mPile;
  int mNotificationData;
  int mHandler;
  int mBarService;
  int mCommandQueue;
  int EXPANDED_FULL_OPEN;
  int EXPANDED_LEAVE_ALONE;
  int SYSTEM_DIALOG_REASON_RECENT_APPS;
  int ENABLE_INTRUDERS;
  int MSG_HIDE_INTRUDER;
  int MSG_SHOW_INTRUDER;
  int MSG_CLOSE_SEARCH_PANEL;
  int MSG_OPEN_SEARCH_PANEL;
  int MSG_CANCEL_PRELOAD_RECENT_APPS;
  int MSG_PRELOAD_RECENT_APPS;
  int MSG_CLOSE_RECENTS_PANEL;
  int MSG_OPEN_RECENTS_PANEL;
  int DEBUG;
  int TAG;
}
class AnimatedImageView {
  int mAttached;
  int mAnim;
}
