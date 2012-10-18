package com.android.systemui.statusbar.policy;
class WimaxIcons {
  int WIMAX_IDLE;
  int WIMAX_DISCONNECTED;
  int WIMAX_SIGNAL_STRENGTH;
}
class WifiIcons {
  int WIFI_LEVEL_COUNT;
  int WIFI_SIGNAL_STRENGTH;
}
class VolumeController {
  int mHasVibrator;
  int mVolume;
  int mMute;
  int mAudioManager;
  int mControl;
  int mContext;
  int STREAM;
  int TAG;
}
class ToggleSlider {
  int mLabel;
  int mSlider;
  int mToggle;
  int mTracking;
  int mListener;
  class Listener {
  }
  int TAG;
}
class TelephonyIcons {
  int DATA_4G;
  int DATA_1X;
  int DATA_H;
  int DATA_E;
  int DATA_3G;
  int DATA_G;
  int DATA_SIGNAL_STRENGTH;
  int TELEPHONY_SIGNAL_STRENGTH_ROAMING;
  int TELEPHONY_SIGNAL_STRENGTH;
}
class Prefs {
  int SHOWN_COMPAT_MODE_HELP;
  int DO_NOT_DISTURB_DEFAULT;
  int DO_NOT_DISTURB_PREF;
  int SHARED_PREFS_NAME;
}
class OnSizeChangedListener {
}
class NotificationRowLayout {
  int mRealLayoutTransition;
  int mRemoveViews;
  int mOnSizeChangedListener;
  int mSwipeHelper;
  int mDisappearingViews;
  int mAppearingViews;
  int mTmpRect;
  int mAnimateBounds;
  int DISAPPEAR_ANIM_LEN;
  int APPEAR_ANIM_LEN;
  int SLOW_ANIMATIONS;
  int DEBUG;
  int TAG;
}
class NetworkController {
  class WifiHandler {
  }
  int mPhoneStateListener;
  class SignalCluster {
  }
  int mBatteryStats;
  int mDataAndWifiStacked;
  int mHasMobileDataFeature;
  int mLastCombinedLabel;
  int mLastDataTypeIconId;
  int mLastCombinedSignalIconId;
  int mLastWimaxIconId;
  int mLastWifiIconId;
  int mLastDataDirectionOverlayIconId;
  int mLastDataDirectionIconId;
  int mLastPhoneSignalIconId;
  int mSignalClusters;
  int mWifiLabelViews;
  int mMobileLabelViews;
  int mCombinedLabelViews;
  int mDataTypeIconViews;
  int mCombinedSignalIconViews;
  int mWimaxIconViews;
  int mWifiIconViews;
  int mDataDirectionOverlayIconViews;
  int mDataDirectionIconViews;
  int mPhoneSignalIconViews;
  int mContext;
  int mLastAirplaneMode;
  int mAirplaneMode;
  int INET_CONDITION_THRESHOLD;
  int mInetCondition;
  int mConnectedNetworkTypeName;
  int mConnectedNetworkType;
  int mConnected;
  int mWimaxExtraState;
  int mWimaxState;
  int mWimaxSignal;
  int mWimaxIconId;
  int mWimaxIdle;
  int mWimaxConnected;
  int mIsWimaxEnabled;
  int mWimaxSupported;
  int mBluetoothTetherIconId;
  int mBluetoothTethered;
  int mWifiActivity;
  int mWifiActivityIconId;
  int mWifiIconId;
  int mWifiSsid;
  int mWifiLevel;
  int mWifiRssi;
  int mWifiConnected;
  int mWifiEnabled;
  int mWifiChannel;
  int mWifiManager;
  int mContentDescriptionDataType;
  int mContentDescriptionCombinedSignal;
  int mContentDescriptionWimax;
  int mContentDescriptionWifi;
  int mContentDescriptionPhoneSignal;
  int mAlwaysShowCdmaRssi;
  int mShowAtLeastThreeGees;
  int mShowPhoneRSSIForData;
  int mLastSignalLevel;
  int mMobileActivityIconId;
  int mDataActive;
  int mAirplaneIconId;
  int mDataTypeIconId;
  int mDataSignalIconId;
  int mDataDirectionIconId;
  int mPhoneSignalIconId;
  int mNetworkNameSeparator;
  int mNetworkNameDefault;
  int mNetworkName;
  int mDataIconList;
  int mSignalStrength;
  int mServiceState;
  int mDataActivity;
  int mDataState;
  int mDataNetType;
  int mPhoneState;
  int mSimState;
  int mDataConnected;
  int mPhone;
  int mHspaDataDistinguishable;
  int CHATTY;
  int DEBUG;
  int TAG;
}
class LocationController {
  int mNotificationService;
  int mContext;
  int GPS_NOTIFICATION_ID;
  int TAG;
}
class KeyButtonView {
  int mCheckLongPress;
  int mPressedAnim;
  int mRect;
  int mSupportsLongpress;
  int mDrawingAlpha;
  int mGlowScale;
  int mGlowAlpha;
  int mGlowHeight;
  int mGlowWidth;
  int mGlowBG;
  int mTouchSlop;
  int mCode;
  int mDownTime;
  int BUTTON_QUIESCENT_ALPHA;
  int GLOW_MAX_SCALE_FACTOR;
  int TAG;
}
class IntruderAlertView {
  int mOnClickListener;
  int mIntruderRemoteViews;
  int mContentHolder;
  int mBar;
  int mSwipeHelper;
  int mTmpRect;
  int DEBUG;
  int TAG;
}
class FixedSizeDrawable {
  int mBottom;
  int mRight;
  int mTop;
  int mLeft;
  int mDrawable;
}
class EventHole {
  int mLoc;
  int mWindowVis;
  int TAG;
}
class DoNotDisturbController {
  int mDoNotDisturb;
  int mCheckBox;
  int mContext;
  int mPrefs;
  int TAG;
}
class DeadZone {
}
class DateView {
  int mIntentReceiver;
  int mUpdating;
  int mWindowVisible;
  int mAttachedToWindow;
  int TAG;
}
class CompatModeButton {
  int mAM;
  int TAG;
  int DEBUG;
}
class Clock {
  int mIntentReceiver;
  int AM_PM_STYLE;
  int AM_PM_STYLE_GONE;
  int AM_PM_STYLE_SMALL;
  int AM_PM_STYLE_NORMAL;
  int mClockFormat;
  int mClockFormatString;
  int mCalendar;
  int mAttached;
}
class BrightnessController {
  int mPower;
  int mControl;
  int mContext;
  int MAXIMUM_BACKLIGHT;
  int MINIMUM_BACKLIGHT;
  int TAG;
}
class BluetoothController {
  int mEnabled;
  int mContentDescriptionId;
  int mIconId;
  int mIconViews;
  int mContext;
  int TAG;
}
class BatteryController {
  int mLabelViews;
  int mIconViews;
  int mContext;
  int TAG;
}
class AutoRotateController {
  class RotationLockCallbacks {
  }
  int mRotationPolicyListener;
  int mAutoRotation;
  int mCallbacks;
  int mCheckbox;
  int mContext;
}
class AirplaneModeController {
  int mAirplaneMode;
  int mCheckBox;
  int mContext;
  int TAG;
}
class AccessibilityContentDescriptions {
  int WIMAX_CONNECTION_STRENGTH;
  int WIFI_CONNECTION_STRENGTH;
  int DATA_CONNECTION_STRENGTH;
  int PHONE_SIGNAL_STRENGTH;
}
