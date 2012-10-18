package com.android.internal.widget;
class WeightedLinearLayout {
  int mMinorWeightMax;
  int mMajorWeightMax;
  int mMinorWeightMin;
  int mMajorWeightMin;
}
class WaveView {
  class OnTriggerListener {
    int CENTER_HANDLE;
    int NO_HANDLE;
  }
  int mAddWaveAction;
  int mLockTimerActions;
  int mFinishWaves;
  int mWavesRunning;
  int mGrabbedState;
  int mLockState;
  int mUnlockHalo;
  int mUnlockDefault;
  int mUnlockRing;
  int mMouseY;
  int mMouseX;
  int mLockCenterY;
  int mLockCenterX;
  int mCurrentWave;
  int mWaveTimerDelay;
  int mWaveCount;
  int mSnapRadius;
  int mRingRadius;
  int mFingerDown;
  int mLightWaves;
  int mDrawables;
  int mOnTriggerListener;
  int mVibrator;
  int GRAB_HANDLE_RADIUS_SCALE_ACCESSIBILITY_ENABLED;
  int GRAB_HANDLE_RADIUS_SCALE_ACCESSIBILITY_DISABLED;
  int WAVE_DELAY;
  int DELAY_INCREMENT2;
  int DELAY_INCREMENT;
  int RESET_TIMEOUT;
  int WAVE_DURATION;
  int SHORT_DELAY;
  int FINAL_DELAY;
  int RING_DELAY;
  int FINAL_DURATION;
  int DURATION;
  int STATE_UNLOCK_SUCCESS;
  int STATE_UNLOCK_ATTEMPT;
  int STATE_ATTEMPTING;
  int STATE_START_ATTEMPT;
  int STATE_READY;
  int STATE_RESET_LOCK;
  int VIBRATE_LONG;
  int VIBRATE_SHORT;
  int WAVE_COUNT;
  int DBG;
  int TAG;
}
class TransportControlView {
  class SavedState {
    int CREATOR;
    int wasShowing;
  }
  class Metadata {
    int bitmap;
    int albumTitle;
    int trackTitle;
    int artist;
  }
  class IRemoteControlDisplayWeak {
    int mLocalHandler;
  }
  int mHandler;
  int mPopulateMetadataWhenAttached;
  int mIRCD;
  int mWidgetCallbacks;
  int mAudioManager;
  int mCurrentPlayState;
  int mTransportControlFlags;
  int mClientIntent;
  int mAttached;
  int mMetadata;
  int mClientGeneration;
  int mBtnNext;
  int mBtnPlay;
  int mBtnPrev;
  int mTrackTitle;
  int mAlbumArt;
  int TAG;
  int DEBUG;
  int DISPLAY_TIMEOUT_MS;
  int MAXDIM;
  int MSG_SET_GENERATION_ID;
  int MSG_SET_ARTWORK;
  int MSG_SET_TRANSPORT_CONTROLS;
  int MSG_SET_METADATA;
  int MSG_UPDATE_STATE;
}
class TextProgressBar {
  int mChronometerGravity;
  int mChronometerFollow;
  int mDuration;
  int mDurationBase;
  int mProgressBar;
  int mChronometer;
  int PROGRESSBAR_ID;
  int CHRONOMETER_ID;
  int TAG;
}
class Smileys {
  int WTF;
  int LAUGHING;
  int LIPS_ARE_SEALED;
  int CRYING;
  int UNDECIDED;
  int ANGEL;
  int EMBARRASSED;
  int FOOT_IN_MOUTH;
  int MONEY_MOUTH;
  int COOL;
  int YELLING;
  int KISSING;
  int SURPRISED;
  int TONGUE_STICKING_OUT;
  int WINKING;
  int SAD;
  int HAPPY;
  int sIconIds;
}
class SlidingTab {
  class Slider {
    int alignment_value;
    int alignment;
    int currentState;
    int target;
    int text;
    int tab;
    int STATE_ACTIVE;
    int STATE_PRESSED;
    int STATE_NORMAL;
    int ALIGN_UNKNOWN;
    int ALIGN_BOTTOM;
    int ALIGN_TOP;
    int ALIGN_RIGHT;
    int ALIGN_LEFT;
  }
  class OnTriggerListener {
    int RIGHT_HANDLE;
    int LEFT_HANDLE;
    int NO_HANDLE;
  }
  int mAnimationDoneListener;
  int mTmpRect;
  int mAnimating;
  int mOtherSlider;
  int mThreshold;
  int mTracking;
  int mCurrentSlider;
  int mRightSlider;
  int mLeftSlider;
  int mOrientation;
  int mDensity;
  int mVibrator;
  int mTriggered;
  int mGrabbedState;
  int mOnTriggerListener;
  int mHoldRightOnTransition;
  int mHoldLeftOnTransition;
  int ANIM_TARGET_TIME;
  int ANIM_DURATION;
  int TRACKING_MARGIN;
  int VIBRATE_LONG;
  int VIBRATE_SHORT;
  int THRESHOLD;
  int VERTICAL;
  int HORIZONTAL;
  int DBG;
  int LOG_TAG;
}
class SizeAdaptiveLayoutTest {
  int mLargeView;
  int mMediumView;
  int mSmallView;
  int mSizeAdaptiveLayout;
  int mFourU;
  int mOneU;
  int mInflater;
}
class SizeAdaptiveLayout {
  class BringToFrontOnEnd {
  }
  class LayoutParams {
    int UNBOUNDED;
    int maxHeight;
    int minHeight;
  }
  int mModestyPanelTop;
  int mModestyPanel;
  int mLeavingView;
  int mEnteringView;
  int mCanceledAnimationCount;
  int mFadeView;
  int mFadePanel;
  int mAnimatorListener;
  int mTransitionAnimation;
  int mLastActive;
  int mActiveChild;
  int MAX_VALID_HEIGHT;
  int MIN_VALID_HEIGHT;
  int CROSSFADE_TIME;
  int REPORT_BAD_BOUNDS;
  int DEBUG;
  int TAG;
}
class ScrollingTabContainerView {
  class VisibilityAnimListener {
    int mFinalVisibility;
    int mCanceled;
  }
  class TabClickListener {
  }
  class TabAdapter {
  }
  class TabView {
    int mCustomView;
    int mIconView;
    int mTextView;
    int mTab;
  }
  int FADE_DURATION;
  int sAlphaInterpolator;
  int mVisAnimListener;
  int mVisibilityAnim;
  int mSelectedTabIndex;
  int mContentHeight;
  int mStackedTabMaxWidth;
  int mMaxTabWidth;
  int mAllowCollapse;
  int mTabSpinner;
  int mTabLayout;
  int mTabClickListener;
  int mTabSelector;
  int TAG;
}
class RotarySelector {
  class OnDialTriggerListener {
    int RIGHT_HANDLE;
    int LEFT_HANDLE;
  }
  int mOrientation;
  int mDimplesOfFling;
  int mMaximumVelocity;
  int mMinimumVelocity;
  int mVelocityTracker;
  int mDimpleSpacing;
  int mInnerRadius;
  int mOuterRadius;
  int mBackgroundHeight;
  int mBackgroundWidth;
  int mDimpleWidth;
  int mEdgeTriggerThresh;
  int SPIN_ANIMATION_DURATION_MILLIS;
  int SNAP_BACK_ANIMATION_DURATION_MILLIS;
  int ROTARY_STROKE_WIDTH_DIP;
  int OUTER_ROTARY_RADIUS_DIP;
  int EDGE_TRIGGER_DIP;
  int EDGE_PADDING_DIP;
  int ARROW_SCRUNCH_DIP;
  int VIBRATE_LONG;
  int VIBRATE_SHORT;
  int mVibrator;
  int mTriggered;
  int RIGHT_HANDLE_GRABBED;
  int LEFT_HANDLE_GRABBED;
  int NOTHING_GRABBED;
  int mGrabbedState;
  int mArrowMatrix;
  int mBgMatrix;
  int mPaint;
  int mInterpolator;
  int mAnimatingDeltaXEnd;
  int mAnimatingDeltaXStart;
  int mAnimationDuration;
  int mAnimationStartTime;
  int mAnimating;
  int mRotaryOffsetX;
  int mRightHandleX;
  int mLeftHandleX;
  int mArrowLongRight;
  int mArrowLongLeft;
  int mArrowShortLeftAndRight;
  int mRightHandleIcon;
  int mLeftHandleIcon;
  int mDimpleDim;
  int mDimple;
  int mBackground;
  int mDensity;
  int mOnDialTriggerListener;
  int VISUAL_DEBUG;
  int DBG;
  int LOG_TAG;
  int VERTICAL;
  int HORIZONTAL;
}
class PointerLocationView {
  class FasterStringBuilder {
    int mLength;
    int mChars;
  }
  int mReusableOvalRect;
  int mPrintCoords;
  int mText;
  int mAltVelocity;
  int mVelocity;
  int mTempCoords;
  int mPointers;
  int mActivePointerId;
  int mMaxNumPointers;
  int mCurNumPointers;
  int mCurDown;
  int mHeaderBottom;
  int mTextMetrics;
  int mPathPaint;
  int mTargetPaint;
  int mPaint;
  int mTextLevelPaint;
  int mTextBackgroundPaint;
  int mTextPaint;
  int mVC;
  int mIm;
  int ESTIMATE_INTERVAL;
  int ESTIMATE_FUTURE_POINTS;
  int ESTIMATE_PAST_POINTS;
  class PointerState {
    int mAltEstimator;
    int mEstimator;
    int mAltYVelocity;
    int mAltXVelocity;
    int mYVelocity;
    int mXVelocity;
    int mToolType;
    int mCoords;
    int mCurDown;
    int mTraceCount;
    int mTraceY;
    int mTraceX;
  }
  int ALT_STRATEGY_PROPERY_KEY;
  int TAG;
}
class PasswordEntryKeyboardView {
  int KEYCODE_NEXT_LANGUAGE;
  int KEYCODE_F1;
  int KEYCODE_VOICE;
  int KEYCODE_SHIFT_LONGPRESS;
  int KEYCODE_OPTIONS;
}
class PasswordEntryKeyboardHelper {
  int mEnableHaptics;
  int mVibratePattern;
  int mKeyboardView;
  int mTargetView;
  int mContext;
  int mNumericKeyboard;
  int mSymbolsKeyboardShifted;
  int mSymbolsKeyboard;
  int mQwertyKeyboardShifted;
  int mQwertyKeyboard;
  int mKeyboardState;
  int mKeyboardMode;
  int TAG;
  int KEYBOARD_STATE_CAPSLOCK;
  int KEYBOARD_STATE_SHIFTED;
  int KEYBOARD_STATE_NORMAL;
  int KEYBOARD_MODE_NUMERIC;
  int KEYBOARD_MODE_ALPHA;
}
class PasswordEntryKeyboard {
  class LatinKey {
    int mEnabled;
    int mShiftLockEnabled;
  }
  int sSpacebarVerticalCorrection;
  int mShiftState;
  int mSpaceKey;
  int mF1Key;
  int mEnterKey;
  int mShiftKeys;
  int mOldShiftIcons;
  int mShiftLockIcon;
  int mShiftIcon;
  int KEYCODE_SPACE;
  int SHIFT_LOCKED;
  int SHIFT_ON;
  int SHIFT_OFF;
}
class LockSettingsService {
  int VALID_SETTINGS;
  class DatabaseHelper {
    int DATABASE_VERSION;
    int DATABASE_NAME;
    int TAG;
  }
  int mContext;
  int LOCK_PASSWORD_FILE;
  int LOCK_PATTERN_FILE;
  int SYSTEM_DIRECTORY;
  int COLUMNS_FOR_QUERY;
  int COLUMN_VALUE;
  int COLUMN_USERID;
  int COLUMN_KEY;
  int TABLE;
  int TAG;
  int mOpenHelper;
}
class LockScreenWidgetInterface {
}
class LockScreenWidgetCallback {
}
class LockPatternView {
  class SavedState {
    int CREATOR;
    int mTactileFeedbackEnabled;
    int mInStealthMode;
    int mInputEnabled;
    int mDisplayMode;
    int mSerializedPattern;
  }
  class OnPatternListener {
  }
  class DisplayMode {
    int Wrong;
    int Animate;
    int Correct;
  }
  class Cell {
    int sCells;
    int column;
    int row;
  }
  int mCircleMatrix;
  int mArrowMatrix;
  int mAspect;
  int mBitmapHeight;
  int mBitmapWidth;
  int mInvalidate;
  int mCurrentPath;
  int mBitmapArrowRedUp;
  int mBitmapArrowGreenUp;
  int mBitmapCircleRed;
  int mBitmapCircleGreen;
  int mBitmapCircleDefault;
  int mBitmapBtnTouched;
  int mBitmapBtnDefault;
  int mSquareHeight;
  int mSquareWidth;
  int mHitFactor;
  int mStrokeAlpha;
  int mDiameterFactor;
  int mPatternInProgress;
  int mEnableHapticFeedback;
  int mInStealthMode;
  int mInputEnabled;
  int mPatternDisplayMode;
  int mAnimatingPeriodStart;
  int mInProgressY;
  int mInProgressX;
  int mPatternDrawLookup;
  int mPattern;
  int mOnPatternListener;
  int MILLIS_PER_CIRCLE_ANIMATING;
  int STATUS_BAR_HEIGHT;
  int mPathPaint;
  int mPaint;
  int mDrawingProfilingStarted;
  int PROFILE_DRAWING;
  int ASPECT_LOCK_HEIGHT;
  int ASPECT_LOCK_WIDTH;
  int ASPECT_SQUARE;
  int TAG;
}
class LockPatternUtils {
  int mCurrentUserId;
  int mLockSettingsService;
  int mDevicePolicyManager;
  int mContentResolver;
  int mContext;
  int PASSWORD_HISTORY_KEY;
  int LOCKSCREEN_POWER_BUTTON_INSTANTLY_LOCKS;
  int BIOMETRIC_WEAK_EVER_CHOSEN_KEY;
  int LOCKSCREEN_BIOMETRIC_WEAK_FALLBACK;
  int LOCKSCREEN_OPTIONS;
  int DISABLE_LOCKSCREEN_KEY;
  int LOCK_PASSWORD_SALT_KEY;
  int PASSWORD_TYPE_ALTERNATE_KEY;
  int PASSWORD_TYPE_KEY;
  int PATTERN_EVER_CHOSEN_KEY;
  int LOCKOUT_ATTEMPT_DEADLINE;
  int LOCKOUT_PERMANENT_KEY;
  int FLAG_BIOMETRIC_WEAK_LIVELINESS;
  int MIN_PATTERN_REGISTER_FAIL;
  int MIN_LOCK_PATTERN_SIZE;
  int FAILED_ATTEMPTS_BEFORE_WIPE_GRACE;
  int FAILED_ATTEMPT_COUNTDOWN_INTERVAL_MS;
  int FAILED_ATTEMPT_TIMEOUT_MS;
  int FAILED_ATTEMPTS_BEFORE_RESET;
  int FAILED_ATTEMPTS_BEFORE_TIMEOUT;
  int TAG;
  int OPTION_ENABLE_FACELOCK;
}
class LinearLayoutWithDefaultTouchRecepient {
  int mDefaultTouchRecepient;
  int mTempRect;
}
class EditableInputConnection {
  int mBatchEditNesting;
  int mTextView;
  int TAG;
  int DEBUG;
}
class DrawableHolder {
  int mNeedToStart;
  int mAnimators;
  int mAlpha;
  int mDrawable;
  int mScaleY;
  int mScaleX;
  int mY;
  int mX;
  int DBG;
  int TAG;
  int EASE_OUT_INTERPOLATOR;
}
class DigitalClock {
  class FormatChangeObserver {
    int mContext;
    int mClock;
  }
  class AmPm {
    int mPmString;
    int mAmString;
    int mAmPmTextView;
  }
  class TimeChangedReceiver {
    int mContext;
    int mClock;
  }
  int sForegroundFont;
  int sBackgroundFont;
  int mIntentReceiver;
  int mHandler;
  int mAttached;
  int mFormatChangeObserver;
  int mAmPm;
  int mTimeDisplayForeground;
  int mTimeDisplayBackground;
  int mFormat;
  int mCalendar;
  int M24;
  int M12;
  int SYSTEM_FONT_TIME_FOREGROUND;
  int SYSTEM_FONT_TIME_BACKGROUND;
  int SYSTEM;
}
class DialogTitle {
}
class ActionBarView {
  class ExpandedActionViewMenuPresenter {
    int mCurrentExpandedItem;
    int mMenu;
  }
  class HomeView {
    int mUpWidth;
    int mIconView;
    int mUpView;
  }
  class SavedState {
    int CREATOR;
    int isOverflowOpen;
    int expandedMenuItemId;
  }
  int mUpClickListener;
  int mExpandedActionViewUpListener;
  int mNavItemSelectedListener;
  int MAX_HOME_SLOP;
  int mMaxHomeSlop;
  int mTempRect;
  int mWindowCallback;
  int mExpandedActionView;
  int mExpandedMenuPresenter;
  int mTabSelector;
  int mCallback;
  int mSpinnerAdapter;
  int mLogoNavItem;
  int mContextView;
  int mOptionsMenu;
  int mIsCollapsed;
  int mIsCollapsable;
  int mIncludeTabs;
  int mUserTitle;
  int mIndeterminateProgressStyle;
  int mProgressStyle;
  int mSubtitleStyleRes;
  int mTitleStyleRes;
  int mItemPadding;
  int mProgressBarPadding;
  int mIndeterminateProgressView;
  int mProgressView;
  int mCustomNavView;
  int mTabScrollView;
  int mListNavLayout;
  int mSpinner;
  int mTitleUpView;
  int mSubtitleView;
  int mTitleView;
  int mTitleLayout;
  int mExpandedHomeLayout;
  int mHomeLayout;
  int mLogo;
  int mIcon;
  int mSubtitle;
  int mTitle;
  int mDisplayOptions;
  int mNavigationMode;
  int DEFAULT_CUSTOM_GRAVITY;
  int DISPLAY_RELAYOUT_MASK;
  int DISPLAY_DEFAULT;
  int TAG;
}
class ActionBarOverlayLayout {
  int mActionBarSizeAttr;
  int mZeroRect;
  int mLastSystemUiVisibility;
  int mActionBarBottom;
  int mActionView;
  int mContainerView;
  int mActionBarTop;
  int mContent;
  int mWindowVisibility;
  int mActionBar;
  int mActionBarHeight;
}
class ActionBarContextView {
  int ANIMATE_OUT;
  int ANIMATE_IN;
  int ANIMATE_IDLE;
  int mAnimationMode;
  int mAnimateInOnLayout;
  int mCurrentAnimation;
  int mTitleOptional;
  int mSplitBackground;
  int mSubtitleStyleRes;
  int mTitleStyleRes;
  int mSubtitleView;
  int mTitleView;
  int mTitleLayout;
  int mCustomView;
  int mClose;
  int mSubtitle;
  int mTitle;
  int TAG;
}
class ActionBarContainer {
  int mIsStacked;
  int mIsSplit;
  int mSplitBackground;
  int mStackedBackground;
  int mBackground;
  int mActionBarView;
  int mTabContainer;
  int mIsTransitioning;
}
class AbsActionBarView {
  class VisibilityAnimListener {
    int mFinalVisibility;
    int mCanceled;
  }
  int FADE_DURATION;
  int sAlphaInterpolator;
  int mVisAnimListener;
  int mVisibilityAnim;
  int mContentHeight;
  int mSplitWhenNarrow;
  int mSplitActionBar;
  int mSplitView;
  int mActionMenuPresenter;
  int mMenuView;
}
