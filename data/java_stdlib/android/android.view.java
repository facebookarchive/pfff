package android.view;
class ZeroSizedTest {
  int mWithNoDimension;
  int mWithNoHeight;
  int mWithNoWdith;
  int mWithDimension;
}
class ZeroSized {
}
class WindowOrientationListener {
  class SensorEventListenerImpl {
    int mTiltHistoryIndex;
    int mTiltHistoryTimestampNanos;
    int mTiltHistory;
    int TILT_HISTORY_SIZE;
    int mAccelerationTimestampNanos;
    int mSwingTimestampNanos;
    int mFlatTimestampNanos;
    int mPredictedRotationTimestampNanos;
    int mPredictedRotation;
    int mProposedRotation;
    int mLastFilteredZ;
    int mLastFilteredY;
    int mLastFilteredX;
    int mLastFilteredTimestampNanos;
    int ADJACENT_ORIENTATION_ANGLE_GAP;
    int TILT_TOLERANCE;
    int MAX_TILT;
    int MAX_ACCELERATION_MAGNITUDE;
    int MIN_ACCELERATION_MAGNITUDE;
    int ACCELERATION_TOLERANCE;
    int NEAR_ZERO_MAGNITUDE;
    int FILTER_TIME_CONSTANT_MS;
    int MAX_FILTER_DELTA_TIME_NANOS;
    int SWING_TIME_NANOS;
    int SWING_AWAY_ANGLE_DELTA;
    int FLAT_TIME_NANOS;
    int FLAT_ANGLE;
    int PROPOSAL_MIN_TIME_SINCE_ACCELERATION_ENDED_NANOS;
    int PROPOSAL_MIN_TIME_SINCE_SWING_ENDED_NANOS;
    int PROPOSAL_MIN_TIME_SINCE_FLAT_ENDED_NANOS;
    int PROPOSAL_SETTLE_TIME_NANOS;
    int mOrientationListener;
    int ACCELEROMETER_DATA_Z;
    int ACCELEROMETER_DATA_Y;
    int ACCELEROMETER_DATA_X;
    int NANOS_PER_MS;
    int RADIANS_TO_DEGREES;
  }
  int mCurrentRotation;
  int mSensorEventListener;
  int mSensor;
  int mRate;
  int mEnabled;
  int mSensorManager;
  int USE_GRAVITY_SENSOR;
  int LOG;
  int TAG;
}
class WindowManagerPolicy {
  class OnKeyguardExitResult {
  }
  class ScreenOnListener {
  }
  int FINISH_LAYOUT_REDO_ANIM;
  int FINISH_LAYOUT_REDO_WALLPAPER;
  int FINISH_LAYOUT_REDO_CONFIG;
  int FINISH_LAYOUT_REDO_LAYOUT;
  int USER_ROTATION_LOCKED;
  int USER_ROTATION_FREE;
  int OFF_BECAUSE_OF_PROX_SENSOR;
  int OFF_BECAUSE_OF_TIMEOUT;
  int OFF_BECAUSE_OF_USER;
  int OFF_BECAUSE_OF_ADMIN;
  int TRANSIT_WALLPAPER_INTRA_CLOSE;
  int TRANSIT_WALLPAPER_INTRA_OPEN;
  int TRANSIT_WALLPAPER_OPEN;
  int TRANSIT_WALLPAPER_CLOSE;
  int TRANSIT_TASK_TO_BACK;
  int TRANSIT_TASK_TO_FRONT;
  int TRANSIT_TASK_CLOSE;
  int TRANSIT_TASK_OPEN;
  int TRANSIT_ACTIVITY_CLOSE;
  int TRANSIT_ACTIVITY_OPEN;
  int TRANSIT_PREVIEW_DONE;
  int TRANSIT_HIDE;
  int TRANSIT_SHOW;
  int TRANSIT_EXIT;
  int TRANSIT_ENTER;
  int TRANSIT_NONE;
  int TRANSIT_UNSET;
  int TRANSIT_EXIT_MASK;
  int TRANSIT_ENTER_MASK;
  class WindowManagerFuncs {
    int LID_OPEN;
    int LID_CLOSED;
    int LID_ABSENT;
  }
  class FakeWindow {
  }
  class WindowState {
  }
  int ACTION_GO_TO_SLEEP;
  int ACTION_POKE_USER_ACTIVITY;
  int ACTION_PASS_TO_USER;
  int EXTRA_HDMI_PLUGGED_STATE;
  int ACTION_HDMI_PLUGGED;
  int WATCH_POINTER;
  int PRESENCE_EXTERNAL;
  int PRESENCE_INTERNAL;
  int FLAG_PASS_TO_USER;
  int FLAG_BRIGHT_HERE;
  int FLAG_WOKE_HERE;
  int FLAG_DISABLE_KEY_REPEAT;
  int FLAG_FILTERED;
  int FLAG_TRUSTED;
  int FLAG_INJECTED;
  int FLAG_VIRTUAL;
  int FLAG_LAUNCHER;
  int FLAG_MENU;
  int FLAG_ALT_GR;
  int FLAG_ALT;
  int FLAG_CAPS_LOCK;
  int FLAG_SHIFT;
  int FLAG_WAKE_DROPPED;
  int FLAG_WAKE;
}
class WindowManagerImpl {
  class CompatModeWrapper {
    int mCompatibilityInfo;
    int mDefaultDisplay;
    int mWindowManager;
  }
  int sCompatWindowManagers;
  int sWindowManager;
  int sLock;
  int mSystemPropertyUpdater;
  int mNeedsEglTerminate;
  int mParams;
  int mRoots;
  int mViews;
  int ADD_PERMISSION_DENIED;
  int ADD_MULTIPLE_SINGLETON;
  int ADD_STARTING_NOT_NEEDED;
  int ADD_DUPLICATE_ADD;
  int ADD_APP_EXITING;
  int ADD_NOT_APP_TOKEN;
  int ADD_BAD_SUBWINDOW_TOKEN;
  int ADD_BAD_APP_TOKEN;
  int ADD_OKAY;
  int ADD_FLAG_IN_TOUCH_MODE;
  int ADD_FLAG_APP_VISIBLE;
  int RELAYOUT_DEFER_SURFACE_DESTROY;
  int RELAYOUT_INSETS_PENDING;
  int RELAYOUT_RES_ANIMATING;
  int RELAYOUT_RES_SURFACE_CHANGED;
  int RELAYOUT_RES_FIRST_TIME;
  int RELAYOUT_RES_IN_TOUCH_MODE;
}
class WindowLeaked {
}
class WindowManager {
  class LayoutParams {
    int mTitle;
    int mCompatibilityParamsBackup;
    int EVERYTHING_CHANGED;
    int PRIVATE_FLAGS_CHANGED;
    int INPUT_FEATURES_CHANGED;
    int SYSTEM_UI_LISTENER_CHANGED;
    int SYSTEM_UI_VISIBILITY_CHANGED;
    int BUTTON_BRIGHTNESS_CHANGED;
    int SCREEN_BRIGHTNESS_CHANGED;
    int SCREEN_ORIENTATION_CHANGED;
    int SOFT_INPUT_MODE_CHANGED;
    int MEMORY_TYPE_CHANGED;
    int ALPHA_CHANGED;
    int TITLE_CHANGED;
    int DIM_AMOUNT_CHANGED;
    int ANIMATION_CHANGED;
    int FORMAT_CHANGED;
    int FLAGS_CHANGED;
    int TYPE_CHANGED;
    int LAYOUT_CHANGED;
    int CREATOR;
    int inputFeatures;
    int INPUT_FEATURE_NO_INPUT_CHANNEL;
    int INPUT_FEATURE_DISABLE_POINTER_GESTURES;
    int hasSystemUiListeners;
    int subtreeSystemUiVisibility;
    int systemUiVisibility;
    int screenOrientation;
    int packageName;
    int token;
    int buttonBrightness;
    int screenBrightness;
    int BRIGHTNESS_OVERRIDE_FULL;
    int BRIGHTNESS_OVERRIDE_OFF;
    int BRIGHTNESS_OVERRIDE_NONE;
    int dimAmount;
    int alpha;
    int windowAnimations;
    int format;
    int verticalMargin;
    int horizontalMargin;
    int gravity;
    int softInputMode;
    int SOFT_INPUT_IS_FORWARD_NAVIGATION;
    int SOFT_INPUT_ADJUST_NOTHING;
    int SOFT_INPUT_ADJUST_PAN;
    int SOFT_INPUT_ADJUST_RESIZE;
    int SOFT_INPUT_ADJUST_UNSPECIFIED;
    int SOFT_INPUT_MASK_ADJUST;
    int SOFT_INPUT_STATE_ALWAYS_VISIBLE;
    int SOFT_INPUT_STATE_VISIBLE;
    int SOFT_INPUT_STATE_ALWAYS_HIDDEN;
    int SOFT_INPUT_STATE_HIDDEN;
    int SOFT_INPUT_STATE_UNCHANGED;
    int SOFT_INPUT_STATE_UNSPECIFIED;
    int SOFT_INPUT_MASK_STATE;
    int privateFlags;
    int PRIVATE_FLAG_SET_NEEDS_MENU_KEY;
    int PRIVATE_FLAG_WANTS_OFFSET_NOTIFICATIONS;
    int PRIVATE_FLAG_FORCE_HARDWARE_ACCELERATED;
    int PRIVATE_FLAG_FAKE_HARDWARE_ACCELERATED;
    int flags;
    int FLAG_SYSTEM_ERROR;
    int FLAG_COMPATIBLE_WINDOW;
    int FLAG_NEEDS_MENU_KEY;
    int FLAG_SLIPPERY;
    int FLAG_HARDWARE_ACCELERATED;
    int FLAG_SPLIT_TOUCH;
    int FLAG_DISMISS_KEYGUARD;
    int FLAG_TURN_SCREEN_ON;
    int FLAG_SHOW_WALLPAPER;
    int FLAG_SHOW_WHEN_LOCKED;
    int FLAG_WATCH_OUTSIDE_TOUCH;
    int FLAG_ALT_FOCUSABLE_IM;
    int FLAG_LAYOUT_INSET_DECOR;
    int FLAG_IGNORE_CHEEK_PRESSES;
    int FLAG_SCALED;
    int FLAG_SECURE;
    int FLAG_DITHER;
    int FLAG_FORCE_NOT_FULLSCREEN;
    int FLAG_FULLSCREEN;
    int FLAG_LAYOUT_NO_LIMITS;
    int FLAG_LAYOUT_IN_SCREEN;
    int FLAG_KEEP_SCREEN_ON;
    int FLAG_TOUCHABLE_WHEN_WAKING;
    int FLAG_NOT_TOUCH_MODAL;
    int FLAG_NOT_TOUCHABLE;
    int FLAG_NOT_FOCUSABLE;
    int FLAG_BLUR_BEHIND;
    int FLAG_DIM_BEHIND;
    int FLAG_ALLOW_LOCK_WHILE_SCREEN_ON;
    int memoryType;
    int MEMORY_TYPE_PUSH_BUFFERS;
    int MEMORY_TYPE_GPU;
    int MEMORY_TYPE_HARDWARE;
    int MEMORY_TYPE_NORMAL;
    int LAST_SYSTEM_WINDOW;
    int TYPE_NAVIGATION_BAR_PANEL;
    int TYPE_DREAM;
    int TYPE_HIDDEN_NAV_CONSUMER;
    int TYPE_BOOT_PROGRESS;
    int TYPE_VOLUME_OVERLAY;
    int TYPE_NAVIGATION_BAR;
    int TYPE_POINTER;
    int TYPE_STATUS_BAR_SUB_PANEL;
    int TYPE_DRAG;
    int TYPE_SECURE_SYSTEM_OVERLAY;
    int TYPE_STATUS_BAR_PANEL;
    int TYPE_WALLPAPER;
    int TYPE_INPUT_METHOD_DIALOG;
    int TYPE_INPUT_METHOD;
    int TYPE_SYSTEM_ERROR;
    int TYPE_KEYGUARD_DIALOG;
    int TYPE_SYSTEM_DIALOG;
    int TYPE_PRIORITY_PHONE;
    int TYPE_SYSTEM_OVERLAY;
    int TYPE_TOAST;
    int TYPE_KEYGUARD;
    int TYPE_SYSTEM_ALERT;
    int TYPE_PHONE;
    int TYPE_SEARCH_BAR;
    int TYPE_STATUS_BAR;
    int FIRST_SYSTEM_WINDOW;
    int LAST_SUB_WINDOW;
    int TYPE_APPLICATION_MEDIA_OVERLAY;
    int TYPE_APPLICATION_ATTACHED_DIALOG;
    int TYPE_APPLICATION_SUB_PANEL;
    int TYPE_APPLICATION_MEDIA;
    int TYPE_APPLICATION_PANEL;
    int FIRST_SUB_WINDOW;
    int LAST_APPLICATION_WINDOW;
    int TYPE_APPLICATION_STARTING;
    int TYPE_APPLICATION;
    int TYPE_BASE_APPLICATION;
    int FIRST_APPLICATION_WINDOW;
    int type;
    int verticalWeight;
    int horizontalWeight;
    int y;
    int x;
  }
  class BadTokenException {
  }
}
class Window {
  class LocalWindowManager {
    int mHardwareAccelerated;
    int PROPERTY_HARDWARE_UI;
  }
  class Callback {
  }
  int mWindowAttributes;
  int mDestroyed;
  int mHasSoftInputMode;
  int mDefaultWindowFormat;
  int mHaveDimAmount;
  int mHaveWindowFormat;
  int mLocalFeatures;
  int mFeatures;
  int mForcedWindowFlags;
  int mSetCloseOnTouchOutside;
  int mCloseOnTouchOutside;
  int mHasChildren;
  int mIsActive;
  int mActiveChild;
  int mContainer;
  int mAppName;
  int mAppToken;
  int mWindowManager;
  int mCallback;
  int mWindowStyle;
  int mContext;
  int ID_ANDROID_CONTENT;
  int DEFAULT_FEATURES;
  int PROGRESS_SECONDARY_END;
  int PROGRESS_SECONDARY_START;
  int PROGRESS_END;
  int PROGRESS_START;
  int PROGRESS_INDETERMINATE_OFF;
  int PROGRESS_INDETERMINATE_ON;
  int PROGRESS_VISIBILITY_OFF;
  int PROGRESS_VISIBILITY_ON;
  int FEATURE_ACTION_MODE_OVERLAY;
  int FEATURE_ACTION_BAR_OVERLAY;
  int FEATURE_ACTION_BAR;
  int FEATURE_CUSTOM_TITLE;
  int FEATURE_CONTEXT_MENU;
  int FEATURE_INDETERMINATE_PROGRESS;
  int FEATURE_RIGHT_ICON;
  int FEATURE_LEFT_ICON;
  int FEATURE_PROGRESS;
  int FEATURE_NO_TITLE;
  int FEATURE_OPTIONS_PANEL;
}
class VolumePanel {
  int mVibrator;
  int mToneGenerators;
  class StreamControl {
    int iconMuteRes;
    int iconRes;
    int seekbarView;
    int icon;
    int group;
    int streamType;
  }
  int STREAMS;
  class StreamResources {
    int RemoteStream;
    int MasterStream;
    int NotificationStream;
    int MediaStream;
    int AlarmStream;
    int VoiceStream;
    int RingerStream;
    int BluetoothSCOStream;
    int show;
    int iconMuteRes;
    int iconRes;
    int descRes;
    int streamType;
  }
  int mStreamControls;
  int mActiveStreamType;
  int mDivider;
  int mMoreButton;
  int mSliderGroup;
  int mPanel;
  int mView;
  int mDialog;
  int mVoiceCapable;
  int mShowCombinedVolumes;
  int mRingIsSilent;
  int mAudioService;
  int mAudioManager;
  int mContext;
  int STREAM_MASTER;
  int MSG_SLIDER_VISIBILITY_CHANGED;
  int MSG_REMOTE_VOLUME_UPDATE_IF_SHOWN;
  int MSG_REMOTE_VOLUME_CHANGED;
  int MSG_MUTE_CHANGED;
  int MSG_RINGER_MODE_CHANGED;
  int MSG_TIMEOUT;
  int MSG_VIBRATE;
  int MSG_STOP_SOUNDS;
  int MSG_PLAY_SOUND;
  int MSG_FREE_RESOURCES;
  int MSG_VOLUME_CHANGED;
  int TIMEOUT_DELAY;
  int FREE_DELAY;
  int MAX_VOLUME;
  int BEEP_DURATION;
  int VIBRATE_DURATION;
  int VIBRATE_DELAY;
  int PLAY_SOUND_DELAY;
  int LOGD;
  int TAG;
}
class VisibilityTest {
  int mGone;
  int mInvisible;
  int mVisible;
  int mVictim;
  int mRefDown;
  int mRefUp;
}
class VisibilityCallbackTest {
  int mGone;
  int mInvisible;
  int mVisible;
  int mParent;
  int mVictim;
  int mRefDown;
  int mRefUp;
}
class VisibilityCallback {
  class MonitoredTextView {
    int mLastChangedVisibility;
    int mLastVisChangedView;
  }
  int mGoneListener;
  int mInvisibleListener;
  int mVisibleListener;
  int mVictim;
  int DEBUG;
}
class Visibility {
  int mVictim;
  int mGoneListener;
  int mInvisibleListener;
  int mVisibleListener;
}
class View_Delegate {
}
class ViewTreeObserver {
  class OnComputeInternalInsetsListener {
  }
  class InternalInsetsInfo {
    int mTouchableInsets;
    int TOUCHABLE_INSETS_REGION;
    int TOUCHABLE_INSETS_VISIBLE;
    int TOUCHABLE_INSETS_CONTENT;
    int TOUCHABLE_INSETS_FRAME;
    int touchableRegion;
    int visibleInsets;
    int contentInsets;
  }
  class OnScrollChangedListener {
  }
  class OnTouchModeChangeListener {
  }
  class OnDrawListener {
  }
  class OnPreDrawListener {
  }
  class OnGlobalLayoutListener {
  }
  class OnGlobalFocusChangeListener {
  }
  int mAlive;
  int mOnDrawListeners;
  int mOnPreDrawListeners;
  int mOnScrollChangedListeners;
  int mOnComputeInternalInsetsListeners;
  int mOnTouchModeChangeListeners;
  int mOnGlobalLayoutListeners;
  int mOnGlobalFocusListeners;
}
class ViewStubTest {
}
class ViewStub {
  class OnInflateListener {
  }
  int mInflateListener;
  int mInflater;
  int mInflatedViewRef;
  int mInflatedId;
  int mLayoutResource;
}
class ViewRootImpl_Delegate {
}
class ViewRootImpl {
  class SendWindowContentChangedAccessibilityEvent {
    int mSource;
  }
  class AccessibilityInteractionConnection {
    int mViewRootImpl;
  }
  class AccessibilityInteractionConnectionManager {
  }
  class RunQueue {
    class HandlerAction {
      int delay;
      int action;
    }
    int mActions;
  }
  int mHolder;
  class CalledFromWrongThreadException {
  }
  class TrackballAxis {
    int nonAccelMovement;
    int dir;
    int step;
    int lastMoveTime;
    int acceleration;
    int absPosition;
    int position;
    int ACCEL_MOVE_SCALING_FACTOR;
    int FAST_MOVE_TIME;
    int MAX_ACCELERATION;
  }
  class W {
    int mViewAncestor;
  }
  class InputMethodCallback {
    int mViewAncestor;
  }
  class TakenSurfaceHolder {
  }
  int mInvalidateOnAnimationRunnable;
  class InvalidateOnAnimationRunnable {
    int mTempViewRects;
    int mTempViews;
    int mViewRects;
    int mViews;
    int mPosted;
  }
  int mConsumeBatchedInputScheduled;
  int mConsumedBatchedInputRunnable;
  class ConsumeBatchedInputRunnable {
  }
  int mInputEventReceiver;
  class WindowInputEventReceiver {
  }
  int mTraversalRunnable;
  class TraversalRunnable {
  }
  class QueuedInputEvent {
    int mFlags;
    int mReceiver;
    int mEvent;
    int mNext;
    int FLAG_DELIVER_POST_IME;
  }
  int mHandler;
  class ViewRootHandler {
  }
  int MSG_INVALIDATE_WORLD;
  int MSG_DISPATCH_DONE_ANIMATING;
  int MSG_CLEAR_ACCESSIBILITY_FOCUS_HOST;
  int MSG_INVALIDATE_DISPLAY_LIST;
  int MSG_DISPATCH_SCREEN_STATE;
  int MSG_PROCESS_INPUT_EVENTS;
  int MSG_UPDATE_CONFIGURATION;
  int MSG_DISPATCH_SYSTEM_UI_VISIBILITY;
  int MSG_DISPATCH_DRAG_LOCATION_EVENT;
  int MSG_DISPATCH_DRAG_EVENT;
  int MSG_CLOSE_SYSTEM_DIALOGS;
  int MSG_CHECK_FOCUS;
  int MSG_FINISH_INPUT_CONNECTION;
  int MSG_DISPATCH_KEY_FROM_IME;
  int MSG_IME_FINISHED_EVENT;
  int MSG_DISPATCH_GET_NEW_SURFACE;
  int MSG_DISPATCH_APP_VISIBILITY;
  int MSG_DISPATCH_KEY;
  int MSG_WINDOW_FOCUS_CHANGED;
  int MSG_RESIZED_REPORT;
  int MSG_RESIZED;
  int MSG_DIE;
  int MSG_INVALIDATE_RECT;
  int MSG_INVALIDATE;
  int mResizePaint;
  int mResizeAlpha;
  int mHardwareYOffset;
  int mProfile;
  class SystemUiVisibilityInfo {
    int localChanges;
    int localValue;
    int globalVisibility;
    int seq;
  }
  int mInputEventConsistencyVerifier;
  int mDensity;
  int mTempHashSet;
  int mSendWindowContentChangedAccessibilityEvent;
  int mAccessibilityInteractionConnectionManager;
  int mAccessibilityInteractionController;
  int mAccessibilityManager;
  int mAudioManager;
  int mDisplayLists;
  int mFpsNumFrames;
  int mFpsPrevTime;
  int mFpsStartTime;
  int mRenderProfilingEnabled;
  int mRenderProfiler;
  int mProfileRendering;
  int mLastTouchPoint;
  int mDragPoint;
  int mLocalDragState;
  int mCurrentDragView;
  int mDragDescription;
  int mViewConfiguration;
  int mPendingTransitions;
  int mResizeInterpolator;
  int mResizeBufferDuration;
  int mResizeBufferStartTime;
  int mResizeBuffer;
  int mScroller;
  int mCurScrollY;
  int mScrollY;
  int mLastScrolledFocus;
  int mSoftInputMode;
  int mScrollMayChange;
  class ResizedInfo {
    int newConfig;
    int visibleInsets;
    int contentInsets;
  }
  int mPendingConfiguration;
  int mLastConfiguration;
  int mFitSystemWindowsInsets;
  int mLastGivenInsets;
  int mPendingContentInsets;
  int mPendingVisibleInsets;
  int mWinFrame;
  int mAddNesting;
  int mCompatibilityInfo;
  int mAddedTouchMode;
  int mAdded;
  int mSurface;
  int mWindowAttributesChangesFlag;
  int mWindowAttributesChanged;
  int mProcessInputEventsScheduled;
  int mCurrentInputEvent;
  int mFirstPendingInputEvent;
  int mQueuedInputEventPoolSize;
  int mQueuedInputEventPool;
  int MAX_QUEUED_INPUT_EVENT_POOL_SIZE;
  int mClientWindowLayoutFlags;
  int mLastSystemUiVisibility;
  int mIsDrawing;
  int mWindowsAnimating;
  int mLastWasImTarget;
  int mHasHadWindowFocus;
  int mNewSurfaceNeeded;
  int mFullRedrawNeeded;
  int mReportNextDraw;
  int mFirst;
  int mLayoutRequested;
  int mFitSystemWindowsRequested;
  int mWillDrawSoon;
  int mTraversalBarrier;
  int mTraversalScheduled;
  int mVisRect;
  int mTempRect;
  int mChoreographer;
  int mFallbackEventHandler;
  int mInputQueue;
  int mInputQueueCallback;
  int mInputChannel;
  int mAttachInfo;
  int mTranslator;
  int mIsAnimating;
  int mPreviousDirty;
  int mCurrentDirty;
  int mDirty;
  int mHeight;
  int mWidth;
  int mPreviousTransparentRegion;
  int mTransparentRegion;
  int mDrawingAllowed;
  int mIsCreating;
  int mSurfaceHolder;
  int mSurfaceHolderCallback;
  int mLastInCompatMode;
  int mStopped;
  int mOrigWindowType;
  int mAppVisible;
  int mViewVisibility;
  int mAccessibilityFocusedVirtualView;
  int mAccessibilityFocusedHost;
  int mOldFocusedView;
  int mRealFocusedView;
  int mFocusedView;
  int mView;
  int mSeq;
  int mTargetSdkVersion;
  int mWindow;
  int mWindowAttributes;
  int mLocation;
  int mThread;
  int mInputMethodCallback;
  int mTmpValue;
  int mTmpLocation;
  int mLastJoystickYKeyCode;
  int mLastJoystickXKeyCode;
  int mLastJoystickYDirection;
  int mLastJoystickXDirection;
  int mTrackballAxisY;
  int mTrackballAxisX;
  int mLastTrackballTime;
  int sRenderThreadQueryLock;
  int sRenderThreadQueried;
  int sUseRenderThread;
  int sConfigCallbacks;
  int sFirstDrawComplete;
  int sFirstDrawHandlers;
  int sRunQueues;
  int mInitialized;
  int mStaticInit;
  int sWindowSession;
  int MAX_TRACKBALL_DELAY;
  int lt;
  int MEASURE_LATENCY;
  int PROPERTY_PROFILE_RENDERING;
  int USE_RENDER_THREAD;
  int DEBUG_FPS;
  int DEBUG_CONFIGURATION;
  int DEBUG_IMF;
  int DEBUG_TRACKBALL;
  int DEBUG_ORIENTATION;
  int DEBUG_INPUT_RESIZE;
  int DEBUG_DIALOG;
  int DEBUG_LAYOUT;
  int DEBUG_DRAW;
  int LOCAL_LOGV;
  int DBG;
  int TAG;
}
class ViewPropertyAnimator {
  class AnimatorEventListener {
  }
  class NameValuesHolder {
    int mDeltaValue;
    int mFromValue;
    int mNameConstant;
  }
  int mAnimatorOnEndMap;
  int mAnimatorOnStartMap;
  int mAnimatorCleanupMap;
  int mAnimatorSetupMap;
  int mAnimatorMap;
  class PropertyBundle {
    int mNameValuesHolder;
    int mPropertyMask;
  }
  int mAnimationStarter;
  int TRANSFORM_MASK;
  int ALPHA;
  int Y;
  int X;
  int ROTATION_Y;
  int ROTATION_X;
  int ROTATION;
  int SCALE_Y;
  int SCALE_X;
  int TRANSLATION_Y;
  int TRANSLATION_X;
  int NONE;
  int mPendingOnEndAction;
  int mPendingOnStartAction;
  int mPendingCleanupAction;
  int mPendingSetupAction;
  int mPendingAnimations;
  int mAnimatorEventListener;
  int mListener;
  int mInterpolatorSet;
  int mInterpolator;
  int mStartDelaySet;
  int mStartDelay;
  int mDurationSet;
  int mDuration;
  int mView;
}
class ViewParent {
}
class ViewManager {
}
class ViewGroupChildrenTest {
  int mGroup;
}
class ViewGroupChildren {
}
class ViewGroupAttributesTest {
  class MyViewGroup {
  }
  int mViewGroup;
}
class ViewGroup {
  class ViewLocationHolder {
    int mLayoutDirection;
    int mView;
    int mLocation;
    int mNext;
    int mIsPooled;
    int sPoolSize;
    int sPool;
    int sPoolLock;
    int MAX_POOL_SIZE;
  }
  class ChildListForAccessibility {
    int mHolders;
    int mChildren;
    int mNext;
    int mIsPooled;
    int sPoolSize;
    int sPool;
    int sPoolLock;
    int MAX_POOL_SIZE;
  }
  class HoverTarget {
    int next;
    int child;
    int sRecycledCount;
    int sRecycleBin;
    int sRecycleLock;
    int MAX_RECYCLED;
  }
  class TouchTarget {
    int next;
    int pointerIdBits;
    int child;
    int ALL_POINTER_IDS;
    int sRecycledCount;
    int sRecycleBin;
    int sRecycleLock;
    int MAX_RECYCLED;
  }
  class MarginLayoutParams {
    int DEFAULT_RELATIVE;
    int endMargin;
    int startMargin;
    int bottomMargin;
    int rightMargin;
    int topMargin;
    int leftMargin;
  }
  class LayoutParams {
    int layoutAnimationParameters;
    int height;
    int width;
    int WRAP_CONTENT;
    int MATCH_PARENT;
    int FILL_PARENT;
  }
  int mLayoutTransitionListener;
  class OnHierarchyChangeListener {
  }
  int mChildCountWithTransientState;
  int mDrawLayers;
  int mVisibilityChangingChildren;
  int mTransitioningViews;
  int mTransition;
  int mCachePaint;
  int sDebugLines;
  int sDebugPaint;
  int ARRAY_CAPACITY_INCREMENT;
  int ARRAY_INITIAL_CAPACITY;
  int mChildrenCount;
  int mLayoutSuppressed;
  int mChildren;
  int CHILD_TOP_INDEX;
  int CHILD_LEFT_INDEX;
  int CLIP_TO_PADDING_MASK;
  int OPTICAL_BOUNDS;
  int CLIP_BOUNDS;
  int PERSISTENT_ALL_CACHES;
  int PERSISTENT_SCROLLING_CACHE;
  int PERSISTENT_ANIMATION_CACHE;
  int PERSISTENT_NO_CACHE;
  int mPersistentDrawingCache;
  int FLAG_PREVENT_DISPATCH_ATTACHED_TO_WINDOW;
  int FLAG_SPLIT_MOTION_EVENTS;
  int FLAG_DISALLOW_INTERCEPT;
  int DESCENDANT_FOCUSABILITY_FLAGS;
  int FOCUS_BLOCK_DESCENDANTS;
  int FOCUS_AFTER_DESCENDANTS;
  int FOCUS_BEFORE_DESCENDANTS;
  int FLAG_MASK_FOCUSABILITY;
  int FLAG_NOTIFY_CHILDREN_ON_DRAWABLE_STATE_CHANGE;
  int FLAG_CHILDREN_DRAWN_WITH_CACHE;
  int FLAG_ALWAYS_DRAWN_WITH_CACHE;
  int FLAG_ADD_STATES_FROM_CHILDREN;
  int FLAG_ALPHA_LOWER_THAN_ONE;
  int FLAG_SUPPORT_STATIC_TRANSFORMATIONS;
  int FLAG_USE_CHILD_DRAWING_ORDER;
  int FLAG_NOTIFY_ANIMATION_LISTENER;
  int FLAG_CLEAR_TRANSFORMATION;
  int FLAG_OPTIMIZE_INVALIDATE;
  int FLAG_ANIMATION_CACHE;
  int FLAG_PADDING_NOT_NULL;
  int FLAG_ANIMATION_DONE;
  int FLAG_RUN_ANIMATION;
  int FLAG_INVALIDATE_REQUIRED;
  int FLAG_CLIP_TO_PADDING;
  int FLAG_CLIP_CHILDREN;
  int mLayoutMode;
  int mGroupFlags;
  int mHoveredSelf;
  int mFirstHoverTarget;
  int mLastTouchDownY;
  int mLastTouchDownX;
  int mLastTouchDownIndex;
  int mLastTouchDownTime;
  int mFirstTouchTarget;
  int mAnimationListener;
  int mLayoutAnimationController;
  int mLocalPoint;
  int mChildAcceptsDrag;
  int mDragNotifiedChildren;
  int mCurrentDrag;
  int mCurrentDragView;
  int mInvalidationTransformation;
  int mInvalidateRegion;
  int mChildTransformation;
  int mFocused;
  int mOnHierarchyChangeListener;
  int mDisappearingChildren;
  int DBG;
  int TAG;
}
class ViewDebug {
  class ViewOperation {
  }
  class RecyclerTraceType {
    int MOVE_FROM_ACTIVE_TO_SCRAP_HEAP;
    int MOVE_TO_SCRAP_HEAP;
    int RECYCLE_FROM_SCRAP_HEAP;
    int RECYCLE_FROM_ACTIVE_HEAP;
    int BIND_VIEW;
    int NEW_VIEW;
  }
  class HierarchyTraceType {
    int BUILD_CACHE;
    int DRAW;
    int ON_MEASURE;
    int ON_LAYOUT;
    int REQUEST_LAYOUT;
    int INVALIDATE_CHILD_IN_PARENT;
    int INVALIDATE_CHILD;
    int INVALIDATE;
  }
  int sAnnotations;
  int sMethodsForClasses;
  int sFieldsForClasses;
  int REMOTE_COMMAND_OUTPUT_DISPLAYLIST;
  int REMOTE_COMMAND_CAPTURE_LAYERS;
  int REMOTE_PROFILE;
  int REMOTE_COMMAND_REQUEST_LAYOUT;
  int REMOTE_COMMAND_INVALIDATE;
  int REMOTE_COMMAND_DUMP;
  int REMOTE_COMMAND_CAPTURE;
  int CAPTURE_TIMEOUT;
  int mCapturedViewFieldsForClasses;
  int mCapturedViewMethodsForClasses;
  int DEBUG_DRAG;
  int TRACE_RECYCLER;
  int TRACE_HIERARCHY;
}
class ViewConfiguration_Accessor {
}
class ViewConfiguration {
  int sConfigurations;
  int sHasPermanentMenuKeySet;
  int sHasPermanentMenuKey;
  int mFadingMarqueeEnabled;
  int mOverflingDistance;
  int mOverscrollDistance;
  int mMaximumDrawingCacheSize;
  int mWindowTouchSlop;
  int mDoubleTapSlop;
  int mPagingTouchSlop;
  int mDoubleTapTouchSlop;
  int mTouchSlop;
  int mScrollbarSize;
  int mMaximumFlingVelocity;
  int mMinimumFlingVelocity;
  int mFadingEdgeLength;
  int mEdgeSlop;
  int OVERFLING_DISTANCE;
  int OVERSCROLL_DISTANCE;
  int SCROLL_FRICTION;
  int MAXIMUM_DRAWING_CACHE_SIZE;
  int SEND_RECURRING_ACCESSIBILITY_EVENTS_INTERVAL_MILLIS;
  int MAXIMUM_FLING_VELOCITY;
  int MINIMUM_FLING_VELOCITY;
  int WINDOW_TOUCH_SLOP;
  int DOUBLE_TAP_SLOP;
  int PAGING_TOUCH_SLOP;
  int DOUBLE_TAP_TOUCH_SLOP;
  int TOUCH_SLOP;
  int EDGE_SLOP;
  int ZOOM_CONTROLS_TIMEOUT;
  int HOVER_TAP_SLOP;
  int HOVER_TAP_TIMEOUT;
  int DOUBLE_TAP_TIMEOUT;
  int JUMP_TAP_TIMEOUT;
  int TAP_TIMEOUT;
  int GLOBAL_ACTIONS_KEY_TIMEOUT;
  int KEY_REPEAT_DELAY;
  int DEFAULT_LONG_PRESS_TIMEOUT;
  int PRESSED_STATE_DURATION;
  int FADING_EDGE_LENGTH;
  int SCROLL_BAR_DEFAULT_DELAY;
  int SCROLL_BAR_FADE_DURATION;
  int SCROLL_BAR_SIZE;
  int ALPHA_THRESHOLD_INT;
  int ALPHA_THRESHOLD;
  int PANEL_BIT_DEPTH;
}
class ViewAttachView {
  int attached;
  int TAG;
}
class ViewAttachTestActivity {
  int TAG;
}
class ViewAttachTest {
}
class View {
  class AccessibilityDelegate {
  }
  class SendViewScrolledAccessibilityEvent {
    int mIsPending;
  }
  class ScrollabilityCache {
    int mLastColor;
    int state;
    int fadeStartTime;
    int TRANSPARENT;
    int OPAQUE;
    int scrollBarInterpolator;
    int shader;
    int matrix;
    int paint;
    int host;
    int interpolatorValues;
    int scrollBar;
    int scrollBarSize;
    int scrollBarFadeDuration;
    int scrollBarDefaultDelayBeforeFade;
    int fadingEdgeLength;
    int fadeScrollBars;
    int FADING;
    int ON;
    int OFF;
  }
  class AttachInfo {
    int mPoint;
    int mDebugLayout;
    int mAccessibilityFocusDrawable;
    int mIncludeNotImportantViews;
    int mAccessibilityWindowId;
    int mTempArrayList;
    int mTmpTransformRect;
    int mTmpInvalRect;
    int mHandler;
    int mViewRootImpl;
    int mCanvas;
    int mTreeObserver;
    int mTmpTransformLocation;
    int mInvalidateChildLocation;
    int mTransparentLocation;
    int mViewScrollChanged;
    int mViewVisibilityChanged;
    int mHasSystemUiListeners;
    int mGlobalSystemUiVisibility;
    int mDisabledSystemUiVisibility;
    int mSystemUiVisibility;
    int mKeepScreenOn;
    int mForceReportNewAttributes;
    int mRecomputeGlobalAttributes;
    int mInTouchMode;
    int mSetIgnoreDirtyState;
    int mIgnoreDirtyState;
    int mDrawingTime;
    int mWindowVisibility;
    int mHasWindowFocus;
    int mKeyDispatchState;
    int mScrollContainers;
    int mGivenInternalInsets;
    int mVisibleInsets;
    int mContentInsets;
    int mUse32BitDrawingCache;
    int mActualWindowTop;
    int mActualWindowLeft;
    int mWindowTop;
    int mWindowLeft;
    int mTurnOffWindowResizeAnim;
    int mScalingRequired;
    int mApplicationScale;
    int mScreenOn;
    int mHardwareRenderer;
    int mHardwareAccelerationRequested;
    int mHardwareAccelerated;
    int mSurface;
    int mPanelParentWindowToken;
    int mRootView;
    int mHardwareCanvas;
    int mRootCallbacks;
    int mWindowToken;
    int mWindow;
    int mSession;
    class InvalidateInfo {
      int bottom;
      int right;
      int top;
      int left;
      int target;
      int mIsPooled;
      int mNext;
      int sPool;
      int POOL_LIMIT;
    }
    class Callbacks {
    }
  }
  class BaseSavedState {
    int CREATOR;
  }
  class UnsetPressedState {
  }
  class OnAttachStateChangeListener {
  }
  class OnSystemUiVisibilityChangeListener {
  }
  class OnCreateContextMenuListener {
  }
  class OnClickListener {
  }
  class OnFocusChangeListener {
  }
  class OnDragListener {
  }
  class OnLongClickListener {
  }
  class OnGenericMotionListener {
  }
  class OnHoverListener {
  }
  class OnTouchListener {
  }
  class OnKeyListener {
  }
  class PerformClick {
  }
  class CheckForTap {
  }
  class CheckForLongPress {
    int mOriginalWindowAttachCount;
  }
  class MeasureSpec {
    int AT_MOST;
    int EXACTLY;
    int UNSPECIFIED;
    int MODE_MASK;
    int MODE_SHIFT;
  }
  int SCALE_Y;
  int SCALE_X;
  int ROTATION_Y;
  int ROTATION_X;
  int ROTATION;
  int Y;
  int X;
  int TRANSLATION_Y;
  int TRANSLATION_X;
  int ALPHA;
  class DragShadowBuilder {
    int mView;
  }
  class OnLayoutChangeListener {
  }
  int mInputEventConsistencyVerifier;
  int mAccessibilityDelegate;
  int mSendingHoverAccessibilityEvents;
  int mLocalDirtyRect;
  int mLayerPaint;
  int mLayerType;
  int LAYER_TYPE_HARDWARE;
  int LAYER_TYPE_SOFTWARE;
  int LAYER_TYPE_NONE;
  int SCROLLBAR_POSITION_RIGHT;
  int SCROLLBAR_POSITION_LEFT;
  int SCROLLBAR_POSITION_DEFAULT;
  int mVerticalScrollbarPosition;
  int mVerticalScrollFactor;
  int DRAG_FLAG_GLOBAL;
  int mAnimator;
  int mTouchSlop;
  int mFloatingTreeObserver;
  int mDrawingCacheBackgroundColor;
  int mTouchDelegate;
  int mMinWidth;
  int mMinHeight;
  int mHasPerformedLongPress;
  int mUnsetPressedState;
  int mSendViewScrolledAccessibilityEvent;
  int mPerformClick;
  int mPendingCheckForTap;
  int mPendingCheckForLongPress;
  int mNextFocusForwardId;
  int mNextFocusDownId;
  int mNextFocusUpId;
  int mNextFocusRightId;
  int mNextFocusLeftId;
  int mDisplayList;
  int mHardwareLayer;
  int mUnscaledDrawingCache;
  int mDrawingCache;
  int mCachingFailed;
  int mDrawableState;
  int mScrollCache;
  int mResources;
  int mContext;
  int mListenerInfo;
  class ListenerInfo {
    int mOnSystemUiVisibilityChangeListener;
    int mOnDragListener;
    int mOnGenericMotionListener;
    int mOnHoverListener;
    int mOnTouchListener;
    int mOnKeyListener;
    int mOnCreateContextMenuListener;
    int mOnLongClickListener;
    int mOnClickListener;
    int mOnAttachStateChangeListeners;
    int mOnLayoutChangeListeners;
    int mOnFocusChangeListener;
  }
  int mBackgroundSizeChanged;
  int mBackgroundResource;
  int mBackground;
  int mOldHeightMeasureSpec;
  int mOldWidthMeasureSpec;
  int mUserPaddingEnd;
  int mUserPaddingStart;
  int mUserPaddingRelative;
  int mUserPaddingLeft;
  int mUserPaddingBottom;
  int mUserPaddingRight;
  int mContentDescription;
  int mLayoutInsets;
  int mPaddingBottom;
  int mPaddingTop;
  int mPaddingRight;
  int mPaddingLeft;
  int mScrollY;
  int mScrollX;
  int mBottom;
  int mTop;
  int mRight;
  int mLeft;
  int NONZERO_EPSILON;
  int mLastIsOpaque;
  int mTransformationInfo;
  class TransformationInfo {
    int mAlpha;
    int mPivotY;
    int mPivotX;
    int mScaleY;
    int mScaleX;
    int mTranslationY;
    int mTranslationX;
    int mRotation;
    int mRotationX;
    int mRotationY;
    int mPrevHeight;
    int mPrevWidth;
    int matrix3D;
    int mCamera;
    int mMatrixIsIdentity;
    int mInverseMatrixDirty;
    int mMatrixDirty;
    int mInverseMatrix;
    int mMatrix;
  }
  int mViewFlags;
  int mLayoutParams;
  int mWindowAttachCount;
  int mTransientStateCount;
  int mSystemUiVisibility;
  int mPrivateFlags3;
  int mPrivateFlags2;
  int mPrivateFlags;
  int mAttachInfo;
  int mParent;
  int mOverScrollMode;
  int SCREEN_STATE_ON;
  int SCREEN_STATE_OFF;
  int ACCESSIBILITY_CURSOR_POSITION_UNDEFINED;
  int FIND_VIEWS_WITH_ACCESSIBILITY_NODE_PROVIDERS;
  int FIND_VIEWS_WITH_CONTENT_DESCRIPTION;
  int FIND_VIEWS_WITH_TEXT;
  int SYSTEM_UI_LAYOUT_FLAGS;
  int SYSTEM_UI_CLEARABLE_FLAGS;
  int PUBLIC_STATUS_BAR_VISIBILITY_MASK;
  int STATUS_BAR_DISABLE_RECENT;
  int STATUS_BAR_DISABLE_CLOCK;
  int STATUS_BAR_DISABLE_BACK;
  int STATUS_BAR_DISABLE_HOME;
  int STATUS_BAR_DISABLE_SYSTEM_INFO;
  int STATUS_BAR_DISABLE_NOTIFICATION_TICKER;
  int STATUS_BAR_DISABLE_NOTIFICATION_ALERTS;
  int STATUS_BAR_DISABLE_NOTIFICATION_ICONS;
  int STATUS_BAR_DISABLE_EXPAND;
  int STATUS_BAR_VISIBLE;
  int STATUS_BAR_HIDDEN;
  int SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN;
  int SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION;
  int SYSTEM_UI_FLAG_LAYOUT_STABLE;
  int SYSTEM_UI_FLAG_FULLSCREEN;
  int SYSTEM_UI_FLAG_HIDE_NAVIGATION;
  int SYSTEM_UI_FLAG_LOW_PROFILE;
  int SYSTEM_UI_FLAG_VISIBLE;
  int OVER_SCROLL_NEVER;
  int OVER_SCROLL_IF_CONTENT_SCROLLS;
  int OVER_SCROLL_ALWAYS;
  int DRAG_MASK;
  int VIEW_IS_ANIMATING_ALPHA;
  int VIEW_IS_ANIMATING_TRANSFORM;
  int ACCESSIBILITY_FOCUSABLE_MASK;
  int ACCESSIBILITY_FOCUSABLE_DEFAULT;
  int ACCESSIBILITY_FOCUSABLE_NO;
  int ACCESSIBILITY_FOCUSABLE_YES;
  int ACCESSIBILITY_FOCUSABLE_AUTO;
  int ACCESSIBILITY_FOCUSABLE_SHIFT;
  int VIEW_QUICK_REJECTED;
  int ACCESSIBILITY_STATE_CHANGED;
  int ACCESSIBILITY_FOCUSED;
  int IMPORTANT_FOR_ACCESSIBILITY_MASK;
  int IMPORTANT_FOR_ACCESSIBILITY_DEFAULT;
  int IMPORTANT_FOR_ACCESSIBILITY_NO;
  int IMPORTANT_FOR_ACCESSIBILITY_YES;
  int IMPORTANT_FOR_ACCESSIBILITY_AUTO;
  int IMPORTANT_FOR_ACCESSIBILITY_SHIFT;
  int TEXT_ALIGNMENT_RESOLVED_DEFAULT;
  int TEXT_ALIGNMENT_RESOLVED_MASK;
  int TEXT_ALIGNMENT_RESOLVED_MASK_SHIFT;
  int TEXT_ALIGNMENT_RESOLVED;
  int TEXT_ALIGNMENT_FLAGS;
  int TEXT_ALIGNMENT_MASK;
  int TEXT_ALIGNMENT_MASK_SHIFT;
  int TEXT_ALIGNMENT_DEFAULT;
  int TEXT_ALIGNMENT_VIEW_END;
  int TEXT_ALIGNMENT_VIEW_START;
  int TEXT_ALIGNMENT_CENTER;
  int TEXT_ALIGNMENT_TEXT_END;
  int TEXT_ALIGNMENT_TEXT_START;
  int TEXT_ALIGNMENT_GRAVITY;
  int TEXT_ALIGNMENT_INHERIT;
  int TEXT_DIRECTION_RESOLVED_DEFAULT;
  int TEXT_DIRECTION_RESOLVED_MASK;
  int TEXT_DIRECTION_RESOLVED_MASK_SHIFT;
  int TEXT_DIRECTION_RESOLVED;
  int TEXT_DIRECTION_FLAGS;
  int TEXT_DIRECTION_MASK;
  int TEXT_DIRECTION_MASK_SHIFT;
  int TEXT_DIRECTION_DEFAULT;
  int TEXT_DIRECTION_LOCALE;
  int TEXT_DIRECTION_RTL;
  int TEXT_DIRECTION_LTR;
  int TEXT_DIRECTION_ANY_RTL;
  int TEXT_DIRECTION_FIRST_STRONG;
  int TEXT_DIRECTION_INHERIT;
  int HAS_TRANSIENT_STATE;
  int LAYOUT_DIRECTION_DEFAULT;
  int LAYOUT_DIRECTION_FLAGS;
  int LAYOUT_DIRECTION_RESOLVED_MASK;
  int LAYOUT_DIRECTION_RESOLVED;
  int LAYOUT_DIRECTION_RESOLVED_RTL;
  int LAYOUT_DIRECTION_MASK;
  int LAYOUT_DIRECTION_MASK_SHIFT;
  int LAYOUT_DIRECTION_LOCALE;
  int LAYOUT_DIRECTION_INHERIT;
  int LAYOUT_DIRECTION_RTL;
  int LAYOUT_DIRECTION_LTR;
  int DRAG_HOVERED;
  int DRAG_CAN_ACCEPT;
  int INVALIDATED;
  int ACTIVATED;
  int PIVOT_EXPLICITLY_SET;
  int HOVERED;
  int AWAKEN_SCROLL_BARS_ON_ATTACH;
  int CANCEL_NEXT_UP_EVENT;
  int PREPRESSED;
  int OPAQUE_MASK;
  int OPAQUE_SCROLLBARS;
  int OPAQUE_BACKGROUND;
  int DIRTY_MASK;
  int DIRTY_OPAQUE;
  int DIRTY;
  int SCROLL_CONTAINER_ADDED;
  int SCROLL_CONTAINER;
  int ALPHA_SET;
  int SAVE_STATE_CALLED;
  int ANIMATION_STARTED;
  int DRAWING_CACHE_VALID;
  int PRESSED;
  int LAYOUT_REQUIRED;
  int FORCE_LAYOUT;
  int MEASURED_DIMENSION_SET;
  int DRAWABLE_STATE_DIRTY;
  int REQUEST_TRANSPARENT_REGIONS;
  int ONLY_DRAWS_BACKGROUND;
  int SKIP_DRAW;
  int DRAW_ANIMATION;
  int DRAWN;
  int HAS_BOUNDS;
  int IS_ROOT_NAMESPACE;
  int SELECTED;
  int FOCUSED;
  int WANTS_FOCUS;
  int mTag;
  int mAccessibilityCursorPosition;
  int mAccessibilityViewId;
  int mID;
  int mRecreateDisplayList;
  int mMeasuredHeight;
  int mMeasuredWidth;
  int mCurrentAnimation;
  int sNextAccessibilityViewId;
  int mKeyedTags;
  int sThreadLocal;
  int POPULATING_ACCESSIBILITY_EVENT_TYPES;
  int VIEW_STATE_IDS;
  int VIEW_STATE_DRAG_HOVERED;
  int VIEW_STATE_DRAG_CAN_ACCEPT;
  int VIEW_STATE_HOVERED;
  int VIEW_STATE_ACCELERATED;
  int VIEW_STATE_ACTIVATED;
  int VIEW_STATE_PRESSED;
  int VIEW_STATE_ENABLED;
  int VIEW_STATE_FOCUSED;
  int VIEW_STATE_SELECTED;
  int VIEW_STATE_WINDOW_FOCUSED;
  int VIEW_STATE_SETS;
  int PRESSED_ENABLED_FOCUSED_SELECTED_WINDOW_FOCUSED_STATE_SET;
  int PRESSED_ENABLED_FOCUSED_SELECTED_STATE_SET;
  int PRESSED_ENABLED_FOCUSED_WINDOW_FOCUSED_STATE_SET;
  int PRESSED_ENABLED_FOCUSED_STATE_SET;
  int PRESSED_ENABLED_SELECTED_WINDOW_FOCUSED_STATE_SET;
  int PRESSED_ENABLED_SELECTED_STATE_SET;
  int PRESSED_ENABLED_WINDOW_FOCUSED_STATE_SET;
  int PRESSED_ENABLED_STATE_SET;
  int PRESSED_FOCUSED_SELECTED_WINDOW_FOCUSED_STATE_SET;
  int PRESSED_FOCUSED_SELECTED_STATE_SET;
  int PRESSED_FOCUSED_WINDOW_FOCUSED_STATE_SET;
  int PRESSED_FOCUSED_STATE_SET;
  int PRESSED_SELECTED_WINDOW_FOCUSED_STATE_SET;
  int PRESSED_SELECTED_STATE_SET;
  int PRESSED_WINDOW_FOCUSED_STATE_SET;
  int ENABLED_FOCUSED_SELECTED_WINDOW_FOCUSED_STATE_SET;
  int FOCUSED_SELECTED_WINDOW_FOCUSED_STATE_SET;
  int ENABLED_SELECTED_WINDOW_FOCUSED_STATE_SET;
  int ENABLED_FOCUSED_WINDOW_FOCUSED_STATE_SET;
  int ENABLED_FOCUSED_SELECTED_STATE_SET;
  int SELECTED_WINDOW_FOCUSED_STATE_SET;
  int FOCUSED_WINDOW_FOCUSED_STATE_SET;
  int FOCUSED_SELECTED_STATE_SET;
  int ENABLED_WINDOW_FOCUSED_STATE_SET;
  int ENABLED_SELECTED_STATE_SET;
  int ENABLED_FOCUSED_STATE_SET;
  int WINDOW_FOCUSED_STATE_SET;
  int PRESSED_STATE_SET;
  int SELECTED_STATE_SET;
  int FOCUSED_STATE_SET;
  int ENABLED_STATE_SET;
  int EMPTY_STATE_SET;
  int MEASURED_STATE_TOO_SMALL;
  int MEASURED_HEIGHT_STATE_SHIFT;
  int MEASURED_STATE_MASK;
  int MEASURED_SIZE_MASK;
  int ACCESSIBILITY_FOCUS_BACKWARD;
  int ACCESSIBILITY_FOCUS_FORWARD;
  int ACCESSIBILITY_FOCUS_DOWN;
  int ACCESSIBILITY_FOCUS_RIGHT;
  int ACCESSIBILITY_FOCUS_UP;
  int ACCESSIBILITY_FOCUS_LEFT;
  int FOCUS_ACCESSIBILITY;
  int FOCUS_DOWN;
  int FOCUS_RIGHT;
  int FOCUS_UP;
  int FOCUS_LEFT;
  int FOCUS_FORWARD;
  int FOCUS_BACKWARD;
  int FOCUSABLES_ACCESSIBILITY;
  int FOCUSABLES_TOUCH_MODE;
  int FOCUSABLES_ALL;
  int PARENT_SAVE_DISABLED_MASK;
  int PARENT_SAVE_DISABLED;
  int HAPTIC_FEEDBACK_ENABLED;
  int SOUND_EFFECTS_ENABLED;
  int KEEP_SCREEN_ON;
  int SCROLLBARS_STYLE_MASK;
  int SCROLLBARS_OUTSIDE_MASK;
  int SCROLLBARS_INSET_MASK;
  int SCROLLBARS_OUTSIDE_INSET;
  int SCROLLBARS_OUTSIDE_OVERLAY;
  int SCROLLBARS_INSIDE_INSET;
  int SCROLLBARS_INSIDE_OVERLAY;
  int DUPLICATE_PARENT_STATE;
  int LONG_CLICKABLE;
  int DRAWING_CACHE_QUALITY_MASK;
  int DRAWING_CACHE_QUALITY_FLAGS;
  int DRAWING_CACHE_QUALITY_AUTO;
  int DRAWING_CACHE_QUALITY_HIGH;
  int DRAWING_CACHE_QUALITY_LOW;
  int FOCUSABLE_IN_TOUCH_MODE;
  int WILL_NOT_CACHE_DRAWING;
  int SAVE_DISABLED_MASK;
  int SAVE_DISABLED;
  int DRAWING_CACHE_ENABLED;
  int CLICKABLE;
  int FADING_EDGE_MASK;
  int FADING_EDGE_VERTICAL;
  int FADING_EDGE_HORIZONTAL;
  int FADING_EDGE_NONE;
  int OPTIONAL_FITS_SYSTEM_WINDOWS;
  int FILTER_TOUCHES_WHEN_OBSCURED;
  int SCROLLBARS_MASK;
  int SCROLLBARS_VERTICAL;
  int SCROLLBARS_HORIZONTAL;
  int SCROLLBARS_NONE;
  int DRAW_MASK;
  int WILL_NOT_DRAW;
  int ENABLED_MASK;
  int DISABLED;
  int ENABLED;
  int VISIBILITY_FLAGS;
  int VISIBILITY_MASK;
  int GONE;
  int INVISIBLE;
  int VISIBLE;
  int FITS_SYSTEM_WINDOWS;
  int FOCUSABLE_MASK;
  int FOCUSABLE;
  int NOT_FOCUSABLE;
  int NO_ID;
  int DEBUG_LAYOUT_PROPERTY;
  int VIEW_LOG_TAG;
  int DBG;
}
class VelocityTracker {
  class Estimator {
    int confidence;
    int degree;
    int yCoeff;
    int xCoeff;
    int MAX_DEGREE;
  }
  int mIsPooled;
  int mNext;
  int mStrategy;
  int mPtr;
  int ACTIVE_POINTER_ID;
  int sPool;
}
class TouchDelegate {
  int mSlop;
  int TO_RIGHT;
  int TO_LEFT;
  int BELOW;
  int ABOVE;
  int mDelegateTargeted;
  int mSlopBounds;
  int mBounds;
  int mDelegateView;
}
class TextureView {
  class SurfaceTextureListener {
  }
  int mNativeWindow;
  int mNativeWindowLock;
  int mSaveCount;
  int mCanvas;
  int mUpdateListener;
  int mUpdateSurface;
  int mUpdateLayer;
  int mLock;
  int mMatrixChanged;
  int mMatrix;
  int mOpaque;
  int mListener;
  int mSurface;
  int mLayer;
  int LOG_TAG;
}
class SurfaceView {
  int mSurfaceHolder;
}
class SurfaceSession {
  int mClient;
}
class SurfaceHolder {
  class Callback2 {
  }
  class Callback {
  }
  class BadSurfaceTypeException {
  }
  int SURFACE_TYPE_PUSH_BUFFERS;
  int SURFACE_TYPE_GPU;
  int SURFACE_TYPE_HARDWARE;
  int SURFACE_TYPE_NORMAL;
}
class Surface {
  int CREATOR;
  class CompatibleCanvas {
    int mOrigMatrix;
  }
  int mCreationStack;
  int mCompatibleMatrix;
  int mCompatibilityTranslator;
  int mName;
  int mSurfaceGenerationId;
  int mNativeSurface;
  int mCanvas;
  int mSaveCount;
  int mSurfaceControl;
  int SURFACE_DITHER;
  int SURFACE_FROZEN;
  int SURFACE_HIDDEN;
  int FX_SURFACE_MASK;
  int FX_SURFACE_SCREENSHOT;
  int FX_SURFACE_DIM;
  int FX_SURFACE_BLUR;
  int FX_SURFACE_NORMAL;
  int PROTECTED_APP;
  int OPAQUE;
  int NON_PREMULTIPLIED;
  int SECURE;
  int HIDDEN;
  class OutOfResourcesException {
  }
  int headless;
  int ROTATION_270;
  int ROTATION_180;
  int ROTATION_90;
  int ROTATION_0;
  int DEBUG_RELEASE;
  int LOG_TAG;
}
class SubMenu {
}
class StubbedView {
}
class SoundEffectConstants {
  int NAVIGATION_DOWN;
  int NAVIGATION_RIGHT;
  int NAVIGATION_UP;
  int NAVIGATION_LEFT;
  int CLICK;
}
class SetTagsTest {
  int mView;
}
class ScaleGestureDetector {
  int mInputEventConsistencyVerifier;
  int mActive0MostRecent;
  int mActiveId1;
  int mActiveId0;
  int mInvalidGesture;
  int mTimeDelta;
  int mPrevPressure;
  int mCurrPressure;
  int mScaleFactor;
  int mPrevLen;
  int mCurrLen;
  int mCurrFingerDiffY;
  int mCurrFingerDiffX;
  int mPrevFingerDiffY;
  int mPrevFingerDiffX;
  int mFocusY;
  int mFocusX;
  int mCurrEvent;
  int mPrevEvent;
  int mGestureInProgress;
  int mListener;
  int mContext;
  int PRESSURE_THRESHOLD;
  class SimpleOnScaleGestureListener {
  }
  class OnScaleGestureListener {
  }
  int TAG;
}
class RunQueueTest {
}
class RunQueue {
  int viewTreeObserver;
  int globalLayout;
  int runnableCancelled;
  int runnableRan;
}
class RemoteViewsActivity {
}
class PreDrawListener {
  class MyLinearLayout {
    int mCancelNextDraw;
  }
  int mFrame;
}
class PopupWindowVisibility {
  int COUNTRIES;
  int mStrings;
  int mShow;
  int mHide;
  int mFrame;
}
class PointerIcon {
  int CREATOR;
  int mHotSpotY;
  int mHotSpotX;
  int mBitmap;
  int mSystemIconResourceId;
  int mStyle;
  int gNullIcon;
  int STYLE_DEFAULT;
  int STYLE_OEM_FIRST;
  int STYLE_SPOT_ANCHOR;
  int STYLE_SPOT_TOUCH;
  int STYLE_SPOT_HOVER;
  int STYLE_ARROW;
  int STYLE_NULL;
  int STYLE_CUSTOM;
  int TAG;
}
class OrientationListener {
  class OrientationEventListenerInternal {
  }
  int ORIENTATION_UNKNOWN;
  int mOrientationEventLis;
}
class OrientationEventListener {
  class SensorEventListenerImpl {
    int _DATA_Z;
    int _DATA_Y;
    int _DATA_X;
  }
  int ORIENTATION_UNKNOWN;
  int mOldListener;
  int mSensorEventListener;
  int mSensor;
  int mRate;
  int mEnabled;
  int mSensorManager;
  int mOrientation;
  int localLOGV;
  int DEBUG;
  int TAG;
}
class MutateDrawableTest {
  int mSecondButton;
  int mFirstButton;
}
class MutateDrawable {
}
class MotionEvent {
  class PointerProperties {
    int toolType;
    int id;
  }
  class PointerCoords {
    int orientation;
    int toolMinor;
    int toolMajor;
    int touchMinor;
    int touchMajor;
    int size;
    int pressure;
    int y;
    int x;
    int mPackedAxisValues;
    int mPackedAxisBits;
    int INITIAL_PACKED_AXIS_VALUES;
  }
  int CREATOR;
  int mNext;
  int mNativePtr;
  int gSharedTempPointerIndexMap;
  int gSharedTempPointerProperties;
  int gSharedTempPointerCoords;
  int gSharedTempLock;
  int gRecyclerTop;
  int gRecyclerUsed;
  int gRecyclerLock;
  int MAX_RECYCLED;
  int HISTORY_CURRENT;
  int TOOL_TYPE_SYMBOLIC_NAMES;
  int TOOL_TYPE_ERASER;
  int TOOL_TYPE_MOUSE;
  int TOOL_TYPE_STYLUS;
  int TOOL_TYPE_FINGER;
  int TOOL_TYPE_UNKNOWN;
  int BUTTON_SYMBOLIC_NAMES;
  int BUTTON_FORWARD;
  int BUTTON_BACK;
  int BUTTON_TERTIARY;
  int BUTTON_SECONDARY;
  int BUTTON_PRIMARY;
  int AXIS_SYMBOLIC_NAMES;
  int AXIS_GENERIC_16;
  int AXIS_GENERIC_15;
  int AXIS_GENERIC_14;
  int AXIS_GENERIC_13;
  int AXIS_GENERIC_12;
  int AXIS_GENERIC_11;
  int AXIS_GENERIC_10;
  int AXIS_GENERIC_9;
  int AXIS_GENERIC_8;
  int AXIS_GENERIC_7;
  int AXIS_GENERIC_6;
  int AXIS_GENERIC_5;
  int AXIS_GENERIC_4;
  int AXIS_GENERIC_3;
  int AXIS_GENERIC_2;
  int AXIS_GENERIC_1;
  int AXIS_TILT;
  int AXIS_DISTANCE;
  int AXIS_BRAKE;
  int AXIS_GAS;
  int AXIS_WHEEL;
  int AXIS_RUDDER;
  int AXIS_THROTTLE;
  int AXIS_RTRIGGER;
  int AXIS_LTRIGGER;
  int AXIS_HAT_Y;
  int AXIS_HAT_X;
  int AXIS_RZ;
  int AXIS_RY;
  int AXIS_RX;
  int AXIS_Z;
  int AXIS_HSCROLL;
  int AXIS_VSCROLL;
  int AXIS_ORIENTATION;
  int AXIS_TOOL_MINOR;
  int AXIS_TOOL_MAJOR;
  int AXIS_TOUCH_MINOR;
  int AXIS_TOUCH_MAJOR;
  int AXIS_SIZE;
  int AXIS_PRESSURE;
  int AXIS_Y;
  int AXIS_X;
  int EDGE_RIGHT;
  int EDGE_LEFT;
  int EDGE_BOTTOM;
  int EDGE_TOP;
  int FLAG_TAINTED;
  int FLAG_WINDOW_IS_OBSCURED;
  int ACTION_POINTER_ID_SHIFT;
  int ACTION_POINTER_ID_MASK;
  int ACTION_POINTER_3_UP;
  int ACTION_POINTER_2_UP;
  int ACTION_POINTER_1_UP;
  int ACTION_POINTER_3_DOWN;
  int ACTION_POINTER_2_DOWN;
  int ACTION_POINTER_1_DOWN;
  int ACTION_POINTER_INDEX_SHIFT;
  int ACTION_POINTER_INDEX_MASK;
  int ACTION_HOVER_EXIT;
  int ACTION_HOVER_ENTER;
  int ACTION_SCROLL;
  int ACTION_HOVER_MOVE;
  int ACTION_POINTER_UP;
  int ACTION_POINTER_DOWN;
  int ACTION_OUTSIDE;
  int ACTION_CANCEL;
  int ACTION_MOVE;
  int ACTION_UP;
  int ACTION_DOWN;
  int ACTION_MASK;
  int INVALID_POINTER_ID;
  int NS_PER_MS;
}
class MockView {
}
class MergeTest {
}
class Merge {
  int mLayout;
}
class MenuTest {
  int mMenu;
}
class MenuItem {
  class OnActionExpandListener {
  }
  class OnMenuItemClickListener {
  }
  int SHOW_AS_ACTION_COLLAPSE_ACTION_VIEW;
  int SHOW_AS_ACTION_WITH_TEXT;
  int SHOW_AS_ACTION_ALWAYS;
  int SHOW_AS_ACTION_IF_ROOM;
  int SHOW_AS_ACTION_NEVER;
}
class MenuInflater {
  class MenuState {
    int defaultItemEnabled;
    int defaultItemVisible;
    int defaultItemChecked;
    int defaultItemCheckable;
    int defaultItemOrder;
    int defaultItemCategory;
    int defaultItemId;
    int defaultGroupId;
    int itemActionProvider;
    int itemListenerMethodName;
    int itemActionProviderClassName;
    int itemActionViewClassName;
    int itemActionViewLayout;
    int itemShowAsAction;
    int itemEnabled;
    int itemVisible;
    int itemChecked;
    int itemCheckable;
    int itemNumericShortcut;
    int itemAlphabeticShortcut;
    int itemIconResId;
    int itemTitleCondensed;
    int itemTitle;
    int itemCategoryOrder;
    int itemId;
    int itemAdded;
    int groupEnabled;
    int groupVisible;
    int groupCheckable;
    int groupOrder;
    int groupCategory;
    int groupId;
    int menu;
  }
  class InflatedOnMenuItemClickListener {
    int mMethod;
    int mRealOwner;
    int PARAM_TYPES;
  }
  int mRealOwner;
  int mContext;
  int mActionProviderConstructorArguments;
  int mActionViewConstructorArguments;
  int ACTION_PROVIDER_CONSTRUCTOR_SIGNATURE;
  int ACTION_VIEW_CONSTRUCTOR_SIGNATURE;
  int NO_ID;
  int XML_ITEM;
  int XML_GROUP;
  int XML_MENU;
  int LOG_TAG;
}
class Menu {
  int FLAG_ALWAYS_PERFORM_CLOSE;
  int FLAG_PERFORM_NO_CLOSE;
  int FLAG_APPEND_TO_GROUP;
  int CATEGORY_ALTERNATIVE;
  int CATEGORY_SECONDARY;
  int CATEGORY_SYSTEM;
  int CATEGORY_CONTAINER;
  int FIRST;
  int NONE;
  int CATEGORY_SHIFT;
  int CATEGORY_MASK;
  int USER_SHIFT;
  int USER_MASK;
}
class LongpressTest {
  int mLongClicked;
  int mSimpleView;
}
class Longpress {
}
class ListContextMenu {
  class ThrashListAdapter {
    int mTitles;
    int mInflater;
  }
  int mAdapter;
  int TAG;
}
class LayoutInflater_Delegate {
  int sIsInInclude;
  int TAG_MERGE;
}
class LayoutInflater {
  class BlinkLayout {
    int mHandler;
    int mBlinkState;
    int mBlink;
    int BLINK_DELAY;
    int MESSAGE_BLINK;
  }
  class FactoryMerger {
    int mF22;
    int mF12;
    int mF2;
    int mF1;
  }
  class Factory2 {
  }
  class Factory {
  }
  class Filter {
  }
  int TAG_REQUEST_FOCUS;
  int TAG_1995;
  int TAG_INCLUDE;
  int TAG_MERGE;
  int mFilterMap;
  int sConstructorMap;
  int mConstructorSignature;
  int mConstructorArgs;
  int mFilter;
  int mPrivateFactory;
  int mFactory2;
  int mFactory;
  int mFactorySet;
  int mContext;
  int DEBUG;
}
class KeyEvent {
  int CREATOR;
  class DispatcherState {
    int mActiveLongPresses;
    int mDownTarget;
    int mDownKeyCode;
  }
  int META_INVALID_MODIFIER_MASK;
  int META_SYNTHETIC_MASK;
  int META_ALL_MASK;
  int META_LOCK_MASK;
  int META_MODIFIER_MASK;
  class Callback {
  }
  int mCharacters;
  int mEventTime;
  int mDownTime;
  int mFlags;
  int mRepeatCount;
  int mScanCode;
  int mKeyCode;
  int mAction;
  int mMetaState;
  int mSource;
  int mDeviceId;
  int mNext;
  int gRecyclerTop;
  int gRecyclerUsed;
  int gRecyclerLock;
  int MAX_RECYCLED;
  int TAG;
  int DEBUG;
  int FLAG_TAINTED;
  int FLAG_START_TRACKING;
  int FLAG_FALLBACK;
  int FLAG_TRACKING;
  int FLAG_CANCELED_LONG_PRESS;
  int FLAG_LONG_PRESS;
  int FLAG_VIRTUAL_HARD_KEY;
  int FLAG_CANCELED;
  int FLAG_EDITOR_ACTION;
  int FLAG_FROM_SYSTEM;
  int FLAG_KEEP_TOUCH_MODE;
  int FLAG_SOFT_KEYBOARD;
  int FLAG_WOKE_HERE;
  int META_META_MASK;
  int META_CTRL_MASK;
  int META_ALT_MASK;
  int META_SHIFT_MASK;
  int META_SCROLL_LOCK_ON;
  int META_NUM_LOCK_ON;
  int META_CAPS_LOCK_ON;
  int META_META_RIGHT_ON;
  int META_META_LEFT_ON;
  int META_META_ON;
  int META_CTRL_RIGHT_ON;
  int META_CTRL_LEFT_ON;
  int META_CTRL_ON;
  int META_FUNCTION_ON;
  int META_SYM_ON;
  int META_SHIFT_RIGHT_ON;
  int META_SHIFT_LEFT_ON;
  int META_SHIFT_ON;
  int META_ALT_RIGHT_ON;
  int META_ALT_LEFT_ON;
  int META_ALT_ON;
  int META_SELECTING;
  int META_SYM_LOCKED;
  int META_ALT_LOCKED;
  int META_CAP_LOCKED;
  int ACTION_MULTIPLE;
  int ACTION_UP;
  int ACTION_DOWN;
  int MAX_KEYCODE;
  int META_SYMBOLIC_NAMES;
  int KEYCODE_SYMBOLIC_NAMES;
  int LAST_KEYCODE;
  int KEYCODE_ASSIST;
  int KEYCODE_KANA;
  int KEYCODE_RO;
  int KEYCODE_YEN;
  int KEYCODE_KATAKANA_HIRAGANA;
  int KEYCODE_HENKAN;
  int KEYCODE_MUHENKAN;
  int KEYCODE_EISU;
  int KEYCODE_ZENKAKU_HANKAKU;
  int KEYCODE_CALCULATOR;
  int KEYCODE_MUSIC;
  int KEYCODE_CALENDAR;
  int KEYCODE_CONTACTS;
  int KEYCODE_3D_MODE;
  int KEYCODE_MANNER_MODE;
  int KEYCODE_LANGUAGE_SWITCH;
  int KEYCODE_BUTTON_16;
  int KEYCODE_BUTTON_15;
  int KEYCODE_BUTTON_14;
  int KEYCODE_BUTTON_13;
  int KEYCODE_BUTTON_12;
  int KEYCODE_BUTTON_11;
  int KEYCODE_BUTTON_10;
  int KEYCODE_BUTTON_9;
  int KEYCODE_BUTTON_8;
  int KEYCODE_BUTTON_7;
  int KEYCODE_BUTTON_6;
  int KEYCODE_BUTTON_5;
  int KEYCODE_BUTTON_4;
  int KEYCODE_BUTTON_3;
  int KEYCODE_BUTTON_2;
  int KEYCODE_BUTTON_1;
  int KEYCODE_APP_SWITCH;
  int KEYCODE_PROG_BLUE;
  int KEYCODE_PROG_YELLOW;
  int KEYCODE_PROG_GREEN;
  int KEYCODE_PROG_RED;
  int KEYCODE_AVR_INPUT;
  int KEYCODE_AVR_POWER;
  int KEYCODE_STB_INPUT;
  int KEYCODE_STB_POWER;
  int KEYCODE_TV_INPUT;
  int KEYCODE_TV_POWER;
  int KEYCODE_SETTINGS;
  int KEYCODE_CAPTIONS;
  int KEYCODE_BOOKMARK;
  int KEYCODE_DVR;
  int KEYCODE_GUIDE;
  int KEYCODE_WINDOW;
  int KEYCODE_TV;
  int KEYCODE_ZOOM_OUT;
  int KEYCODE_ZOOM_IN;
  int KEYCODE_CHANNEL_DOWN;
  int KEYCODE_CHANNEL_UP;
  int KEYCODE_INFO;
  int KEYCODE_VOLUME_MUTE;
  int KEYCODE_NUMPAD_RIGHT_PAREN;
  int KEYCODE_NUMPAD_LEFT_PAREN;
  int KEYCODE_NUMPAD_EQUALS;
  int KEYCODE_NUMPAD_ENTER;
  int KEYCODE_NUMPAD_COMMA;
  int KEYCODE_NUMPAD_DOT;
  int KEYCODE_NUMPAD_ADD;
  int KEYCODE_NUMPAD_SUBTRACT;
  int KEYCODE_NUMPAD_MULTIPLY;
  int KEYCODE_NUMPAD_DIVIDE;
  int KEYCODE_NUMPAD_9;
  int KEYCODE_NUMPAD_8;
  int KEYCODE_NUMPAD_7;
  int KEYCODE_NUMPAD_6;
  int KEYCODE_NUMPAD_5;
  int KEYCODE_NUMPAD_4;
  int KEYCODE_NUMPAD_3;
  int KEYCODE_NUMPAD_2;
  int KEYCODE_NUMPAD_1;
  int KEYCODE_NUMPAD_0;
  int KEYCODE_NUM_LOCK;
  int KEYCODE_F12;
  int KEYCODE_F11;
  int KEYCODE_F10;
  int KEYCODE_F9;
  int KEYCODE_F8;
  int KEYCODE_F7;
  int KEYCODE_F6;
  int KEYCODE_F5;
  int KEYCODE_F4;
  int KEYCODE_F3;
  int KEYCODE_F2;
  int KEYCODE_F1;
  int KEYCODE_MEDIA_RECORD;
  int KEYCODE_MEDIA_EJECT;
  int KEYCODE_MEDIA_CLOSE;
  int KEYCODE_MEDIA_PAUSE;
  int KEYCODE_MEDIA_PLAY;
  int KEYCODE_FORWARD;
  int KEYCODE_INSERT;
  int KEYCODE_MOVE_END;
  int KEYCODE_MOVE_HOME;
  int KEYCODE_BREAK;
  int KEYCODE_SYSRQ;
  int KEYCODE_FUNCTION;
  int KEYCODE_META_RIGHT;
  int KEYCODE_META_LEFT;
  int KEYCODE_SCROLL_LOCK;
  int KEYCODE_CAPS_LOCK;
  int KEYCODE_CTRL_RIGHT;
  int KEYCODE_CTRL_LEFT;
  int KEYCODE_FORWARD_DEL;
  int KEYCODE_ESCAPE;
  int KEYCODE_BUTTON_MODE;
  int KEYCODE_BUTTON_SELECT;
  int KEYCODE_BUTTON_START;
  int KEYCODE_BUTTON_THUMBR;
  int KEYCODE_BUTTON_THUMBL;
  int KEYCODE_BUTTON_R2;
  int KEYCODE_BUTTON_L2;
  int KEYCODE_BUTTON_R1;
  int KEYCODE_BUTTON_L1;
  int KEYCODE_BUTTON_Z;
  int KEYCODE_BUTTON_Y;
  int KEYCODE_BUTTON_X;
  int KEYCODE_BUTTON_C;
  int KEYCODE_BUTTON_B;
  int KEYCODE_BUTTON_A;
  int KEYCODE_SWITCH_CHARSET;
  int KEYCODE_PICTSYMBOLS;
  int KEYCODE_PAGE_DOWN;
  int KEYCODE_PAGE_UP;
  int KEYCODE_MUTE;
  int KEYCODE_MEDIA_FAST_FORWARD;
  int KEYCODE_MEDIA_REWIND;
  int KEYCODE_MEDIA_PREVIOUS;
  int KEYCODE_MEDIA_NEXT;
  int KEYCODE_MEDIA_STOP;
  int KEYCODE_MEDIA_PLAY_PAUSE;
  int KEYCODE_SEARCH;
  int KEYCODE_NOTIFICATION;
  int KEYCODE_MENU;
  int KEYCODE_PLUS;
  int KEYCODE_FOCUS;
  int KEYCODE_HEADSETHOOK;
  int KEYCODE_NUM;
  int KEYCODE_AT;
  int KEYCODE_SLASH;
  int KEYCODE_APOSTROPHE;
  int KEYCODE_SEMICOLON;
  int KEYCODE_BACKSLASH;
  int KEYCODE_RIGHT_BRACKET;
  int KEYCODE_LEFT_BRACKET;
  int KEYCODE_EQUALS;
  int KEYCODE_MINUS;
  int KEYCODE_GRAVE;
  int KEYCODE_DEL;
  int KEYCODE_ENTER;
  int KEYCODE_ENVELOPE;
  int KEYCODE_EXPLORER;
  int KEYCODE_SYM;
  int KEYCODE_SPACE;
  int KEYCODE_TAB;
  int KEYCODE_SHIFT_RIGHT;
  int KEYCODE_SHIFT_LEFT;
  int KEYCODE_ALT_RIGHT;
  int KEYCODE_ALT_LEFT;
  int KEYCODE_PERIOD;
  int KEYCODE_COMMA;
  int KEYCODE_Z;
  int KEYCODE_Y;
  int KEYCODE_X;
  int KEYCODE_W;
  int KEYCODE_V;
  int KEYCODE_U;
  int KEYCODE_T;
  int KEYCODE_S;
  int KEYCODE_R;
  int KEYCODE_Q;
  int KEYCODE_P;
  int KEYCODE_O;
  int KEYCODE_N;
  int KEYCODE_M;
  int KEYCODE_L;
  int KEYCODE_K;
  int KEYCODE_J;
  int KEYCODE_I;
  int KEYCODE_H;
  int KEYCODE_G;
  int KEYCODE_F;
  int KEYCODE_E;
  int KEYCODE_D;
  int KEYCODE_C;
  int KEYCODE_B;
  int KEYCODE_A;
  int KEYCODE_CLEAR;
  int KEYCODE_CAMERA;
  int KEYCODE_POWER;
  int KEYCODE_VOLUME_DOWN;
  int KEYCODE_VOLUME_UP;
  int KEYCODE_DPAD_CENTER;
  int KEYCODE_DPAD_RIGHT;
  int KEYCODE_DPAD_LEFT;
  int KEYCODE_DPAD_DOWN;
  int KEYCODE_DPAD_UP;
  int KEYCODE_POUND;
  int KEYCODE_STAR;
  int KEYCODE_9;
  int KEYCODE_8;
  int KEYCODE_7;
  int KEYCODE_6;
  int KEYCODE_5;
  int KEYCODE_4;
  int KEYCODE_3;
  int KEYCODE_2;
  int KEYCODE_1;
  int KEYCODE_0;
  int KEYCODE_ENDCALL;
  int KEYCODE_CALL;
  int KEYCODE_BACK;
  int KEYCODE_HOME;
  int KEYCODE_SOFT_RIGHT;
  int KEYCODE_SOFT_LEFT;
  int KEYCODE_UNKNOWN;
}
class KeyCharacterMap {
  class FallbackAction {
    int metaState;
    int keyCode;
    int next;
    int sRecycledCount;
    int sRecycleBin;
    int sRecycleLock;
    int MAX_RECYCLED;
  }
  class UnavailableException {
  }
  class KeyData {
    int meta;
    int number;
    int displayLabel;
    int META_LENGTH;
  }
  int mPtr;
  int CREATOR;
  int sDeadKeyBuilder;
  int sDeadKeyCache;
  int sAccentToCombining;
  int sCombiningToAccent;
  int ACCENT_TILDE_LEGACY;
  int ACCENT_CIRCUMFLEX_LEGACY;
  int ACCENT_GRAVE_LEGACY;
  int ACCENT_VERTICAL_LINE_BELOW;
  int ACCENT_VERTICAL_LINE_ABOVE;
  int ACCENT_UMLAUT;
  int ACCENT_TURNED_COMMA_ABOVE;
  int ACCENT_TILDE;
  int ACCENT_STROKE;
  int ACCENT_RING_ABOVE;
  int ACCENT_REVERSED_COMMA_ABOVE;
  int ACCENT_OGONEK;
  int ACCENT_MACRON_BELOW;
  int ACCENT_MACRON;
  int ACCENT_HORN;
  int ACCENT_HOOK_ABOVE;
  int ACCENT_GRAVE;
  int ACCENT_DOUBLE_ACUTE;
  int ACCENT_DOT_BELOW;
  int ACCENT_DOT_ABOVE;
  int ACCENT_COMMA_ABOVE_RIGHT;
  int ACCENT_COMMA_ABOVE;
  int ACCENT_CIRCUMFLEX;
  int ACCENT_CEDILLA;
  int ACCENT_CARON;
  int ACCENT_BREVE;
  int ACCENT_ACUTE;
  int COMBINING_ACCENT_MASK;
  int COMBINING_ACCENT;
  int MODIFIER_BEHAVIOR_CHORDED_OR_TOGGLED;
  int MODIFIER_BEHAVIOR_CHORDED;
  int PICKER_DIALOG_INPUT;
  int HEX_INPUT;
  int SPECIAL_FUNCTION;
  int FULL;
  int ALPHA;
  int PREDICTIVE;
  int NUMERIC;
  int VIRTUAL_KEYBOARD;
  int BUILT_IN_KEYBOARD;
}
class InputQueue {
  int mChannel;
  class Callback {
  }
}
class InputEventReceiver {
  class Factory {
  }
  int mSeqMap;
  int mMessageQueue;
  int mInputChannel;
  int mReceiverPtr;
  int mCloseGuard;
  int TAG;
}
class InputEventConsistencyVerifier {
  class KeyState {
    int unhandled;
    int keyCode;
    int source;
    int deviceId;
    int next;
    int mRecycledList;
    int mRecycledListLock;
  }
  int FLAG_RAW_DEVICE_INPUT;
  int mViolationMessage;
  int mHoverEntered;
  int mTouchEventStreamUnhandled;
  int mTouchEventStreamIsTainted;
  int mTouchEventStreamSource;
  int mTouchEventStreamDeviceId;
  int mTouchEventStreamPointers;
  int mTrackballUnhandled;
  int mTrackballDown;
  int mKeyStateList;
  int mCurrentEventType;
  int mCurrentEvent;
  int mMostRecentEventIndex;
  int mRecentEventsUnhandled;
  int mRecentEvents;
  int mLastNestingLevel;
  int mLastEventType;
  int mLastEventSeq;
  int mLogTag;
  int mFlags;
  int mCaller;
  int RECENT_EVENTS_TO_LOG;
  int EVENT_TYPE_GENERIC_MOTION;
  int EVENT_TYPE_TOUCH;
  int EVENT_TYPE_TRACKBALL;
  int EVENT_TYPE_KEY;
  int IS_ENG_BUILD;
}
class InputEvent {
  int CREATOR;
  int mRecycledLocation;
  int TRACK_RECYCLED_LOCATION;
  int mRecycled;
  int mSeq;
  int mNextSeq;
  int PARCEL_TOKEN_KEY_EVENT;
  int PARCEL_TOKEN_MOTION_EVENT;
}
class InputDevice {
  class MotionRange {
    int mFuzz;
    int mFlat;
    int mMax;
    int mMin;
    int mSource;
    int mAxis;
  }
  int CREATOR;
  int KEYBOARD_TYPE_ALPHABETIC;
  int KEYBOARD_TYPE_NON_ALPHABETIC;
  int KEYBOARD_TYPE_NONE;
  int MOTION_RANGE_ORIENTATION;
  int MOTION_RANGE_TOOL_MINOR;
  int MOTION_RANGE_TOOL_MAJOR;
  int MOTION_RANGE_TOUCH_MINOR;
  int MOTION_RANGE_TOUCH_MAJOR;
  int MOTION_RANGE_SIZE;
  int MOTION_RANGE_PRESSURE;
  int MOTION_RANGE_Y;
  int MOTION_RANGE_X;
  int SOURCE_ANY;
  int SOURCE_JOYSTICK;
  int SOURCE_TOUCHPAD;
  int SOURCE_TRACKBALL;
  int SOURCE_STYLUS;
  int SOURCE_MOUSE;
  int SOURCE_TOUCHSCREEN;
  int SOURCE_GAMEPAD;
  int SOURCE_DPAD;
  int SOURCE_KEYBOARD;
  int SOURCE_UNKNOWN;
  int SOURCE_CLASS_JOYSTICK;
  int SOURCE_CLASS_POSITION;
  int SOURCE_CLASS_TRACKBALL;
  int SOURCE_CLASS_POINTER;
  int SOURCE_CLASS_BUTTON;
  int SOURCE_CLASS_MASK;
  int mVibrator;
  int mMotionRanges;
  int mHasVibrator;
  int mKeyCharacterMap;
  int mKeyboardType;
  int mSources;
  int mIsExternal;
  int mDescriptor;
  int mName;
  int mGeneration;
  int mId;
}
class InputChannel {
  int mPtr;
  int CREATOR;
  int DEBUG;
  int TAG;
}
class InflateTest {
  class ViewOne {
  }
  int mView;
  int mResources;
  int mInflater;
}
class InflateException {
}
class IncludeTest {
}
class Include {
}
class HardwareRenderer {
  class Gl20Renderer {
    class Gl20RendererEglContext {
      int mHandler;
    }
    int sPbufferLock;
    int sPbuffer;
    int mGlCanvas;
  }
  class GlRenderer {
    class FunctorsRunnable {
      int attachInfo;
    }
    int mFunctorsRunnable;
    int mSurfaceSize;
    int mRedrawClip;
    int mDestroyed;
    int mTranslucent;
    int mGlVersion;
    int mDebugDirtyRegions;
    int mProfileCurrentFrame;
    int mProfileLock;
    int mProfileData;
    int mProfileEnabled;
    int mVsyncDisabled;
    int mUpdateDirtyRegions;
    int mDirtyRegionsEnabled;
    int sDirtyRegionsRequested;
    int sDirtyRegions;
    int mDebugPaint;
    int mFrameCount;
    int mCanvas;
    int mGl;
    int mEglSurface;
    int mEglThread;
    int mEglContext;
    int sEglContextStorage;
    int mHeight;
    int mWidth;
    int sEglLock;
    int sEglConfig;
    int sEglDisplay;
    int sEgl;
    int FUNCTOR_PROCESS_DELAY;
    int SURFACE_STATE_UPDATED;
    int SURFACE_STATE_SUCCESS;
    int SURFACE_STATE_ERROR;
    int EGL_SWAP_BEHAVIOR_PRESERVED_BIT;
    int EGL_SURFACE_TYPE;
    int EGL_OPENGL_ES2_BIT;
    int EGL_CONTEXT_CLIENT_VERSION;
  }
  class HardwareDrawCallbacks {
  }
  int mRequested;
  int mEnabled;
  int PROFILE_FRAME_DATA_COUNT;
  int PROFILE_MAX_FRAMES;
  int sSystemRendererDisabled;
  int sRendererDisabled;
  int DEBUG_DIRTY_REGIONS_PROPERTY;
  int PRINT_CONFIG_PROPERTY;
  int PROFILE_MAXFRAMES_PROPERTY;
  int PROFILE_PROPERTY;
  int DISABLE_VSYNC_PROPERTY;
  int RENDER_DIRTY_REGIONS_PROPERTY;
  int RENDER_DIRTY_REGIONS;
  int CACHE_PATH_SHADERS;
  int LOG_TAG;
}
class HardwareLayer {
  int mOpaque;
  int mDisplayList;
  int mHeight;
  int mWidth;
  int DIMENSION_UNDEFINED;
}
class HardwareCanvas {
}
class HapticFeedbackConstants {
  int FLAG_IGNORE_GLOBAL_SETTING;
  int FLAG_IGNORE_VIEW_SETTING;
  int SAFE_MODE_ENABLED;
  int SAFE_MODE_DISABLED;
  int KEYBOARD_TAP;
  int VIRTUAL_KEY;
  int LONG_PRESS;
}
class Gravity {
  int RELATIVE_HORIZONTAL_GRAVITY_MASK;
  int END;
  int START;
  int DISPLAY_CLIP_HORIZONTAL;
  int DISPLAY_CLIP_VERTICAL;
  int VERTICAL_GRAVITY_MASK;
  int HORIZONTAL_GRAVITY_MASK;
  int RELATIVE_LAYOUT_DIRECTION;
  int CLIP_HORIZONTAL;
  int CLIP_VERTICAL;
  int FILL;
  int CENTER;
  int FILL_HORIZONTAL;
  int CENTER_HORIZONTAL;
  int FILL_VERTICAL;
  int CENTER_VERTICAL;
  int RIGHT;
  int LEFT;
  int BOTTOM;
  int TOP;
  int AXIS_Y_SHIFT;
  int AXIS_X_SHIFT;
  int AXIS_CLIP;
  int AXIS_PULL_AFTER;
  int AXIS_PULL_BEFORE;
  int AXIS_SPECIFIED;
  int NO_GRAVITY;
}
class GlobalFocusChangeTest {
  int mRight;
  int mLeft;
  int mActivity;
}
class GlobalFocusChange {
  int mNewFocus;
  int mOldFocus;
}
class GestureDetector {
  class GestureHandler {
  }
  int mInputEventConsistencyVerifier;
  int mVelocityTracker;
  int mIgnoreMultitouch;
  int mIsLongpressEnabled;
  int mLastMotionX;
  int mLastMotionY;
  int mIsDoubleTapping;
  int mPreviousUpEvent;
  int mCurrentDownEvent;
  int mAlwaysInBiggerTapRegion;
  int mAlwaysInTapRegion;
  int mInLongPress;
  int mStillDown;
  int mDoubleTapListener;
  int mListener;
  int mHandler;
  int TAP;
  int LONG_PRESS;
  int SHOW_PRESS;
  int DOUBLE_TAP_TIMEOUT;
  int TAP_TIMEOUT;
  int LONGPRESS_TIMEOUT;
  int mMaximumFlingVelocity;
  int mMinimumFlingVelocity;
  int mDoubleTapSlopSquare;
  int mDoubleTapTouchSlopSquare;
  int mTouchSlopSquare;
  class SimpleOnGestureListener {
  }
  class OnDoubleTapListener {
  }
  class OnGestureListener {
  }
}
class GLES20TextureLayer {
  int mSurface;
  int mTexture;
}
class GLES20RenderLayer {
  int mCanvas;
  int mLayerHeight;
  int mLayerWidth;
}
class GLES20RecordingCanvas {
  int mDisplayList;
  int mIsPooled;
  int mNextPoolable;
  int sPool;
  int POOL_LIMIT;
}
class GLES20Layer {
  class Finalizer {
    int mLayerId;
  }
  int mFinalizer;
  int mLayer;
}
class GLES20DisplayList {
  class DisplayListFinalizer {
    int mNativeDisplayList;
  }
  int mFinalizer;
  int mName;
  int mValid;
  int mCanvas;
  int mBitmaps;
}
class GLES20Canvas {
  int FLUSH_CACHES_FULL;
  int FLUSH_CACHES_MODERATE;
  int FLUSH_CACHES_LAYERS;
  class CanvasFinalizer {
    int mRenderer;
  }
  int sIsAvailable;
  int mFilter;
  int mPathBounds;
  int mClipBounds;
  int mLine;
  int mPoint;
  int mHeight;
  int mWidth;
  int mFinalizer;
  int mRenderer;
  int mOpaque;
  int MODIFIER_COLOR_FILTER;
  int MODIFIER_SHADER;
  int MODIFIER_SHADOW;
  int MODIFIER_NONE;
}
class FocusFinderTest {
  int mFocusFinder;
}
class FocusFinderHelper {
  int mFocusFinder;
}
class FocusFinder {
  class SequentialFocusComparator {
    int mRoot;
    int mSecondRect;
    int mFirstRect;
  }
  int mTempList;
  int mSequentialFocusComparator;
  int mBestCandidateRect;
  int mOtherRect;
  int mFocusedRect;
  int tlFocusFinder;
}
class FallbackEventHandler {
}
class DrawableBgMinSizeTest {
  int mAbsoluteLayout;
  int mFrameLayout;
  int mRelativeLayout;
  int mLinearLayout;
  int mTextView;
  int mBigBackgroundDrawable;
  int mBackgroundDrawable;
  int mChangeBackgroundsButton;
}
class DrawableBgMinSize {
  int mAbsoluteLayout;
  int mFrameLayout;
  int mRelativeLayout;
  int mLinearLayout;
  int mTextView;
  int mChangeBackgroundsButton;
  int mBigBackgroundDrawable;
  int mBackgroundDrawable;
  int mUsingBigBg;
}
class DragEvent {
  int CREATOR;
  int ACTION_DRAG_EXITED;
  int ACTION_DRAG_ENTERED;
  int ACTION_DRAG_ENDED;
  int ACTION_DROP;
  int ACTION_DRAG_LOCATION;
  int ACTION_DRAG_STARTED;
  int gRecyclerTop;
  int gRecyclerUsed;
  int gRecyclerLock;
  int MAX_RECYCLED;
  int mRecycled;
  int mRecycledLocation;
  int mNext;
  int mDragResult;
  int mLocalState;
  int mClipData;
  int mClipDescription;
  int mY;
  int mX;
  int mAction;
  int TRACK_RECYCLED_LOCATION;
}
class Display_Delegate {
}
class DisplayList {
  int STATUS_DREW;
  int STATUS_INVOKE;
  int STATUS_DRAW;
  int STATUS_DONE;
  int FLAG_CLIP_CHILDREN;
}
class DisplayEventReceiver {
  int mMessageQueue;
  int mReceiverPtr;
  int mCloseGuard;
  int TAG;
}
class Display {
  int sWindowManager;
  int sInitialized;
  int sStaticInit;
  int mLastGetTime;
  int mTmpMetrics;
  int mTmpPoint;
  int mDpiY;
  int mDpiX;
  int mDensity;
  int mRefreshRate;
  int mPixelFormat;
  int mDisplay;
  int mCompatibilityInfo;
  int DEFAULT_DISPLAY;
  int DEBUG_DISPLAY_SIZE;
  int TAG;
}
class DisabledTest {
  int mParentClicked;
  int mClicked;
  int mDisabledParent;
  int mDisabled;
}
class DisabledLongpressTest {
  int mLongClicked;
  int mSimpleView;
}
class Disabled {
}
class CreateViewTest {
  class ViewOne {
  }
}
class ContextThemeWrapper {
  int mInflater;
  int mTheme;
  int mThemeResource;
  int mBase;
}
class ContextMenu {
  class ContextMenuInfo {
  }
}
class CompatibilityInfoHolder {
  int mCompatInfo;
}
class CollapsibleActionView {
}
class Choreographer {
  class CallbackQueue {
    int mHead;
  }
  class CallbackRecord {
    int token;
    int action;
    int dueTime;
    int next;
  }
  class FrameDisplayEventReceiver {
    int mFrame;
    int mTimestampNanos;
    int mHavePendingVsync;
  }
  class FrameHandler {
  }
  class FrameCallback {
  }
  int CALLBACK_LAST;
  int CALLBACK_TRAVERSAL;
  int CALLBACK_ANIMATION;
  int CALLBACK_INPUT;
  int mFrameIntervalNanos;
  int mLastFrameTimeNanos;
  int mCallbacksRunning;
  int mFrameScheduled;
  int mCallbackQueues;
  int mCallbackPool;
  int mDisplayEventReceiver;
  int mHandler;
  int mLooper;
  int mLock;
  int FRAME_CALLBACK_TOKEN;
  int MSG_DO_SCHEDULE_CALLBACK;
  int MSG_DO_SCHEDULE_VSYNC;
  int MSG_DO_FRAME;
  int NANOS_PER_MS;
  int SKIPPED_FRAME_WARNING_LIMIT;
  int USE_FRAME_TIME;
  int USE_VSYNC;
  int sThreadInstance;
  int sFrameDelay;
  int DEFAULT_FRAME_DELAY;
  int DEBUG;
  int TAG;
}
class BridgeInflater {
  int sClassPrefixList;
  int mResourceReference;
  int mIsInMerge;
  int mProjectCallback;
}
class BitmapDrawable {
}
class BigCacheTest {
  int mLarge;
  int mTiny;
}
class BigCache {
}
class AttachInfo_Accessor {
}
class ActionProvider {
  class VisibilityListener {
  }
  class SubUiVisibilityListener {
  }
  int mVisibilityListener;
  int mSubUiVisibilityListener;
  int TAG;
}
class ActionMode {
  class Callback {
  }
  int mTitleOptionalHint;
  int mTag;
}
class AccessibilityIterators {
  class ParagraphTextSegmentIterator {
    int sInstance;
  }
  class WordTextSegmentIterator {
    int sInstance;
  }
  class CharacterTextSegmentIterator {
    int mImpl;
    int mLocale;
    int sInstance;
  }
  class AbstractTextSegmentIterator {
    int mSegment;
    int mText;
  }
  class TextSegmentIterator {
  }
}
class AccessibilityInteractionController {
  class PrivateHandler {
    int MSG_FOCUS_SEARCH;
    int MSG_FIND_FOCUS;
    int MSG_FIND_ACCESSIBLITY_NODE_INFO_BY_TEXT;
    int MSG_FIND_ACCESSIBLITY_NODE_INFO_BY_VIEW_ID;
    int MSG_FIND_ACCESSIBLITY_NODE_INFO_BY_ACCESSIBILITY_ID;
    int MSG_PERFORM_ACCESSIBILITY_ACTION;
  }
  class AccessibilityNodePrefetcher {
    int mTempViewList;
    int MAX_ACCESSIBILITY_NODE_INFO_BATCH_SIZE;
  }
  class SomeArgs {
    int argi3;
    int argi2;
    int argi1;
    int arg2;
    int arg1;
    int mIsPooled;
    int mNext;
  }
  int mPool;
  int mTempArrayList;
  int mMyProcessId;
  int mMyLooperThreadId;
  int mPrefetcher;
  int mViewRootImpl;
  int mHandler;
  int mTempAccessibilityNodeInfoList;
  int POOL_SIZE;
}
class AbsSavedState {
  int CREATOR;
  int mSuperState;
  int EMPTY_STATE;
}
