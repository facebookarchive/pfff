package com.android.server.accessibility;
class TouchExplorer {
  class ReceivedPointerTracker {
    int mLastReceivedEvent;
    int mLastReceivedUpPointerDownY;
    int mLastReceivedUpPointerDownX;
    int mLastReceivedUpPointerActive;
    int mLastReceivedUpPointerId;
    int mLastReceivedUpPointerDownTime;
    int mHasMovingActivePointer;
    int mPrimaryActivePointerId;
    int mActivePointers;
    int mReceivedPointersDown;
    int mReceivedPointerDownTime;
    int mReceivedPointerDownY;
    int mReceivedPointerDownX;
    int mThresholdActivePointer;
    int COEFFICIENT_ACTIVE_POINTER;
    int LOG_TAG_RECEIVED_POINTER_TRACKER;
  }
  class InjectedPointerTracker {
    int mLastInjectedHoverEventForClick;
    int mLastInjectedHoverEvent;
    int mLastInjectedDownEventTime;
    int mInjectedPointersDown;
    int LOG_TAG_INJECTED_POINTER_TRACKER;
  }
  class SendHoverDelayed {
    int mPolicyFlags;
    int mPointerIdBits;
    int mPrototype;
    int mGestureStarted;
    int mHoverAction;
    int LOG_TAG_SEND_HOVER_DELAYED;
  }
  class PerformLongPressDelayed {
    int mPolicyFlags;
    int mEvent;
  }
  class ExitGestureDetectionModeDelayed {
  }
  class DoubleTapDetector {
    int mFirstTapEvent;
    int mDownEvent;
  }
  int mLastTouchedWindowId;
  int mLongPressingPointerDeltaY;
  int mLongPressingPointerDeltaX;
  int mLongPressingPointerId;
  int mGestureLibrary;
  int MIN_PREDICTION_SCORE;
  int TOUCH_TOLERANCE;
  int mStrokeBuffer;
  int mPreviousY;
  int mPreviousX;
  int mTempRect;
  int mAms;
  int mInjectedPointerTracker;
  int mReceivedPointerTracker;
  int mVelocityTracker;
  int mScaledGestureDetectionVelocity;
  int mScaledMinPointerDistanceToUseMiddleLocation;
  int mDoubleTapDetector;
  int mExitGestureDetectionModeDelayed;
  int mPerformLongPressDelayed;
  int mSendHoverExitDelayed;
  int mSendHoverEnterDelayed;
  int mHandler;
  int mDraggingPointerId;
  int mCurrentState;
  int mInputFilter;
  int mDoubleTapSlop;
  int mTouchSlop;
  int mDoubleTapTimeout;
  int mTapTimeout;
  int mDetermineUserIntentTimeout;
  int mTempPointerIds;
  int EXIT_GESTURE_DETECTION_TIMEOUT;
  int MIN_POINTER_DISTANCE_TO_USE_MIDDLE_LOCATION_DIP;
  int GESTURE_DETECTION_VELOCITY_DIP;
  int INVALID_POINTER_ID;
  int MAX_POINTER_COUNT;
  int ALL_POINTER_ID_BITS;
  int MAX_DRAGGING_ANGLE_COS;
  int STATE_GESTURE_DETECTING;
  int STATE_DELEGATING;
  int STATE_DRAGGING;
  int STATE_TOUCH_EXPLORING;
  int LOG_TAG;
  int DEBUG;
}
class AccessibilityManagerService {
  class SecurityPolicy {
    int mActiveWindowId;
    int RETRIEVAL_ALLOWING_EVENT_TYPES;
    int VALID_ACTIONS;
  }
  class Service {
    int mHandler;
    int mPendingEvents;
    int mResolveInfo;
    int mTempBounds;
    int mIsAutomation;
    int mReqeustTouchExplorationMode;
    int mCanRetrieveScreenContent;
    int mIntent;
    int mComponentName;
    int mNotificationTimeout;
    int mIncludeNotImportantViews;
    int mRequestTouchExplorationMode;
    int mIsDefault;
    int mPackageNames;
    int mFeedbackType;
    int mEventTypes;
    int mServiceInterface;
    int mService;
    int mAccessibilityServiceInfo;
    int mId;
    int MSG_ON_GESTURE;
  }
  class MainHanler {
  }
  class AccessibilityConnectionWrapper {
    int mConnection;
    int mWindowId;
  }
  int mEnableTouchExplorationDialog;
  int mTouchExplorationGestureStarted;
  int mTouchExplorationGestureEnded;
  int mQueryBridge;
  int mUiAutomationService;
  int mMainHandler;
  int mSecurityPolicy;
  int mWindowManagerService;
  int mIsTouchExplorationEnabled;
  int mEnabledServicesForFeedbackTempList;
  int mHasInputFilter;
  int mInputFilter;
  int mIsAccessibilityEnabled;
  int mHandledFeedbackTypes;
  int mPackageManager;
  int mStringColonSplitter;
  int mWindowIdToWindowTokenMap;
  int mWindowIdToInteractionConnectionWrapperMap;
  int mTouchExplorationGrantedServices;
  int mEnabledServices;
  int mInstalledServices;
  int mComponentNameToServiceMap;
  int mClients;
  int mServices;
  int mLock;
  int mContext;
  int sNextWindowId;
  int sIdCounter;
  int MSG_SEND_ACCESSIBILITY_EVENT_TO_INPUT_FILTER;
  int MSG_TOGGLE_TOUCH_EXPLORATION;
  int MSG_SHOW_ENABLE_TOUCH_EXPLORATION_DIALOG;
  int OWN_PROCESS_ID;
  int COMPONENT_NAME_SEPARATOR;
  int FUNCTION_REGISTER_UI_TEST_AUTOMATION_SERVICE;
  int LOG_TAG;
  int DEBUG;
}
class AccessibilityInputFilter {
  int mTouchscreenSourceDeviceId;
  int mTouchExplorer;
  class Explorer {
  }
  int mAms;
  int mPm;
  int mContext;
  int DEBUG;
  int TAG;
}
