package android.accessibilityservice;
class UiTestAutomationBridge {
  int mHandlerThread;
  int mUnprocessedEventAvailable;
  int mWaitingForEventDelivery;
  int mLastEvent;
  int mListener;
  int mConnectionId;
  int mLock;
  int FIND_ACCESSIBILITY_NODE_INFO_PREFETCH_FLAGS;
  int UNDEFINED;
  int ROOT_NODE_ID;
  int ACTIVE_WINDOW_ID;
  int TIMEOUT_REGISTER_SERVICE;
  int LOG_TAG;
}
class InterrogationActivityTest {
  int mUiTestAutomationBridge;
  int GLOBAL_IDLE_DETECTION_TIMEOUT_MILLIS;
  int IDLE_EVENT_TIME_DELTA_MILLIS;
  int TIMEOUT_PROPAGATE_ACCESSIBILITY_EVENT_MILLIS;
  int LOG_TAG;
  int DEBUG;
}
class InterrogationActivity {
}
class AccessibilityServiceInfo {
  int CREATOR;
  int mNonLocalizedDescription;
  int mDescriptionResId;
  int mCanRetrieveWindowContent;
  int mSettingsActivityName;
  int mResolveInfo;
  int mId;
  int flags;
  int notificationTimeout;
  int feedbackType;
  int packageNames;
  int eventTypes;
  int FLAG_REQUEST_TOUCH_EXPLORATION_MODE;
  int FLAG_INCLUDE_NOT_IMPORTANT_VIEWS;
  int DEFAULT;
  int FEEDBACK_ALL_MASK;
  int FEEDBACK_GENERIC;
  int FEEDBACK_VISUAL;
  int FEEDBACK_AUDIBLE;
  int FEEDBACK_HAPTIC;
  int FEEDBACK_SPOKEN;
  int TAG_ACCESSIBILITY_SERVICE;
}
class AccessibilityService {
  class IAccessibilityServiceClientWrapper {
    int mCallback;
    int mCaller;
    int DO_ON_GESTURE;
    int DO_ON_ACCESSIBILITY_EVENT;
    int DO_ON_INTERRUPT;
    int DO_SET_SET_CONNECTION;
    int NO_ID;
  }
  int mInfo;
  int mConnectionId;
  class Callbacks {
  }
  int LOG_TAG;
  int GLOBAL_ACTION_NOTIFICATIONS;
  int GLOBAL_ACTION_RECENTS;
  int GLOBAL_ACTION_HOME;
  int GLOBAL_ACTION_BACK;
  int SERVICE_META_DATA;
  int SERVICE_INTERFACE;
  int GESTURE_SWIPE_DOWN_AND_RIGHT;
  int GESTURE_SWIPE_DOWN_AND_LEFT;
  int GESTURE_SWIPE_UP_AND_RIGHT;
  int GESTURE_SWIPE_UP_AND_LEFT;
  int GESTURE_SWIPE_RIGHT_AND_DOWN;
  int GESTURE_SWIPE_RIGHT_AND_UP;
  int GESTURE_SWIPE_LEFT_AND_DOWN;
  int GESTURE_SWIPE_LEFT_AND_UP;
  int GESTURE_SWIPE_DOWN_AND_UP;
  int GESTURE_SWIPE_UP_AND_DOWN;
  int GESTURE_SWIPE_RIGHT_AND_LEFT;
  int GESTURE_SWIPE_LEFT_AND_RIGHT;
  int GESTURE_SWIPE_RIGHT;
  int GESTURE_SWIPE_LEFT;
  int GESTURE_SWIPE_DOWN;
  int GESTURE_SWIPE_UP;
}
