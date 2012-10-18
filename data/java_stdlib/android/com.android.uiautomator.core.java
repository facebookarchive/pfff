package com.android.uiautomator.core;
class UiWatcher {
}
class UiSelector {
  int LOG_TAG;
  int mSelectorAttributes;
  int SELECTOR_COUNT;
  int SELECTOR_PARENT;
  int SELECTOR_PATTERN;
  int SELECTOR_CONTAINER;
  int SELECTOR_CHILD;
  int SELECTOR_PACKAGE_NAME;
  int SELECTOR_ID;
  int SELECTOR_SELECTED;
  int SELECTOR_CHECKED;
  int SELECTOR_CLICKABLE;
  int SELECTOR_SCROLLABLE;
  int SELECTOR_FOCUSABLE;
  int SELECTOR_FOCUSED;
  int SELECTOR_ENABLED;
  int SELECTOR_INSTANCE;
  int SELECTOR_INDEX;
  int SELECTOR_CONTAINS_DESCRIPTION;
  int SELECTOR_START_DESCRIPTION;
  int SELECTOR_DESCRIPTION;
  int SELECTOR_CLASS;
  int SELECTOR_CONTAINS_TEXT;
  int SELECTOR_START_TEXT;
  int SELECTOR_TEXT;
  int SELECTOR_NIL;
}
class UiScrollable {
  int mSwipeDeadZonePercentage;
  int mIsVerticalList;
  int mMaxSearchSwipes;
  int DEFAULT_SWIPE_DEADZONE_PCT;
  int FLING_STEPS;
  int SCROLL_STEPS;
  int LOG_TAG;
}
class UiObjectNotFoundException {
  int serialVersionUID;
}
class UiObject {
  int mUiAutomationBridge;
  int mDevice;
  int mSelector;
  int SWIPE_MARGIN_LIMIT;
  int WAIT_FOR_WINDOW_TMEOUT;
  int WAIT_FOR_SELECTOR_POLL;
  int WAIT_FOR_SELECTOR_TIMEOUT;
  int LOG_TAG;
}
class UiDevice {
  int mIsPhone;
  int mDevice;
  int mUiAutomationBridge;
  int mInWatcherContext;
  int mWatchersTriggers;
  int mWatchers;
  int DEFAULT_TIMEOUT_MILLIS;
  int LOG_TAG;
}
class UiCollection {
}
class UiAutomatorBridge {
  class AccessibilityEventListener {
  }
  int mEventQueue;
  int TIMEOUT_ASYNC_PROCESSING;
  int mWaitingForEventDelivery;
  int mLastOperationTime;
  int mLastEventTime;
  int mQueryController;
  int mInteractionController;
  int mLock;
  int mListeners;
  int BUSY_STATE_POLL_TIME;
  int TOTAL_TIME_TO_WAIT_FOR_IDLE_STATE;
  int WAIT_TIME_FROM_IDLE_TO_BUSY_STATE;
  int QUIET_TIME_TO_BE_CONSIDERD_IDLE_STATE;
  int LOGTAG;
}
class QueryController {
  int mLastTraversedText;
  int mLogParentIndent;
  int mLogIndent;
  int mPatternIndexer;
  int mPatternCounter;
  int mLastPackageName;
  int mLastActivityName;
  int mLock;
  int mUiAutomatorBridge;
  int DEBUG;
  int LOG_TAG;
}
class InteractionController {
  int mDownTime;
  int mLongPressTimeout;
  int mWindowManager;
  int mUiAutomatorBridge;
  int mKeyCharacterMap;
  int DEFAULT_SCROLL_EVENT_TIMEOUT_MILLIS;
  int DEBUG;
  int LOG_TAG;
}
class AccessibilityNodeInfoDumper {
  int EXCLUDED_CLASSES;
  int LOGTAG;
}
