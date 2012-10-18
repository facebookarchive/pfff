package android.view.accessibility;
class RecycleAccessibilityEventTest {
  int REMOVED_COUNT;
  int ADDED_COUNT;
  int FROM_INDEX;
  int CURRENT_ITEM_INDEX;
  int ITEM_COUNT;
  int CONTENT_DESCRIPTION;
  int TEXT;
  int PACKAGE_NAME;
  int CLASS_NAME;
}
class AccessibilityRecord {
  int mConnectionId;
  int mText;
  int mParcelableData;
  int mBeforeText;
  int mContentDescription;
  int mClassName;
  int mSourceWindowId;
  int mSourceNodeId;
  int mRemovedCount;
  int mAddedCount;
  int mMaxScrollY;
  int mMaxScrollX;
  int mScrollY;
  int mScrollX;
  int mToIndex;
  int mFromIndex;
  int mItemCount;
  int mCurrentItemIndex;
  int mBooleanProperties;
  int mSealed;
  int mIsInPool;
  int mNext;
  int sPoolSize;
  int sPool;
  int sPoolLock;
  int MAX_POOL_SIZE;
  int GET_SOURCE_PREFETCH_FLAGS;
  int PROPERTY_IMPORTANT_FOR_ACCESSIBILITY;
  int PROPERTY_SCROLLABLE;
  int PROPERTY_FULL_SCREEN;
  int PROPERTY_PASSWORD;
  int PROPERTY_ENABLED;
  int PROPERTY_CHECKED;
  int UNDEFINED;
}
class AccessibilityNodeProvider {
}
class AccessibilityNodeInfoCache {
  int mWindowId;
  int mCacheImpl;
  int mLock;
  int CHECK_INTEGRITY;
  int DEBUG;
  int ENABLED;
  int LOG_TAG;
}
class AccessibilityNodeInfo {
  int CREATOR;
  int mActualAndReportedWindowTopDelta;
  int mActualAndReportedWindowLeftDelta;
  int mConnectionId;
  int mMovementGranularities;
  int mActions;
  int mChildNodeIds;
  int mContentDescription;
  int mText;
  int mClassName;
  int mPackageName;
  int mBoundsInScreen;
  int mBoundsInParent;
  int mBooleanProperties;
  int mParentNodeId;
  int mSourceNodeId;
  int mWindowId;
  int mSealed;
  int mIsInPool;
  int mNext;
  int sPoolSize;
  int sPool;
  int sPoolLock;
  int MAX_POOL_SIZE;
  int VIRTUAL_DESCENDANT_ID_SHIFT;
  int VIRTUAL_DESCENDANT_ID_MASK;
  int PROPERTY_VISIBLE_TO_USER;
  int PROPERTY_ACCESSIBILITY_FOCUSED;
  int PROPERTY_SCROLLABLE;
  int PROPERTY_PASSWORD;
  int PROPERTY_ENABLED;
  int PROPERTY_LONG_CLICKABLE;
  int PROPERTY_CLICKABLE;
  int PROPERTY_SELECTED;
  int PROPERTY_FOCUSED;
  int PROPERTY_FOCUSABLE;
  int PROPERTY_CHECKED;
  int PROPERTY_CHECKABLE;
  int MOVEMENT_GRANULARITY_PAGE;
  int MOVEMENT_GRANULARITY_PARAGRAPH;
  int MOVEMENT_GRANULARITY_LINE;
  int MOVEMENT_GRANULARITY_WORD;
  int MOVEMENT_GRANULARITY_CHARACTER;
  int FOCUS_ACCESSIBILITY;
  int FOCUS_INPUT;
  int ACTION_ARGUMENT_HTML_ELEMENT_STRING;
  int ACTION_ARGUMENT_MOVEMENT_GRANULARITY_INT;
  int ACTION_SCROLL_BACKWARD;
  int ACTION_SCROLL_FORWARD;
  int ACTION_PREVIOUS_HTML_ELEMENT;
  int ACTION_NEXT_HTML_ELEMENT;
  int ACTION_PREVIOUS_AT_MOVEMENT_GRANULARITY;
  int ACTION_NEXT_AT_MOVEMENT_GRANULARITY;
  int ACTION_CLEAR_ACCESSIBILITY_FOCUS;
  int ACTION_ACCESSIBILITY_FOCUS;
  int ACTION_LONG_CLICK;
  int ACTION_CLICK;
  int ACTION_CLEAR_SELECTION;
  int ACTION_SELECT;
  int ACTION_CLEAR_FOCUS;
  int ACTION_FOCUS;
  int INCLUDE_NOT_IMPORTANT_VIEWS;
  int FLAG_PREFETCH_DESCENDANTS;
  int FLAG_PREFETCH_SIBLINGS;
  int FLAG_PREFETCH_PREDECESSORS;
  int ACTIVE_WINDOW_ID;
  int ROOT_NODE_ID;
  int UNDEFINED;
  int DEBUG;
}
class AccessibilityManager {
  class AccessibilityStateChangeListener {
  }
  int sInstance;
}
class AccessibilityInteractionClient {
  int sAccessibilityNodeInfoCache;
  int sConnectionCache;
  int mTempBounds;
  int mSameThreadMessage;
  int mPerformAccessibilityActionResult;
  int mFindAccessibilityNodeInfosResult;
  int mFindAccessibilityNodeInfoResult;
  int mInteractionId;
  int mInstanceLock;
  int mInteractionIdCounter;
  int sClients;
  int sStaticLock;
  int TIMEOUT_INTERACTION_MILLIS;
  int CHECK_INTEGRITY;
  int DEBUG;
  int LOG_TAG;
  int NO_ID;
}
class AccessibilityEventSource {
}
class AccessibilityEvent {
  int CREATOR;
  int mRecords;
  int mAction;
  int mMovementGranularity;
  int mEventTime;
  int mPackageName;
  int mEventType;
  int mIsInPool;
  int mNext;
  int sPoolSize;
  int sPool;
  int sPoolLock;
  int MAX_POOL_SIZE;
  int TYPES_ALL_MASK;
  int TYPE_VIEW_TEXT_TRAVERSED_AT_MOVEMENT_GRANULARITY;
  int TYPE_VIEW_ACCESSIBILITY_FOCUS_CLEARED;
  int TYPE_VIEW_ACCESSIBILITY_FOCUSED;
  int TYPE_ANNOUNCEMENT;
  int TYPE_VIEW_TEXT_SELECTION_CHANGED;
  int TYPE_VIEW_SCROLLED;
  int TYPE_WINDOW_CONTENT_CHANGED;
  int TYPE_TOUCH_EXPLORATION_GESTURE_END;
  int TYPE_TOUCH_EXPLORATION_GESTURE_START;
  int TYPE_VIEW_HOVER_EXIT;
  int TYPE_VIEW_HOVER_ENTER;
  int TYPE_NOTIFICATION_STATE_CHANGED;
  int TYPE_WINDOW_STATE_CHANGED;
  int TYPE_VIEW_TEXT_CHANGED;
  int TYPE_VIEW_FOCUSED;
  int TYPE_VIEW_SELECTED;
  int TYPE_VIEW_LONG_CLICKED;
  int TYPE_VIEW_CLICKED;
  int MAX_TEXT_LENGTH;
  int INVALID_POSITION;
  int DEBUG;
}
