package android.support.v4.view;
class ViewPager {
  class LayoutParams {
    int needsMeasure;
    int widthFactor;
    int gravity;
    int isDecor;
  }
  class PagerObserver {
  }
  class MyAccessibilityDelegate {
  }
  class SavedState {
    int CREATOR;
    int loader;
    int adapterState;
    int position;
  }
  class Decor {
  }
  class OnAdapterChangeListener {
  }
  class SimpleOnPageChangeListener {
  }
  class OnPageChangeListener {
  }
  int mScrollState;
  int SCROLL_STATE_SETTLING;
  int SCROLL_STATE_DRAGGING;
  int SCROLL_STATE_IDLE;
  int mAdapterChangeListener;
  int mInternalPageChangeListener;
  int mOnPageChangeListener;
  int mDecorChildCount;
  int mCalledSuper;
  int mNeedCalculatePageOffsets;
  int mFirstLayout;
  int mRightEdge;
  int mLeftEdge;
  int mFakeDragBeginTime;
  int mFakeDragging;
  int CLOSE_ENOUGH;
  int mCloseEnough;
  int mFlingDistance;
  int mMaximumVelocity;
  int mMinimumVelocity;
  int mVelocityTracker;
  int INVALID_POINTER;
  int mActivePointerId;
  int mLastMotionY;
  int mLastMotionX;
  int mInitialMotionX;
  int mTouchSlop;
  int mGutterSize;
  int mDefaultGutterSize;
  int mIgnoreGutter;
  int mIsUnableToDrag;
  int mIsBeingDragged;
  int mOffscreenPageLimit;
  int mPopulatePending;
  int mScrollingCacheEnabled;
  int mInLayout;
  int mChildHeightMeasureSpec;
  int mChildWidthMeasureSpec;
  int mLastOffset;
  int mFirstOffset;
  int mBottomPageBounds;
  int mTopPageBounds;
  int mMarginDrawable;
  int mPageMargin;
  int mObserver;
  int mScroller;
  int mRestoredClassLoader;
  int mRestoredAdapterState;
  int mRestoredCurItem;
  int mCurItem;
  int mAdapter;
  int mTempRect;
  int mTempItem;
  int mItems;
  int sInterpolator;
  int COMPARATOR;
  class ItemInfo {
    int offset;
    int widthFactor;
    int scrolling;
    int position;
    int object;
  }
  int LAYOUT_ATTRS;
  int DEFAULT_GUTTER_SIZE;
  int MIN_DISTANCE_FOR_FLING;
  int MAX_SETTLE_DURATION;
  int DEFAULT_OFFSCREEN_PAGES;
  int USE_CACHE;
  int DEBUG;
  int TAG;
}
class ViewGroupCompatIcs {
}
class ViewGroupCompat {
  int IMPL;
  class ViewGroupCompatIcsImpl {
  }
  class ViewGroupCompatStubImpl {
  }
  class ViewGroupCompatImpl {
  }
}
class ViewConfigurationCompatFroyo {
}
class ViewConfigurationCompat {
  int IMPL;
  class FroyoViewConfigurationVersionImpl {
  }
  class BaseViewConfigurationVersionImpl {
  }
  class ViewConfigurationVersionImpl {
  }
}
class ViewCompatJB {
}
class ViewCompatICS {
}
class ViewCompatHC {
}
class ViewCompatGingerbread {
}
class ViewCompat {
  int IMPL;
  class JBViewCompatImpl {
  }
  class ICSViewCompatImpl {
  }
  class HCViewCompatImpl {
  }
  class GBViewCompatImpl {
  }
  class BaseViewCompatImpl {
  }
  class ViewCompatImpl {
  }
  int IMPORTANT_FOR_ACCESSIBILITY_NO;
  int IMPORTANT_FOR_ACCESSIBILITY_YES;
  int IMPORTANT_FOR_ACCESSIBILITY_AUTO;
  int FAKE_FRAME_TIME;
  int OVER_SCROLL_NEVER;
  int OVER_SCROLL_IF_CONTENT_SCROLLS;
  int OVER_SCROLL_ALWAYS;
}
class VelocityTrackerCompatHoneycomb {
}
class VelocityTrackerCompat {
  int IMPL;
  class HoneycombVelocityTrackerVersionImpl {
  }
  class BaseVelocityTrackerVersionImpl {
  }
  class VelocityTrackerVersionImpl {
  }
}
class PagerTitleStripIcs {
  class SingleLineAllCapsTransform {
    int mLocale;
    int TAG;
  }
}
class PagerTitleStrip {
  class PageListener {
    int mScrollState;
  }
  int IMPL;
  class PagerTitleStripImplIcs {
  }
  class PagerTitleStripImplBase {
  }
  class PagerTitleStripImpl {
  }
  int mTextColor;
  int mNonPrimaryAlpha;
  int TEXT_SPACING;
  int SIDE_ALPHA;
  int TEXT_ATTRS;
  int ATTRS;
  int mPageListener;
  int mUpdatingPositions;
  int mUpdatingText;
  int mGravity;
  int mScaledTextSpacing;
  int mLastKnownPositionOffset;
  int mLastKnownCurrentPage;
  int mNextText;
  int mCurrText;
  int mPrevText;
  int mPager;
  int TAG;
}
class PagerTabStrip {
  int mTouchSlop;
  int mInitialMotionY;
  int mInitialMotionX;
  int mIgnoreTap;
  int mFullUnderlineHeight;
  int mDrawFullUnderlineSet;
  int mDrawFullUnderline;
  int mTabAlpha;
  int mTempRect;
  int mTabPaint;
  int mTabPadding;
  int mMinStripHeight;
  int mMinTextSpacing;
  int mMinPaddingBottom;
  int mIndicatorHeight;
  int mIndicatorColor;
  int MIN_STRIP_HEIGHT;
  int FULL_UNDERLINE_HEIGHT;
  int MIN_TEXT_SPACING;
  int TAB_SPACING;
  int TAB_PADDING;
  int MIN_PADDING_BOTTOM;
  int INDICATOR_HEIGHT;
  int TAG;
}
class PagerAdapter {
  int POSITION_NONE;
  int POSITION_UNCHANGED;
  int mObservable;
}
class MotionEventCompatEclair {
}
class MotionEventCompat {
  int ACTION_HOVER_EXIT;
  int ACTION_HOVER_ENTER;
  int ACTION_POINTER_INDEX_SHIFT;
  int ACTION_POINTER_INDEX_MASK;
  int ACTION_SCROLL;
  int ACTION_HOVER_MOVE;
  int ACTION_POINTER_UP;
  int ACTION_POINTER_DOWN;
  int ACTION_MASK;
  int IMPL;
  class EclairMotionEventVersionImpl {
  }
  class BaseMotionEventVersionImpl {
  }
  class MotionEventVersionImpl {
  }
}
class MenuItemCompatHoneycomb {
}
class MenuItemCompat {
  int IMPL;
  class HoneycombMenuVersionImpl {
  }
  class BaseMenuVersionImpl {
  }
  class MenuVersionImpl {
  }
  int SHOW_AS_ACTION_COLLAPSE_ACTION_VIEW;
  int SHOW_AS_ACTION_WITH_TEXT;
  int SHOW_AS_ACTION_ALWAYS;
  int SHOW_AS_ACTION_IF_ROOM;
  int SHOW_AS_ACTION_NEVER;
}
class MenuCompat {
  int IMPL;
  class HoneycombMenuVersionImpl {
  }
  class BaseMenuVersionImpl {
  }
  class MenuVersionImpl {
  }
}
class KeyEventCompatHoneycomb {
}
class KeyEventCompat {
  int IMPL;
  class HoneycombKeyEventVersionImpl {
  }
  class BaseKeyEventVersionImpl {
    int META_ALL_MASK;
    int META_MODIFIER_MASK;
  }
  class KeyEventVersionImpl {
  }
}
class AccessibilityDelegateCompatJellyBean {
  class AccessibilityDelegateBridgeJellyBean {
  }
}
class AccessibilityDelegateCompatIcs {
  class AccessibilityDelegateBridge {
  }
}
class AccessibilityDelegateCompat {
  int mBridge;
  int DEFAULT_DELEGATE;
  int IMPL;
  class AccessibilityDelegateJellyBeanImpl {
  }
  class AccessibilityDelegateIcsImpl {
  }
  class AccessibilityDelegateStubImpl {
  }
  class AccessibilityDelegateImpl {
  }
}
