package com.android.systemui.recent;
class TaskDescription {
  int mLoaded;
  int mLabel;
  int mIcon;
  int mThumbnail;
  int description;
  int packageName;
  int intent;
  int persistentTaskId;
  int taskId;
  int resolveInfo;
}
class StatusBarTouchProxy {
  int mStatusBar;
}
class RecentsVerticalScrollView {
  int mNumItemsInOneScreenful;
  int mRecycledViews;
  int mPerformanceHelper;
  int mSwipeHelper;
  int mLastScrollPosition;
  int mCallback;
  int mAdapter;
  int mLinearLayout;
  int DEBUG;
  int TAG;
}
class RecentsScrollViewPerformanceHelper {
  int mAttachedToWindow;
  int mSoftwareRendered;
  int mIsVertical;
  int mContext;
  int mFadingEdgeLength;
  int mCallback;
  int mScrollView;
  int USE_DARK_FADE_IN_HW_ACCELERATED_MODE;
  int OPTIMIZE_SW_RENDERED_RECENTS;
}
class RecentsPanelView {
  int mThumbnailScaleUpStarted;
  class TaskDescriptionAdapter {
    int mInflater;
  }
  class ViewHolder {
    int loadedThumbnailAndIcon;
    int taskDescription;
    int descriptionView;
    int labelView;
    int iconView;
    int thumbnailViewImageBitmap;
    int thumbnailViewImage;
    int thumbnailView;
  }
  class OnLongClickDelegate {
    int mOtherView;
  }
  class RecentsScrollView {
  }
  class OnRecentsPanelVisibilityChangedListener {
  }
  int mHighEndGfx;
  int mFirstScreenful;
  int mRecentItemLayoutId;
  int mFitThumbnailToXY;
  int mThumbnailWidth;
  int mListAdapter;
  int mRecentTasksDirty;
  int mPreloadTasksRunnable;
  int mRecentTaskDescriptions;
  int mRecentTasksLoader;
  int mHideRecentsAfterThumbnailScaleUpStarted;
  int mTransitionBg;
  int mPlaceholderThumbnail;
  int mVisibilityChangedListener;
  int mChoreo;
  int mNumItemsWaitingForThumbnailsAndIcons;
  int mReadyToShow;
  int mWaitingToShowAnimated;
  int mWaitingToShow;
  int mShowing;
  int mStatusBarTouchProxy;
  int mRecentsContainer;
  int mRecentsNoApps;
  int mRecentsScrim;
  int mPopup;
  int mBar;
  int mContext;
  int DEBUG;
  int TAG;
}
class RecentsHorizontalScrollView {
  int mNumItemsInOneScreenful;
  int mRecycledViews;
  int mPerformanceHelper;
  int mSwipeHelper;
  int mLastScrollPosition;
  int mCallback;
  int mAdapter;
  int mLinearLayout;
  int DEBUG;
  int TAG;
}
class RecentsCallback {
  int SWIPE_DOWN;
  int SWIPE_UP;
  int SWIPE_RIGHT;
  int SWIPE_LEFT;
}
class RecentTasksLoader {
  int mNumTasksInFirstScreenful;
  int mDefaultIconBackground;
  int mDefaultThumbnailBackground;
  int mIconDpi;
  int mHandler;
  int mThumbnailLoader;
  int mTaskLoader;
  int mRecentsPanel;
  int mContext;
  int MAX_TASKS;
  int DISPLAY_TASKS;
  int DEBUG;
  int TAG;
}
class Constants {
  int ALPHA_FADE_END;
  int ALPHA_FADE_START;
  int ESCAPE_VELOCITY;
  int SNAP_BACK_DURATION;
  int MAX_ESCAPE_ANIMATION_DURATION;
}
class Choreographer {
  int HYPERSPACE_OFFRAMP;
  int mListener;
  int mContentAnim;
  int mNoRecentAppsView;
  int mContentView;
  int mScrimView;
  int mRootView;
  int mPanelHeight;
  int mVisible;
  int DEBUG;
  int TAG;
  int SCRIM_DURATION;
  int CLOSE_DURATION;
  int OPEN_DURATION;
}
