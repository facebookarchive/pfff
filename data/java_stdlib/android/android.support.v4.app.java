package android.support.v4.app;
class TaskStackBuilderJellybean {
}
class TaskStackBuilderHoneycomb {
}
class TaskStackBuilder {
  int mSourceContext;
  int mIntents;
  int IMPL;
  class TaskStackBuilderImplJellybean {
  }
  class TaskStackBuilderImplHoneycomb {
  }
  class TaskStackBuilderImplBase {
  }
  class TaskStackBuilderImpl {
  }
  int TAG;
}
class SuperNotCalledException {
}
class ShareCompatJB {
}
class ShareCompatICS {
  int HISTORY_FILENAME_PREFIX;
}
class ShareCompat {
  class IntentReader {
    int mStreams;
    int mCallingActivity;
    int mCallingPackage;
    int mIntent;
    int mActivity;
    int TAG;
  }
  class IntentBuilder {
    int mStreams;
    int mBccAddresses;
    int mCcAddresses;
    int mToAddresses;
    int mChooserTitle;
    int mIntent;
    int mActivity;
  }
  int IMPL;
  class ShareCompatImplJB {
  }
  class ShareCompatImplICS {
  }
  class ShareCompatImplBase {
  }
  class ShareCompatImpl {
  }
  int EXTRA_CALLING_ACTIVITY;
  int EXTRA_CALLING_PACKAGE;
}
class ServiceCompat {
  int START_STICKY;
}
class NotificationCompatHoneycomb {
}
class NotificationCompat {
  class Builder {
    int mNotification;
    int mNumber;
    int mContentInfo;
    int mLargeIcon;
    int mTickerView;
    int mFullScreenIntent;
    int mContentIntent;
    int mContentText;
    int mContentTitle;
    int mContext;
  }
  class NotificationCompatImplHoneycomb {
  }
  class NotificationCompatImplBase {
  }
  class NotificationCompatImpl {
  }
  int IMPL;
  int FLAG_HIGH_PRIORITY;
}
class NoSaveStateFrameLayout {
}
class NavUtilsJB {
}
class NavUtils {
  int IMPL;
  class NavUtilsImplJB {
  }
  class NavUtilsImplBase {
  }
  class NavUtilsImpl {
  }
  int PARENT_ACTIVITY;
  int TAG;
}
class LoaderManagerImpl {
  class LoaderInfo {
    int mPendingLoader;
    int mListenerRegistered;
    int mDestroyed;
    int mReportNextStart;
    int mRetainingStarted;
    int mRetaining;
    int mStarted;
    int mData;
    int mDeliveredData;
    int mHaveData;
    int mLoader;
    int mCallbacks;
    int mArgs;
    int mId;
  }
  int mCreatingLoader;
  int mRetainingStarted;
  int mRetaining;
  int mStarted;
  int mActivity;
  int mInactiveLoaders;
  int mLoaders;
  int DEBUG;
  int TAG;
}
class LoaderManager {
  class LoaderCallbacks {
  }
}
class ListFragment {
  int mListShown;
  int mEmptyText;
  int mListContainer;
  int mProgressContainer;
  int mStandardEmptyView;
  int mEmptyView;
  int mList;
  int mAdapter;
  int mOnClickListener;
  int mRequestFocus;
  int mHandler;
  int INTERNAL_LIST_CONTAINER_ID;
  int INTERNAL_PROGRESS_CONTAINER_ID;
  int INTERNAL_EMPTY_ID;
}
class FragmentTransaction {
  int TRANSIT_FRAGMENT_FADE;
  int TRANSIT_FRAGMENT_CLOSE;
  int TRANSIT_FRAGMENT_OPEN;
  int TRANSIT_NONE;
  int TRANSIT_UNSET;
  int TRANSIT_EXIT_MASK;
  int TRANSIT_ENTER_MASK;
}
class FragmentStatePagerAdapter {
  int mCurrentPrimaryItem;
  int mFragments;
  int mSavedState;
  int mCurTransaction;
  int mFragmentManager;
  int DEBUG;
  int TAG;
}
class FragmentPagerAdapter {
  int mCurrentPrimaryItem;
  int mCurTransaction;
  int mFragmentManager;
  int DEBUG;
  int TAG;
}
class FragmentManagerImpl {
  int ANIM_STYLE_FADE_EXIT;
  int ANIM_STYLE_FADE_ENTER;
  int ANIM_STYLE_CLOSE_EXIT;
  int ANIM_STYLE_CLOSE_ENTER;
  int ANIM_STYLE_OPEN_EXIT;
  int ANIM_STYLE_OPEN_ENTER;
  int ANIM_DUR;
  int ACCELERATE_CUBIC;
  int ACCELERATE_QUINT;
  int DECELERATE_CUBIC;
  int DECELERATE_QUINT;
  int mExecCommit;
  int mStateArray;
  int mStateBundle;
  int mHavePendingDeferredStart;
  int mNoTransactionsBecause;
  int mDestroyed;
  int mStateSaved;
  int mNeedMenuInvalidate;
  int mActivity;
  int mCurState;
  int mBackStackChangeListeners;
  int mAvailBackStackIndices;
  int mBackStackIndices;
  int mCreatedMenus;
  int mBackStack;
  int mAvailIndices;
  int mAdded;
  int mActive;
  int mExecutingActions;
  int mTmpActions;
  int mPendingActions;
  int USER_VISIBLE_HINT_TAG;
  int VIEW_STATE_TAG;
  int TARGET_STATE_TAG;
  int TARGET_REQUEST_CODE_STATE_TAG;
  int HONEYCOMB;
  int TAG;
  int DEBUG;
}
class FragmentManagerState {
  int CREATOR;
  int mBackStack;
  int mAdded;
  int mActive;
}
class FragmentManager {
  int POP_BACK_STACK_INCLUSIVE;
  class OnBackStackChangedListener {
  }
  class BackStackEntry {
  }
}
class FragmentActivity {
  class FragmentTag {
    int Fragment_tag;
    int Fragment_name;
    int Fragment_id;
    int Fragment;
  }
  class NonConfigurationInstances {
    int loaders;
    int fragments;
    int children;
    int custom;
    int activity;
  }
  int mLoaderManager;
  int mAllLoaderManagers;
  int mLoadersStarted;
  int mCheckedForLoaderManager;
  int mOptionsMenuInvalidated;
  int mRetaining;
  int mReallyStopped;
  int mStopped;
  int mResumed;
  int mCreated;
  int mFragments;
  int mHandler;
  int MSG_RESUME_PENDING;
  int MSG_REALLY_STOPPED;
  int HONEYCOMB;
  int FRAGMENTS_TAG;
  int TAG;
}
class Fragment {
  class InstantiationException {
  }
  class SavedState {
    int CREATOR;
    int mState;
  }
  int mCheckedForLoaderManager;
  int mLoadersStarted;
  int mLoaderManager;
  int mUserVisibleHint;
  int mDeferStart;
  int mInnerView;
  int mView;
  int mContainer;
  int mNextAnim;
  int mCalled;
  int mMenuVisible;
  int mHasMenu;
  int mRetaining;
  int mRetainInstance;
  int mDetached;
  int mHidden;
  int mTag;
  int mContainerId;
  int mFragmentId;
  int mActivity;
  int mFragmentManager;
  int mBackStackNesting;
  int mRestored;
  int mInLayout;
  int mFromLayout;
  int mResumed;
  int mRemoving;
  int mAdded;
  int mTargetRequestCode;
  int mTargetIndex;
  int mTarget;
  int mArguments;
  int mWho;
  int mIndex;
  int mSavedViewState;
  int mSavedFragmentState;
  int mStateAfterAnimating;
  int mAnimatingAway;
  int mState;
  int RESUMED;
  int STARTED;
  int STOPPED;
  int ACTIVITY_CREATED;
  int CREATED;
  int INITIALIZING;
  int sClassMap;
}
class FragmentState {
  int CREATOR;
  int mInstance;
  int mSavedFragmentState;
  int mArguments;
  int mDetached;
  int mRetainInstance;
  int mTag;
  int mContainerId;
  int mFragmentId;
  int mFromLayout;
  int mIndex;
  int mClassName;
}
class DialogFragment {
  int mShownByMe;
  int mDismissed;
  int mViewDestroyed;
  int mDialog;
  int mBackStackId;
  int mShowsDialog;
  int mCancelable;
  int mTheme;
  int mStyle;
  int SAVED_BACK_STACK_ID;
  int SAVED_SHOWS_DIALOG;
  int SAVED_CANCELABLE;
  int SAVED_THEME;
  int SAVED_STYLE;
  int SAVED_DIALOG_STATE_TAG;
  int STYLE_NO_INPUT;
  int STYLE_NO_FRAME;
  int STYLE_NO_TITLE;
  int STYLE_NORMAL;
}
class BackStackRecord {
  int mBreadCrumbShortTitleText;
  int mBreadCrumbShortTitleRes;
  int mBreadCrumbTitleText;
  int mBreadCrumbTitleRes;
  int mIndex;
  int mCommitted;
  int mName;
  int mAllowAddToBackStack;
  int mAddToBackStack;
  int mTransitionStyle;
  int mTransition;
  int mPopExitAnim;
  int mPopEnterAnim;
  int mExitAnim;
  int mEnterAnim;
  int mNumOp;
  int mTail;
  int mHead;
  class Op {
    int removed;
    int popExitAnim;
    int popEnterAnim;
    int exitAnim;
    int enterAnim;
    int fragment;
    int cmd;
    int prev;
    int next;
  }
  int OP_ATTACH;
  int OP_DETACH;
  int OP_SHOW;
  int OP_HIDE;
  int OP_REMOVE;
  int OP_REPLACE;
  int OP_ADD;
  int OP_NULL;
  int mManager;
  int TAG;
}
class BackStackState {
  int CREATOR;
  int mBreadCrumbShortTitleText;
  int mBreadCrumbShortTitleRes;
  int mBreadCrumbTitleText;
  int mBreadCrumbTitleRes;
  int mIndex;
  int mName;
  int mTransitionStyle;
  int mTransition;
  int mOps;
}
class ActivityCompatHoneycomb {
}
class ActivityCompat {
}
