package android.support.v4.content;
class ModernAsyncTask {
  class AsyncTaskResult {
    int mData;
    int mTask;
  }
  class WorkerRunnable {
    int mParams;
  }
  class InternalHandler {
  }
  class Status {
    int FINISHED;
    int RUNNING;
    int PENDING;
  }
  int mTaskInvoked;
  int mStatus;
  int mFuture;
  int mWorker;
  int sDefaultExecutor;
  int sHandler;
  int MESSAGE_POST_PROGRESS;
  int MESSAGE_POST_RESULT;
  int THREAD_POOL_EXECUTOR;
  int sPoolWorkQueue;
  int sThreadFactory;
  int KEEP_ALIVE;
  int MAXIMUM_POOL_SIZE;
  int CORE_POOL_SIZE;
  int LOG_TAG;
}
class LocalBroadcastManager {
  int mInstance;
  int mLock;
  int mHandler;
  int MSG_EXEC_PENDING_BROADCASTS;
  int mPendingBroadcasts;
  int mActions;
  int mReceivers;
  int mAppContext;
  int DEBUG;
  int TAG;
  class BroadcastRecord {
    int receivers;
    int intent;
  }
  class ReceiverRecord {
    int broadcasting;
    int receiver;
    int filter;
  }
}
class Loader {
  class OnLoadCompleteListener {
  }
  class ForceLoadContentObserver {
  }
  int mContentChanged;
  int mReset;
  int mAbandoned;
  int mStarted;
  int mContext;
  int mListener;
  int mId;
}
class IntentCompat {
  int FLAG_ACTIVITY_CLEAR_TASK;
  int FLAG_ACTIVITY_TASK_ON_HOME;
  int EXTRA_HTML_TEXT;
  int EXTRA_CHANGED_UID_LIST;
  int EXTRA_CHANGED_PACKAGE_LIST;
  int ACTION_EXTERNAL_APPLICATIONS_UNAVAILABLE;
  int ACTION_EXTERNAL_APPLICATIONS_AVAILABLE;
}
class CursorLoader {
  int mCursor;
  int mSortOrder;
  int mSelectionArgs;
  int mSelection;
  int mProjection;
  int mUri;
  int mObserver;
}
class ContextCompatJellybean {
}
class ContextCompatHoneycomb {
}
class ContextCompat {
}
class AsyncTaskLoader {
  int mHandler;
  int mLastLoadCompleteTime;
  int mUpdateThrottle;
  int mCancellingTask;
  int mTask;
  class LoadTask {
    int done;
    int waiting;
    int result;
  }
  int DEBUG;
  int TAG;
}
