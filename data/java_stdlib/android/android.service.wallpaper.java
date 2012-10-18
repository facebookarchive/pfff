package android.service.wallpaper;
class WallpaperSettingsActivity {
  int EXTRA_PREVIEW_MODE;
}
class WallpaperService {
  class IWallpaperServiceWrapper {
    int mTarget;
  }
  class IWallpaperEngineWrapper {
    int mEngine;
    int mReqHeight;
    int mReqWidth;
    int mIsPreview;
    int mWindowType;
    int mWindowToken;
    int mConnection;
    int mCaller;
  }
  class Engine {
    int mWindow;
    int mInputEventReceiver;
    class WallpaperInputEventReceiver {
    }
    int mSurfaceHolder;
    int mReceiver;
    int mPendingMove;
    int mPendingSync;
    int mPendingYOffsetStep;
    int mPendingXOffsetStep;
    int mPendingYOffset;
    int mPendingXOffset;
    int mOffsetMessageEnqueued;
    int mLock;
    int mInputChannel;
    int mSession;
    int mLayout;
    int mConfiguration;
    int mContentInsets;
    int mWinFrame;
    int mVisibleInsets;
    int mCurWindowPrivateFlags;
    int mCurWindowFlags;
    int mWindowPrivateFlags;
    int mWindowFlags;
    int mCurHeight;
    int mCurWidth;
    int mType;
    int mFormat;
    int mHeight;
    int mWidth;
    int mFixedSizeAllowed;
    int mOffsetsChanged;
    int mDrawingAllowed;
    int mIsCreating;
    int mSurfaceCreated;
    int mCreated;
    int mDestroyed;
    int mReportedVisible;
    int mScreenOn;
    int mVisible;
    int mInitializing;
    int mWindowToken;
    int mConnection;
    int mCaller;
    int mIWallpaperEngine;
  }
  class WallpaperCommand {
    int sync;
    int extras;
    int z;
    int y;
    int x;
    int action;
  }
  int mActiveEngines;
  int mCallbackLooper;
  int MSG_TOUCH_EVENT;
  int MSG_WINDOW_RESIZED;
  int MSG_WALLPAPER_COMMAND;
  int MSG_WALLPAPER_OFFSETS;
  int MSG_VISIBILITY_CHANGED;
  int MSG_UPDATE_SURFACE;
  int DO_SET_DESIRED_SIZE;
  int DO_DETACH;
  int DO_ATTACH;
  int DEBUG;
  int TAG;
  int SERVICE_META_DATA;
  int SERVICE_INTERFACE;
}
