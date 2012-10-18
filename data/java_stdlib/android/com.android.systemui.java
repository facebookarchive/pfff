package com.android.systemui;
class SystemUIService {
  int mServices;
  int SERVICES;
  int TAG;
}
class SystemUI {
  int mContext;
}
class SwipeHelper {
  class Callback {
  }
  int mLongPressTimeout;
  int mWatchLongPress;
  int mLongPressListener;
  int mLongPressSent;
  int mDensityScale;
  int mCanCurrViewBeDimissed;
  int mCurrAnimView;
  int mCurrView;
  int mDragging;
  int mInitialTouchPos;
  int mVelocityTracker;
  int mSwipeDirection;
  int mHandler;
  int mCallback;
  int mPagingTouchSlop;
  int mMinAlpha;
  int ALPHA_FADE_END;
  int ALPHA_FADE_START;
  int SNAP_ANIM_LEN;
  int MAX_DISMISS_VELOCITY;
  int MAX_ESCAPE_ANIMATION_DURATION;
  int DEFAULT_ESCAPE_ANIMATION_DURATION;
  int SWIPE_ESCAPE_VELOCITY;
  int sLinearInterpolator;
  int Y;
  int X;
  int DISMISS_IF_SWIPED_FAR_ENOUGH;
  int FADE_OUT_DURING_SWIPE;
  int CONSTRAIN_SWIPE;
  int SLOW_ANIMATIONS;
  int DEBUG_INVALIDATE;
  int DEBUG;
  int TAG;
}
class SearchPanelView {
  int mPreDrawListener;
  int mGlowPadViewListener;
  class GlowPadTriggerListener {
    int mWaitingForLaunch;
  }
  int mGlowPadView;
  int mSearchTargetsContainer;
  int mShowing;
  int mStatusBarTouchProxy;
  int mBar;
  int mContext;
  int ASSIST_ICON_METADATA_NAME;
  int DEBUG;
  int TAG;
  int SEARCH_PANEL_HOLD_DURATION;
}
class LoadAverageService {
  class LoadView {
    int mNeededHeight;
    int mNeededWidth;
    int mFH;
    int mAscent;
    int mUserPaint;
    int mSystemPaint;
    int mIrqPaint;
    int mShadow2Paint;
    int mShadowPaint;
    int mRemovedPaint;
    int mAddedPaint;
    int mLoadPaint;
    int mStats;
    int mHandler;
  }
  class Stats {
    int mPaint;
    int mLoadWidth;
    int mLoadText;
  }
  int mView;
}
class ImageWallpaper {
  class DrawableEngine {
    class WallpaperObserver {
    }
    int TRIANGLE_VERTICES_DATA_UV_OFFSET;
    int TRIANGLE_VERTICES_DATA_POS_OFFSET;
    int TRIANGLE_VERTICES_DATA_STRIDE_BYTES;
    int FLOAT_SIZE_BYTES;
    int sSimpleFS;
    int sSimpleVS;
    int mGL;
    int mEglSurface;
    int mEglContext;
    int mEglConfig;
    int mEglDisplay;
    int mEgl;
    int mLastYTranslation;
    int mLastXTranslation;
    int mOffsetsChanged;
    int mRedrawNeeded;
    int mVisible;
    int mYOffset;
    int mXOffset;
    int mBackgroundHeight;
    int mBackgroundWidth;
    int mBackground;
    int mReceiver;
    int mLock;
    int EGL_OPENGL_ES2_BIT;
    int EGL_CONTEXT_CLIENT_VERSION;
  }
  int mIsHwAccelerated;
  int mWallpaperManager;
  int USE_OPENGL;
  int FIXED_SIZED_SURFACE;
  int PROPERTY_KERNEL_QEMU;
  int DEBUG;
  int GL_LOG_TAG;
  int TAG;
}
class Gefingerpoken {
}
class ExpandHelper {
  class ViewScaler {
    int mView;
  }
  int mGravity;
  int mMaximumStretch;
  int mLargeSize;
  int mSmallSize;
  int mGlowBottomAnimation;
  int mGlowTopAnimation;
  int mGlowAnimationSet;
  int mScaleAnimation;
  int mScaler;
  int mDetector;
  int mCallback;
  int mInitialTouchSpan;
  int mInitialTouchFocusY;
  int mNaturalHeight;
  int mOldHeight;
  int mCurrViewBottomGlow;
  int mCurrViewTopGlow;
  int mCurrView;
  int mEventSource;
  int mStretching;
  int mContext;
  int GLOW_BASE;
  int STRETCH_INTERVAL;
  int USE_SPAN;
  int USE_DRAG;
  int GLOW_DURATION;
  int EXPAND_DURATION;
  int DEBUG;
  int TAG;
  class Callback {
  }
}
class DreamsDockLauncher {
  class DockEventReceiver {
  }
  int TAG;
}
class BootReceiver {
  int TAG;
}
class BeanBag {
  int mBoard;
  class Board {
    int boardHeight;
    int boardWidth;
    int mAnim;
    class Bean {
      int graby_offset;
      int grabx_offset;
      int grabtime;
      int graby;
      int grabx;
      int grabbed;
      int w;
      int h;
      int z;
      int r;
      int vy;
      int vx;
      int va;
      int a;
      int y;
      int x;
      int VMIN;
      int VMAX;
    }
    int COLORS;
    int BEANS;
    int MAX_RADIUS;
    int LUCKY;
    int MAX_SCALE;
    int MIN_SCALE;
    int NUM_BEANS;
    int sRNG;
  }
  int DEBUG;
}
