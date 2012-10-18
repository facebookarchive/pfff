package com.android.internal.widget.multiwaveview;
class Tweener {
  int mCleanupListener;
  int sTweens;
  int animator;
  int DEBUG;
  int TAG;
}
class TargetDrawable {
  class DrawableWithAlpha {
    int mRealDrawable;
    int mAlpha;
  }
  int mResourceId;
  int mEnabled;
  int mDrawable;
  int mAlpha;
  int mScaleY;
  int mScaleX;
  int mPositionY;
  int mPositionX;
  int mTranslationY;
  int mTranslationX;
  int STATE_FOCUSED;
  int STATE_INACTIVE;
  int STATE_ACTIVE;
  int DEBUG;
  int TAG;
}
class PointCloud {
  class Point {
    int radius;
    int y;
    int x;
  }
  class GlowManager {
    int alpha;
    int radius;
    int y;
    int x;
  }
  class WaveManager {
    int alpha;
    int width;
    int radius;
  }
  int mOuterRadius;
  int glowManager;
  int waveManager;
  int PI;
  int mScale;
  int mPaint;
  int mCenterY;
  int mCenterX;
  int mDrawable;
  int mPointCloud;
  int TAG;
  int INNER_POINTS;
  int MAX_POINT_SIZE;
  int MIN_POINT_SIZE;
}
class MultiWaveView {
  int mBackgroundAnimator;
  int mInitialLayout;
  int mGravity;
  int mVerticalInset;
  int mHorizontalInset;
  int mAlwaysTrackFinger;
  int mDirectionDescriptionsResourceId;
  int mTargetDescriptionsResourceId;
  int mTargetResourceId;
  int mTargetUpdateListener;
  int mAnimatingTargets;
  int mUpdateListener;
  int mResetListenerWithPing;
  int mResetListener;
  class AnimationBundle {
    int mSuspended;
    int serialVersionUID;
  }
  int mNewTargetResources;
  int mDragging;
  int mSnapMargin;
  int mOuterRadius;
  int mMaxTargetWidth;
  int mMaxTargetHeight;
  int mWaveCenterY;
  int mWaveCenterX;
  int mTapRadius;
  int mActiveTarget;
  int mGrabbedState;
  int mVibrationDuration;
  int mFeedbackCount;
  int mVibrator;
  int mOuterRing;
  int mHandleDrawable;
  int mOnTriggerListener;
  int mDirectionDescriptions;
  int mTargetDescriptions;
  int mHandleAnimations;
  int mTargetAnimations;
  int mChevronAnimations;
  int mChevronDrawables;
  int mTargetDrawables;
  int mChevronAnimationInterpolator;
  int RING_SCALE_COLLAPSED;
  int RING_SCALE_EXPANDED;
  int TARGET_SCALE_COLLAPSED;
  int TARGET_SCALE_EXPANDED;
  int TAP_RADIUS_SCALE_ACCESSIBILITY_ENABLED;
  int INITIAL_SHOW_HANDLE_DURATION;
  int SHOW_ANIMATION_DELAY;
  int SHOW_ANIMATION_DURATION;
  int HIDE_ANIMATION_DURATION;
  int HIDE_ANIMATION_DELAY;
  int RETURN_TO_HOME_DURATION;
  int RETURN_TO_HOME_DELAY;
  int CHEVRON_ANIMATION_DURATION;
  int CHEVRON_INCREMENTAL_DELAY;
  class OnTriggerListener {
    int CENTER_HANDLE;
    int NO_HANDLE;
  }
  int SNAP_MARGIN_DEFAULT;
  int STATE_FINISH;
  int STATE_SNAP;
  int STATE_TRACKING;
  int STATE_FIRST_TOUCH;
  int STATE_START;
  int STATE_IDLE;
  int DEBUG;
  int TAG;
}
class GlowPadView {
  int mInnerRadius;
  int mPointCloud;
  int mBackgroundAnimator;
  int mInitialLayout;
  int mGravity;
  int mVerticalInset;
  int mHorizontalInset;
  int mAlwaysTrackFinger;
  int mDirectionDescriptionsResourceId;
  int mTargetDescriptionsResourceId;
  int mTargetResourceId;
  int mTargetUpdateListener;
  int mAnimatingTargets;
  int mUpdateListener;
  int mResetListenerWithPing;
  int mResetListener;
  class AnimationBundle {
    int mSuspended;
    int serialVersionUID;
  }
  int mNewTargetResources;
  int mDragging;
  int mSnapMargin;
  int mOuterRadius;
  int mMaxTargetWidth;
  int mMaxTargetHeight;
  int mWaveCenterY;
  int mWaveCenterX;
  int mGlowRadius;
  int mActiveTarget;
  int mGrabbedState;
  int mVibrationDuration;
  int mFeedbackCount;
  int mVibrator;
  int mOuterRing;
  int mHandleDrawable;
  int mOnTriggerListener;
  int mDirectionDescriptions;
  int mTargetDescriptions;
  int mGlowAnimations;
  int mTargetAnimations;
  int mWaveAnimations;
  int mTargetDrawables;
  int RING_SCALE_COLLAPSED;
  int RING_SCALE_EXPANDED;
  int TARGET_SCALE_COLLAPSED;
  int TARGET_SCALE_EXPANDED;
  int TAP_RADIUS_SCALE_ACCESSIBILITY_ENABLED;
  int REVEAL_GLOW_DURATION;
  int REVEAL_GLOW_DELAY;
  int INITIAL_SHOW_HANDLE_DURATION;
  int SHOW_ANIMATION_DELAY;
  int SHOW_ANIMATION_DURATION;
  int HIDE_ANIMATION_DURATION;
  int HIDE_ANIMATION_DELAY;
  int RETURN_TO_HOME_DURATION;
  int RETURN_TO_HOME_DELAY;
  int WAVE_ANIMATION_DURATION;
  class OnTriggerListener {
    int CENTER_HANDLE;
    int NO_HANDLE;
  }
  int SNAP_MARGIN_DEFAULT;
  int STATE_FINISH;
  int STATE_SNAP;
  int STATE_TRACKING;
  int STATE_FIRST_TOUCH;
  int STATE_START;
  int STATE_IDLE;
  int DEBUG;
  int TAG;
}
class Ease {
  class Sine {
    int easeInOut;
    int easeOut;
    int easeIn;
  }
  class Quint {
    int easeInOut;
    int easeOut;
    int easeIn;
  }
  class Quart {
    int easeInOut;
    int easeOut;
    int easeIn;
  }
  class Quad {
    int easeInOut;
    int easeOut;
    int easeIn;
  }
  class Cubic {
    int easeInOut;
    int easeOut;
    int easeIn;
  }
  class Linear {
    int easeNone;
  }
  int START;
  int DURATION;
  int DOMAIN;
}
