package android.view.animation;
class TranslateAnimation {
  int mToYDelta;
  int mFromYDelta;
  int mToXDelta;
  int mFromXDelta;
  int mToYValue;
  int mFromYValue;
  int mToXValue;
  int mFromXValue;
  int mToYType;
  int mFromYType;
  int mToXType;
  int mFromXType;
}
class Transformation {
  int mTransformationType;
  int mAlpha;
  int mMatrix;
  int TYPE_BOTH;
  int TYPE_MATRIX;
  int TYPE_ALPHA;
  int TYPE_IDENTITY;
}
class ScaleAnimation {
  int mPivotY;
  int mPivotX;
  int mPivotYValue;
  int mPivotXValue;
  int mPivotYType;
  int mPivotXType;
  int mToYData;
  int mFromYData;
  int mToXData;
  int mFromXData;
  int mToYType;
  int mFromYType;
  int mToXType;
  int mFromXType;
  int mToY;
  int mFromY;
  int mToX;
  int mFromX;
  int mResources;
}
class RotateAnimation {
  int mPivotY;
  int mPivotX;
  int mPivotYValue;
  int mPivotXValue;
  int mPivotYType;
  int mPivotXType;
  int mToDegrees;
  int mFromDegrees;
}
class OvershootInterpolator {
  int mTension;
}
class LinearInterpolator {
}
class LayoutAnimationController {
  class AnimationParameters {
    int index;
    int count;
  }
  int mMaxDelay;
  int mDuration;
  int mOrder;
  int mDelay;
  int mInterpolator;
  int mRandomizer;
  int mAnimation;
  int ORDER_RANDOM;
  int ORDER_REVERSE;
  int ORDER_NORMAL;
}
class Interpolator {
}
class GridLayoutAnimationController {
  class AnimationParameters {
    int rowsCount;
    int columnsCount;
    int row;
    int column;
  }
  int mDirectionPriority;
  int mDirection;
  int mRowDelay;
  int mColumnDelay;
  int PRIORITY_ROW;
  int PRIORITY_COLUMN;
  int PRIORITY_NONE;
  int DIRECTION_VERTICAL_MASK;
  int DIRECTION_HORIZONTAL_MASK;
  int DIRECTION_BOTTOM_TO_TOP;
  int DIRECTION_TOP_TO_BOTTOM;
  int DIRECTION_RIGHT_TO_LEFT;
  int DIRECTION_LEFT_TO_RIGHT;
}
class DecelerateInterpolator {
  int mFactor;
}
class CycleInterpolator {
  int mCycles;
}
class BounceInterpolator {
}
class AnticipateOvershootInterpolator {
  int mTension;
}
class AnticipateInterpolator {
  int mTension;
}
class AnimationUtils {
  int SEQUENTIALLY;
  int TOGETHER;
}
class AnimationSet {
  int mStoredOffsets;
  int mLastEnd;
  int mTempTransformation;
  int mAnimations;
  int mHasAlpha;
  int mDirty;
  int mFlags;
  int PROPERTY_CHANGE_BOUNDS_MASK;
  int PROPERTY_MORPH_MATRIX_MASK;
  int PROPERTY_DURATION_MASK;
  int PROPERTY_SHARE_INTERPOLATOR_MASK;
  int PROPERTY_START_OFFSET_MASK;
  int PROPERTY_REPEAT_MODE_MASK;
  int PROPERTY_FILL_BEFORE_MASK;
  int PROPERTY_FILL_AFTER_MASK;
}
class Animation {
  class AnimationListener {
  }
  class Description {
    int value;
    int type;
  }
  int mOnEnd;
  int mOnRepeat;
  int mOnStart;
  int mListenerHandler;
  int guard;
  int mPreviousTransformation;
  int mTransformation;
  int mRegion;
  int mPreviousRegion;
  int mOneMoreTime;
  int mMore;
  int mDetachWallpaper;
  int mScaleFactor;
  int mBackgroundColor;
  int mZAdjustment;
  int mListener;
  int mInterpolator;
  int mRepeatMode;
  int mRepeated;
  int mRepeatCount;
  int mDuration;
  int mStartOffset;
  int mStartTime;
  int mFillEnabled;
  int mFillAfter;
  int mFillBefore;
  int mInitialized;
  int mCycleFlip;
  int mStarted;
  int mEnded;
  int USE_CLOSEGUARD;
  int ZORDER_BOTTOM;
  int ZORDER_TOP;
  int ZORDER_NORMAL;
  int RELATIVE_TO_PARENT;
  int RELATIVE_TO_SELF;
  int ABSOLUTE;
  int START_ON_FIRST_FRAME;
  int REVERSE;
  int RESTART;
  int INFINITE;
}
class AlphaAnimation {
  int mToAlpha;
  int mFromAlpha;
}
class AccelerateInterpolator {
  int mDoubleFactor;
  int mFactor;
}
class AccelerateDecelerateInterpolator {
}
