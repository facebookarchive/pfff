package android.graphics.drawable;
class TransitionDrawable {
  class TransitionState {
  }
  int mCrossFade;
  int mAlpha;
  int mOriginalDuration;
  int mDuration;
  int mTo;
  int mFrom;
  int mStartTimeMillis;
  int mReverse;
  int mTransitionState;
  int TRANSITION_NONE;
  int TRANSITION_RUNNING;
  int TRANSITION_STARTING;
}
class StateListDrawableTest {
  class MockDrawable {
    int wasDrawn;
  }
  int mockDefaultDrawable;
  int mockView;
  int mockCheckedDrawable;
  int mockFocusedDrawable;
  int slDrawable;
}
class StateListDrawable {
  class StateListState {
    int mStateSets;
  }
  int mMutated;
  int mStateListState;
  int DEFAULT_DITHER;
  int TAG;
  int DEBUG;
}
class ShapeDrawable {
  class ShaderFactory {
  }
  class ShapeState {
    int mShaderFactory;
    int mAlpha;
    int mIntrinsicHeight;
    int mIntrinsicWidth;
    int mPadding;
    int mShape;
    int mPaint;
    int mChangingConfigurations;
  }
  int mMutated;
  int mShapeState;
}
class ScaleDrawable {
  class ScaleState {
    int mCanConstantState;
    int mCheckedConstantState;
    int mUseIntrinsicSizeAsMin;
    int mGravity;
    int mScaleHeight;
    int mScaleWidth;
    int mChangingConfigurations;
    int mDrawable;
  }
  int mTmpRect;
  int mMutated;
  int mScaleState;
}
class RotateDrawable {
  class RotateState {
    int mCheckedConstantState;
    int mCanConstantState;
    int mCurrentDegrees;
    int mToDegrees;
    int mFromDegrees;
    int mPivotY;
    int mPivotYRel;
    int mPivotX;
    int mPivotXRel;
    int mChangingConfigurations;
    int mDrawable;
  }
  int mMutated;
  int mState;
  int MAX_LEVEL;
}
class PictureDrawable {
  int mPicture;
}
class PaintDrawable {
}
class NinePatchDrawable {
  class NinePatchState {
    int mTargetDensity;
    int mChangingConfigurations;
    int mDither;
    int mLayoutInsets;
    int mPadding;
    int mNinePatch;
  }
  int mBitmapHeight;
  int mBitmapWidth;
  int mTargetDensity;
  int mMutated;
  int mPaint;
  int mLayoutInsets;
  int mPadding;
  int mNinePatch;
  int mNinePatchState;
  int DEFAULT_DITHER;
}
class MipmapDrawableTest {
  class MockDrawable {
    int mMinimumHeight;
    int mIntrinsicHeight;
  }
  class MockMipmapDrawable {
    int mHasCalledOnBoundsChanged;
  }
  int mDrawableContainerState;
  int mMipmapDrawable;
}
class MipmapDrawable {
  class MipmapContainerState {
    int mMipmapHeights;
  }
  int mMutated;
  int mMipmapContainerState;
}
class LevelListDrawable {
  class LevelListState {
    int mHighs;
    int mLows;
  }
  int mMutated;
  int mLevelListState;
}
class LayerDrawable {
  class LayerState {
    int mCanConstantState;
    int mCheckedConstantState;
    int mStateful;
    int mHaveStateful;
    int mOpacity;
    int mHaveOpacity;
    int mChildrenChangingConfigurations;
    int mChangingConfigurations;
    int mChildren;
    int mNum;
  }
  class ChildDrawable {
    int mId;
    int mInsetB;
    int mInsetR;
    int mInsetT;
    int mInsetL;
    int mDrawable;
  }
  int mMutated;
  int mTmpRect;
  int mPaddingB;
  int mPaddingR;
  int mPaddingT;
  int mPaddingL;
  int mOpacityOverride;
  int mLayerState;
}
class InsetDrawable {
  class InsetState {
    int mCanConstantState;
    int mCheckedConstantState;
    int mInsetBottom;
    int mInsetRight;
    int mInsetTop;
    int mInsetLeft;
    int mChangingConfigurations;
    int mDrawable;
  }
  int mMutated;
  int mTmpRect;
  int mInsetState;
}
class GradientDrawable {
  class GradientState {
    int mUseLevelForShape;
    int mUseLevel;
    int mGradientRadius;
    int mCenterY;
    int mCenterX;
    int mThickness;
    int mInnerRadius;
    int mThicknessRatio;
    int mInnerRadiusRatio;
    int mHeight;
    int mWidth;
    int mPadding;
    int mRadiusArray;
    int mRadius;
    int mStrokeDashGap;
    int mStrokeDashWidth;
    int mStrokeColor;
    int mStrokeWidth;
    int mSolidColor;
    int mHasSolidColor;
    int mPositions;
    int mTempPositions;
    int mTempColors;
    int mColors;
    int mOrientation;
    int mGradient;
    int mShape;
    int mChangingConfigurations;
  }
  class Orientation {
    int TL_BR;
    int LEFT_RIGHT;
    int BL_TR;
    int BOTTOM_TOP;
    int BR_TL;
    int RIGHT_LEFT;
    int TR_BL;
    int TOP_BOTTOM;
  }
  int mPathIsDirty;
  int mRingPath;
  int mMutated;
  int mRectIsDirty;
  int mLayerPaint;
  int mRect;
  int mPath;
  int mDither;
  int mAlpha;
  int mColorFilter;
  int mStrokePaint;
  int mPadding;
  int mFillPaint;
  int mGradientState;
  int SWEEP_GRADIENT;
  int RADIAL_GRADIENT;
  int LINEAR_GRADIENT;
  int RING;
  int LINE;
  int OVAL;
  int RECTANGLE;
}
class DrawableContainer {
  class DrawableContainerState {
    int mExitFadeDuration;
    int mEnterFadeDuration;
    int mDither;
    int mPaddingChecked;
    int mCanConstantState;
    int mCheckedConstantState;
    int mStateful;
    int mHaveStateful;
    int mOpacity;
    int mHaveOpacity;
    int mConstantMinimumHeight;
    int mConstantMinimumWidth;
    int mConstantHeight;
    int mConstantWidth;
    int mComputedConstantSize;
    int mConstantSize;
    int mConstantPadding;
    int mVariablePadding;
    int mNumChildren;
    int mDrawables;
    int mChildrenChangingConfigurations;
    int mChangingConfigurations;
    int mOwner;
  }
  int mLastDrawable;
  int mExitAnimationEnd;
  int mEnterAnimationEnd;
  int mAnimationRunnable;
  int mMutated;
  int mCurIndex;
  int mColorFilter;
  int mAlpha;
  int mCurrDrawable;
  int mDrawableContainerState;
  int DEFAULT_DITHER;
  int TAG;
  int DEBUG;
}
class Drawable {
  class ConstantState {
  }
  class Callback2 {
  }
  class Callback {
  }
  int mVisible;
  int mCallback;
  int mBounds;
  int mChangingConfigurations;
  int mLevel;
  int mStateSet;
  int ZERO_BOUNDS_RECT;
}
class ColorDrawable {
  class ColorState {
    int mChangingConfigurations;
    int mUseColor;
    int mBaseColor;
  }
  int mPaint;
  int mState;
}
class ClipDrawable {
  class ClipState {
    int mCanConstantState;
    int mCheckedConstantState;
    int mGravity;
    int mOrientation;
    int mChangingConfigurations;
    int mDrawable;
  }
  int VERTICAL;
  int HORIZONTAL;
  int mTmpRect;
  int mClipState;
}
class BitmapDrawable {
  class BitmapState {
    int mRebuildShader;
    int mTargetDensity;
    int mTileModeY;
    int mTileModeX;
    int mPaint;
    int mGravity;
    int mChangingConfigurations;
    int mBitmap;
  }
  int mBitmapHeight;
  int mBitmapWidth;
  int mMutated;
  int mApplyGravity;
  int mDstRect;
  int mTargetDensity;
  int mBitmap;
  int mBitmapState;
  int DEFAULT_PAINT_FLAGS;
}
class AnimationDrawable {
  class AnimationState {
    int mOneShot;
    int mDurations;
  }
  int mMutated;
  int mCurFrame;
  int mAnimationState;
}
class AnimatedRotateDrawable {
  class AnimatedRotateState {
    int mCheckedConstantState;
    int mCanConstantState;
    int mFramesCount;
    int mFrameDuration;
    int mPivotY;
    int mPivotYRel;
    int mPivotX;
    int mPivotXRel;
    int mChangingConfigurations;
    int mDrawable;
  }
  int mRunning;
  int mIncrement;
  int mCurrentDegrees;
  int mMutated;
  int mState;
}
class Animatable {
}
