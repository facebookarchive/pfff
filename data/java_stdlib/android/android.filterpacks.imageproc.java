package android.filterpacks.imageproc;
class VignetteFilter {
  int mVignetteShader;
  int mShade;
  int mSlope;
  int mTarget;
  int mHeight;
  int mWidth;
  int mProgram;
  int mTileSize;
  int mScale;
}
class ToRGBFilter {
  int mLastFormat;
  int mProgram;
  int mInputBPP;
}
class ToRGBAFilter {
  int mLastFormat;
  int mProgram;
  int mInputBPP;
}
class ToPackedGrayFilter {
  int mColorToPackedGrayShader;
  int mProgram;
  int mKeepAspectRatio;
  int mOHeight;
  int mOWidth;
}
class ToGrayFilter {
  int mColorToGray4Shader;
  int mOutputFormat;
  int mTileSize;
  int mInvertSource;
}
class TintFilter {
  int mTintShader;
  int mTarget;
  int mProgram;
  int mTileSize;
  int mTint;
}
class StraightenFilter {
  int DEGREE_TO_RADIAN;
  int mTarget;
  int mHeight;
  int mWidth;
  int mProgram;
  int mTileSize;
  int mMaxAngle;
  int mAngle;
}
class SimpleImageFilter {
  int mParameterName;
  int mProgram;
  int mCurrentTarget;
}
class SharpenFilter {
  int mSharpenShader;
  int mTarget;
  int mHeight;
  int mWidth;
  int mProgram;
  int mTileSize;
  int mScale;
}
class SepiaFilter {
  int mSepiaShader;
  int mTarget;
  int mProgram;
  int mTileSize;
}
class SaturateFilter {
  int mHerfSaturateShader;
  int mBenSaturateShader;
  int mTarget;
  int mHerfProgram;
  int mBenProgram;
  int mTileSize;
  int mScale;
}
class RotateFilter {
  int mOutputHeight;
  int mOutputWidth;
  int mTarget;
  int mHeight;
  int mWidth;
  int mProgram;
  int mTileSize;
  int mAngle;
}
class ResizeFilter {
  int mInputChannels;
  int mOutputFormat;
  int mLastFormat;
  int mProgram;
  int mGenerateMipMap;
  int mKeepAspectRatio;
  int mOHeight;
  int mOWidth;
}
class RedEyeFilter {
  int mRedEyeShader;
  int mTarget;
  int mProgram;
  int mHeight;
  int mWidth;
  int mRadius;
  int mPaint;
  int mCanvas;
  int mRedEyeBitmap;
  int mRedEyeFrame;
  int mTileSize;
  int mCenters;
  int DEFAULT_RED_INTENSITY;
  int MIN_RADIUS;
  int RADIUS_RATIO;
}
class PosterizeFilter {
  int mPosterizeShader;
  int mTarget;
  int mProgram;
  int mTileSize;
}
class NegativeFilter {
  int mNegativeShader;
  int mTarget;
  int mProgram;
  int mTileSize;
}
class LomoishFilter {
  int mLomoishShader;
  int mTarget;
  int mHeight;
  int mWidth;
  int mRandom;
  int mProgram;
  int mTileSize;
}
class Invert {
  int mInvertShader;
}
class ImageStitcher {
  int mSliceIndex;
  int mSliceHeight;
  int mSliceWidth;
  int mImageHeight;
  int mImageWidth;
  int mInputHeight;
  int mInputWidth;
  int mOutputFrame;
  int mProgram;
  int mPadSize;
  int mYSlices;
  int mXSlices;
}
class ImageSlicer {
  int mOutputHeight;
  int mOutputWidth;
  int mSliceHeight;
  int mSliceWidth;
  int mInputHeight;
  int mInputWidth;
  int mProgram;
  int mOriginalFrame;
  int mSliceIndex;
  int mPadSize;
  int mYSlices;
  int mXSlices;
}
class ImageEncoder {
  int mQuality;
  int mOutputStream;
}
class ImageCombineFilter {
  int mCurrentTarget;
  int mParameterName;
  int mOutputName;
  int mInputNames;
  int mProgram;
}
class GrainFilter {
  int mGrainShader;
  int mNoiseShader;
  int mRandom;
  int mTarget;
  int mHeight;
  int mWidth;
  int mNoiseProgram;
  int mGrainProgram;
  int mTileSize;
  int mScale;
  int RAND_THRESHOLD;
}
class FlipFilter {
  int mTarget;
  int mProgram;
  int mTileSize;
  int mHorizontal;
  int mVertical;
}
class FixedRotationFilter {
  int mProgram;
  int mRotation;
}
class FisheyeFilter {
  int mFisheyeShader;
  int mTarget;
  int mHeight;
  int mWidth;
  int mProgram;
  int mTileSize;
  int mScale;
  int TAG;
}
class FillLightFilter {
  int mFillLightShader;
  int mTarget;
  int mProgram;
  int mBacklight;
  int mTileSize;
}
class DuotoneFilter {
  int mDuotoneShader;
  int mTarget;
  int mProgram;
  int mTileSize;
  int mSecondColor;
  int mFirstColor;
}
class DrawRectFilter {
  int mProgram;
  int mFixedColorFragmentShader;
  int mVertexShader;
  int mColorBlue;
  int mColorGreen;
  int mColorRed;
}
class DrawOverlayFilter {
  int mProgram;
}
class DocumentaryFilter {
  int mDocumentaryShader;
  int mTarget;
  int mHeight;
  int mWidth;
  int mRandom;
  int mProgram;
  int mTileSize;
}
class CrossProcessFilter {
  int mCrossProcessShader;
  int mTarget;
  int mProgram;
  int mTileSize;
}
class CropRectFilter {
  int mTarget;
  int mHeight;
  int mWidth;
  int mProgram;
  int mTileSize;
  int mOutputHeight;
  int mOutputWidth;
  int mYorigin;
  int mXorigin;
}
class CropFilter {
  int mFragShader;
  int mFillBlack;
  int mOutputHeight;
  int mOutputWidth;
  int mLastFormat;
  int mProgram;
}
class ContrastFilter {
  int mContrastShader;
}
class ColorTemperatureFilter {
  int mColorTemperatureShader;
  int mTarget;
  int mProgram;
  int mTileSize;
  int mScale;
}
class BrightnessFilter {
  int mBrightnessShader;
}
class BlendFilter {
  int mBlendShader;
}
class BlackWhiteFilter {
  int mBlackWhiteShader;
  int mTarget;
  int mRandom;
  int mProgram;
  int mTileSize;
  int mWhite;
  int mBlack;
}
class BitmapSource {
  int mImageFrame;
  int mTarget;
  int mRepeatFrame;
  int mRecycleBitmap;
  int mBitmap;
  int mTargetString;
}
class BitmapOverlayFilter {
  int mOverlayShader;
  int mTarget;
  int mFrame;
  int mProgram;
  int mTileSize;
  int mBitmap;
}
class AutoFixFilter {
  int mDensityFrame;
  int mHistFrame;
  int mTarget;
  int mHeight;
  int mWidth;
  int mNativeProgram;
  int mShaderProgram;
  int mAutoFixShader;
  int normal_cdf;
  int mScale;
  int mTileSize;
}
class AlphaBlendFilter {
  int mAlphaBlendShader;
}
