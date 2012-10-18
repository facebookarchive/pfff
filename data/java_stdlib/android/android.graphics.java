package android.graphics;
class YuvImage {
  int mHeight;
  int mWidth;
  int mStrides;
  int mData;
  int mFormat;
  int WORKING_COMPRESS_STORAGE;
}
class Xfermode_Delegate {
  int sManager;
}
class Xfermode {
  int native_instance;
}
class Typeface_Delegate {
  int mFonts;
  int mStyle;
  int mFamily;
  int sPostInitDelegate;
  int sFontLoader;
  int DEFAULT_FAMILY;
  int sManager;
  int SYSTEM_FONTS;
}
class Typeface_Accessor {
}
class TypefaceTest {
  int mFaces;
}
class Typeface {
  int mStyle;
  int BOLD_ITALIC;
  int ITALIC;
  int BOLD;
  int NORMAL;
  int native_instance;
  int sTypefaceCache;
  int sDefaults;
  int MONOSPACE;
  int SERIF;
  int SANS_SERIF;
  int DEFAULT_BOLD;
  int DEFAULT;
}
class ThreadBitmapTest {
  class MThread {
    int b;
  }
}
class TemporaryBuffer {
  int sTemp;
}
class TableMaskFilter {
}
class SweepGradient_Delegate {
  class SweepGradientPaint {
    class SweepGradientPaintContext {
      int mColorModel;
      int mLocalMatrix;
      int mCanvasMatrix;
    }
    int mCy;
    int mCx;
  }
  int mJavaPaint;
}
class SweepGradient {
}
class SurfaceTexture {
  class EventHandler {
  }
  class OutOfResourcesException {
  }
  class OnFrameAvailableListener {
  }
  int mSurfaceTexture;
  int mOnFrameAvailableListener;
  int mEventHandler;
}
class SumPathEffect_Delegate {
}
class SumPathEffect {
}
class Shader_Delegate {
  int mLocalMatrix;
  int sManager;
}
class Shader {
  class TileMode {
    int MIRROR;
    int REPEAT;
    int CLAMP;
    int nativeInt;
  }
  int mLocalMatrix;
  int native_shader;
  int native_instance;
}
class Region_Delegate {
  int mArea;
  int sManager;
}
class RegionIterator {
  int mNativeIter;
}
class Region {
  int CREATOR;
  class Op {
    int REPLACE;
    int REVERSE_DIFFERENCE;
    int XOR;
    int UNION;
    int INTERSECT;
    int DIFFERENCE;
    int nativeInt;
  }
  int mNativeRegion;
}
class RectF {
  int CREATOR;
  int bottom;
  int right;
  int top;
  int left;
}
class Rect {
  int CREATOR;
  int FLATTENED_PATTERN;
  int bottom;
  int right;
  int top;
  int left;
}
class Rasterizer_Delegate {
  int sManager;
}
class Rasterizer {
  int native_instance;
}
class RadialGradient_Delegate {
  class RadialGradientPaint {
    class RadialGradientPaintContext {
      int mColorModel;
      int mLocalMatrix;
      int mCanvasMatrix;
    }
    int mRadius;
    int mY;
    int mX;
  }
  int mJavaPaint;
}
class RadialGradient {
}
class PorterDuffXfermode_Delegate {
  int mMode;
}
class PorterDuffXfermode {
  int mode;
}
class PorterDuffColorFilter_Delegate {
}
class PorterDuffColorFilter {
}
class PorterDuff {
  class Mode {
    int OVERLAY;
    int ADD;
    int SCREEN;
    int MULTIPLY;
    int LIGHTEN;
    int DARKEN;
    int XOR;
    int DST_ATOP;
    int SRC_ATOP;
    int DST_OUT;
    int SRC_OUT;
    int DST_IN;
    int SRC_IN;
    int DST_OVER;
    int SRC_OVER;
    int DST;
    int SRC;
    int CLEAR;
    int nativeInt;
  }
}
class PointF {
  int CREATOR;
  int y;
  int x;
}
class Point {
  int CREATOR;
  int y;
  int x;
}
class PixelXorXfermode_Delegate {
}
class PixelXorXfermode {
}
class PixelFormat {
  int bitsPerPixel;
  int bytesPerPixel;
  int JPEG;
  int YCbCr_422_I;
  int YCbCr_420_SP;
  int YCbCr_422_SP;
  int RGB_332;
  int LA_88;
  int L_8;
  int A_8;
  int RGBA_4444;
  int RGBA_5551;
  int RGB_565;
  int RGB_888;
  int RGBX_8888;
  int RGBA_8888;
  int OPAQUE;
  int TRANSPARENT;
  int TRANSLUCENT;
  int UNKNOWN;
}
class Picture {
  class RecordingCanvas {
    int mPicture;
  }
  int WORKING_STREAM_STORAGE;
  int createdFromStream;
  int mNativePicture;
  int mRecordingCanvas;
}
class Path_Delegate {
  int mLastY;
  int mLastX;
  int mPath;
  int mFillType;
  int sManager;
}
class PathMeasure {
  int native_instance;
  int TANGENT_MATRIX_FLAG;
  int POSITION_MATRIX_FLAG;
  int mPath;
}
class PathEffect_Delegate {
  int sManager;
}
class PathEffect {
  int native_instance;
}
class PathDashPathEffect_Delegate {
}
class PathDashPathEffect {
  class Style {
    int MORPH;
    int ROTATE;
    int TRANSLATE;
    int native_style;
  }
}
class Path {
  class Direction {
    int CCW;
    int CW;
    int nativeInt;
  }
  int sFillTypeArray;
  class FillType {
    int INVERSE_EVEN_ODD;
    int INVERSE_WINDING;
    int EVEN_ODD;
    int WINDING;
    int nativeInt;
  }
  int mLastDirection;
  int mDetectSimplePaths;
  int rects;
  int isSimplePath;
  int mNativePath;
}
class Paint_Delegate {
  int mLocale;
  int mRasterizer;
  int mMaskFilter;
  int mPathEffect;
  int mShader;
  int mColorFilter;
  int mXfermode;
  int mHintingMode;
  int mTextSkewX;
  int mTextScaleX;
  int mTextSize;
  int mStrokeMiter;
  int mStrokeWidth;
  int mTypeface;
  int mTextAlign;
  int mJoin;
  int mCap;
  int mStyle;
  int mColor;
  int mFlags;
  int mFontContext;
  int mFonts;
  int sManager;
  class FontInfo {
    int mMetrics;
    int mFont;
  }
}
class PaintFlagsDrawFilter_Delegate {
}
class PaintFlagsDrawFilter {
  int setBits;
  int clearBits;
}
class Paint {
  class FontMetricsInt {
    int leading;
    int bottom;
    int descent;
    int ascent;
    int top;
  }
  class FontMetrics {
    int leading;
    int bottom;
    int descent;
    int ascent;
    int top;
  }
  class Align {
    int RIGHT;
    int CENTER;
    int LEFT;
    int nativeInt;
  }
  class Join {
    int BEVEL;
    int ROUND;
    int MITER;
    int nativeInt;
  }
  class Cap {
    int SQUARE;
    int ROUND;
    int BUTT;
    int nativeInt;
  }
  class Style {
    int FILL_AND_STROKE;
    int STROKE;
    int FILL;
    int nativeInt;
  }
  int CURSOR_OPT_MAX_VALUE;
  int CURSOR_AT;
  int CURSOR_AT_OR_BEFORE;
  int CURSOR_BEFORE;
  int CURSOR_AT_OR_AFTER;
  int CURSOR_AFTER;
  int DIRECTION_RTL;
  int DIRECTION_LTR;
  int BIDI_FLAG_MASK;
  int BIDI_MAX_FLAG_VALUE;
  int BIDI_FORCE_RTL;
  int BIDI_FORCE_LTR;
  int BIDI_DEFAULT_RTL;
  int BIDI_DEFAULT_LTR;
  int BIDI_RTL;
  int BIDI_LTR;
  int HINTING_ON;
  int HINTING_OFF;
  int DEFAULT_PAINT_FLAGS;
  int DEV_KERN_TEXT_FLAG;
  int SUBPIXEL_TEXT_FLAG;
  int LINEAR_TEXT_FLAG;
  int FAKE_BOLD_TEXT_FLAG;
  int STRIKE_THRU_TEXT_FLAG;
  int UNDERLINE_TEXT_FLAG;
  int DITHER_FLAG;
  int FILTER_BITMAP_FLAG;
  int ANTI_ALIAS_FLAG;
  int sAlignArray;
  int sJoinArray;
  int sCapArray;
  int sStyleArray;
  int mBidiFlags;
  int shadowColor;
  int shadowRadius;
  int shadowDy;
  int shadowDx;
  int hasShadow;
  int mLocale;
  int mInvCompatScaling;
  int mCompatScaling;
  int mHasCompatScaling;
  int mXfermode;
  int mTypeface;
  int mShader;
  int mRasterizer;
  int mPathEffect;
  int mMaskFilter;
  int mColorFilter;
  int mNativePaint;
}
class NinePatch_Delegate {
  int sChunkCache;
}
class NinePatch {
  int mRect;
  int mSrcName;
  int mPaint;
  int mChunk;
  int mBitmap;
}
class Movie {
  int mNativeMovie;
}
class Matrix_DelegateTest {
}
class Matrix_Delegate {
  int kRectStaysRect_Shift;
  int kPerspective_Shift;
  int kAffine_Shift;
  int kScale_Shift;
  int kTranslate_Shift;
  int kAllMasks;
  int kUnknown_Mask;
  int kRectStaysRect_Mask;
  int kPerspective_Mask;
  int kAffine_Mask;
  int kScale_Mask;
  int kTranslate_Mask;
  int kIdentity_Mask;
  int mValues;
  int sManager;
  int MATRIX_SIZE;
}
class Matrix {
  class ScaleToFit {
    int END;
    int CENTER;
    int START;
    int FILL;
    int nativeInt;
  }
  int native_instance;
  int IDENTITY_MATRIX;
  int MPERSP_2;
  int MPERSP_1;
  int MPERSP_0;
  int MTRANS_Y;
  int MSCALE_Y;
  int MSKEW_Y;
  int MTRANS_X;
  int MSKEW_X;
  int MSCALE_X;
}
class MaskFilter_Delegate {
  int sManager;
}
class MaskFilter {
  int native_instance;
}
class LinearGradient_Delegate {
  class LinearGradientPaint {
    class LinearGradientPaintContext {
      int mColorModel;
      int mLocalMatrix;
      int mCanvasMatrix;
    }
    int mDSize2;
    int mDy;
    int mDx;
    int mY0;
    int mX0;
  }
  int mJavaPaint;
}
class LinearGradient {
}
class LightingColorFilter_Delegate {
}
class LightingColorFilter {
}
class LayerRasterizer_Delegate {
}
class LayerRasterizer {
}
class LargeBitmap {
  int mRecycled;
  int mNativeLargeBitmap;
}
class Interpolator {
  int native_instance;
  int mFrameCount;
  int mValueCount;
  class Result {
    int FREEZE_END;
    int FREEZE_START;
    int NORMAL;
  }
}
class Insets {
  int bottom;
  int right;
  int top;
  int left;
  int NONE;
}
class ImageFormat {
  int BAYER_RGGB;
  int JPEG;
  int YUY2;
  int NV21;
  int NV16;
  int YV12;
  int RGB_565;
  int UNKNOWN;
}
class GraphicsTests {
}
class GraphicsPerformanceTests {
  class DrawBitmap320x480 {
  }
  class DrawBitmap320x240 {
  }
  class DrawBitmap128x128 {
  }
  class DrawBitmap64x64 {
  }
  class DrawBitmap32x32 {
  }
  class DrawBitmap16x16 {
  }
  class DrawBitmap8x8 {
  }
  class DrawBitmap319x479 {
  }
  class DrawBitmap319x239 {
  }
  class DrawBitmap127x127 {
  }
  class DrawBitmap63x63 {
  }
  class DrawBitmap31x31 {
  }
  class DrawBitmap15x15 {
  }
  class DrawBitmap7x7 {
  }
  class DrawBitmapTest {
    int mBitmap;
    int ITERATIONS;
  }
  class DecodeBitmapTest {
    int mResources;
    int DECODE_ITERATIONS;
  }
  class GraphicsTestBase {
    int mPaint;
    int mCanvas;
    int mDestBitmap;
    int mIterations;
    int SCREEN_HEIGHT;
    int SCREEN_WIDTH;
    int DEFAULT_ITERATIONS;
  }
  int TAG;
}
class Gradient_Delegate {
  class GradientPaint {
    int mGradient;
    int mTileMode;
    int mPositions;
    int mColors;
    int GRADIENT_SIZE;
  }
  int mPositions;
  int mColors;
}
class EmbossMaskFilter_Delegate {
}
class EmbossMaskFilter {
}
class DrawFilter_Delegate {
  int sManager;
}
class DrawFilter {
  int mNativeInt;
}
class DiscretePathEffect_Delegate {
}
class DiscretePathEffect {
}
class DashPathEffect_Delegate {
  int mPhase;
  int mIntervals;
}
class DashPathEffect {
}
class CornerPathEffect_Delegate {
}
class CornerPathEffect {
}
class ComposeShader_Delegate {
}
class ComposeShader {
  int mShaderB;
  int mShaderA;
}
class ComposePathEffect_Delegate {
}
class ComposePathEffect {
}
class ColorStateListTest {
  int mFailureColor;
  int mResources;
}
class ColorMatrixColorFilter_Delegate {
}
class ColorMatrixColorFilter {
}
class ColorMatrix {
  int mArray;
}
class ColorFilter_Delegate {
  int sManager;
}
class ColorFilter {
  int nativeColorFilter;
  int native_instance;
}
class Color {
  int sColorNameMap;
  int TRANSPARENT;
  int MAGENTA;
  int CYAN;
  int YELLOW;
  int BLUE;
  int GREEN;
  int RED;
  int WHITE;
  int LTGRAY;
  int GRAY;
  int DKGRAY;
  int BLACK;
}
class Canvas_Delegate {
  int mDrawFilter;
  int mSnapshot;
  int mBitmap;
  int sBoolOut;
  int sManager;
}
class Canvas {
  class VertexMode {
    int TRIANGLE_FAN;
    int TRIANGLE_STRIP;
    int TRIANGLES;
    int nativeInt;
  }
  class EdgeType {
    int AA;
    int BW;
    int nativeInt;
  }
  int ALL_SAVE_FLAG;
  int CLIP_TO_LAYER_SAVE_FLAG;
  int FULL_COLOR_LAYER_SAVE_FLAG;
  int HAS_ALPHA_LAYER_SAVE_FLAG;
  int CLIP_SAVE_FLAG;
  int MATRIX_SAVE_FLAG;
  class CanvasFinalizer {
    int mNativeCanvas;
  }
  int mFinalizer;
  int MAXMIMUM_BITMAP_SIZE;
  int DIRECTION_RTL;
  int DIRECTION_LTR;
  int mSurfaceFormat;
  int mScreenDensity;
  int mDensity;
  int mDrawFilter;
  int mBitmap;
  int mNativeCanvas;
}
class Camera {
  int native_instance;
}
class BlurMaskFilter_Delegate {
}
class BlurMaskFilter {
  class Blur {
    int INNER;
    int OUTER;
    int SOLID;
    int NORMAL;
    int native_int;
  }
}
class Bitmap_Delegate {
  int mGenerationId;
  int mHasAlpha;
  int mImage;
  int mConfig;
  int sManager;
}
class BitmapTest {
}
class BitmapShader_Delegate {
  class BitmapShaderPaint {
    class BitmapShaderContext {
      int mColorModel;
      int mLocalMatrix;
      int mCanvasMatrix;
    }
    int mTileModeY;
    int mTileModeX;
    int mImage;
  }
  int mJavaPaint;
}
class BitmapShader {
  int mBitmap;
}
class BitmapRegionDecoder {
  int mRecycled;
  int mNativeBitmapRegionDecoder;
}
class BitmapFactory_Delegate {
}
class BitmapFactoryTest {
}
class BitmapFactory {
  class Options {
    int mCancel;
    int inTempStorage;
    int outMimeType;
    int outHeight;
    int outWidth;
    int inPreferQualityOverSpeed;
    int inInputShareable;
    int inPurgeable;
    int inScaled;
    int inScreenDensity;
    int inTargetDensity;
    int inDensity;
    int inDither;
    int inPreferredConfig;
    int inSampleSize;
    int inJustDecodeBounds;
    int inMutable;
    int inBitmap;
  }
  int DECODE_BUFFER_SIZE;
}
class AvoidXfermode_Delegate {
}
class AvoidXfermode {
  class Mode {
    int TARGET;
    int AVOID;
    int nativeInt;
  }
}
