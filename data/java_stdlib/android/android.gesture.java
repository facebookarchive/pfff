package android.gesture;
class Prediction {
  int score;
  int name;
}
class OrientedBoundingBox {
  int centerY;
  int centerX;
  int orientation;
  int height;
  int width;
  int squareness;
}
class Learner {
  int mInstances;
}
class InstanceLearner {
  int sComparator;
}
class Instance {
  int id;
  int label;
  int vector;
  int ORIENTATIONS;
  int PATCH_SAMPLE_SIZE;
  int SEQUENCE_SAMPLE_SIZE;
}
class GestureUtils {
  int NONUNIFORM_SCALE;
  int SCALING_THRESHOLD;
}
class GestureStroke {
  int mCachedPath;
  int timestamps;
  int points;
  int length;
  int boundingBox;
  int TOUCH_TOLERANCE;
}
class GestureStore {
  int mChanged;
  int mClassifier;
  int mNamedGestures;
  int mOrientationStyle;
  int mSequenceType;
  int PROFILE_LOADING_SAVING;
  int FILE_FORMAT_VERSION;
  int ORIENTATION_SENSITIVE_8;
  int ORIENTATION_SENSITIVE_4;
  int ORIENTATION_SENSITIVE;
  int ORIENTATION_INVARIANT;
  int SEQUENCE_SENSITIVE;
  int SEQUENCE_INVARIANT;
}
class GesturePoint {
  int timestamp;
  int y;
  int x;
}
class GestureOverlayView {
  class OnGesturePerformedListener {
  }
  class OnGestureListener {
  }
  class OnGesturingListener {
  }
  class FadeOutRunnable {
    int resetMultipleStrokes;
    int fireActionPerformed;
  }
  int mFadingOut;
  int mInterpolator;
  int mFadingAlpha;
  int mIsFadingOut;
  int mHandleGestureActions;
  int mOnGesturingListeners;
  int mOnGesturePerformedListeners;
  int mOnGestureListeners;
  int mStrokeBuffer;
  int mCurrentGesture;
  int mResetGesture;
  int mIsListeningForGestures;
  int mInterceptEvents;
  int mPreviousWasGesturing;
  int mIsGesturing;
  int mTotalLength;
  int mCurveEndY;
  int mCurveEndX;
  int mY;
  int mX;
  int mGestureVisible;
  int mPath;
  int mInvalidRect;
  int mOrientation;
  int mGestureStrokeAngleThreshold;
  int mGestureStrokeSquarenessTreshold;
  int mGestureStrokeLengthThreshold;
  int mGestureStrokeType;
  int mInvalidateExtraBorder;
  int mGestureStrokeWidth;
  int mUncertainGestureColor;
  int mCertainGestureColor;
  int mCurrentColor;
  int mFadeEnabled;
  int mFadingHasStarted;
  int mFadingStart;
  int mFadeOffset;
  int mFadeDuration;
  int mGesturePaint;
  int DITHER_FLAG;
  int GESTURE_RENDERING_ANTIALIAS;
  int FADE_ANIMATION_RATE;
  int ORIENTATION_VERTICAL;
  int ORIENTATION_HORIZONTAL;
  int GESTURE_STROKE_TYPE_MULTIPLE;
  int GESTURE_STROKE_TYPE_SINGLE;
}
class GestureLibrary {
  int mStore;
}
class GestureLibraries {
  class ResourceGestureLibrary {
    int mResourceId;
    int mContext;
  }
  class FileGestureLibrary {
    int mPath;
  }
}
class GestureConstants {
  int LOG_TAG;
  int IO_BUFFER_SIZE;
  int STROKE_POINT_BUFFER_SIZE;
  int STROKE_STRING_BUFFER_SIZE;
}
class Gesture {
  int CREATOR;
  int mStrokes;
  int mGestureID;
  int mBoundingBox;
  int sGestureCount;
  int BITMAP_RENDERING_DITHER;
  int BITMAP_RENDERING_ANTIALIAS;
  int BITMAP_RENDERING_WIDTH;
  int GESTURE_ID_BASE;
}
