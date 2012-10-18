package android.filterpacks.base;
class RetargetFilter {
  int mTarget;
  int mOutputFormat;
  int mTargetString;
}
class OutputStreamTarget {
  int mOutputStream;
}
class ObjectSource {
  int mFrame;
  int mRepeatFrame;
  int mOutputFormat;
  int mObject;
}
class NullFilter {
}
class InputStreamSource {
  int mOutputFormat;
  int mInputStream;
  int mTarget;
}
class GLTextureTarget {
  int mTexId;
}
class GLTextureSource {
  int mFrame;
  int mTimestamp;
  int mRepeatFrame;
  int mHeight;
  int mWidth;
  int mTexId;
}
class FrameStore {
  int mKey;
}
class FrameSource {
  int mRepeatFrame;
  int mFrame;
  int mFormat;
}
class FrameFetch {
  int mRepeatFrame;
  int mKey;
  int mFormat;
}
class FrameBranch {
  int mNumberOfOutputs;
}
class CallbackFilter {
  class CallbackRunnable {
    int mListener;
    int mUserData;
    int mFrame;
    int mFilter;
  }
  int mUiThreadHandler;
  int mCallbacksOnUiThread;
  int mUserData;
  int mListener;
}
