package android.filterpacks.videosrc;
class SurfaceTextureTarget {
  int TAG;
  int mLogVerbose;
  int mAspectRatio;
  int mRenderMode;
  int mScreen;
  int mProgram;
  int mSurfaceId;
  int mTargetQuad;
  int mSourceQuad;
  int mRenderModeString;
  int mScreenHeight;
  int mScreenWidth;
  int mSurfaceTexture;
  int RENDERMODE_CUSTOMIZE;
  int RENDERMODE_FILL_CROP;
  int RENDERMODE_FIT;
  int RENDERMODE_STRETCH;
}
class SurfaceTextureSource {
  int onFrameAvailableListener;
  int mLogVerbose;
  int TAG;
  int mRenderShader;
  int mSourceCoords;
  int mMappedCoords;
  int mFrameTransform;
  int mFirstFrame;
  int mNewFrameAvailable;
  int mOutputFormat;
  int mSurfaceTexture;
  int mFrameExtractor;
  int mMediaFrame;
  int mCloseOnTimeout;
  int mWaitTimeout;
  int mWaitForNewFrame;
  int mHeight;
  int mWidth;
  int mSourceListener;
  class SurfaceTextureSourceListener {
  }
}
class MediaSource {
  int onMediaFrameAvailableListener;
  int onCompletionListener;
  int onPreparedListener;
  int onVideoSizeChangedListener;
  int TAG;
  int mLogVerbose;
  int mCompleted;
  int mPaused;
  int mOrientationUpdated;
  int mNewFrameAvailable;
  int mPlaying;
  int mPrepared;
  int mGotSize;
  int mSourceCoords_90;
  int mSourceCoords_180;
  int mSourceCoords_270;
  int mSourceCoords_0;
  int mFrameShader;
  int NEWFRAME_TIMEOUT_REPEAT;
  int NEWFRAME_TIMEOUT;
  int PREP_TIMEOUT_REPEAT;
  int PREP_TIMEOUT;
  int mHeight;
  int mWidth;
  int mOutputFormat;
  int mFrameExtractor;
  int mSurfaceTexture;
  int mMediaFrame;
  int mMediaPlayer;
  int mOrientation;
  int mVolume;
  int mLooping;
  int mWaitForNewFrame;
  int mSelectedIsUrl;
  int mSourceAsset;
  int mSourceUrl;
}
class CameraSource {
  int onCameraFrameAvailableListener;
  int TAG;
  int mLogVerbose;
  int mFrameShader;
  int mCameraParameters;
  int mNewFrameAvailable;
  int NEWFRAME_TIMEOUT_REPEAT;
  int NEWFRAME_TIMEOUT;
  int mSourceCoords;
  int mMappedCoords;
  int mCameraTransform;
  int mOutputFormat;
  int mFrameExtractor;
  int mSurfaceTexture;
  int mCameraFrame;
  int mCamera;
  int mWaitForNewFrame;
  int mFps;
  int mHeight;
  int mWidth;
  int mCameraId;
}
