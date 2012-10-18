package com.android.mediaframeworktest.functional.audio;
class SimTonesTest {
  int mContext;
  int TAG;
}
class MediaVisualizerTest {
  int mCaptureFft;
  int mCaptureWaveform;
  int mFft;
  int mWaveform;
  int lock;
  int mLooper;
  int mInitialized;
  int mSession;
  int mVisualizer;
  int VOLUME_EFFECT_UUID;
  int MAX_CAPTURE_SIZE_MIN;
  int MIN_CAPTURE_SIZE_MAX;
  int MAX_SAMPLING_RATE;
  int MIN_SAMPLING_RATE;
  int MIN_CAPTURE_RATE_MAX;
  int TAG;
}
class MediaVirtualizerTest {
  int mSession;
  int mVirtualizer;
  int TEST_STRENGTH;
  int TAG;
}
class MediaPresetReverbTest {
  int mSession;
  int mReverb;
  int PRESET_REVERB_EFFECT_UUID;
  int VOLUME_EFFECT_UUID;
  int TAG;
}
class MediaEqualizerTest {
  int mSession;
  int mEqualizer;
  int MIN_NUMBER_OF_PRESETS;
  int TEST_FREQUENCY_MILLIHERTZ;
  int MAX_BAND_LEVEL;
  int MIN_BAND_LEVEL;
  int MIN_NUMBER_OF_BANDS;
  int TAG;
}
class MediaEnvReverbTest {
  int mSession;
  int mReverb;
  int ENV_REVERB_EFFECT_UUID;
  int VOLUME_EFFECT_UUID;
  int RATIO_TOLERANCE;
  int DELAY_TOLERANCE;
  int MILLIBEL_TOLERANCE;
  int TAG;
}
class MediaBassBoostTest {
  int mSession;
  int mBassBoost;
  int TEST_STRENGTH;
  int TAG;
}
class MediaAudioTrackTest {
  class TestResults {
    int mResultLog;
    int mResult;
  }
  int TAG;
}
class MediaAudioManagerTest {
  class AudioFocusListener {
    int mFocusChangeCounter;
    int mLastFocusChange;
  }
  int WAIT_FOR_AUDIOFOCUS_LOSS_MS;
  int INVALID_FOCUS;
  int mAudioFocusListener;
  int ringtoneMode;
  int WAIT_FOR_LOOPER_TO_INITIALIZE_MS;
  int mLooperLock;
  int mAudioManagerLooper;
  int mAudioManager;
  int TAG;
}
class MediaAudioEffectTest {
  class ListenerThread {
    int mParameter;
    int mEnable;
    int mControl;
  }
  int SAMPLING_RATE;
  int lock;
  int mError;
  int mLooper;
  int mInitialized;
  int mMediaPlayer;
  int mParameterChanged;
  int mIsEnabled;
  int mHasControl;
  int mEffect;
  int TAG;
}
