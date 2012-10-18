package com.android.mediaframeworktest.unit;
class MediaRecorderStopStateUnitTest {
  int SLEEP_TIME_BEFORE_STOP;
  int TAG;
  int mTestTemplate;
}
class MediaRecorderStateUnitTestTemplate {
  int mMethodUnderTest;
  int mMediaRecorderState;
  int mMediaRecorder;
  int mStateErrors;
  int TAG;
  int AUDIO_SOURCE;
  int AUDIO_ENCODER;
  int OUTPUT_FORMAT;
  int RECORD_OUTPUT_PATH;
}
class MediaRecorderStateErrors {
  int errorInErrorState;
  int errorInRecordingState;
  int errorInPreparedState;
  int errorInDataSourceConfiguredState;
  int errorInInitializedState;
  int errorInInitialStateAfterStop;
  int errorInInitialStateAfterReset;
  int errorInInitialState;
  class MediaRecorderState {
    int ERROR;
    int RECORDING;
    int PREPARED;
    int DATASOURCECONFIGURED;
    int INITIALIZED;
    int INITIAL_AFTER_STOP;
    int INITIAL_AFTER_RESET;
    int INITIAL;
  }
}
class MediaRecorderStartStateUnitTest {
  int mTestTemplate;
}
class MediaRecorderSetOutputFormatStateUnitTest {
  int mTestTemplate;
}
class MediaRecorderSetOutputFileStateUnitTest {
  int mTestTemplate;
}
class MediaRecorderSetAudioSourceStateUnitTest {
  int mTestTemplate;
}
class MediaRecorderSetAudioEncoderStateUnitTest {
  int mTestTemplate;
}
class MediaRecorderResetStateUnitTest {
  int mTestTemplate;
}
class MediaRecorderPrepareStateUnitTest {
  int mTestTemplate;
}
class MediaRecorderMethodUnderTest {
}
class MediaPlayerStopStateUnitTest {
  int mTestTemplate;
}
class MediaPlayerStateUnitTestTemplate {
  int mHandler;
  int mMethodUnderTest;
  int lock;
  int mLooper;
  int mMediaPlayerState;
  int mOnCompletionHasBeenCalled;
  int mInitialized;
  int mMediaPlayer;
  int mStateErrors;
  int WAIT_FOR_COMMAND_TO_COMPLETE;
  int SEEK_TO_END;
  int TAG;
  int TEST_PATH;
}
class MediaPlayerStateErrors {
  int errorInErrorState;
  int errorInPlaybackCompletedState;
  int errorInPreparedStateAfterStop;
  int errorInStoppedState;
  int errorInStartedStateAfterPause;
  int errorInPausedState;
  int errorInStartedState;
  int errorInPreparedState;
  int errorInInitializedState;
  int errorInIdleStateAfterReset;
  int errorInIdleState;
  class MediaPlayerState {
    int ERROR;
    int PLAYBACK_COMPLETED;
    int STOPPED;
    int PAUSED;
    int STARTED_AFTER_PAUSE;
    int STARTED;
    int PREPARED_AFTER_STOP;
    int PREPARED;
    int INITIALIZED;
    int IDLE_AFTER_RESET;
    int IDLE;
  }
  int MEDIA_PLAYER_ERROR;
}
class MediaPlayerStartStateUnitTest {
  int mTestTemplate;
}
class MediaPlayerSetVolumeStateUnitTest {
  int mTestTemplate;
}
class MediaPlayerSetLoopingStateUnitTest {
  int looping;
  int mTestTemplate;
}
class MediaPlayerSetAudioStreamTypeStateUnitTest {
  int mTestTemplate;
}
class MediaPlayerSeekToStateUnitTest {
  int mTestTemplate;
}
class MediaPlayerResetStateUnitTest {
  int mTestTemplate;
}
class MediaPlayerPauseStateUnitTest {
  int mTestTemplate;
}
class MediaPlayerMethodUnderTest {
}
class MediaPlayerMetadataParserTest {
  int mParcel;
  int mMetadata;
  int kHeaderSize;
  int kMarker;
  int TAG;
}
class MediaPlayerIsPlayingStateUnitTest {
  int mTestTemplate;
}
class MediaPlayerGetVideoWidthStateUnitTest {
  int mTestTemplate;
}
class MediaPlayerGetVideoHeightStateUnitTest {
  int mTestTemplate;
}
class MediaPlayerGetDurationStateUnitTest {
  int mTestTemplate;
}
class MediaPlayerGetCurrentPositionStateUnitTest {
  int mTestTemplate;
}
class MediaMetadataRetrieverTest {
  int TAG;
}
class MediaInserterTest {
  class MediaUriMatcher {
    int mUri;
  }
  int sFilesUri;
  int sImagesUri;
  int sVideoUri;
  int sAudioUri;
  int sVolumeName;
  int mImagesCounter;
  int mVideoCounter;
  int mAudioCounter;
  int mFilesCounter;
  int mMockProvider;
  int TEST_BUFFER_SIZE;
  int mMediaInserter;
}
