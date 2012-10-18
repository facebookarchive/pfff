package android.speech.tts;
class UtteranceProgressListener {
}
class TtsEngines {
  class EngineInfoComparator {
    int INSTANCE;
  }
  int XML_TAG_NAME;
  int mContext;
  int LOCALE_DELIMITER;
  int DBG;
  int TAG;
}
class TextToSpeechService {
  class CallbackMap {
    int mCallerToCallback;
  }
  int mBinder;
  class SilenceSpeechItem {
    int mDuration;
  }
  class AudioSpeechItem {
    int mItem;
  }
  class SynthesisToFileSpeechItem {
    int mFile;
  }
  class SynthesisSpeechItem {
    int mEventLogger;
    int mSynthesisCallback;
    int mDefaultLocale;
    int mSynthesisRequest;
    int mText;
  }
  class SpeechItem {
    int mStopped;
    int mStarted;
    int mCallerPid;
    int mCallerUid;
    int mParams;
    int mCallerIdentity;
  }
  class UtteranceProgressDispatcher {
  }
  class SynthHandler {
    int mCurrentSpeechItem;
  }
  class SynthThread {
    int mFirstIdle;
  }
  int mPackageName;
  int mCallbacks;
  int mEngineHelper;
  int mAudioPlaybackHandler;
  int mSynthHandler;
  int SYNTH_THREAD_NAME;
  int MAX_SPEECH_ITEM_CHAR_LENGTH;
  int TAG;
  int DBG;
}
class TextToSpeech {
  class EngineInfo {
    int priority;
    int system;
    int icon;
    int label;
    int name;
  }
  class Action {
  }
  class Connection {
    int mCallback;
    int mService;
  }
  int mCurrentEngine;
  int mPackageName;
  int mEnginesHelper;
  int mParams;
  int mUtterances;
  int mEarcons;
  int mUseFallback;
  int mRequestedEngine;
  int mStartLock;
  int mUtteranceProgressListener;
  int mInitListener;
  int mServiceConnection;
  int mContext;
  class Engine {
    int KEY_FEATURE_EMBEDDED_SYNTHESIS;
    int KEY_FEATURE_NETWORK_SYNTHESIS;
    int KEY_PARAM_PAN;
    int KEY_PARAM_VOLUME;
    int KEY_PARAM_UTTERANCE_ID;
    int KEY_PARAM_STREAM;
    int KEY_PARAM_PITCH;
    int KEY_PARAM_ENGINE;
    int KEY_PARAM_VARIANT;
    int KEY_PARAM_COUNTRY;
    int KEY_PARAM_LANGUAGE;
    int KEY_PARAM_RATE;
    int EXTRA_TTS_DATA_INSTALLED;
    int EXTRA_CHECK_VOICE_DATA_FOR;
    int EXTRA_UNAVAILABLE_VOICES;
    int EXTRA_AVAILABLE_VOICES;
    int EXTRA_VOICE_DATA_FILES_INFO;
    int EXTRA_VOICE_DATA_FILES;
    int EXTRA_VOICE_DATA_ROOT_DIRECTORY;
    int ACTION_GET_SAMPLE_TEXT;
    int ACTION_CHECK_TTS_DATA;
    int ACTION_TTS_DATA_INSTALLED;
    int ACTION_INSTALL_TTS_DATA;
    int SERVICE_META_DATA;
    int INTENT_ACTION_TTS_SERVICE;
    int CHECK_VOICE_DATA_MISSING_VOLUME;
    int CHECK_VOICE_DATA_MISSING_DATA;
    int CHECK_VOICE_DATA_BAD_DATA;
    int CHECK_VOICE_DATA_FAIL;
    int CHECK_VOICE_DATA_PASS;
    int DEFAULT_STREAM;
    int DEFAULT_ENGINE;
    int USE_DEFAULTS;
    int DEFAULT_PAN;
    int DEFAULT_VOLUME;
    int DEFAULT_PITCH;
    int DEFAULT_RATE;
  }
  class OnUtteranceCompletedListener {
  }
  class OnInitListener {
  }
  int ACTION_TTS_QUEUE_PROCESSING_COMPLETED;
  int LANG_NOT_SUPPORTED;
  int LANG_MISSING_DATA;
  int LANG_AVAILABLE;
  int LANG_COUNTRY_AVAILABLE;
  int LANG_COUNTRY_VAR_AVAILABLE;
  int QUEUE_DESTROY;
  int QUEUE_ADD;
  int QUEUE_FLUSH;
  int ERROR;
  int SUCCESS;
  int TAG;
}
class SynthesisRequest {
  int mPitch;
  int mSpeechRate;
  int mVariant;
  int mCountry;
  int mLanguage;
  int mParams;
  int mText;
}
class SynthesisPlaybackQueueItem {
  class ListEntry {
    int mBytes;
  }
  int mLogger;
  int mAudioTrack;
  int mIsError;
  int mDone;
  int mStopped;
  int mUnconsumedBytes;
  int mDataBufferList;
  int mNotFull;
  int mReadReady;
  int mListLock;
  int MAX_UNCONSUMED_AUDIO_MS;
  int DBG;
  int TAG;
}
class SynthesisCallback {
}
class SilencePlaybackQueueItem {
  int mSilenceDurationMs;
  int mCondVar;
}
class PlaybackSynthesisCallback {
  int mLogger;
  int mCallerIdentity;
  int mDispatcher;
  int mDone;
  int mStopped;
  int mItem;
  int mAudioTrackHandler;
  int mStateLock;
  int mPan;
  int mVolume;
  int mStreamType;
  int MIN_AUDIO_BUFFER_SIZE;
  int DBG;
  int TAG;
}
class PlaybackQueueItem {
  int mCallerIdentity;
  int mDispatcher;
}
class FileSynthesisCallback {
  int mDone;
  int mStopped;
  int mFile;
  int mChannelCount;
  int mAudioFormat;
  int mSampleRateInHz;
  int mFileName;
  int mStateLock;
  int WAV_FORMAT_PCM;
  int WAV_HEADER_LENGTH;
  int MAX_AUDIO_BUFFER_SIZE;
  int DBG;
  int TAG;
}
class EventLogger {
  int mLogWritten;
  int mStopped;
  int mError;
  int mEngineCompleteTime;
  int mEngineStartTime;
  int mRequestProcessingStartTime;
  int mPlaybackStartTime;
  int mReceivedTime;
  int mCallerPid;
  int mCallerUid;
  int mServiceApp;
  int mRequest;
}
class BlockingAudioTrack {
  int mAudioTrackLock;
  int mStopped;
  int mAudioTrack;
  int mBytesWritten;
  int mAudioBufferSize;
  int mIsShortUtterance;
  int mBytesPerFrame;
  int mPan;
  int mVolume;
  int mChannelCount;
  int mAudioFormat;
  int mSampleRateInHz;
  int mStreamType;
  int MIN_AUDIO_BUFFER_SIZE;
  int MAX_PROGRESS_WAIT_MS;
  int MAX_SLEEP_TIME_MS;
  int MIN_SLEEP_TIME_MS;
  int DBG;
  int TAG;
}
class AudioPlaybackQueueItem {
  int mFinished;
  int mPlayer;
  int mDone;
  int mStreamType;
  int mUri;
  int mContext;
  int TAG;
}
class AudioPlaybackHandler {
  class MessageLoop {
  }
  int mCurrentWorkItem;
  int mHandlerThread;
  int mQueue;
  int DBG;
  int TAG;
}
class AbstractSynthesisCallback {
}
