package android.media.audiofx;
class Visualizer {
  class NativeEventHandler {
    int mVisualizer;
  }
  class OnServerDiedListener {
  }
  class OnDataCaptureListener {
  }
  int mJniData;
  int mNativeVisualizer;
  int mServerDiedListener;
  int mCaptureListener;
  int mNativeEventHandler;
  int mListenerLock;
  int mId;
  int mStateLock;
  int mState;
  int ERROR_DEAD_OBJECT;
  int ERROR_NO_MEMORY;
  int ERROR_INVALID_OPERATION;
  int ERROR_BAD_VALUE;
  int ERROR_NO_INIT;
  int ALREADY_EXISTS;
  int ERROR;
  int SUCCESS;
  int NATIVE_EVENT_SERVER_DIED;
  int NATIVE_EVENT_FFT_CAPTURE;
  int NATIVE_EVENT_PCM_CAPTURE;
  int SCALING_MODE_AS_PLAYED;
  int SCALING_MODE_NORMALIZED;
  int STATE_ENABLED;
  int STATE_INITIALIZED;
  int STATE_UNINITIALIZED;
  int TAG;
}
class Virtualizer {
  class Settings {
    int strength;
  }
  class BaseParameterListener {
  }
  class OnParameterChangeListener {
  }
  int mParamListenerLock;
  int mBaseParamListener;
  int mParamListener;
  int mStrengthSupported;
  int PARAM_STRENGTH;
  int PARAM_STRENGTH_SUPPORTED;
  int TAG;
}
class PresetReverb {
  class Settings {
    int preset;
  }
  class BaseParameterListener {
  }
  class OnParameterChangeListener {
  }
  int mParamListenerLock;
  int mBaseParamListener;
  int mParamListener;
  int PRESET_PLATE;
  int PRESET_LARGEHALL;
  int PRESET_MEDIUMHALL;
  int PRESET_LARGEROOM;
  int PRESET_MEDIUMROOM;
  int PRESET_SMALLROOM;
  int PRESET_NONE;
  int PARAM_PRESET;
  int TAG;
}
class NoiseSuppressor {
  int TAG;
}
class Equalizer {
  class Settings {
    int bandLevels;
    int numBands;
    int curPreset;
  }
  class BaseParameterListener {
  }
  class OnParameterChangeListener {
  }
  int mParamListenerLock;
  int mBaseParamListener;
  int mParamListener;
  int mPresetNames;
  int mNumPresets;
  int mNumBands;
  int PARAM_STRING_SIZE_MAX;
  int PARAM_PROPERTIES;
  int PARAM_GET_PRESET_NAME;
  int PARAM_GET_NUM_OF_PRESETS;
  int PARAM_CURRENT_PRESET;
  int PARAM_GET_BAND;
  int PARAM_BAND_FREQ_RANGE;
  int PARAM_CENTER_FREQ;
  int PARAM_BAND_LEVEL;
  int PARAM_LEVEL_RANGE;
  int PARAM_NUM_BANDS;
  int TAG;
}
class EnvironmentalReverb {
  int PROPERTY_SIZE;
  class Settings {
    int density;
    int diffusion;
    int reverbDelay;
    int reverbLevel;
    int reflectionsDelay;
    int reflectionsLevel;
    int decayHFRatio;
    int decayTime;
    int roomHFLevel;
    int roomLevel;
  }
  class BaseParameterListener {
  }
  class OnParameterChangeListener {
  }
  int mParamListenerLock;
  int mBaseParamListener;
  int mParamListener;
  int PARAM_PROPERTIES;
  int PARAM_DENSITY;
  int PARAM_DIFFUSION;
  int PARAM_REVERB_DELAY;
  int PARAM_REVERB_LEVEL;
  int PARAM_REFLECTIONS_DELAY;
  int PARAM_REFLECTIONS_LEVEL;
  int PARAM_DECAY_HF_RATIO;
  int PARAM_DECAY_TIME;
  int PARAM_ROOM_HF_LEVEL;
  int PARAM_ROOM_LEVEL;
  int TAG;
}
class BassBoost {
  class Settings {
    int strength;
  }
  class BaseParameterListener {
  }
  class OnParameterChangeListener {
  }
  int mParamListenerLock;
  int mBaseParamListener;
  int mParamListener;
  int mStrengthSupported;
  int PARAM_STRENGTH;
  int PARAM_STRENGTH_SUPPORTED;
  int TAG;
}
class AutomaticGainControl {
  int TAG;
}
class AudioEffect {
  class NativeEventHandler {
    int mAudioEffect;
  }
  int CONTENT_TYPE_VOICE;
  int CONTENT_TYPE_GAME;
  int CONTENT_TYPE_MOVIE;
  int CONTENT_TYPE_MUSIC;
  int EXTRA_CONTENT_TYPE;
  int EXTRA_PACKAGE_NAME;
  int EXTRA_AUDIO_SESSION;
  int ACTION_CLOSE_AUDIO_EFFECT_CONTROL_SESSION;
  int ACTION_OPEN_AUDIO_EFFECT_CONTROL_SESSION;
  int ACTION_DISPLAY_AUDIO_EFFECT_CONTROL_PANEL;
  class OnParameterChangeListener {
  }
  class OnControlStatusChangeListener {
  }
  class OnEnableStatusChangeListener {
  }
  int mNativeEventHandler;
  int mListenerLock;
  int mParameterChangeListener;
  int mControlChangeStatusListener;
  int mEnableStatusChangeListener;
  int mDescriptor;
  int mJniData;
  int mNativeAudioEffect;
  int mId;
  int mStateLock;
  int mState;
  int EFFECT_PRE_PROCESSING;
  int EFFECT_AUXILIARY;
  int EFFECT_INSERT;
  class Descriptor {
    int implementor;
    int name;
    int connectMode;
    int uuid;
    int type;
  }
  int ERROR_DEAD_OBJECT;
  int ERROR_NO_MEMORY;
  int ERROR_INVALID_OPERATION;
  int ERROR_BAD_VALUE;
  int ERROR_NO_INIT;
  int ALREADY_EXISTS;
  int ERROR;
  int SUCCESS;
  int NATIVE_EVENT_PARAMETER_CHANGED;
  int NATIVE_EVENT_ENABLED_STATUS;
  int NATIVE_EVENT_CONTROL_STATUS;
  int STATE_INITIALIZED;
  int STATE_UNINITIALIZED;
  int EFFECT_TYPE_NULL;
  int EFFECT_TYPE_NS;
  int EFFECT_TYPE_AEC;
  int EFFECT_TYPE_AGC;
  int EFFECT_TYPE_VIRTUALIZER;
  int EFFECT_TYPE_BASS_BOOST;
  int EFFECT_TYPE_EQUALIZER;
  int EFFECT_TYPE_PRESET_REVERB;
  int EFFECT_TYPE_ENV_REVERB;
  int TAG;
}
class AcousticEchoCanceler {
  int TAG;
}
