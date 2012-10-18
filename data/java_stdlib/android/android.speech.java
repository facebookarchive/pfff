package android.speech;
class SpeechRecognizer {
  class InternalListener {
    int mInternalHandler;
    int MSG_ON_EVENT;
    int MSG_RMS_CHANGED;
    int MSG_PARTIAL_RESULTS;
    int MSG_RESULTS;
    int MSG_READY_FOR_SPEECH;
    int MSG_ERROR;
    int MSG_END_OF_SPEECH;
    int MSG_BUFFER_RECEIVED;
    int MSG_BEGINNING_OF_SPEECH;
    int mInternalListener;
  }
  class Connection {
  }
  int mListener;
  int mPendingTasks;
  int mHandler;
  int mServiceComponent;
  int mContext;
  int mConnection;
  int mService;
  int MSG_CHANGE_LISTENER;
  int MSG_CANCEL;
  int MSG_STOP;
  int MSG_START;
  int ERROR_INSUFFICIENT_PERMISSIONS;
  int ERROR_RECOGNIZER_BUSY;
  int ERROR_NO_MATCH;
  int ERROR_SPEECH_TIMEOUT;
  int ERROR_CLIENT;
  int ERROR_SERVER;
  int ERROR_AUDIO;
  int ERROR_NETWORK;
  int ERROR_NETWORK_TIMEOUT;
  int CONFIDENCE_SCORES;
  int RESULTS_RECOGNITION;
  int TAG;
  int DBG;
}
class RecognizerResultsIntent {
  int URI_SCHEME_INLINE;
  int EXTRA_VOICE_SEARCH_RESULT_HTTP_HEADERS;
  int EXTRA_VOICE_SEARCH_RESULT_HTML_BASE_URLS;
  int EXTRA_VOICE_SEARCH_RESULT_HTML;
  int EXTRA_VOICE_SEARCH_RESULT_URLS;
  int EXTRA_VOICE_SEARCH_RESULT_STRINGS;
  int ACTION_VOICE_SEARCH_RESULTS;
}
class RecognizerIntent {
  int EXTRA_SUPPORTED_LANGUAGES;
  int EXTRA_LANGUAGE_PREFERENCE;
  int EXTRA_ONLY_RETURN_LANGUAGE_PREFERENCE;
  int ACTION_GET_LANGUAGE_DETAILS;
  int DETAILS_META_DATA;
  int EXTRA_CONFIDENCE_SCORES;
  int EXTRA_RESULTS;
  int RESULT_AUDIO_ERROR;
  int RESULT_NETWORK_ERROR;
  int RESULT_SERVER_ERROR;
  int RESULT_CLIENT_ERROR;
  int RESULT_NO_MATCH;
  int EXTRA_RESULTS_PENDINGINTENT_BUNDLE;
  int EXTRA_RESULTS_PENDINGINTENT;
  int EXTRA_PARTIAL_RESULTS;
  int EXTRA_WEB_SEARCH_ONLY;
  int EXTRA_MAX_RESULTS;
  int EXTRA_ORIGIN;
  int EXTRA_LANGUAGE;
  int EXTRA_PROMPT;
  int LANGUAGE_MODEL_WEB_SEARCH;
  int LANGUAGE_MODEL_FREE_FORM;
  int EXTRA_LANGUAGE_MODEL;
  int EXTRA_SPEECH_INPUT_POSSIBLY_COMPLETE_SILENCE_LENGTH_MILLIS;
  int EXTRA_SPEECH_INPUT_COMPLETE_SILENCE_LENGTH_MILLIS;
  int EXTRA_SPEECH_INPUT_MINIMUM_LENGTH_MILLIS;
  int EXTRA_SECURE;
  int ACTION_VOICE_SEARCH_HANDS_FREE;
  int ACTION_WEB_SEARCH;
  int ACTION_RECOGNIZE_SPEECH;
  int EXTRA_CALLING_PACKAGE;
}
class RecognitionService {
  class RecognitionServiceBinder {
    int mInternalService;
  }
  class Callback {
    int mListener;
  }
  class StartListeningArgs {
    int mListener;
    int mIntent;
  }
  int mHandler;
  int MSG_RESET;
  int MSG_CANCEL;
  int MSG_STOP_LISTENING;
  int MSG_START_LISTENING;
  int mCurrentCallback;
  int mBinder;
  int DBG;
  int TAG;
  int SERVICE_META_DATA;
  int SERVICE_INTERFACE;
}
class RecognitionListener {
}
