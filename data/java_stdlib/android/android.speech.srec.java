package android.speech.srec;
class WaveHeader {
  int mNumBytes;
  int mBitsPerSample;
  int mSampleRate;
  int mNumChannels;
  int mFormat;
  int FORMAT_ULAW;
  int FORMAT_ALAW;
  int FORMAT_PCM;
  int HEADER_LENGTH;
  int TAG;
}
class UlawEncoderInputStream {
  int mOneByte;
  int mBufCount;
  int mBuf;
  int mMax;
  int mIn;
  int SCALE_BITS;
  int MAX_ULAW;
  int TAG;
}
class Recognizer {
  int EVENT_MAX_SPEECH;
  int EVENT_NEED_MORE_AUDIO;
  int EVENT_RECOGNITION_TIMEOUT;
  int EVENT_START_OF_UTTERANCE_TIMEOUT;
  int EVENT_RECOGNITION_RESULT;
  int EVENT_SPOKE_TOO_SOON;
  int EVENT_END_OF_VOICING;
  int EVENT_START_OF_VOICING;
  int EVENT_STOPPED;
  int EVENT_STARTED;
  int EVENT_INCOMPLETE;
  int EVENT_NO_MATCH;
  int EVENT_INVALID;
  int mPutAudioBuffer;
  class Grammar {
    int mGrammar;
  }
  int mActiveGrammar;
  int mRecognizer;
  int mVocabulary;
  int KEY_MEANING;
  int KEY_LITERAL;
  int KEY_CONFIDENCE;
  int TAG;
}
class MicrophoneInputStream {
  int mOneByte;
  int mAudioRecord;
  int TAG;
}
