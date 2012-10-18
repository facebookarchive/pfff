package android.service.textservice;
class SpellCheckerService {
  class SentenceLevelAdapter {
    int mWordIterator;
    class SentenceTextInfoParams {
      int mSize;
      int mItems;
      int mOriginalTextInfo;
    }
    class SentenceWordItem {
      int mLength;
      int mStart;
      int mTextInfo;
    }
    int EMPTY_SUGGESTIONS_INFO;
    int EMPTY_SENTENCE_SUGGESTIONS_INFOS;
  }
  class SpellCheckerServiceBinder {
    int mInternalServiceRef;
  }
  class InternalISpellCheckerSession {
    int mBundle;
    int mLocale;
    int mSession;
    int mListener;
  }
  class Session {
    int mSentenceLevelAdapter;
    int mInternalSession;
  }
  int mBinder;
  int SERVICE_INTERFACE;
  int DBG;
  int TAG;
}
