package android.view.textservice;
class TextServicesManager {
  int sService;
  int sInstance;
  int DBG;
  int TAG;
}
class TextInfo {
  int CREATOR;
  int mSequence;
  int mCookie;
  int mText;
}
class SuggestionsInfo {
  int CREATOR;
  int mSequence;
  int mCookie;
  int mSuggestionsAvailable;
  int mSuggestions;
  int mSuggestionsAttributes;
  int RESULT_ATTR_HAS_RECOMMENDED_SUGGESTIONS;
  int RESULT_ATTR_LOOKS_LIKE_TYPO;
  int RESULT_ATTR_IN_THE_DICTIONARY;
  int EMPTY;
}
class SpellCheckerSubtype {
  int CREATOR;
  int mExtraValueHashMapCache;
  int mSubtypeExtraValue;
  int mSubtypeLocale;
  int mSubtypeNameResId;
  int mSubtypeHashCode;
  int EXTRA_VALUE_KEY_VALUE_SEPARATOR;
  int EXTRA_VALUE_PAIR_SEPARATOR;
  int TAG;
}
class SpellCheckerSession {
  class InternalListener {
    int mParentSpellCheckerSessionListenerImpl;
  }
  class SpellCheckerSessionListener {
  }
  class SpellCheckerSessionListenerImpl {
    class SpellCheckerParams {
      int mSession;
      int mSequentialWords;
      int mSuggestionsLimit;
      int mTextInfos;
      int mWhat;
    }
    int mAsyncHandler;
    int mThread;
    int mISpellCheckerSession;
    int mOpened;
    int mHandler;
    int mPendingTasks;
    int TASK_GET_SUGGESTIONS_MULTIPLE_FOR_SENTENCE;
    int TASK_CLOSE;
    int TASK_GET_SUGGESTIONS_MULTIPLE;
    int TASK_CANCEL;
  }
  int mHandler;
  int mSpellCheckerSessionListener;
  int mIsUsed;
  int mSubtype;
  int mSpellCheckerSessionListenerImpl;
  int mSpellCheckerInfo;
  int mTextServicesManager;
  int mInternalListener;
  int MSG_ON_GET_SUGGESTION_MULTIPLE_FOR_SENTENCE;
  int MSG_ON_GET_SUGGESTION_MULTIPLE;
  int SERVICE_META_DATA;
  int DBG;
  int TAG;
}
class SpellCheckerInfo {
  int CREATOR;
  int mSubtypes;
  int mSettingsActivityName;
  int mLabel;
  int mId;
  int mService;
  int TAG;
}
class SentenceSuggestionsInfo {
  int CREATOR;
  int mLengths;
  int mOffsets;
  int mSuggestionsInfos;
}
