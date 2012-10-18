package android.view.inputmethod;
class InputMethodSubtype {
  int CREATOR;
  int mExtraValueHashMapCache;
  int mSubtypeExtraValue;
  int mSubtypeMode;
  int mSubtypeLocale;
  int mSubtypeNameResId;
  int mSubtypeIconResId;
  int mSubtypeHashCode;
  int mOverridesImplicitlyEnabledSubtype;
  int mIsAuxiliary;
  int EXTRA_KEY_UNTRANSLATABLE_STRING_IN_SUBTYPE_NAME;
  int EXTRA_VALUE_KEY_VALUE_SEPARATOR;
  int EXTRA_VALUE_PAIR_SEPARATOR;
  int TAG;
}
class InputMethodSession {
  class EventCallback {
  }
}
class InputMethodManager_Delegate {
}
class InputMethodManager_Accessor {
}
class InputMethodManager {
  int HIDE_NOT_ALWAYS;
  int HIDE_IMPLICIT_ONLY;
  int RESULT_HIDDEN;
  int RESULT_SHOWN;
  int RESULT_UNCHANGED_HIDDEN;
  int RESULT_UNCHANGED_SHOWN;
  int SHOW_FORCED;
  int SHOW_IMPLICIT;
  int mDummyInputConnection;
  int mClient;
  class ControlledInputConnectionWrapper {
    int mActive;
    int mParentInputMethodManager;
  }
  class H {
  }
  int MSG_SET_ACTIVE;
  int MSG_UNBIND;
  int MSG_BIND;
  int MSG_DUMP;
  int mCurMethod;
  int mCurId;
  int mBindSequence;
  int mCursorCandEnd;
  int mCursorCandStart;
  int mCursorSelEnd;
  int mCursorSelStart;
  int mCursorRect;
  int mTmpCursorRect;
  int mCompletions;
  int mServedInputConnectionWrapper;
  int mServedInputConnection;
  int mCurrentTextBoxAttribute;
  int mServedConnecting;
  int mNextServedView;
  int mServedView;
  int mCurRootView;
  int mFullscreenMode;
  int mHasBeenInactive;
  int mActive;
  int mIInputContext;
  int mH;
  int mMainLooper;
  int mService;
  int CONTROL_START_INITIAL;
  int CONTROL_WINDOW_FIRST;
  int CONTROL_WINDOW_IS_TEXT_EDITOR;
  int CONTROL_WINDOW_VIEW_HAS_FOCUS;
  int mInstance;
  int mInstanceSync;
  int TAG;
  int DEBUG;
}
class InputMethodInfo {
  int CREATOR;
  int mIsAuxIme;
  int mSubtypes;
  int mIsDefaultResId;
  int mSettingsActivityName;
  int mId;
  int mService;
  int TAG;
}
class InputMethod {
  int SHOW_FORCED;
  int SHOW_EXPLICIT;
  class SessionCallback {
  }
  int SERVICE_META_DATA;
  int SERVICE_INTERFACE;
}
class InputConnectionWrapper {
  int mMutable;
  int mTarget;
}
class InputConnection {
  int GET_EXTRACTED_TEXT_MONITOR;
  int GET_TEXT_WITH_STYLES;
}
class InputBinding {
  int CREATOR;
  int mPid;
  int mUid;
  int mConnectionToken;
  int mConnection;
  int TAG;
}
class ExtractedTextRequest {
  int CREATOR;
  int hintMaxChars;
  int hintMaxLines;
  int flags;
  int token;
}
class ExtractedText {
  int CREATOR;
  int flags;
  int FLAG_SELECTING;
  int FLAG_SINGLE_LINE;
  int selectionEnd;
  int selectionStart;
  int partialEndOffset;
  int partialStartOffset;
  int startOffset;
  int text;
}
class EditorInfo {
  int CREATOR;
  int extras;
  int fieldName;
  int fieldId;
  int packageName;
  int label;
  int hintText;
  int initialCapsMode;
  int initialSelEnd;
  int initialSelStart;
  int actionId;
  int actionLabel;
  int privateImeOptions;
  int imeOptions;
  int IME_NULL;
  int IME_FLAG_FORCE_ASCII;
  int IME_FLAG_NO_ENTER_ACTION;
  int IME_FLAG_NO_ACCESSORY_ACTION;
  int IME_FLAG_NO_EXTRACT_UI;
  int IME_FLAG_NAVIGATE_NEXT;
  int IME_FLAG_NAVIGATE_PREVIOUS;
  int IME_FLAG_NO_FULLSCREEN;
  int IME_ACTION_PREVIOUS;
  int IME_ACTION_DONE;
  int IME_ACTION_NEXT;
  int IME_ACTION_SEND;
  int IME_ACTION_SEARCH;
  int IME_ACTION_GO;
  int IME_ACTION_NONE;
  int IME_ACTION_UNSPECIFIED;
  int IME_MASK_ACTION;
  int inputType;
}
class CorrectionInfo {
  int CREATOR;
  int mNewText;
  int mOldText;
  int mOffset;
}
class CompletionInfo {
  int CREATOR;
  int mLabel;
  int mText;
  int mPosition;
  int mId;
}
class BaseInputConnection {
  int mKeyCharacterMap;
  int mEditable;
  int mDefaultComposingSpans;
  int mDummyMode;
  int mTargetView;
  int mIMM;
  int COMPOSING;
  int TAG;
  int DEBUG;
}
class ComposingText {
}
