package android.inputmethodservice;
class SoftInputWindow {
  int mBounds;
  int mDispatcherState;
}
class KeyboardView {
  class SwipeTracker {
    int mXVelocity;
    int mYVelocity;
    int mPastTime;
    int mPastY;
    int mPastX;
    int LONGEST_PAST_TIME;
    int NUM_PAST;
  }
  int mHandler;
  int mHeadsetRequiredToHearPasswordsAnnounced;
  int mAudioManager;
  int mAccessibilityManager;
  int mCanvas;
  int mKeyboardChanged;
  int mBuffer;
  int mDirtyRect;
  int mDrawPending;
  int mPreviewLabel;
  int MULTITAP_INTERVAL;
  int mInMultiTap;
  int mLastTapTime;
  int mTapCount;
  int mLastSentIndex;
  int mDistances;
  int MAX_NEARBY_KEYS;
  int LONGPRESS_TIMEOUT;
  int REPEAT_START_DELAY;
  int REPEAT_INTERVAL;
  int mKeyBackground;
  int mOldPointerY;
  int mOldPointerX;
  int mOldPointerCount;
  int mDisambiguateSwipe;
  int mSwipeThreshold;
  int mSwipeTracker;
  int mPossiblePoly;
  int mClipRegion;
  int mInvalidatedKey;
  int mAbortKey;
  int mPopupLayout;
  int mRepeatKeyIndex;
  int mPopupY;
  int mPopupX;
  int mGestureDetector;
  int mKeyIndices;
  int mCurrentKeyTime;
  int mLastKeyTime;
  int mDownKey;
  int mCurrentKey;
  int mLastCodeY;
  int mLastCodeX;
  int mLastKey;
  int mLastMoveTime;
  int mDownTime;
  int mPadding;
  int mPaint;
  int mProximityCorrectOn;
  int mStartY;
  int mStartX;
  int mLastY;
  int mLastX;
  int mPopupPreviewY;
  int mPopupPreviewX;
  int mShowTouchPoints;
  int mShowPreview;
  int mPreviewCentered;
  int mProximityThreshold;
  int mVerticalCorrection;
  int DEBOUNCE_TIME;
  int DELAY_AFTER_PREVIEW;
  int DELAY_BEFORE_PREVIEW;
  int MSG_LONGPRESS;
  int MSG_REPEAT;
  int MSG_REMOVE_PREVIEW;
  int MSG_SHOW_PREVIEW;
  int mKeyboardActionListener;
  int mKeys;
  int mMiniKeyboardCache;
  int mMiniKeyboardOffsetY;
  int mMiniKeyboardOffsetX;
  int mPopupParent;
  int mMiniKeyboardOnScreen;
  int mMiniKeyboard;
  int mMiniKeyboardContainer;
  int mPopupKeyboard;
  int mCoordinates;
  int mPreviewHeight;
  int mPreviewOffset;
  int mPreviewTextSizeLarge;
  int mPreviewPopup;
  int mPreviewText;
  int mBackgroundDimAmount;
  int mShadowColor;
  int mShadowRadius;
  int mKeyTextColor;
  int mKeyTextSize;
  int mLabelTextSize;
  int mCurrentKeyIndex;
  int mKeyboard;
  int LONG_PRESSABLE_STATE_SET;
  int KEY_DELETE;
  int NOT_A_KEY;
  int DEBUG;
  class OnKeyboardActionListener {
  }
}
class Keyboard {
  class Key {
    int KEY_STATE_PRESSED;
    int KEY_STATE_NORMAL;
    int KEY_STATE_PRESSED_OFF;
    int KEY_STATE_NORMAL_OFF;
    int KEY_STATE_PRESSED_ON;
    int KEY_STATE_NORMAL_ON;
    int repeatable;
    int popupResId;
    int keyboard;
    int modifier;
    int edgeFlags;
    int popupCharacters;
    int text;
    int on;
    int pressed;
    int y;
    int x;
    int sticky;
    int gap;
    int height;
    int width;
    int iconPreview;
    int icon;
    int label;
    int codes;
  }
  class Row {
    int parent;
    int mode;
    int rowEdgeFlags;
    int mKeys;
    int verticalGap;
    int defaultHorizontalGap;
    int defaultHeight;
    int defaultWidth;
  }
  int rows;
  int SEARCH_DISTANCE;
  int mProximityThreshold;
  int mGridNeighbors;
  int mCellHeight;
  int mCellWidth;
  int GRID_SIZE;
  int GRID_HEIGHT;
  int GRID_WIDTH;
  int mKeyboardMode;
  int mDisplayHeight;
  int mDisplayWidth;
  int mModifierKeys;
  int mKeys;
  int mTotalWidth;
  int mTotalHeight;
  int mKeyHeight;
  int mKeyWidth;
  int mShiftKeyIndices;
  int mShiftKeys;
  int mShifted;
  int mDefaultVerticalGap;
  int mDefaultHeight;
  int mDefaultWidth;
  int mDefaultHorizontalGap;
  int mLabel;
  int KEYCODE_ALT;
  int KEYCODE_DELETE;
  int KEYCODE_DONE;
  int KEYCODE_CANCEL;
  int KEYCODE_MODE_CHANGE;
  int KEYCODE_SHIFT;
  int EDGE_BOTTOM;
  int EDGE_TOP;
  int EDGE_RIGHT;
  int EDGE_LEFT;
  int TAG_KEY;
  int TAG_ROW;
  int TAG_KEYBOARD;
  int TAG;
}
class InputMethodService {
  int MOVEMENT_UP;
  int MOVEMENT_DOWN;
  class Insets {
    int touchableInsets;
    int TOUCHABLE_INSETS_REGION;
    int TOUCHABLE_INSETS_VISIBLE;
    int TOUCHABLE_INSETS_CONTENT;
    int TOUCHABLE_INSETS_FRAME;
    int touchableRegion;
    int visibleTopInsets;
    int contentTopInsets;
  }
  class InputMethodSessionImpl {
  }
  class InputMethodImpl {
  }
  int mActionClickListener;
  int mInsetsComputer;
  int mTmpLocation;
  int mTmpInsets;
  int mBackDisposition;
  int mStatusIcon;
  int mIsInputViewShown;
  int mInputView;
  int mExtractedToken;
  int mExtractedText;
  int mExtractAction;
  int mExtractAccessories;
  int mExtractEditText;
  int mExtractViewHidden;
  int mExtractView;
  int mIsFullscreen;
  int mFullscreenApplied;
  int mShowInputForced;
  int mCurCompletions;
  int mCandidatesVisibility;
  int mLastShowInputRequested;
  int mShowInputRequested;
  int mShowInputFlags;
  int mInputEditorInfo;
  int mStartedInputConnection;
  int mCandidatesViewStarted;
  int mInputViewStarted;
  int mInputStarted;
  int mInputConnection;
  int mInputBinding;
  int mToken;
  int mInputFrame;
  int mCandidatesFrame;
  int mExtractFrame;
  int mFullscreenArea;
  int mInShowWindow;
  int mWindowWasVisible;
  int mWindowVisible;
  int mWindowAdded;
  int mWindowCreated;
  int mInitialized;
  int mWindow;
  int mRootView;
  int mThemeAttrs;
  int mInflater;
  int mTheme;
  int mImm;
  int IME_VISIBLE;
  int IME_ACTIVE;
  int BACK_DISPOSITION_WILL_DISMISS;
  int BACK_DISPOSITION_WILL_NOT_DISMISS;
  int BACK_DISPOSITION_DEFAULT;
  int DEBUG;
  int TAG;
}
class IInputMethodWrapper {
  class InputMethodSessionCallbackWrapper {
    int mCb;
    int mContext;
  }
  class Notifier {
    int notified;
  }
  int mTargetSdkVersion;
  int mInputMethod;
  int mCaller;
  int mTarget;
  int DO_CHANGE_INPUTMETHOD_SUBTYPE;
  int DO_HIDE_SOFT_INPUT;
  int DO_SHOW_SOFT_INPUT;
  int DO_REVOKE_SESSION;
  int DO_SET_SESSION_ENABLED;
  int DO_CREATE_SESSION;
  int DO_RESTART_INPUT;
  int DO_START_INPUT;
  int DO_UNSET_INPUT_CONTEXT;
  int DO_SET_INPUT_CONTEXT;
  int DO_ATTACH_TOKEN;
  int DO_DUMP;
  int DEBUG;
  int TAG;
}
class IInputMethodSessionWrapper {
  class InputMethodEventCallbackWrapper {
    int mCb;
  }
  int mInputMethodSession;
  int mCaller;
  int DO_VIEW_CLICKED;
  int DO_FINISH_SESSION;
  int DO_TOGGLE_SOFT_INPUT;
  int DO_APP_PRIVATE_COMMAND;
  int DO_UPDATE_CURSOR;
  int DO_UPDATE_SELECTION;
  int DO_DISPATCH_TRACKBALL_EVENT;
  int DO_DISPATCH_KEY_EVENT;
  int DO_UPDATE_EXTRACTED_TEXT;
  int DO_DISPLAY_COMPLETIONS;
  int DO_FINISH_INPUT;
  int DEBUG;
  int TAG;
}
class ExtractEditText {
  int mSettingExtractedText;
  int mIME;
}
class ExtractEditLayout {
  class ExtractActionMode {
    int mMenu;
    int mCallback;
  }
  int mEditButton;
  int mExtractActionButton;
  int mActionMode;
}
class ExtractButton {
}
class AbstractInputMethodService {
  class AbstractInputMethodSessionImpl {
    int mRevoked;
    int mEnabled;
  }
  class AbstractInputMethodImpl {
  }
  int mDispatcherState;
  int mInputMethod;
}
