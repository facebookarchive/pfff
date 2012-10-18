package com.android.ex.editstyledtext;
class EditStyledText {
  class EditModeActions {
    class SizeAction {
    }
    class ColorAction {
    }
    class MarqueeDialogAction {
    }
    class SwingAction {
    }
    class TelopAction {
    }
    class AlignAction {
    }
    class SetSpanActionBase {
    }
    class ShowMenuAction {
    }
    class ResetAction {
    }
    class EndEditAction {
    }
    class StartEditAction {
    }
    class PreviewAction {
    }
    class BackgroundColorAction {
    }
    class ImageAction {
    }
    class CancelAction {
    }
    class StopSelectionAction {
    }
    class ClearStylesAction {
    }
    class HorizontalLineAction {
    }
    class SelectAllAction {
    }
    class PasteAction {
    }
    class SelectAction {
    }
    class CutAction {
    }
    class CopyAction {
    }
    class TextViewAction {
    }
    class TextViewActionBase {
    }
    class NothingAction {
    }
    class EditModeActionBase {
      int mParams;
    }
    int mSizeAction;
    int mColorAction;
    int mMarqueeDialogAction;
    int mSwingAction;
    int mTelopAction;
    int mAlignAction;
    int mShowMenuAction;
    int mResetAction;
    int mEndEditAction;
    int mStartEditAction;
    int mTextViewAction;
    int mCancelEditAction;
    int mPreviewAction;
    int mBackgroundColorAction;
    int mImageAction;
    int mClearStylesAction;
    int mStopSelectionAction;
    int mHorizontalLineAction;
    int mSelectAllAction;
    int mCutAction;
    int mSelectAction;
    int mPasteAction;
    int mCopyAction;
    int mNothingAction;
    int mActionMap;
    int mMode;
    int mDialog;
    int mManager;
    int mEST;
    int DBG;
    int TAG;
  }
  class ColorPaletteDrawable {
    int mRect;
  }
  class EditStyledTextSpans {
    class HorizontalLineDrawable {
      int DBG_HL;
      int mWidth;
      int mSpannable;
    }
    class RescalableImageSpan {
      int MAXWIDTH;
      int mIntrinsicHeight;
      int mIntrinsicWidth;
      int mContext;
      int mDrawable;
      int mContentUri;
    }
    class MarqueeSpan {
      int mMarqueeColor;
      int mType;
      int NOTHING;
      int ALTERNATE;
      int SCROLL;
    }
    class HorizontalLineSpan {
      int mDrawable;
    }
    int LOG_TAG;
  }
  class StyledTextInputConnection {
    int mEST;
  }
  class StyledTextArrowKeyMethod {
    int LOG_TAG;
    int mManager;
  }
  class MenuHandler {
  }
  class StyledTextDialog {
    int mEST;
    int mColorDefaultMessage;
    int mMarqueeNames;
    int mAlignNames;
    int mSizeSendInts;
    int mSizeDisplayInts;
    int mSizeNames;
    int mColorInts;
    int mColorNames;
    int mMarqueeTitle;
    int mAlignTitle;
    int mSizeTitle;
    int mColorTitle;
    int mAlertDialog;
    int mBuilder;
    int TYPE_BACKGROUND;
    int TYPE_FOREGROUND;
  }
  class SavedStyledTextState {
    int mBackgroundColor;
  }
  class SoftKeyReceiver {
    int mEST;
    int mNewEnd;
    int mNewStart;
  }
  class StyledTextConverter {
    int mHtml;
    int mEST;
  }
  class StyledTextHtmlStandard {
  }
  class EditorManager {
    int mCopyBuffer;
    int mSkr;
    int mActions;
    int mEST;
    int mComposingTextMask;
    int mBackgroundColor;
    int mSizeWaitInput;
    int mColorWaitInput;
    int mCurEnd;
    int mCurStart;
    int mState;
    int mMode;
    int mTextIsFinishedFlag;
    int mWaitInputFlag;
    int mKeepNonLineSpan;
    int mSoftKeyBlockFlag;
    int mEditFlag;
    int LOG_TAG;
  }
  class EditStyledTextNotifier {
  }
  class StyledTextHtmlConverter {
  }
  int PRESSED;
  int SELECTING;
  int mDialog;
  int mConverter;
  int mInputConnection;
  int mManager;
  int mDefaultBackground;
  int mESTNotifiers;
  int mPaddingScale;
  int STR_PASTE;
  int STR_CLEARSTYLES;
  int STR_HORIZONTALLINE;
  int MAXIMAGEWIDTHDIP;
  int ID_HIDEEDIT;
  int ID_SHOWEDIT;
  int ID_CLEARSTYLES;
  int ID_HORIZONTALLINE;
  int ID_CUT;
  int ID_COPY;
  int ID_PASTE;
  int ID_STOP_SELECTING_TEXT;
  int ID_START_SELECTING_TEXT;
  int ID_SELECT_ALL;
  int IMAGECHAR;
  int ZEROWIDTHCHAR;
  int DEFAULT_FOREGROUND_COLOR;
  int DEFAULT_TRANSPARENT_COLOR;
  int HINT_MSG_END_COMPOSE;
  int HINT_MSG_END_PREVIEW;
  int HINT_MSG_BIG_SIZE_ERROR;
  int HINT_MSG_PUSH_COMPETE;
  int HINT_MSG_SELECT_END;
  int HINT_MSG_SELECT_START;
  int HINT_MSG_COPY_BUF_BLANK;
  int HINT_MSG_NULL;
  int STATE_SELECT_FIX;
  int STATE_SELECTED;
  int STATE_SELECT_ON;
  int STATE_SELECT_OFF;
  int MODE_SHOW_MENU;
  int MODE_RESET;
  int MODE_END_EDIT;
  int MODE_START_EDIT;
  int MODE_TEXTVIEWFUNCTION;
  int MODE_CANCEL;
  int MODE_PREVIEW;
  int MODE_BGCOLOR;
  int MODE_IMAGE;
  int MODE_CLEARSTYLES;
  int MODE_STOP_SELECT;
  int MODE_HORIZONTALLINE;
  int MODE_SELECTALL;
  int MODE_MARQUEE;
  int MODE_SWING;
  int MODE_TELOP;
  int MODE_CUT;
  int MODE_ALIGN;
  int MODE_SELECT;
  int MODE_COLOR;
  int MODE_SIZE;
  int MODE_PASTE;
  int MODE_COPY;
  int MODE_NOTHING;
  int DBG;
  int TAG;
}
