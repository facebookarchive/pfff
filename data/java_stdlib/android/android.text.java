package android.text;
class TextWatcher {
}
class TextUtilsTest {
  class MockSpanned {
    int nonEmptySpans;
    int allSpans;
  }
  class Wrapper {
    int mString;
  }
}
class TextUtils {
  int ELLIPSIS_TWO_DOTS;
  int ELLIPSIS_NORMAL;
  int ZWNBS_CHAR;
  int EMPTY_STRING_ARRAY;
  int sTemp;
  int sLock;
  int CAP_MODE_SENTENCES;
  int CAP_MODE_WORDS;
  int CAP_MODE_CHARACTERS;
  int FIRST_RIGHT_TO_LEFT;
  class EllipsizeCallback {
  }
  class TruncateAt {
    int END_SMALL;
    int MARQUEE;
    int END;
    int MIDDLE;
    int START;
  }
  int CHAR_SEQUENCE_CREATOR;
  int EASY_EDIT_SPAN;
  int SUGGESTION_RANGE_SPAN;
  int SPELL_CHECK_SPAN;
  int SUGGESTION_SPAN;
  int ANNOTATION;
  int TEXT_APPEARANCE_SPAN;
  int ABSOLUTE_SIZE_SPAN;
  int SUBSCRIPT_SPAN;
  int SUPERSCRIPT_SPAN;
  int TYPEFACE_SPAN;
  int BACKGROUND_COLOR_SPAN;
  int URL_SPAN;
  int LEADING_MARGIN_SPAN;
  int QUOTE_SPAN;
  int BULLET_SPAN;
  int STYLE_SPAN;
  int UNDERLINE_SPAN;
  int STRIKETHROUGH_SPAN;
  int SCALE_X_SPAN;
  int RELATIVE_SIZE_SPAN;
  int FOREGROUND_COLOR_SPAN;
  int ALIGNMENT_SPAN;
  class Reverser {
    int mEnd;
    int mStart;
    int mSource;
  }
  class SimpleStringSplitter {
    int mLength;
    int mPosition;
    int mDelimiter;
    int mString;
  }
  class StringSplitter {
  }
}
class TextPaint {
  int underlineThickness;
  int underlineColor;
  int density;
  int drawableState;
  int linkColor;
  int baselineShift;
  int bgColor;
}
class TextLine {
  int TAB_INCREMENT;
  int sCached;
  int mReplacementSpanSpanSet;
  int mCharacterStyleSpanSet;
  int mMetricAffectingSpanSpanSet;
  int mWorkPaint;
  int mSpanned;
  int mCharsValid;
  int mChars;
  int mTabs;
  int mHasTabs;
  int mDirections;
  int mDir;
  int mLen;
  int mStart;
  int mText;
  int mPaint;
  int DEBUG;
}
class TextLayoutTest {
  int mPaint;
  int mString;
}
class TextDirectionHeuristics {
  class TextDirectionHeuristicLocale {
    int INSTANCE;
  }
  class AnyStrong {
    int INSTANCE_LTR;
    int INSTANCE_RTL;
    int mLookForRtl;
  }
  class FirstStrong {
    int INSTANCE;
  }
  class TextDirectionAlgorithm {
  }
  class TextDirectionHeuristicInternal {
    int mDefaultIsRtl;
  }
  class TextDirectionHeuristicImpl {
    int mAlgorithm;
  }
  class TriState {
    int UNKNOWN;
    int FALSE;
    int TRUE;
  }
  int LOCALE;
  int ANYRTL_LTR;
  int FIRSTSTRONG_RTL;
  int FIRSTSTRONG_LTR;
  int RTL;
  int LTR;
}
class TextDirectionHeuristic {
}
class StaticLayoutTest {
  class LayoutBuilder {
    int includePad;
    int spacingAdd;
    int spacingMult;
    int align;
    int width;
    int paint;
    int text;
  }
  class Scaler {
    int sAdd;
    int sMult;
  }
}
class StaticLayoutDirectionsTest {
  int expected;
  int texts;
  int LVL2_2;
  int LVL2_1;
  int LVL1_1;
  int DIRS_ALL_RIGHT_TO_LEFT;
  int DIRS_ALL_LEFT_TO_RIGHT;
  int RUN_RTL_FLAG;
  int RUN_LEVEL_MASK;
  int RUN_LEVEL_SHIFT;
  int RUN_LENGTH_MASK;
  int ALEF;
}
class StaticLayoutBidiTest {
  int DALET;
  int GIMEL;
  int BET;
  int ALEF;
  int SP;
  int R;
  int L;
  int REQ_R;
  int REQ_L;
  int REQ_DR;
  int REQ_DL;
}
class StaticLayout {
  int mFontMetricsInt;
  int mMeasured;
  int CHAR_LAST_LOW_SURROGATE;
  int CHAR_FIRST_HIGH_SURROGATE;
  int EXTRA_ROUNDING;
  int CHAR_HYPHEN;
  int CHAR_SLASH;
  int CHAR_SEMICOLON;
  int CHAR_COLON;
  int CHAR_COMMA;
  int CHAR_DOT;
  int CHAR_SPACE;
  int CHAR_TAB;
  int CHAR_NEW_LINE;
  int CHAR_FIRST_CJK;
  int TAB_INCREMENT;
  int TAB_MASK;
  int DIR_SHIFT;
  int START_MASK;
  int mMaximumVisibleLineCount;
  int mLineDirections;
  int mLines;
  int ELLIPSIS_COUNT;
  int ELLIPSIS_START;
  int DESCENT;
  int TOP;
  int TAB;
  int DIR;
  int START;
  int COLUMNS_ELLIPSIZE;
  int COLUMNS_NORMAL;
  int mEllipsizedWidth;
  int mColumns;
  int mBottomPadding;
  int mTopPadding;
  int mLineCount;
  int TAG;
}
class SpannedTest {
  class Watcher {
    int mSequence;
  }
  int mExpect;
}
class SpannedString {
}
class Spanned {
  int SPAN_PRIORITY;
  int SPAN_PRIORITY_SHIFT;
  int SPAN_USER;
  int SPAN_USER_SHIFT;
  int SPAN_INTERMEDIATE;
  int SPAN_COMPOSING;
  int SPAN_EXCLUSIVE_INCLUSIVE;
  int SPAN_EXCLUSIVE_EXCLUSIVE;
  int SPAN_INCLUSIVE_INCLUSIVE;
  int SPAN_INCLUSIVE_EXCLUSIVE;
  int SPAN_PARAGRAPH;
  int SPAN_POINT_POINT;
  int SPAN_POINT_MARK;
  int SPAN_MARK_POINT;
  int SPAN_MARK_MARK;
  int SPAN_POINT_MARK_MASK;
}
class SpannableTest {
}
class SpannableStringTest {
}
class SpannableStringInternal {
  int COLUMNS;
  int FLAGS;
  int END;
  int START;
  int EMPTY;
  int mSpanCount;
  int mSpanData;
  int mSpans;
  int mText;
}
class SpannableStringBuilderTest {
}
class SpannableStringBuilder {
  int SPAN_START_END_MASK;
  int SPAN_END_AT_END;
  int SPAN_END_AT_START;
  int SPAN_START_AT_END;
  int SPAN_START_AT_START;
  int START_SHIFT;
  int END_MASK;
  int START_MASK;
  int PARAGRAPH;
  int POINT;
  int MARK;
  int mSpanCountBeforeAdd;
  int mSpanCount;
  int mSpanFlags;
  int mSpanEnds;
  int mSpanStarts;
  int mSpans;
  int mGapLength;
  int mGapStart;
  int mText;
  int mFilters;
  int NO_FILTERS;
}
class SpannableString {
}
class Spannable {
  class Factory {
    int sInstance;
  }
}
class SpanWatcher {
}
class SpanSet {
  int spanFlags;
  int spanEnds;
  int spanStarts;
  int spans;
  int numberOfSpans;
  int classType;
}
class Selection {
  int SELECTION_END;
  int SELECTION_START;
  class END {
  }
  class START {
  }
  class PositionIterator {
    int DONE;
  }
}
class ParcelableSpan {
}
class PackedObjectVector {
  int mValues;
  int mRowGapLength;
  int mRowGapStart;
  int mRows;
  int mColumns;
}
class PackedIntVectorTest {
}
class PackedIntVector {
  int mValueGap;
  int mValues;
  int mRowGapLength;
  int mRowGapStart;
  int mRows;
  int mColumns;
}
class NoCopySpan {
  class Concrete {
  }
}
class MeasuredText {
  int sCached;
  int sLock;
  int mWorkPaint;
  int mPos;
  int mLen;
  int mEasy;
  int mDir;
  int mLevels;
  int mChars;
  int mWidths;
  int mTextStart;
  int mText;
  int localLOGV;
}
class LoginFilter {
  class PasswordFilterGMail {
  }
  class UsernameFilterGeneric {
    int mAllowed;
  }
  class UsernameFilterGMail {
  }
  int mAppendInvalid;
}
class Layout {
  int ELLIPSIS_TWO_DOTS;
  int ELLIPSIS_NORMAL;
  int DIRS_ALL_RIGHT_TO_LEFT;
  int DIRS_ALL_LEFT_TO_RIGHT;
  int TAB_INCREMENT;
  class Alignment {
    int ALIGN_RIGHT;
    int ALIGN_LEFT;
    int ALIGN_CENTER;
    int ALIGN_OPPOSITE;
    int ALIGN_NORMAL;
  }
  int RUN_RTL_FLAG;
  int RUN_LEVEL_MASK;
  int RUN_LEVEL_SHIFT;
  int RUN_LENGTH_MASK;
  int DIR_REQUEST_DEFAULT_RTL;
  int DIR_REQUEST_DEFAULT_LTR;
  int DIR_REQUEST_RTL;
  int DIR_REQUEST_LTR;
  int DIR_RIGHT_TO_LEFT;
  int DIR_LEFT_TO_RIGHT;
  int mLineBackgroundSpans;
  int mTextDir;
  int mSpannedText;
  int sTempRect;
  int mSpacingAdd;
  int mSpacingMult;
  int mAlignment;
  int mWidth;
  int mWorkPaint;
  int mPaint;
  int mText;
  class SpannedEllipsizer {
    int mSpanned;
  }
  class Ellipsizer {
    int mMethod;
    int mWidth;
    int mLayout;
    int mText;
  }
  class Directions {
    int mDirections;
  }
  class TabStops {
    int mIncrement;
    int mNumStops;
    int mStops;
  }
  int MAX_EMOJI;
  int MIN_EMOJI;
  int EMOJI_FACTORY;
  int NO_PARA_SPANS;
}
class InputType {
  int TYPE_DATETIME_VARIATION_TIME;
  int TYPE_DATETIME_VARIATION_DATE;
  int TYPE_DATETIME_VARIATION_NORMAL;
  int TYPE_CLASS_DATETIME;
  int TYPE_CLASS_PHONE;
  int TYPE_NUMBER_VARIATION_PASSWORD;
  int TYPE_NUMBER_VARIATION_NORMAL;
  int TYPE_NUMBER_FLAG_DECIMAL;
  int TYPE_NUMBER_FLAG_SIGNED;
  int TYPE_CLASS_NUMBER;
  int TYPE_TEXT_VARIATION_WEB_PASSWORD;
  int TYPE_TEXT_VARIATION_WEB_EMAIL_ADDRESS;
  int TYPE_TEXT_VARIATION_PHONETIC;
  int TYPE_TEXT_VARIATION_FILTER;
  int TYPE_TEXT_VARIATION_WEB_EDIT_TEXT;
  int TYPE_TEXT_VARIATION_VISIBLE_PASSWORD;
  int TYPE_TEXT_VARIATION_PASSWORD;
  int TYPE_TEXT_VARIATION_POSTAL_ADDRESS;
  int TYPE_TEXT_VARIATION_PERSON_NAME;
  int TYPE_TEXT_VARIATION_LONG_MESSAGE;
  int TYPE_TEXT_VARIATION_SHORT_MESSAGE;
  int TYPE_TEXT_VARIATION_EMAIL_SUBJECT;
  int TYPE_TEXT_VARIATION_EMAIL_ADDRESS;
  int TYPE_TEXT_VARIATION_URI;
  int TYPE_TEXT_VARIATION_NORMAL;
  int TYPE_TEXT_FLAG_NO_SUGGESTIONS;
  int TYPE_TEXT_FLAG_IME_MULTI_LINE;
  int TYPE_TEXT_FLAG_MULTI_LINE;
  int TYPE_TEXT_FLAG_AUTO_COMPLETE;
  int TYPE_TEXT_FLAG_AUTO_CORRECT;
  int TYPE_TEXT_FLAG_CAP_SENTENCES;
  int TYPE_TEXT_FLAG_CAP_WORDS;
  int TYPE_TEXT_FLAG_CAP_CHARACTERS;
  int TYPE_CLASS_TEXT;
  int TYPE_NULL;
  int TYPE_MASK_FLAGS;
  int TYPE_MASK_VARIATION;
  int TYPE_MASK_CLASS;
}
class InputFilter {
  class LengthFilter {
    int mMax;
  }
  class AllCaps {
  }
}
class HtmlTest {
}
class HtmlToSpannedConverter {
  int COLORS;
  class Header {
    int mLevel;
  }
  class Href {
    int mHref;
  }
  class Font {
    int mFace;
    int mColor;
  }
  class Sub {
  }
  class Super {
  }
  class Blockquote {
  }
  class Monospace {
  }
  class Small {
  }
  class Big {
  }
  class Underline {
  }
  class Italic {
  }
  class Bold {
  }
  int mTagHandler;
  int mImageGetter;
  int mSpannableStringBuilder;
  int mReader;
  int mSource;
  int HEADER_SIZES;
}
class Html {
  class HtmlParser {
    int schema;
  }
  class TagHandler {
  }
  class ImageGetter {
  }
}
class GraphicsOperations {
}
class GetChars {
}
class Editable {
  class Factory {
    int sInstance;
  }
}
class DynamicLayoutBlocksTest {
  int initialBlockIndices;
  int initialBlockEnds;
  int ___;
  int dl;
}
class DynamicLayout {
  int ELLIPSIS_UNDEFINED;
  int TAB_MASK;
  int DIR_SHIFT;
  int START_MASK;
  int COLUMNS_ELLIPSIZE;
  int ELLIPSIS_COUNT;
  int ELLIPSIS_START;
  int COLUMNS_NORMAL;
  int DESCENT;
  int TOP;
  int TAB;
  int DIR;
  int START;
  int sLock;
  int sStaticLayout;
  int mBottomPadding;
  int mTopPadding;
  int mNumberOfBlocks;
  int mBlockIndices;
  int mBlockEndLines;
  int INVALID_BLOCK_INDEX;
  int mObjects;
  int mInts;
  int mEllipsizeAt;
  int mEllipsizedWidth;
  int mEllipsize;
  int mIncludePad;
  int mWatcher;
  int mDisplay;
  int mBase;
  class ChangeWatcher {
    int mLayout;
  }
  int BLOCK_MINIMUM_CHARACTER_LENGTH;
  int PRIORITY;
}
class ClipboardManager {
}
class BoringLayout {
  class Metrics {
    int width;
  }
  int sTemp;
  int mEllipsizedCount;
  int mEllipsizedStart;
  int mEllipsizedWidth;
  int mMax;
  int mBottomPadding;
  int mTopPadding;
  int mDesc;
  int mBottom;
  int mPaint;
  int mDirect;
  int FIRST_RIGHT_TO_LEFT;
}
class AutoText {
  int mSize;
  int mLocale;
  int mText;
  int mTrieUsed;
  int mTrie;
  int sLock;
  int sInstance;
  int RIGHT;
  int DEFAULT;
  int INCREMENT;
  int TRIE_ROOT;
  int TRIE_NULL;
  int TRIE_SIZEOF;
  int TRIE_NEXT;
  int TRIE_CHILD;
  int TRIE_OFF;
  int TRIE_C;
}
class Annotation {
  int mValue;
  int mKey;
}
class AndroidCharacter {
  int EAST_ASIAN_WIDTH_WIDE;
  int EAST_ASIAN_WIDTH_NARROW;
  int EAST_ASIAN_WIDTH_FULL_WIDTH;
  int EAST_ASIAN_WIDTH_HALF_WIDTH;
  int EAST_ASIAN_WIDTH_AMBIGUOUS;
  int EAST_ASIAN_WIDTH_NEUTRAL;
}
class AndroidBidi_Delegate {
}
class AndroidBidi {
}
class AlteredCharSequence {
  int mSource;
  int mChars;
  int mEnd;
  int mStart;
  class AlteredSpanned {
    int mSpanned;
  }
}
