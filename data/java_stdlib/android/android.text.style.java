package android.text.style;
class WrapTogetherSpan {
}
class UpdateLayout {
}
class UpdateAppearance {
}
class UnderlineSpan {
}
class URLSpan {
  int mURL;
}
class TypefaceSpan {
  int mFamily;
}
class TextAppearanceSpan {
  int mTextColorLink;
  int mTextColor;
  int mTextSize;
  int mStyle;
  int mTypeface;
}
class TabStopSpan {
  class Standard {
    int mTab;
  }
}
class SuperscriptSpan {
}
class SuggestionSpan {
  int CREATOR;
  int mAutoCorrectionUnderlineColor;
  int mAutoCorrectionUnderlineThickness;
  int mMisspelledUnderlineColor;
  int mMisspelledUnderlineThickness;
  int mEasyCorrectUnderlineColor;
  int mEasyCorrectUnderlineThickness;
  int mHashCode;
  int mNotificationTargetClassName;
  int mLocaleString;
  int mSuggestions;
  int mFlags;
  int SUGGESTIONS_MAX_SIZE;
  int SUGGESTION_SPAN_PICKED_HASHCODE;
  int SUGGESTION_SPAN_PICKED_BEFORE;
  int SUGGESTION_SPAN_PICKED_AFTER;
  int ACTION_SUGGESTION_PICKED;
  int FLAG_AUTO_CORRECTION;
  int FLAG_MISSPELLED;
  int FLAG_EASY_CORRECT;
}
class SuggestionRangeSpan {
  int mBackgroundColor;
}
class SubscriptSpan {
}
class StyleSpan {
  int mStyle;
}
class StrikethroughSpan {
}
class SpellCheckSpan {
  int mSpellCheckInProgress;
}
class ScaleXSpan {
  int mProportion;
}
class ReplacementSpan {
}
class RelativeSizeSpan {
  int mProportion;
}
class RasterizerSpan {
  int mRasterizer;
}
class QuoteSpan {
  int mColor;
  int GAP_WIDTH;
  int STRIPE_WIDTH;
}
class ParagraphStyle {
}
class MetricAffectingSpan {
  class Passthrough {
    int mStyle;
  }
}
class MaskFilterSpan {
  int mFilter;
}
class LineHeightSpan {
  class WithDensity {
  }
}
class LineBackgroundSpan {
}
class LeadingMarginSpan {
  class Standard {
    int mRest;
    int mFirst;
  }
  class LeadingMarginSpan2 {
  }
}
class ImageSpan {
  int mSource;
  int mContext;
  int mResourceId;
  int mContentUri;
  int mDrawable;
}
class IconMarginSpan {
  int mPad;
  int mBitmap;
}
class ForegroundColorSpan {
  int mColor;
}
class EasyEditSpan {
}
class DynamicDrawableSpan {
  int mDrawableRef;
  int mVerticalAlignment;
  int ALIGN_BASELINE;
  int ALIGN_BOTTOM;
  int TAG;
}
class DrawableMarginSpan {
  int mPad;
  int mDrawable;
}
class ClickableSpan {
}
class CharacterStyle {
  class Passthrough {
    int mStyle;
  }
}
class BulletSpan {
  int STANDARD_GAP_WIDTH;
  int sBulletPath;
  int BULLET_RADIUS;
  int mColor;
  int mWantColor;
  int mGapWidth;
}
class BackgroundColorSpan {
  int mColor;
}
class AlignmentSpan {
  class Standard {
    int mAlignment;
  }
}
class AbsoluteSizeSpan {
  int mDip;
  int mSize;
}
