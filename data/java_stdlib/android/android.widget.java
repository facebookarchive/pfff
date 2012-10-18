package android.widget;
class ZoomControls {
  int mZoomOut;
  int mZoomIn;
}
class ZoomButtonsController {
  class Container {
  }
  class OnZoomListener {
  }
  int mHandler;
  int MSG_POST_SET_VISIBLE;
  int MSG_DISMISS_ZOOM_CONTROLS;
  int MSG_POST_CONFIGURATION_CHANGED;
  int mConfigurationChangedReceiver;
  int mConfigurationChangedFilter;
  int mPostedVisibleInitializer;
  int mCallback;
  int mTempIntArray;
  int mTempRect;
  int mIsVisible;
  int mReleaseTouchListenerOnUp;
  int mTouchTargetWindowLocation;
  int mTouchTargetView;
  int mControls;
  int mContainerRawLocation;
  int mContainerLayoutParams;
  int mContainer;
  int mOwnerViewRawLocation;
  int mOwnerView;
  int mAutoDismissControls;
  int mWindowManager;
  int mContext;
  int mTouchPaddingScaledSq;
  int ZOOM_CONTROLS_TOUCH_PADDING;
  int ZOOM_CONTROLS_TIMEOUT;
  int TAG;
}
class ZoomButton {
  int mIsInLongpress;
  int mZoomSpeed;
  int mRunnable;
  int mHandler;
}
class WrapperListAdapter {
}
class ViewSwitcher {
  class ViewFactory {
  }
  int mFactory;
}
class ViewFlipper {
  int mHandler;
  int FLIP_MSG;
  int mReceiver;
  int mUserPresent;
  int mVisible;
  int mStarted;
  int mRunning;
  int mAutoStart;
  int mFlipInterval;
  int DEFAULT_INTERVAL;
  int LOGD;
  int TAG;
}
class ViewAnimator {
  int mOutAnimation;
  int mInAnimation;
  int mAnimateFirstTime;
  int mFirstTime;
  int mWhichChild;
}
class VideoView {
  int mSHCallback;
  int mBufferingUpdateListener;
  int mErrorListener;
  int mCompletionListener;
  int mPreparedListener;
  int mSizeChangedListener;
  int mCanSeekForward;
  int mCanSeekBack;
  int mCanPause;
  int mSeekWhenPrepared;
  int mOnErrorListener;
  int mCurrentBufferPercentage;
  int mOnPreparedListener;
  int mOnCompletionListener;
  int mMediaController;
  int mSurfaceHeight;
  int mSurfaceWidth;
  int mVideoHeight;
  int mVideoWidth;
  int mMediaPlayer;
  int mSurfaceHolder;
  int mTargetState;
  int mCurrentState;
  int STATE_PLAYBACK_COMPLETED;
  int STATE_PAUSED;
  int STATE_PLAYING;
  int STATE_PREPARED;
  int STATE_PREPARING;
  int STATE_IDLE;
  int STATE_ERROR;
  int mDuration;
  int mHeaders;
  int mUri;
  int TAG;
}
class TwoLineListItem {
  int mText2;
  int mText1;
}
class ToggleButton {
  int mDisabledAlpha;
  int NO_ALPHA;
  int mIndicatorDrawable;
  int mTextOff;
  int mTextOn;
}
class Toast {
  class TN {
    int mWM;
    int mNextView;
    int mView;
    int mVerticalMargin;
    int mHorizontalMargin;
    int mY;
    int mX;
    int mGravity;
    int mHandler;
    int mParams;
    int mHide;
    int mShow;
  }
  int sService;
  int mNextView;
  int mDuration;
  int mTN;
  int mContext;
  int LENGTH_LONG;
  int LENGTH_SHORT;
  int localLOGV;
  int TAG;
}
class TimePicker {
  class SavedState {
    int CREATOR;
    int mMinute;
    int mHour;
  }
  class OnTimeChangedListener {
  }
  int mCurrentLocale;
  int mTempCalendar;
  int mOnTimeChangedListener;
  int mIsEnabled;
  int mAmPmStrings;
  int mAmPmButton;
  int mDivider;
  int mAmPmSpinnerInput;
  int mMinuteSpinnerInput;
  int mHourSpinnerInput;
  int mAmPmSpinner;
  int mMinuteSpinner;
  int mHourSpinner;
  int mIsAm;
  int mIs24HourView;
  int NO_OP_CHANGE_LISTENER;
  int HOURS_IN_HALF_DAY;
  int DEFAULT_ENABLED_STATE;
}
class TextViewWordLimitsTest {
  int mSelectionControllerEnabled;
  int mContextMenuTriggeredByKey;
  int mSelectCurrentWord;
  int mGetWordLimits;
  int mTv;
}
class TextViewTest {
}
class TextViewPerformanceTest {
  class PerformanceLabelView {
  }
  class PerformanceTextView {
  }
  int mLabelView;
  int mPaint;
  int mTextView;
  int mCanvas;
  int mString;
}
class TextView {
  class ChangeWatcher {
    int mBeforeText;
  }
  class Marquee {
    int mScroll;
    int mRepeatLimit;
    int mFadeStop;
    int mGhostOffset;
    int mGhostStart;
    int mMaxFadeScroll;
    int mMaxScroll;
    int mScrollUnit;
    int mStatus;
    int mView;
    int MESSAGE_RESTART;
    int MESSAGE_TICK;
    int MESSAGE_START;
    int MARQUEE_RUNNING;
    int MARQUEE_STARTING;
    int MARQUEE_STOPPED;
    int MARQUEE_PIXELS_PER_SECOND;
    int MARQUEE_RESOLUTION;
    int MARQUEE_RESTART_DELAY;
    int MARQUEE_DELAY;
    int MARQUEE_DELTA_MAX;
  }
  class CharWrapper {
    int mLength;
    int mStart;
    int mChars;
  }
  class SavedState {
    int CREATOR;
    int error;
    int frozenWithFocus;
    int text;
    int selEnd;
    int selStart;
  }
  int ID_PASTE;
  int ID_COPY;
  int ID_CUT;
  int ID_SELECT_ALL;
  class BufferType {
    int EDITABLE;
    int SPANNABLE;
    int NORMAL;
  }
  int UNKNOWN_BORING;
  class OnEditorActionListener {
  }
  int mEditor;
  int mTextEditSuggestionItemLayout;
  int mTextSelectHandleRes;
  int mTextSelectHandleRightRes;
  int mTextSelectHandleLeftRes;
  int mCursorDrawableRes;
  int mHighlightPathBogus;
  int mHighlightPaint;
  int mHighlightPath;
  int mHighlightColor;
  int mFilters;
  int mTextDir;
  int mSavedHintLayout;
  int mSavedLayout;
  int mHintBoring;
  int mBoring;
  int mScroller;
  int mLastScroll;
  int mTempRect;
  int mIncludePad;
  int mDesiredHeightAtMeasure;
  int mSingleLine;
  int mMinWidthMode;
  int mMinWidth;
  int mMaxWidthMode;
  int mMaxWidth;
  int mOldMaxMode;
  int mOldMaximum;
  int mMinMode;
  int mMinimum;
  int mMaxMode;
  int mMaximum;
  int mSpacingAdd;
  int mSpacingMult;
  int mLinksClickable;
  int mAutoLinkMask;
  int mHorizontallyScrolling;
  int mGravity;
  int mLayout;
  int mUserSetTextScaleX;
  int mTextPaint;
  int mListeners;
  int mChangeWatcher;
  int mAllowTransformationLengthChange;
  int mTransformation;
  int mMovement;
  int mHintLayout;
  int mHint;
  int mBufferType;
  int mTransformed;
  int mText;
  int mSavedMarqueeModeLayout;
  int mMarqueeFadeMode;
  int mResolvedDrawables;
  int mLayoutAlignment;
  int mMarqueeRepeatLimit;
  int mRestartMarquee;
  int mMarquee;
  int mCharWrapper;
  int mDrawables;
  class Drawables {
    int mDrawablePadding;
    int mDrawableHeightEnd;
    int mDrawableHeightStart;
    int mDrawableHeightRight;
    int mDrawableHeightLeft;
    int mDrawableWidthBottom;
    int mDrawableWidthTop;
    int mDrawableSizeEnd;
    int mDrawableSizeStart;
    int mDrawableSizeRight;
    int mDrawableSizeLeft;
    int mDrawableSizeBottom;
    int mDrawableSizeTop;
    int mDrawableEnd;
    int mDrawableStart;
    int mDrawableRight;
    int mDrawableLeft;
    int mDrawableBottom;
    int mDrawableTop;
    int mCompoundRect;
  }
  int mEllipsize;
  int mPreDrawRegistered;
  int mShadowDy;
  int mShadowDx;
  int mShadowRadius;
  int mSpannableFactory;
  int mEditableFactory;
  int mDispatchTemporaryDetach;
  int mTemporaryDetach;
  int mFreezesText;
  int mCurHintTextColor;
  int mCurTextColor;
  int mLinkTextColor;
  int mHintTextColor;
  int mTextColor;
  int LAST_CUT_OR_COPY_TIME;
  int MULTILINE_STATE_SET;
  int CHANGE_WATCHER_PRIORITY;
  int EMPTY_SPANNED;
  int NO_FILTERS;
  int ANIMATED_SCROLL_GAP;
  int VERY_WIDE;
  int TEMP_RECTF;
  int PIXELS;
  int EMS;
  int LINES;
  int MARQUEE_FADE_SWITCH_SHOW_FADE;
  int MARQUEE_FADE_SWITCH_SHOW_ELLIPSIS;
  int MARQUEE_FADE_NORMAL;
  int DECIMAL;
  int SIGNED;
  int MONOSPACE;
  int SERIF;
  int SANS;
  int DEBUG_EXTRACT;
  int LOG_TAG;
}
class TextSwitcher {
}
class TableRow {
  class ChildrenTracker {
    int listener;
  }
  class LayoutParams {
    int mOffset;
    int LOCATION_NEXT;
    int LOCATION;
    int span;
    int column;
  }
  int mChildrenTracker;
  int mColumnToChildIndex;
  int mConstrainedColumnWidths;
  int mColumnWidths;
  int mNumColumns;
}
class TableLayout {
  class PassThroughHierarchyChangeListener {
    int mOnHierarchyChangeListener;
  }
  class LayoutParams {
  }
  int mInitialized;
  int mPassThroughListener;
  int mStretchAllColumns;
  int mShrinkAllColumns;
  int mCollapsedColumns;
  int mShrinkableColumns;
  int mStretchableColumns;
  int mMaxWidths;
}
class TabWidget {
  class OnTabSelectionChanged {
  }
  class TabClickListener {
    int mTabIndex;
  }
  int mImposedTabWidths;
  int mImposedTabsHeight;
  int mBounds;
  int mStripMoved;
  int mDrawBottomStrips;
  int mRightStrip;
  int mLeftStrip;
  int mSelectedTab;
  int mSelectionChangedListener;
}
class TabHost {
  class IntentContentStrategy {
    int mLaunchedView;
    int mIntent;
    int mTag;
  }
  class FactoryContentStrategy {
    int mFactory;
    int mTag;
    int mTabContent;
  }
  class ViewIdContentStrategy {
    int mView;
  }
  class ViewIndicatorStrategy {
    int mView;
  }
  class LabelAndIconIndicatorStrategy {
    int mIcon;
    int mLabel;
  }
  class LabelIndicatorStrategy {
    int mLabel;
  }
  class ContentStrategy {
  }
  class IndicatorStrategy {
  }
  class TabSpec {
    int mContentStrategy;
    int mIndicatorStrategy;
    int mTag;
  }
  class TabContentFactory {
  }
  class OnTabChangeListener {
  }
  int mTabLayoutId;
  int mTabKeyListener;
  int mOnTabChangeListener;
  int mLocalActivityManager;
  int mCurrentView;
  int mCurrentTab;
  int mTabSpecs;
  int mTabContent;
  int mTabWidget;
}
class Switch {
  int CHECKED_STATE_SET;
  int mTempRect;
  int mSwitchTransformationMethod;
  int mOffLayout;
  int mOnLayout;
  int mTextColors;
  int mTextPaint;
  int mSwitchBottom;
  int mSwitchRight;
  int mSwitchTop;
  int mSwitchLeft;
  int mThumbWidth;
  int mSwitchHeight;
  int mSwitchWidth;
  int mThumbPosition;
  int mMinFlingVelocity;
  int mVelocityTracker;
  int mTouchY;
  int mTouchX;
  int mTouchSlop;
  int mTouchMode;
  int mTextOff;
  int mTextOn;
  int mSwitchPadding;
  int mSwitchMinWidth;
  int mThumbTextPadding;
  int mTrackDrawable;
  int mThumbDrawable;
  int MONOSPACE;
  int SERIF;
  int SANS;
  int TOUCH_MODE_DRAGGING;
  int TOUCH_MODE_DOWN;
  int TOUCH_MODE_IDLE;
}
class SuggestionsAdapter {
  class ChildViewCache {
    int mIconRefine;
    int mIcon2;
    int mIcon1;
    int mText2;
    int mText1;
  }
  int DELETE_KEY_POST_DELAY;
  int mFlagsCol;
  int mIconName2Col;
  int mIconName1Col;
  int mText2UrlCol;
  int mText2Col;
  int mText1Col;
  int INVALID_INDEX;
  int mUrlColor;
  int mQueryRefinement;
  int mClosed;
  int mOutsideDrawablesCache;
  int mProviderContext;
  int mSearchable;
  int mSearchView;
  int mSearchManager;
  int REFINE_ALL;
  int REFINE_BY_ENTRY;
  int REFINE_NONE;
  int QUERY_LIMIT;
  int LOG_TAG;
  int DBG;
}
class StackView {
  class HolographicHelper {
    int mIdentityMatrix;
    int mTmpXY;
    int mMaskCanvas;
    int mCanvas;
    int mLargeBlurMaskFilter;
    int mSmallBlurMaskFilter;
    int mDensity;
    int CLICK_FEEDBACK;
    int RES_OUT;
    int mBlurPaint;
    int mErasePaint;
    int mHolographicPaint;
  }
  class LayoutParams {
    int globalInvalidateRect;
    int invalidateRectf;
    int invalidateRect;
    int parentRect;
    int mView;
    int verticalOffset;
    int horizontalOffset;
  }
  class StackSlider {
    int mMode;
    int END_OF_STACK_MODE;
    int BEGINNING_OF_STACK_MODE;
    int NORMAL_MODE;
    int mXProgress;
    int mYProgress;
    int mView;
  }
  class StackFrame {
    int sliderAnimator;
    int transformAnimator;
  }
  int stackInvalidateRect;
  int mFramePadding;
  int mStackMode;
  int mLastScrollTime;
  int mLastInteractionTime;
  int mFirstLayoutHappened;
  int mStackSlider;
  int mClickFeedbackIsValid;
  int mClickFeedback;
  int mHighlight;
  int sHolographicHelper;
  int mClickColor;
  int mResOutColor;
  int mTransitionIsSetup;
  int mVelocityTracker;
  int mMaximumVelocity;
  int mTouchSlop;
  int mSwipeThreshold;
  int mSlideAmount;
  int mSwipeGestureType;
  int mYVelocity;
  int mActivePointerId;
  int mInitialX;
  int mInitialY;
  int MIN_TIME_BETWEEN_SCROLLS;
  int MIN_TIME_BETWEEN_INTERACTION_AND_AUTOADVANCE;
  int mTouchRect;
  int FRAME_PADDING;
  int NUM_ACTIVE_VIEWS;
  int INVALID_POINTER;
  int SLIDE_UP_RATIO;
  int SWIPE_THRESHOLD_RATIO;
  int GESTURE_SLIDE_DOWN;
  int GESTURE_SLIDE_UP;
  int GESTURE_NONE;
  int ITEMS_SLIDE_DOWN;
  int ITEMS_SLIDE_UP;
  int PERSPECTIVE_SCALE_FACTOR;
  int mNewPerspectiveShiftY;
  int mNewPerspectiveShiftX;
  int mPerspectiveShiftY;
  int mPerspectiveShiftX;
  int PERSPECTIVE_SHIFT_FACTOR_X;
  int PERSPECTIVE_SHIFT_FACTOR_Y;
  int STACK_RELAYOUT_DURATION;
  int MINIMUM_ANIMATION_DURATION;
  int DEFAULT_ANIMATION_DURATION;
  int TAG;
}
class SpinnerAdapter {
}
class Spinner {
  class DropdownPopup {
    int mAdapter;
    int mHintText;
  }
  class DialogPopup {
    int mPrompt;
    int mListAdapter;
    int mPopup;
  }
  class SpinnerPopup {
  }
  class DropDownAdapter {
    int mListAdapter;
    int mAdapter;
  }
  int mTempRect;
  int mDisableChildrenWhenDisabled;
  int mGravity;
  int mDropDownWidth;
  int mTempAdapter;
  int mPopup;
  int MODE_THEME;
  int MODE_DROPDOWN;
  int MODE_DIALOG;
  int MAX_ITEMS_MEASURED;
  int TAG;
}
class SpellChecker {
  class SpellParser {
    int mRange;
  }
  int mSuggestionSpanCache;
  int SUGGESTION_SPAN_CACHE_SIZE;
  int mSpellRunnable;
  int mTextServicesManager;
  int mWordIterator;
  int mCurrentLocale;
  int mSpanSequenceCounter;
  int mSpellParsers;
  int mLength;
  int mSpellCheckSpans;
  int mIds;
  int mCookie;
  int mIsSentenceSpellCheckSupported;
  int mSpellCheckerSession;
  int mTextView;
  int USE_SPAN_RANGE;
  int MIN_SENTENCE_LENGTH;
  int SPELL_PAUSE_DURATION;
  int WORD_ITERATOR_INTERVAL;
  int AVERAGE_WORD_LENGTH;
  int MAX_NUMBER_OF_WORDS;
  int DBG;
  int TAG;
}
class Space {
}
class SlidingDrawer {
  class SlidingHandler {
  }
  class DrawerToggler {
  }
  class OnDrawerScrollListener {
  }
  class OnDrawerCloseListener {
  }
  class OnDrawerOpenListener {
  }
  int mVelocityUnits;
  int mMaximumAcceleration;
  int mMaximumMajorVelocity;
  int mMaximumMinorVelocity;
  int mMaximumTapVelocity;
  int mTapThreshold;
  int mAnimateOnClick;
  int mAllowSingleTap;
  int mAnimating;
  int mTouchDelta;
  int mCurrentAnimationTime;
  int mAnimationLastTime;
  int mAnimationPosition;
  int mAnimatedVelocity;
  int mAnimatedAcceleration;
  int mHandler;
  int mOnDrawerScrollListener;
  int mOnDrawerCloseListener;
  int mOnDrawerOpenListener;
  int mHandleWidth;
  int mHandleHeight;
  int mTopOffset;
  int mBottomOffset;
  int mExpanded;
  int mVertical;
  int mVelocityTracker;
  int mLocked;
  int mTracking;
  int mInvalidate;
  int mFrame;
  int mContent;
  int mHandle;
  int mContentId;
  int mHandleId;
  int COLLAPSED_FULL_CLOSED;
  int EXPANDED_FULL_OPEN;
  int ANIMATION_FRAME_DURATION;
  int MSG_ANIMATE;
  int VELOCITY_UNITS;
  int MAXIMUM_ACCELERATION;
  int MAXIMUM_MAJOR_VELOCITY;
  int MAXIMUM_MINOR_VELOCITY;
  int MAXIMUM_TAP_VELOCITY;
  int TAP_THRESHOLD;
  int ORIENTATION_VERTICAL;
  int ORIENTATION_HORIZONTAL;
}
class SimpleExpandableListAdapter {
  int mInflater;
  int mChildTo;
  int mChildFrom;
  int mLastChildLayout;
  int mChildLayout;
  int mChildData;
  int mGroupTo;
  int mGroupFrom;
  int mCollapsedGroupLayout;
  int mExpandedGroupLayout;
  int mGroupData;
}
class SimpleCursorTreeAdapter {
  class ViewBinder {
  }
  int mViewBinder;
  int mChildTo;
  int mChildFrom;
  int mChildFromNames;
  int mGroupTo;
  int mGroupFrom;
  int mGroupFromNames;
}
class SimpleCursorAdapterTest {
  class TestSimpleCursorAdapter {
  }
  int mCursor2x2;
  int mData2x2;
  int mContext;
  int mLayout;
  int mTo;
  int mFrom;
}
class SimpleCursorAdapter {
  class CursorToStringConverter {
  }
  class ViewBinder {
  }
  int mOriginalFrom;
  int mViewBinder;
  int mCursorToStringConverter;
  int mStringConversionColumn;
  int mTo;
  int mFrom;
}
class SimpleAdapter {
  class SimpleFilter {
  }
  class ViewBinder {
  }
  int mUnfilteredData;
  int mFilter;
  int mInflater;
  int mDropDownResource;
  int mResource;
  int mData;
  int mViewBinder;
  int mFrom;
  int mTo;
}
class ShareActionProvider {
  class ShareAcitivityChooserModelPolicy {
  }
  class ShareMenuItemOnMenuItemClickListener {
  }
  int mOnChooseActivityListener;
  int mOnShareTargetSelectedListener;
  int mShareHistoryFileName;
  int mContext;
  int DEFAULT_SHARE_HISTORY_FILE_NAME;
  int mOnMenuItemClickListener;
  int mMaxShownActivityCount;
  int DEFAULT_INITIAL_ACTIVITY_COUNT;
  class OnShareTargetSelectedListener {
  }
}
class SeekBar {
  int mOnSeekBarChangeListener;
  class OnSeekBarChangeListener {
  }
}
class SectionIndexer {
}
class SearchView {
  class SearchAutoComplete {
    int mSearchView;
    int mThreshold;
  }
  int mTextWatcher;
  int mOnItemSelectedListener;
  int mOnItemClickListener;
  int mOnEditorActionListener;
  int mTextKeyListener;
  int mOnClickListener;
  class OnSuggestionListener {
  }
  class OnCloseListener {
  }
  class OnQueryTextListener {
  }
  int mOutsideDrawablesCache;
  int mVoiceAppSearchIntent;
  int mVoiceWebSearchIntent;
  int mReleaseCursorRunnable;
  int mUpdateDrawableStateRunnable;
  int mShowImeRunnable;
  int mAppSearchData;
  int mSearchable;
  int mCollapsedImeOptions;
  int mExpandedInActionView;
  int mUserQuery;
  int mOldQueryText;
  int mVoiceButtonEnabled;
  int mMaxWidth;
  int mClearingFocus;
  int mQueryRefinement;
  int mQueryHint;
  int mSubmitButtonEnabled;
  int mSearchHintIcon;
  int mDropDownAnchor;
  int mQueryTextView;
  int mVoiceButton;
  int mSearchEditFrame;
  int mCloseButton;
  int mSubmitArea;
  int mSearchPlate;
  int mSubmitButton;
  int mSearchButton;
  int mSuggestionsAdapter;
  int mIconified;
  int mIconifiedByDefault;
  int mOnSearchClickListener;
  int mOnSuggestionListener;
  int mOnQueryTextFocusChangeListener;
  int mOnCloseListener;
  int mOnQueryChangeListener;
  int IME_OPTION_NO_MICROPHONE;
  int LOG_TAG;
  int DBG;
}
class Scroller {
  int sViscousFluidNormalize;
  int sViscousFluidScale;
  int mPpi;
  int mDeceleration;
  int SPLINE;
  int NB_SAMPLES;
  int END_TENSION;
  int START_TENSION;
  int ALPHA;
  int DECELERATION_RATE;
  int FLING_MODE;
  int SCROLL_MODE;
  int DEFAULT_DURATION;
  int mVelocity;
  int mFlywheel;
  int mInterpolator;
  int mFinished;
  int mDeltaY;
  int mDeltaX;
  int mDurationReciprocal;
  int mDuration;
  int mStartTime;
  int mCurrY;
  int mCurrX;
  int mMaxY;
  int mMinY;
  int mMaxX;
  int mMinX;
  int mFinalY;
  int mFinalX;
  int mStartY;
  int mStartX;
  int mMode;
}
class ScrollView {
  int INVALID_POINTER;
  int mFlingStrictSpan;
  int mScrollStrictSpan;
  int mActivePointerId;
  int mOverflingDistance;
  int mOverscrollDistance;
  int mMaximumVelocity;
  int mMinimumVelocity;
  int mTouchSlop;
  int mSmoothScrollingEnabled;
  int mFillViewport;
  int mVelocityTracker;
  int mIsBeingDragged;
  int mChildToScrollTo;
  int mIsLayoutDirty;
  int mLastMotionY;
  int mEdgeGlowBottom;
  int mEdgeGlowTop;
  int mScroller;
  int mTempRect;
  int mLastScroll;
  int MAX_SCROLL_FACTOR;
  int ANIMATED_SCROLL_GAP;
}
class ScrollBarDrawable {
  int mAlwaysDrawVerticalTrack;
  int mAlwaysDrawHorizontalTrack;
  int mTempBounds;
  int mRangeChanged;
  int mChanged;
  int mVertical;
  int mExtent;
  int mOffset;
  int mRange;
  int mHorizontalThumb;
  int mVerticalThumb;
  int mHorizontalTrack;
  int mVerticalTrack;
}
class ResourceCursorTreeAdapter {
  int mInflater;
  int mLastChildLayout;
  int mChildLayout;
  int mExpandedGroupLayout;
  int mCollapsedGroupLayout;
}
class ResourceCursorAdapter {
  int mInflater;
  int mDropDownLayout;
  int mLayout;
}
class RemoteViewsService {
  class RemoteViewsFactoryAdapter {
    int mIsCreated;
    int mFactory;
  }
  class RemoteViewsFactory {
  }
  int sLock;
  int sRemoteViewFactories;
  int LOG_TAG;
}
class RemoteViewsAdapter {
  class FixedSizeRemoteViewsCache {
    int sMaxMemoryLimitInBytes;
    int sMaxCountSlackPercent;
    int mMaxCountSlack;
    int mMaxCount;
    int mPreloadUpperBound;
    int mPreloadLowerBound;
    int mLoadIndices;
    int mLastRequestedIndex;
    int mRequestedIndices;
    int mIndexRemoteViews;
    int mIndexMetaData;
    int mTemporaryMetaData;
    int mMetaData;
    int TAG;
  }
  class RemoteViewsIndexMetaData {
    int isRequested;
    int itemId;
    int typeId;
  }
  class RemoteViewsMetaData {
    int mTypeIdIndexMap;
    int mFirstViewHeight;
    int mFirstView;
    int mUserLoadingView;
    int hasStableIds;
    int viewTypeCount;
    int count;
  }
  class RemoteViewsFrameLayoutRefSet {
    int mReferences;
  }
  class RemoteViewsFrameLayout {
  }
  class RemoteViewsAdapterServiceConnection {
    int mRemoteViewsFactory;
    int mAdapter;
    int mIsConnecting;
    int mIsConnected;
  }
  class RemoteAdapterConnectionCallback {
  }
  int mMainQueue;
  int mWorkerQueue;
  int mWorkerThread;
  int mRequestedViews;
  int mNotifyDataSetChangedAfterOnServiceConnected;
  int mVisibleWindowUpperBound;
  int mVisibleWindowLowerBound;
  int mCache;
  int mCallback;
  int mServiceConnection;
  int mLayoutInflater;
  int mAppWidgetId;
  int mIntent;
  int mContext;
  int sUnbindServiceMessageType;
  int sDefaultMessageType;
  int sDefaultLoadingViewHeight;
  int sUnbindServiceDelay;
  int sDefaultCacheSize;
  int TAG;
}
class RemoteViews {
  int CREATOR;
  class MemoryUsageCounter {
    int mMemoryUsage;
  }
  class ViewPaddingAction {
    int TAG;
    int bottom;
    int right;
    int top;
    int left;
    int viewId;
  }
  class TextViewSizeAction {
    int TAG;
    int size;
    int units;
    int viewId;
  }
  class TextViewDrawableAction {
    int TAG;
    int d4;
    int d3;
    int d2;
    int d1;
    int isRelative;
    int viewId;
  }
  class ViewGroupAction {
    int TAG;
    int nestedViews;
    int viewId;
  }
  class ReflectionAction {
    int value;
    int type;
    int methodName;
    int viewId;
    int INTENT;
    int BUNDLE;
    int BITMAP;
    int URI;
    int CHAR_SEQUENCE;
    int STRING;
    int CHAR;
    int DOUBLE;
    int FLOAT;
    int LONG;
    int INT;
    int SHORT;
    int BYTE;
    int BOOLEAN;
    int TAG;
  }
  class BitmapReflectionAction {
    int TAG;
    int methodName;
    int bitmap;
    int viewId;
    int bitmapId;
  }
  class BitmapCache {
    int mBitmaps;
  }
  class ReflectionActionWithoutParams {
    int TAG;
    int methodName;
    int viewId;
  }
  class SetDrawableParameters {
    int TAG;
    int level;
    int filterMode;
    int colorFilter;
    int alpha;
    int targetBackground;
    int viewId;
  }
  class SetOnClickPendingIntent {
    int TAG;
    int pendingIntent;
    int viewId;
  }
  class SetRemoteViewsAdapterIntent {
    int TAG;
    int intent;
    int viewId;
  }
  class SetPendingIntentTemplate {
    int TAG;
    int pendingIntentTemplate;
    int viewId;
  }
  class SetOnClickFillInIntent {
    int TAG;
    int fillInIntent;
    int viewId;
  }
  class SetEmptyView {
    int TAG;
    int emptyViewId;
    int viewId;
  }
  class Action {
  }
  class OnClickHandler {
  }
  class ActionException {
  }
  int DEFAULT_ON_CLICK_HANDLER;
  int mIsWidgetCollectionChild;
  int mPortrait;
  int mLandscape;
  int MODE_HAS_LANDSCAPE_AND_PORTRAIT;
  int MODE_NORMAL;
  int mIsRoot;
  int mBitmapCache;
  int mMemoryUsageCounter;
  int mActions;
  int mLayoutId;
  int mPackage;
  int EXTRA_REMOTEADAPTER_APPWIDGET_ID;
  int LOG_TAG;
}
class RelativeLayout {
  class DependencyGraph {
    class Node {
      int mIsPooled;
      int mNext;
      int sPool;
      int POOL_LIMIT;
      int dependencies;
      int dependents;
      int view;
    }
    int mRoots;
    int mKeyNodes;
    int mNodes;
  }
  class LayoutParams {
    int alignWithParent;
    int mBottom;
    int mRight;
    int mTop;
    int mLeft;
    int mRules;
  }
  class TopToBottomLeftToRightComparator {
  }
  int mGraph;
  int mSortedVerticalChildren;
  int mSortedHorizontalChildren;
  int mDirtyHierarchy;
  int mTopToBottomLeftToRightSet;
  int mIgnoreGravity;
  int mSelfBounds;
  int mContentBounds;
  int mGravity;
  int mHasBaselineAlignedChild;
  int mBaselineView;
  int RULES_HORIZONTAL;
  int RULES_VERTICAL;
  int VERB_COUNT;
  int CENTER_VERTICAL;
  int CENTER_HORIZONTAL;
  int CENTER_IN_PARENT;
  int ALIGN_PARENT_BOTTOM;
  int ALIGN_PARENT_RIGHT;
  int ALIGN_PARENT_TOP;
  int ALIGN_PARENT_LEFT;
  int ALIGN_BOTTOM;
  int ALIGN_RIGHT;
  int ALIGN_TOP;
  int ALIGN_LEFT;
  int ALIGN_BASELINE;
  int BELOW;
  int ABOVE;
  int RIGHT_OF;
  int LEFT_OF;
  int TRUE;
  int DEBUG_GRAPH;
  int LOG_TAG;
}
class RatingBar {
  int mOnRatingBarChangeListener;
  int mProgressOnStartTracking;
  int mNumStars;
  class OnRatingBarChangeListener {
  }
}
class RadioGroupPreCheckedTest {
}
class RadioGroupActivity {
}
class RadioGroup {
  class PassThroughHierarchyChangeListener {
    int mOnHierarchyChangeListener;
  }
  class CheckedStateTracker {
  }
  class OnCheckedChangeListener {
  }
  class LayoutParams {
  }
  int mPassThroughListener;
  int mOnCheckedChangeListener;
  int mProtectFromCheckedChange;
  int mChildOnCheckedChangeListener;
  int mCheckedId;
}
class RadioButton {
}
class QuickContactBadge {
  class QueryHandler {
  }
  int PHONE_LOOKUP_STRING_COLUMN_INDEX;
  int PHONE_ID_COLUMN_INDEX;
  int PHONE_LOOKUP_PROJECTION;
  int EMAIL_LOOKUP_STRING_COLUMN_INDEX;
  int EMAIL_ID_COLUMN_INDEX;
  int EMAIL_LOOKUP_PROJECTION;
  int TOKEN_PHONE_LOOKUP_AND_TRIGGER;
  int TOKEN_EMAIL_LOOKUP_AND_TRIGGER;
  int TOKEN_PHONE_LOOKUP;
  int TOKEN_EMAIL_LOOKUP;
  int mExcludeMimes;
  int mDefaultAvatar;
  int mQueryHandler;
  int mOverlay;
  int mContactPhone;
  int mContactEmail;
  int mContactUri;
}
class ProgressBar {
  class AccessibilityEventSender {
  }
  class SavedState {
    int CREATOR;
    int secondaryProgress;
    int progress;
  }
  class RefreshData {
    int sPool;
    int POOL_MAX;
    int mIsPooled;
    int mNext;
    int fromUser;
    int progress;
    int id;
  }
  class RefreshProgressRunnable {
  }
  int mAccessibilityEventSender;
  int mRefreshData;
  int mRefreshIsPosted;
  int mAttached;
  int mInDrawing;
  int mShouldStartAnimationDrawable;
  int mUiThreadId;
  int mRefreshProgressRunnable;
  int mInterpolator;
  int mNoInvalidate;
  int mSampleTile;
  int mCurrentDrawable;
  int mProgressDrawable;
  int mIndeterminateDrawable;
  int mHasAnimation;
  int mAnimation;
  int mTransformation;
  int mOnlyIndeterminate;
  int mIndeterminate;
  int mDuration;
  int mBehavior;
  int mMax;
  int mSecondaryProgress;
  int mProgress;
  int mMaxHeight;
  int mMinHeight;
  int mMaxWidth;
  int mMinWidth;
  int TIMEOUT_SEND_ACCESSIBILITY_EVENT;
  int MAX_LEVEL;
}
class PopupWindow {
  class PopupViewContainer {
    int TAG;
  }
  class OnDismissListener {
  }
  int mAnchorYoff;
  int mAnchorXoff;
  int mOnScrollChangedListener;
  int mAnchor;
  int ABOVE_ANCHOR_STATE_SET;
  int mAnimationStyle;
  int mIgnoreCheekPress;
  int mOnDismissListener;
  int mWindowLayoutType;
  int mAboveAnchor;
  int mBelowAnchorBackgroundDrawable;
  int mAboveAnchorBackgroundDrawable;
  int mBackground;
  int mTempRect;
  int mScreenLocation;
  int mDrawingLocation;
  int mPopupHeight;
  int mPopupWidth;
  int mLastHeight;
  int mHeight;
  int mHeightMode;
  int mLastWidth;
  int mWidth;
  int mWidthMode;
  int mTouchInterceptor;
  int mNotTouchModal;
  int mLayoutInsetDecor;
  int mAllowScrollingAnchorParent;
  int mClipToScreen;
  int mLayoutInScreen;
  int mSplitTouchEnabled;
  int mClippingEnabled;
  int mOutsideTouchable;
  int mTouchable;
  int mSoftInputMode;
  int mInputMethodMode;
  int mFocusable;
  int mPopupView;
  int mContentView;
  int mIsDropdown;
  int mIsShowing;
  int mWindowManager;
  int mContext;
  int INPUT_METHOD_NOT_NEEDED;
  int INPUT_METHOD_NEEDED;
  int INPUT_METHOD_FROM_FOCUSABLE;
}
class PopupMenu {
  class OnMenuItemClickListener {
  }
  class OnDismissListener {
  }
  int mDismissListener;
  int mMenuItemClickListener;
  int mPopup;
  int mAnchor;
  int mMenu;
  int mContext;
}
class OverScroller {
  class SplineOverScroller {
    int BALLISTIC;
    int CUBIC;
    int SPLINE;
    int SPLINE_TIME;
    int SPLINE_POSITION;
    int NB_SAMPLES;
    int P2;
    int P1;
    int END_TENSION;
    int START_TENSION;
    int INFLEXION;
    int DECELERATION_RATE;
    int PHYSICAL_COEF;
    int GRAVITY;
    int mState;
    int mFlingFriction;
    int mOver;
    int mFinished;
    int mSplineDistance;
    int mSplineDuration;
    int mDuration;
    int mStartTime;
    int mDeceleration;
    int mCurrVelocity;
    int mVelocity;
    int mFinal;
    int mCurrentPosition;
    int mStart;
  }
  int FLING_MODE;
  int SCROLL_MODE;
  int DEFAULT_DURATION;
  int mFlywheel;
  int mInterpolator;
  int mScrollerY;
  int mScrollerX;
  int mMode;
}
class NumberPicker {
  class AccessibilityNodeProviderImpl {
    int mAccessibilityFocusedView;
    int mTempArray;
    int mTempRect;
    int VIRTUAL_VIEW_ID_DECREMENT;
    int VIRTUAL_VIEW_ID_INPUT;
    int VIRTUAL_VIEW_ID_INCREMENT;
    int UNDEFINED;
  }
  class BeginSoftInputOnLongPressCommand {
  }
  class CustomEditText {
  }
  class ChangeCurrentByOneFromLongPressCommand {
    int mIncrement;
  }
  class SetSelectionCommand {
    int mSelectionEnd;
    int mSelectionStart;
  }
  class PressedStateHelper {
    int mMode;
    int mManagedButton;
    int MODE_TAPPED;
    int MODE_PRESS;
    int BUTTON_DECREMENT;
    int BUTTON_INCREMENT;
  }
  class InputTextFilter {
  }
  class Formatter {
  }
  class OnScrollListener {
    int SCROLL_STATE_FLING;
    int SCROLL_STATE_TOUCH_SCROLL;
    int SCROLL_STATE_IDLE;
  }
  class OnValueChangeListener {
  }
  int mPressedStateHelper;
  int mAccessibilityNodeProvider;
  int mDecrementVirtualButtonPressed;
  int mIncrementVirtualButtonPressed;
  int mLastHoveredChildVirtualViewId;
  int mBottomSelectionDividerBottom;
  int mTopSelectionDividerTop;
  int mShowSoftInputOnTap;
  int mIngonreMoveEvents;
  int mScrollState;
  int mSelectionDividerHeight;
  int mSelectionDivider;
  int mHasSelectorWheel;
  int mSolidColor;
  int mWrapSelectorWheel;
  int mMaximumFlingVelocity;
  int mMinimumFlingVelocity;
  int mTouchSlop;
  int mVelocityTracker;
  int mLastDownOrMoveEventY;
  int mLastDownEventTime;
  int mLastDownEventY;
  int mBeginSoftInputOnLongPressCommand;
  int mChangeCurrentByOneFromLongPressCommand;
  int mSetSelectionCommand;
  int mPreviousScrollerY;
  int mAdjustScroller;
  int mFlingScroller;
  int mCurrentScrollOffset;
  int mInitialScrollOffset;
  int mSelectorElementHeight;
  int mVirtualButtonPressedDrawable;
  int mSelectorWheelPaint;
  int mSelectorIndices;
  int mSelectorIndexToStringCache;
  int mLongPressUpdateInterval;
  int mFormatter;
  int mOnScrollListener;
  int mOnValueChangeListener;
  int mValue;
  int mMaxValue;
  int mMinValue;
  int mDisplayedValues;
  int mSelectorTextGapHeight;
  int mTextSize;
  int mComputeMaxWidth;
  int mMaxWidth;
  int mMinWidth;
  int mMaxHeight;
  int mMinHeight;
  int mSelectionDividersDistance;
  int mInputText;
  int mDecrementButton;
  int mIncrementButton;
  int TWO_DIGIT_FORMATTER;
  int SIZE_UNSPECIFIED;
  int DIGIT_CHARACTERS;
  int DEFAULT_LAYOUT_RESOURCE_ID;
  int UNSCALED_DEFAULT_SELECTION_DIVIDERS_DISTANCE;
  int UNSCALED_DEFAULT_SELECTION_DIVIDER_HEIGHT;
  int TOP_AND_BOTTOM_FADING_EDGE_STRENGTH;
  int SNAP_SCROLL_DURATION;
  int SELECTOR_ADJUSTMENT_DURATION_MILLIS;
  int SELECTOR_MAX_FLING_VELOCITY_ADJUSTMENT;
  int SELECTOR_MIDDLE_ITEM_INDEX;
  int DEFAULT_LONG_PRESS_UPDATE_INTERVAL;
  int SELECTOR_WHEEL_ITEM_COUNT;
}
class MultiAutoCompleteTextView {
  class CommaTokenizer {
  }
  class Tokenizer {
  }
  int mTokenizer;
}
class MediaController {
  class MediaPlayerControl {
  }
  int mFfwdListener;
  int mRewListener;
  int mSeekListener;
  int mPauseListener;
  int mHandler;
  int mTouchListener;
  int mLayoutChangeListener;
  int mPrevButton;
  int mNextButton;
  int mRewButton;
  int mFfwdButton;
  int mPauseButton;
  int mFormatter;
  int mFormatBuilder;
  int mPrevListener;
  int mNextListener;
  int mListenersSet;
  int mFromXml;
  int mUseFastForward;
  int SHOW_PROGRESS;
  int FADE_OUT;
  int sDefaultTimeout;
  int mDragging;
  int mShowing;
  int mCurrentTime;
  int mEndTime;
  int mProgress;
  int mDecorLayoutParams;
  int mDecor;
  int mWindow;
  int mWindowManager;
  int mRoot;
  int mAnchor;
  int mContext;
  int mPlayer;
}
class ListViewTest {
  class Adapter {
  }
  class MockView {
    int onMeasureCalled;
  }
  class MockContext2 {
  }
}
class ListView {
  class ArrowScrollFocusResult {
    int mAmountToScroll;
    int mSelectedPosition;
  }
  class FocusSelector {
    int mPositionTop;
    int mPosition;
  }
  int mFocusSelector;
  int mArrowScrollFocusResult;
  int mDividerPaint;
  int mTempRect;
  int mItemsCanFocus;
  int mAreAllItemsSelectable;
  int mFooterDividersEnabled;
  int mHeaderDividersEnabled;
  int mDividerIsOpaque;
  int mIsCacheColorOpaque;
  int mOverScrollFooter;
  int mOverScrollHeader;
  int mDividerHeight;
  int mDivider;
  int mFooterViewInfos;
  int mHeaderViewInfos;
  class FixedViewInfo {
    int isSelectable;
    int data;
    int view;
  }
  int MIN_SCROLL_PREVIEW_PIXELS;
  int MAX_SCROLL_FACTOR;
  int NO_POSITION;
}
class ListPopupWindow {
  class PopupScrollListener {
  }
  class PopupTouchInterceptor {
  }
  class ResizePopupRunnable {
  }
  class ListSelectorHider {
  }
  class PopupDataSetObserver {
  }
  class DropDownListView {
    int mHijackFocus;
    int mListSelectionHidden;
    int TAG;
  }
  int INPUT_METHOD_NOT_NEEDED;
  int INPUT_METHOD_NEEDED;
  int INPUT_METHOD_FROM_FOCUSABLE;
  int WRAP_CONTENT;
  int MATCH_PARENT;
  int POSITION_PROMPT_BELOW;
  int POSITION_PROMPT_ABOVE;
  int mModal;
  int mTempRect;
  int mHandler;
  int mShowDropDownRunnable;
  int mHideSelector;
  int mScrollListener;
  int mTouchInterceptor;
  int mResizePopupRunnable;
  int mItemSelectedListener;
  int mItemClickListener;
  int mDropDownListHighlight;
  int mDropDownAnchorView;
  int mObserver;
  int mPromptPosition;
  int mPromptView;
  int mListItemExpandMaximum;
  int mForceIgnoreOutsideTouch;
  int mDropDownAlwaysVisible;
  int mDropDownVerticalOffsetSet;
  int mDropDownVerticalOffset;
  int mDropDownHorizontalOffset;
  int mDropDownWidth;
  int mDropDownHeight;
  int mDropDownList;
  int mAdapter;
  int mPopup;
  int mContext;
  int EXPAND_LIST_TIMEOUT;
  int DEBUG;
  int TAG;
}
class ListAdapter {
}
class LabelView {
  int mAscent;
  int mText;
  int mTextPaint;
}
class ImageView {
  int sS2FArray;
  class ScaleType {
    int CENTER_INSIDE;
    int CENTER_CROP;
    int CENTER;
    int FIT_END;
    int FIT_CENTER;
    int FIT_START;
    int FIT_XY;
    int MATRIX;
    int nativeInt;
  }
  int sScaleTypeArray;
  int mBaselineAlignBottom;
  int mBaseline;
  int mCropToPadding;
  int mTempDst;
  int mTempSrc;
  int mDrawMatrix;
  int mDrawableHeight;
  int mDrawableWidth;
  int mLevel;
  int mMergeState;
  int mState;
  int mDrawable;
  int mColorMod;
  int mViewAlphaScale;
  int mAlpha;
  int mColorFilter;
  int mMaxHeight;
  int mMaxWidth;
  int mAdjustViewBounds;
  int mHaveFrame;
  int mScaleType;
  int mMatrix;
  int mResource;
  int mUri;
}
class ImageSwitcher {
}
class ImageButton {
}
class HorizontalScrollView {
  int INVALID_POINTER;
  int mActivePointerId;
  int mOverflingDistance;
  int mOverscrollDistance;
  int mMaximumVelocity;
  int mMinimumVelocity;
  int mTouchSlop;
  int mSmoothScrollingEnabled;
  int mFillViewport;
  int mVelocityTracker;
  int mIsBeingDragged;
  int mChildToScrollTo;
  int mIsLayoutDirty;
  int mLastMotionX;
  int mEdgeGlowRight;
  int mEdgeGlowLeft;
  int mScroller;
  int mTempRect;
  int mLastScroll;
  int MAX_SCROLL_FACTOR;
  int ANIMATED_SCROLL_GAP;
}
class HeterogeneousExpandableList {
}
class HeaderViewListAdapter {
  int mIsFilterable;
  int mAreAllFixedViewsSelectable;
  int EMPTY_INFO_LIST;
  int mFooterViewInfos;
  int mHeaderViewInfos;
  int mAdapter;
}
class GridView {
  int mTempRect;
  int mGravity;
  int mReferenceViewInSelectedRow;
  int mReferenceView;
  int mRequestedNumColumns;
  int mRequestedColumnWidth;
  int mColumnWidth;
  int mStretchMode;
  int mVerticalSpacing;
  int mRequestedHorizontalSpacing;
  int mHorizontalSpacing;
  int mNumColumns;
  int AUTO_FIT;
  int STRETCH_SPACING_UNIFORM;
  int STRETCH_COLUMN_WIDTH;
  int STRETCH_SPACING;
  int NO_STRETCH;
}
class GridLayout {
  int CAN_STRETCH;
  int INFLEXIBLE;
  int FILL;
  int BASELINE;
  int CENTER;
  int RIGHT;
  int LEFT;
  int END;
  int START;
  int BOTTOM;
  int TOP;
  int TRAILING;
  int LEADING;
  int UNDEFINED_ALIGNMENT;
  class Alignment {
  }
  class Spec {
    int alignment;
    int span;
    int startDefined;
    int UNDEFINED;
  }
  class Interval {
    int max;
    int min;
  }
  class Bounds {
    int flexibility;
    int after;
    int before;
  }
  class PackedMap {
    int values;
    int keys;
    int index;
  }
  class Assoc {
    int valueType;
    int keyType;
  }
  class MutableInt {
    int value;
  }
  class Arc {
    int valid;
    int value;
    int span;
  }
  class LayoutParams {
    int columnSpec;
    int rowSpec;
    int GRAVITY;
    int ROW_SPAN;
    int ROW;
    int COLUMN_SPAN;
    int COLUMN;
    int BOTTOM_MARGIN;
    int RIGHT_MARGIN;
    int TOP_MARGIN;
    int LEFT_MARGIN;
    int MARGIN;
    int DEFAULT_SPAN_SIZE;
    int DEFAULT_SPAN;
    int DEFAULT_COLUMN;
    int DEFAULT_ROW;
    int DEFAULT_MARGIN;
    int DEFAULT_HEIGHT;
    int DEFAULT_WIDTH;
  }
  class Axis {
    int parentMax;
    int parentMin;
    int orderPreserved;
    int locationsValid;
    int locations;
    int arcsValid;
    int arcs;
    int trailingMarginsValid;
    int trailingMargins;
    int leadingMarginsValid;
    int leadingMargins;
    int backwardLinksValid;
    int backwardLinks;
    int forwardLinksValid;
    int forwardLinks;
    int groupBoundsValid;
    int groupBounds;
    int maxIndex;
    int definedCount;
    int horizontal;
    int COMPLETE;
    int PENDING;
    int NEW;
  }
  int lastLayoutParamsHashCode;
  int defaultGap;
  int alignmentMode;
  int useDefaultMargins;
  int orientation;
  int verticalAxis;
  int horizontalAxis;
  int COLUMN_ORDER_PRESERVED;
  int ROW_ORDER_PRESERVED;
  int ALIGNMENT_MODE;
  int USE_DEFAULT_MARGINS;
  int COLUMN_COUNT;
  int ROW_COUNT;
  int ORIENTATION;
  int DEFAULT_ALIGNMENT_MODE;
  int DEFAULT_ORDER_PRESERVED;
  int DEFAULT_USE_DEFAULT_MARGINS;
  int DEFAULT_COUNT;
  int DEFAULT_ORIENTATION;
  int UNINITIALIZED_HASH;
  int DEFAULT_CONTAINER_MARGIN;
  int MAX_SIZE;
  int TAG;
  int ALIGN_MARGINS;
  int ALIGN_BOUNDS;
  int UNDEFINED;
  int VERTICAL;
  int HORIZONTAL;
}
class Gallery {
  class LayoutParams {
  }
  class FlingRunnable {
    int mLastFlingX;
    int mScroller;
  }
  int mIsRtl;
  int mIsFirstScroll;
  int mContextMenuInfo;
  int mReceivedInvokeKeyDown;
  int mSuppressSelectionChanged;
  int mShouldCallbackOnUnselectedItemClick;
  int mShouldCallbackDuringFling;
  int mSelectedChild;
  int mShouldStopFling;
  int mDisableSuppressSelectionChangedRunnable;
  int mFlingRunnable;
  int mDownTouchView;
  int mDownTouchPosition;
  int mGestureDetector;
  int mGravity;
  int mRightMost;
  int mLeftMost;
  int mUnselectedAlpha;
  int mAnimationDuration;
  int mSpacing;
  int SCROLL_TO_FLING_UNCERTAINTY_TIMEOUT;
  int localLOGV;
  int TAG;
}
class FrameLayout {
  class LayoutParams {
    int gravity;
  }
  int mMatchParentChildren;
  int mForegroundBoundsChanged;
  int mForegroundInPadding;
  int mForegroundGravity;
  int mOverlayBounds;
  int mSelfBounds;
  int mForegroundPaddingBottom;
  int mForegroundPaddingRight;
  int mForegroundPaddingTop;
  int mForegroundPaddingLeft;
  int mForeground;
  int mMeasureAllChildren;
  int DEFAULT_CHILD_GRAVITY;
}
class Filterable {
}
class FilterQueryProvider {
}
class Filter {
  class Delayer {
  }
  class RequestArguments {
    int results;
    int listener;
    int constraint;
  }
  class ResultsHandler {
  }
  class RequestHandler {
  }
  class FilterListener {
  }
  class FilterResults {
    int count;
    int values;
  }
  int mLock;
  int mDelayer;
  int mResultHandler;
  int mThreadHandler;
  int FINISH_TOKEN;
  int FILTER_TOKEN;
  int THREAD_NAME;
  int LOG_TAG;
}
class FastScroller {
  class ScrollFade {
    int FADE_DURATION;
    int ALPHA_MAX;
    int mFadeDuration;
    int mStartTime;
  }
  int mDeferStartDrag;
  int mTmpRect;
  int PENDING_DRAG_DELAY;
  int FADE_TIMEOUT;
  int mScaledTouchSlop;
  int mPendingDrag;
  int mInitialTouchY;
  int mMatchDragPosition;
  int mOverlayPosition;
  int mAlwaysShow;
  int mPosition;
  int mChangedBounds;
  int mSectionIndexer;
  int mListAdapter;
  int mHandler;
  int mState;
  int mScrollFade;
  int mDrawOverlay;
  int mSectionText;
  int mSections;
  int mLongList;
  int mItemCount;
  int mListOffset;
  int mPaint;
  int mVisibleItem;
  int mScrollCompleted;
  int mList;
  int mOverlaySize;
  int mOverlayPos;
  int mThumbY;
  int mThumbW;
  int mThumbH;
  int mOverlayDrawableRight;
  int mOverlayDrawableLeft;
  int mTrackDrawable;
  int mOverlayDrawable;
  int mThumbDrawable;
  int OVERLAY_AT_THUMB;
  int OVERLAY_FLOATING;
  int OVERLAY_POSITION;
  int PREVIEW_BACKGROUND_RIGHT;
  int PREVIEW_BACKGROUND_LEFT;
  int TRACK_DRAWABLE;
  int THUMB_DRAWABLE;
  int TEXT_COLOR;
  int ATTRS;
  int DEFAULT_STATES;
  int PRESSED_STATES;
  int STATE_EXIT;
  int STATE_DRAGGING;
  int STATE_VISIBLE;
  int STATE_ENTER;
  int STATE_NONE;
  int MIN_PAGES;
  int TAG;
}
class ExpandableListView {
  class SavedState {
    int CREATOR;
    int expandedGroupMetadataList;
  }
  class ExpandableListContextMenuInfo {
    int id;
    int packedPosition;
    int targetView;
  }
  int mOnChildClickListener;
  class OnChildClickListener {
  }
  int mOnGroupClickListener;
  class OnGroupClickListener {
  }
  int mOnGroupExpandListener;
  class OnGroupExpandListener {
  }
  int mOnGroupCollapseListener;
  class OnGroupCollapseListener {
  }
  int mIndicatorRect;
  int mChildDivider;
  int CHILD_LAST_STATE_SET;
  int GROUP_STATE_SETS;
  int GROUP_EXPANDED_EMPTY_STATE_SET;
  int GROUP_EMPTY_STATE_SET;
  int GROUP_EXPANDED_STATE_SET;
  int EMPTY_STATE_SET;
  int mChildIndicator;
  int mGroupIndicator;
  int CHILD_INDICATOR_INHERIT;
  int mChildIndicatorRight;
  int mChildIndicatorLeft;
  int mIndicatorRight;
  int mIndicatorLeft;
  int mAdapter;
  int mConnector;
  int PACKED_POSITION_INT_MASK_GROUP;
  int PACKED_POSITION_INT_MASK_CHILD;
  int PACKED_POSITION_SHIFT_TYPE;
  int PACKED_POSITION_SHIFT_GROUP;
  int PACKED_POSITION_MASK_TYPE;
  int PACKED_POSITION_MASK_GROUP;
  int PACKED_POSITION_MASK_CHILD;
  int PACKED_POSITION_VALUE_NULL;
  int PACKED_POSITION_TYPE_NULL;
  int PACKED_POSITION_TYPE_CHILD;
  int PACKED_POSITION_TYPE_GROUP;
}
class ExpandableListPosition {
  int type;
  int flatListPos;
  int childPos;
  int groupPos;
  int GROUP;
  int CHILD;
  int sPool;
  int MAX_POOL_SIZE;
}
class ExpandableListConnector {
  class PositionMetadata {
    int groupInsertIndex;
    int groupMetadata;
    int position;
    int sPool;
    int MAX_POOL_SIZE;
  }
  class GroupMetadata {
    int CREATOR;
    int gId;
    int gPos;
    int lastChildFlPos;
    int flPos;
    int REFRESH;
  }
  class MyDataSetObserver {
  }
  int mDataSetObserver;
  int mMaxExpGroupCount;
  int mTotalExpChildrenCount;
  int mExpGroupMetadataList;
  int mExpandableListAdapter;
}
class ExpandableListAdapter {
}
class Editor {
  class InputMethodState {
    int mChangedDelta;
    int mChangedEnd;
    int mChangedStart;
    int mContentChanged;
    int mSelectionModeChanged;
    int mCursorChanged;
    int mBatchEditNesting;
    int mExtractedText;
    int mExtractedTextRequest;
    int mTmpOffset;
    int mTmpRectF;
    int mCursorRectInWindow;
  }
  class InputContentType {
    int enterDown;
    int onEditorActionListener;
    int extras;
    int imeActionId;
    int imeActionLabel;
    int privateImeOptions;
    int imeOptions;
  }
  class ErrorPopup {
    int mPopupInlineErrorAboveBackgroundId;
    int mPopupInlineErrorBackgroundId;
    int mView;
    int mAbove;
  }
  class CorrectionHighlighter {
    int FADE_OUT_DURATION;
    int mTempRectF;
    int mFadingStartTime;
    int mEnd;
    int mStart;
    int mPaint;
    int mPath;
  }
  class SelectionModifierCursorController {
    int mGestureStayedInTapRegion;
    int mDownPositionY;
    int mDownPositionX;
    int mPreviousTapUpTime;
    int mMaxTouchOffset;
    int mMinTouchOffset;
    int mEndHandle;
    int mStartHandle;
    int DELAY_BEFORE_REPLACE_ACTION;
  }
  class InsertionPointCursorController {
    int mHandle;
  }
  class CursorController {
  }
  class SelectionEndHandleView {
  }
  class SelectionStartHandleView {
  }
  class InsertionHandleView {
    int mHider;
    int mDownPositionY;
    int mDownPositionX;
    int RECENT_CUT_COPY_DURATION;
    int DELAY_BEFORE_HANDLE_FADES_OUT;
  }
  class HandleView {
    int mNumberPreviousOffsets;
    int mPreviousOffsetIndex;
    int mPreviousOffsets;
    int mPreviousOffsetsTimes;
    int TOUCH_UP_FILTER_DELAY_BEFORE;
    int TOUCH_UP_FILTER_DELAY_AFTER;
    int HISTORY_SIZE;
    int mActionPopupShower;
    int mPositionHasChanged;
    int mPreviousOffset;
    int mActionPopupWindow;
    int mLastParentY;
    int mLastParentX;
    int mIdealVerticalOffset;
    int mTouchOffsetY;
    int mHotspotX;
    int mTouchToWindowOffsetY;
    int mTouchToWindowOffsetX;
    int mIsDragging;
    int mPositionY;
    int mPositionX;
    int mContainer;
    int mDrawableRtl;
    int mDrawableLtr;
    int mDrawable;
  }
  class ActionPopupWindow {
    int mReplaceTextView;
    int mPasteTextView;
    int POPUP_TEXT_LAYOUT;
  }
  class SelectionActionModeCallback {
  }
  class SuggestionsPopupWindow {
    class SuggestionSpanComparator {
    }
    class SuggestionAdapter {
      int mInflater;
    }
    class SuggestionInfo {
      int highlightSpan;
      int text;
      int suggestionIndex;
      int suggestionSpan;
      int suggestionEnd;
      int suggestionStart;
    }
    class CustomPopupWindow {
    }
    int mSpansLengths;
    int mSuggestionSpanComparator;
    int mSuggestionsAdapter;
    int mIsShowingUp;
    int mCursorWasVisibleBeforeSuggestions;
    int mNumberOfSuggestions;
    int mSuggestionInfos;
    int DELETE_TEXT;
    int ADD_TO_DICTIONARY;
    int MAX_NUMBER_SUGGESTIONS;
  }
  class PinnedPopupWindow {
    int mPositionY;
    int mPositionX;
    int mContentView;
    int mPopupWindow;
  }
  class PositionListener {
    int mTempCoords;
    int mScrollHasChanged;
    int mNumberOfListeners;
    int mPositionY;
    int mPositionX;
    int mPositionHasChanged;
    int mCanMove;
    int mPositionListeners;
    int MAXIMUM_NUMBER_OF_LISTENERS;
  }
  class EasyEditPopupWindow {
    int mEasyEditSpan;
    int mDeleteTextView;
    int POPUP_TEXT_LAYOUT;
  }
  class EasyEditSpanController {
    int mHidePopup;
    int mPopupWindow;
    int DISPLAY_TIMEOUT_MS;
  }
  class DragLocalState {
    int end;
    int start;
    int sourceTextView;
  }
  class Blink {
    int mCancelled;
  }
  int EXTRACT_UNKNOWN;
  int EXTRACT_NOTHING;
  class TextViewPositionListener {
  }
  int mTextView;
  int mTempRect;
  int mSpellChecker;
  int mWordIterator;
  int mEasyEditSpanController;
  int mCreatedWithASelection;
  int mCustomSelectionActionModeCallback;
  int mLastDownPositionY;
  int mLastDownPositionX;
  int mPositionListener;
  int mSelectHandleCenter;
  int mSelectHandleRight;
  int mSelectHandleLeft;
  int mCursorCount;
  int mCursorDrawable;
  int mShowSuggestionRunnable;
  int mSuggestionRangeSpan;
  int mSuggestionsPopupWindow;
  int mTemporaryDetach;
  int mPreserveDetachedSelection;
  int mShowSoftInputOnFocus;
  int mInBatchEditControllers;
  int mShowErrorAfterAttach;
  int mErrorPopup;
  int mErrorWasChanged;
  int mError;
  int mTextIsSelectable;
  int mSelectAllOnFocus;
  int mCursorVisible;
  int mBlink;
  int mShowCursor;
  int mIgnoreActionUpEvent;
  int mDiscardNextActionUp;
  int mInputType;
  int mKeyListener;
  int mTouchFocusSelected;
  int mSelectionMoved;
  int mFrozenWithFocus;
  int mTextDisplayLists;
  int mInputMethodState;
  int mInputContentType;
  int mCorrectionHighlighter;
  int mSelectionControllerEnabled;
  int mInsertionControllerEnabled;
  int mSelectionActionMode;
  int mSelectionModifierCursorController;
  int mInsertionPointCursorController;
  int DRAG_SHADOW_MAX_TEXT_LENGTH;
  int TEMP_POSITION;
  int BLINK;
  int TAG;
}
class EditText {
}
class EdgeEffect {
  int mMaxEffectHeight;
  int mGlowWidth;
  int mGlowHeight;
  int mEdgeHeight;
  int mBounds;
  int mPullDistance;
  int mState;
  int VELOCITY_GLOW_FACTOR;
  int VELOCITY_EDGE_FACTOR;
  int PULL_DISTANCE_ALPHA_GLOW_FACTOR;
  int PULL_DISTANCE_GLOW_FACTOR;
  int PULL_DISTANCE_EDGE_FACTOR;
  int STATE_PULL_DECAY;
  int STATE_RECEDE;
  int STATE_ABSORB;
  int STATE_PULL;
  int STATE_IDLE;
  int mInterpolator;
  int mDuration;
  int mStartTime;
  int mGlowScaleYFinish;
  int mGlowScaleYStart;
  int mGlowAlphaFinish;
  int mGlowAlphaStart;
  int mEdgeScaleYFinish;
  int mEdgeScaleYStart;
  int mEdgeAlphaFinish;
  int mEdgeAlphaStart;
  int mGlowScaleY;
  int mGlowAlpha;
  int mEdgeScaleY;
  int mEdgeAlpha;
  int mMinWidth;
  int MIN_WIDTH;
  int mY;
  int mX;
  int mHeight;
  int mWidth;
  int mGlow;
  int mEdge;
  int EPSILON;
  int MIN_VELOCITY;
  int PULL_EDGE_BEGIN;
  int PULL_GLOW_BEGIN;
  int MAX_GLOW_HEIGHT;
  int HELD_EDGE_SCALE_Y;
  int MAX_ALPHA;
  int PULL_DECAY_TIME;
  int PULL_TIME;
  int RECEDE_TIME;
  int TAG;
}
class DoubleDigitManager {
  class CallBack {
  }
  int intermediateDigit;
  int mCallBack;
  int timeoutInMillis;
}
class DigitalClock {
  class FormatChangeObserver {
  }
  int mFormat;
  int mTickerStopped;
  int mHandler;
  int mTicker;
  int mFormatChangeObserver;
  int m24;
  int m12;
  int mCalendar;
}
class DialerFilter {
  int mIsQwerty;
  int mMode;
  int mIcon;
  int mInputFilters;
  int mHint;
  int mPrimary;
  int mDigits;
  int mLetters;
  int LETTERS_ONLY;
  int DIGITS_ONLY;
  int DIGITS_AND_LETTERS_NO_LETTERS;
  int DIGITS_AND_LETTERS_NO_DIGITS;
  int DIGITS_AND_LETTERS;
}
class DateTimeView {
  int mContentObserver;
  int mBroadcastReceiver;
  int mUpdateTimeMillis;
  int mAttachedToWindow;
  int mLastFormat;
  int mLastDisplay;
  int mTimeMillis;
  int mTime;
  int SHOW_MONTH_DAY_YEAR;
  int SHOW_TIME;
  int TWENTY_FOUR_HOURS_IN_MILLIS;
  int TWELVE_HOURS_IN_MINUTES;
  int TAG;
}
class DatePicker {
  class SavedState {
    int CREATOR;
    int mDay;
    int mMonth;
    int mYear;
  }
  class OnDateChangedListener {
  }
  int mIsEnabled;
  int mCurrentDate;
  int mMaxDate;
  int mMinDate;
  int mTempDate;
  int mNumberOfMonths;
  int mDateFormat;
  int mShortMonths;
  int mOnDateChangedListener;
  int mCurrentLocale;
  int mCalendarView;
  int mYearSpinnerInput;
  int mMonthSpinnerInput;
  int mDaySpinnerInput;
  int mYearSpinner;
  int mMonthSpinner;
  int mDaySpinner;
  int mSpinners;
  int DEFAULT_ENABLED_STATE;
  int DEFAULT_SPINNERS_SHOWN;
  int DEFAULT_CALENDAR_VIEW_SHOWN;
  int DEFAULT_END_YEAR;
  int DEFAULT_START_YEAR;
  int DATE_FORMAT;
  int LOG_TAG;
}
class CursorTreeAdapter {
  class MyCursorHelper {
    class MyDataSetObserver {
    }
    class MyContentObserver {
    }
    int mDataSetObserver;
    int mContentObserver;
    int mRowIDColumn;
    int mDataValid;
    int mCursor;
  }
  int mFilterQueryProvider;
  int mCursorFilter;
  int mChildrenCursorHelpers;
  int mGroupCursorHelper;
  int mAutoRequery;
  int mHandler;
  int mContext;
}
class CursorFilter {
  class CursorFilterClient {
  }
  int mClient;
}
class CursorAdapter {
  class MyDataSetObserver {
  }
  class ChangeObserver {
  }
  int FLAG_REGISTER_CONTENT_OBSERVER;
  int FLAG_AUTO_REQUERY;
  int mFilterQueryProvider;
  int mCursorFilter;
  int mDataSetObserver;
  int mChangeObserver;
  int mRowIDColumn;
  int mContext;
  int mCursor;
  int mAutoRequery;
  int mDataValid;
}
class CompoundButton {
  class SavedState {
    int CREATOR;
    int checked;
  }
  class OnCheckedChangeListener {
  }
  int CHECKED_STATE_SET;
  int mOnCheckedChangeWidgetListener;
  int mOnCheckedChangeListener;
  int mButtonDrawable;
  int mBroadcasting;
  int mButtonResource;
  int mChecked;
}
class Chronometer {
  int mHandler;
  int TICK_WHAT;
  int mRecycle;
  int mOnChronometerTickListener;
  int mFormatBuilder;
  int mFormatterArgs;
  int mFormatterLocale;
  int mFormatter;
  int mFormat;
  int mLogged;
  int mRunning;
  int mStarted;
  int mVisible;
  int mBase;
  class OnChronometerTickListener {
  }
  int TAG;
}
class CheckedTextView {
  int CHECKED_STATE_SET;
  int mNeedRequestlayout;
  int mCheckMarkWidth;
  int mBasePadding;
  int mCheckMarkDrawable;
  int mCheckMarkResource;
  int mChecked;
}
class Checkable {
}
class CheckBox {
}
class CalendarView {
  class WeekView {
    int mSelectedRight;
    int mSelectedLeft;
    int mNumCells;
    int mSelectedDay;
    int mHasSelectedDay;
    int mHeight;
    int mWidth;
    int mWeek;
    int mLastWeekDayMonth;
    int mMonthOfFirstWeekDay;
    int mFirstDay;
    int mHasUnfocusedDay;
    int mHasFocusedDay;
    int mFocusDay;
    int mDayNumbers;
    int mMonthNumDrawPaint;
    int mDrawPaint;
    int mTempRect;
  }
  class WeeksAdapter {
    class CalendarGestureListener {
    }
    int mTotalWeekCount;
    int mSelectedDate;
    int mFocusedMonth;
    int mGestureDetector;
    int mSelectedWeek;
  }
  class ScrollStateRunnable {
    int mNewState;
    int mView;
  }
  class OnDateChangeListener {
  }
  int mCurrentLocale;
  int mDateFormat;
  int mMaxDate;
  int mMinDate;
  int mFirstDayOfMonth;
  int mTempDate;
  int mScrollStateChangedRunnable;
  int mOnDateChangeListener;
  int mCurrentScrollState;
  int mPreviousScrollState;
  int mIsScrollingUp;
  int mPreviousScrollPosition;
  int mCurrentMonthDisplayed;
  int mFirstDayOfWeek;
  int mDayLabels;
  int mDayNamesHeader;
  int mMonthName;
  int mListView;
  int mAdapter;
  int mVelocityScale;
  int mFriction;
  int mDaysPerWeek;
  int mShowWeekNumber;
  int mShownWeekCount;
  int mBottomBuffer;
  int mWeekMinVisibleHeight;
  int mListScrollTopOffset;
  int mDateTextAppearanceResId;
  int mWeekDayTextAppearanceResId;
  int mWeekNumberColor;
  int mWeekSeparatorLineColor;
  int mUnfocusedMonthDateColor;
  int mFocusedMonthDateColor;
  int mSelectedWeekBackgroundColor;
  int mSelectedDateVerticalBarWidth;
  int mSelectedDateVerticalBar;
  int mDateTextSize;
  int mWeekSeperatorLineWidth;
  int DEFAULT_WEEK_DAY_TEXT_APPEARANCE_RES_ID;
  int UNSCALED_WEEK_SEPARATOR_LINE_WIDTH;
  int UNSCALED_BOTTOM_BUFFER;
  int UNSCALED_LIST_SCROLL_TOP_OFFSET;
  int UNSCALED_WEEK_MIN_VISIBLE_HEIGHT;
  int UNSCALED_SELECTED_DATE_VERTICAL_BAR_WIDTH;
  int DEFAULT_DATE_TEXT_SIZE;
  int DEFAULT_SHOWN_WEEK_COUNT;
  int DEFAULT_MAX_DATE;
  int DEFAULT_MIN_DATE;
  int DATE_FORMAT;
  int SCROLL_CHANGE_DELAY;
  int ADJUSTMENT_SCROLL_DURATION;
  int GOTO_SCROLL_DURATION;
  int SCROLL_HYST_WEEKS;
  int MILLIS_IN_WEEK;
  int DAYS_PER_WEEK;
  int MILLIS_IN_DAY;
  int DEFAULT_SHOW_WEEK_NUMBER;
  int LOG_TAG;
}
class Button {
}
class BaseExpandableListAdapter {
  int mDataSetObservable;
}
class BaseAdapter {
  int mDataSetObservable;
}
class AutoCompleteTextViewSimple {
  int mNothingSelectedCalled;
  int mItemSelectedPosition;
  int mItemSelectedCalled;
  int mItemClickPosition;
  int mItemClickCalled;
  int mTextView;
  int LOG_TAG;
}
class AutoCompleteTextViewPopup {
  int LOOP_AMOUNT;
  int SLEEP_TIME;
}
class AutoCompleteTextViewCallbacks {
  int WAIT_TIME;
}
class AutoCompleteTextView {
  class PopupDataSetObserver {
  }
  class PassThroughClickListener {
    int mWrapped;
  }
  class Validator {
  }
  class DropDownItemClickListener {
  }
  class MyWatcher {
  }
  int mObserver;
  int mPassThroughClickListener;
  int mPopupCanBeUpdated;
  int mBlockCompletion;
  int mValidator;
  int mOpenBefore;
  int mLastKeyCode;
  int mDropDownDismissedOnCompletion;
  int mItemSelectedListener;
  int mItemClickListener;
  int mDropDownAnchorId;
  int mPopup;
  int mThreshold;
  int mFilter;
  int mAdapter;
  int mHintResource;
  int mHintView;
  int mHintText;
  int EXPAND_MAX;
  int TAG;
  int DEBUG;
}
class ArrayAdapter {
  class ArrayFilter {
  }
  int mInflater;
  int mFilter;
  int mOriginalValues;
  int mContext;
  int mNotifyOnChange;
  int mFieldId;
  int mDropDownResource;
  int mResource;
  int mLock;
  int mObjects;
}
class AppSecurityPermissions {
  class PermissionInfoComparator {
    int sCollator;
    int mPm;
  }
  int mNoPermsView;
  int mGroupLabelCache;
  int mDangerousList;
  int mNonDangerousList;
  int mCurrentState;
  int mShowMoreIcon;
  int mShowMoreText;
  int mShowMore;
  int mShowMinIcon;
  int mShowMaxIcon;
  int mExpanded;
  int mDangerousIcon;
  int mNormalIcon;
  int mPermFormat;
  int mDefaultGrpName;
  int mDefaultGrpLabel;
  int mPermsList;
  int mNormalMap;
  int mDangerousMap;
  int mPermsView;
  int mPm;
  int mInflater;
  int mContext;
  int localLOGV;
  int TAG;
  class State {
    int BOTH;
    int NORMAL_ONLY;
    int DANGEROUS_ONLY;
    int NO_PERMS;
  }
}
class AnalogClock {
  int mIntentReceiver;
  int mChanged;
  int mHour;
  int mMinutes;
  int mHandler;
  int mAttached;
  int mDialHeight;
  int mDialWidth;
  int mDial;
  int mMinuteHand;
  int mHourHand;
  int mCalendar;
}
class AlphabetIndexer {
  int mAlphabetArray;
  int mCollator;
  int mAlphaMap;
  int mAlphabetLength;
  int mAlphabet;
  int mColumnIndex;
  int mDataCursor;
}
class Advanceable {
}
class AdapterViewFlipper {
  int mHandler;
  int FLIP_MSG;
  int mReceiver;
  int mAdvancedByHost;
  int mUserPresent;
  int mVisible;
  int mStarted;
  int mRunning;
  int mAutoStart;
  int mFlipInterval;
  int DEFAULT_INTERVAL;
  int LOGD;
  int TAG;
}
class AdapterViewAnimator {
  class SavedState {
    int CREATOR;
    int whichChild;
  }
  class CheckForTap {
  }
  class ViewAndMetaData {
    int itemId;
    int adapterPosition;
    int relativeIndex;
    int view;
  }
  int DEFAULT_ANIMATION_DURATION;
  int mPendingCheckForTap;
  int TOUCH_MODE_HANDLED;
  int TOUCH_MODE_DOWN_IN_CURRENT_VIEW;
  int TOUCH_MODE_NONE;
  int mTouchMode;
  int mOutAnimation;
  int mInAnimation;
  int mReferenceChildHeight;
  int mReferenceChildWidth;
  int mLoopViews;
  int mFirstTime;
  int mDeferNotifyDataSetChanged;
  int mRemoteViewsAdapter;
  int mAdapter;
  int mDataSetObserver;
  int mCurrentWindowStartUnbounded;
  int mCurrentWindowEnd;
  int mCurrentWindowStart;
  int mPreviousViews;
  int mViewsMap;
  int mMaxNumActiveViews;
  int mActiveOffset;
  int mAnimateFirstTime;
  int mRestoreWhichChild;
  int mWhichChild;
  int TAG;
}
class AdapterView {
  class SelectionNotifier {
  }
  class AdapterDataSetObserver {
    int mInstanceState;
  }
  class AdapterContextMenuInfo {
    int id;
    int position;
    int targetView;
  }
  class OnItemSelectedListener {
  }
  class OnItemLongClickListener {
  }
  class OnItemClickListener {
  }
  int mBlockLayoutRequests;
  int mSelectionNotifier;
  int mDesiredFocusableInTouchModeState;
  int mDesiredFocusableState;
  int mOldSelectedRowId;
  int mOldSelectedPosition;
  int INVALID_ROW_ID;
  int INVALID_POSITION;
  int mOldItemCount;
  int mItemCount;
  int mEmptyView;
  int mSelectedRowId;
  int mSelectedPosition;
  int mNextSelectedRowId;
  int mNextSelectedPosition;
  int mDataChanged;
  int mOnItemLongClickListener;
  int mOnItemClickListener;
  int mOnItemSelectedListener;
  int mInLayout;
  int SYNC_MAX_DURATION_MILLIS;
  int SYNC_FIRST_POSITION;
  int SYNC_SELECTED_POSITION;
  int mLayoutHeight;
  int mSyncMode;
  int mNeedSync;
  int mSyncHeight;
  int mSyncRowId;
  int mSyncPosition;
  int mSpecificTop;
  int mFirstPosition;
  int ITEM_VIEW_TYPE_HEADER_OR_FOOTER;
  int ITEM_VIEW_TYPE_IGNORE;
}
class Adapter {
  int NO_SELECTION;
  int IGNORE_ITEM_VIEW_TYPE;
}
class ActivityChooserView {
  class ActivityChooserViewAdapter {
    int mShowFooterView;
    int mHighlightDefaultActivity;
    int mShowDefaultActivity;
    int mMaxActivityCount;
    int mDataModel;
    int ITEM_VIEW_TYPE_COUNT;
    int ITEM_VIEW_TYPE_FOOTER;
    int ITEM_VIEW_TYPE_ACTIVITY;
    int MAX_ACTIVITY_COUNT_DEFAULT;
    int MAX_ACTIVITY_COUNT_UNLIMITED;
  }
  class Callbacks {
  }
  int mDefaultActionButtonContentDescription;
  int mIsAttachedToWindow;
  int mInitialActivityCount;
  int mIsSelectingDefaultActivity;
  int mOnDismissListener;
  int mListPopupWindow;
  int mOnGlobalLayoutListener;
  int mModelDataSetOberver;
  int mProvider;
  int mListPopupMaxWidth;
  int mDefaultActivityButtonImage;
  int mDefaultActivityButton;
  int mExpandActivityOverflowButtonImage;
  int mExpandActivityOverflowButton;
  int mActivityChooserContentBackground;
  int mActivityChooserContent;
  int mCallbacks;
  int mAdapter;
}
class ActivityChooserModel {
  class DataModelPackageMonitor {
  }
  class PersistHistoryAsyncTask {
  }
  class DefaultSorter {
    int mPackageNameToActivityMap;
    int WEIGHT_DECAY_COEFFICIENT;
  }
  class ActivityResolveInfo {
    int weight;
    int resolveInfo;
  }
  class HistoricalRecord {
    int weight;
    int time;
    int activity;
  }
  int mActivityChoserModelPolicy;
  int mReloadActivities;
  int mHistoricalRecordsChanged;
  int mReadShareHistoryCalled;
  int mCanReadHistoricalData;
  int mHistoryMaxSize;
  int mActivitySorter;
  int mIntent;
  int mHistoryFileName;
  int mContext;
  int mPackageMonitor;
  int mHistoricalRecords;
  int mActivities;
  int mInstanceLock;
  int sDataModelRegistry;
  int sRegistryLock;
  int INVALID_INDEX;
  int HISTORY_FILE_EXTENSION;
  int DEFAULT_HISTORICAL_RECORD_WEIGHT;
  int DEFAULT_ACTIVITY_INFLATION;
  int DEFAULT_HISTORY_MAX_LENGTH;
  int DEFAULT_HISTORY_FILE_NAME;
  int ATTRIBUTE_WEIGHT;
  int ATTRIBUTE_TIME;
  int ATTRIBUTE_ACTIVITY;
  int TAG_HISTORICAL_RECORD;
  int TAG_HISTORICAL_RECORDS;
  int LOG_TAG;
  int DEBUG;
  class OnChooseActivityListener {
  }
  class ActivitySorter {
  }
  class ActivityChooserModelClient {
  }
}
class AccessibilityIterators {
  class PageTextSegmentIterator {
    int mTempRect;
    int mView;
    int sPageInstance;
  }
  class LineTextSegmentIterator {
    int mLayout;
    int DIRECTION_END;
    int DIRECTION_START;
    int sLineInstance;
  }
}
class AbsoluteLayout {
  class LayoutParams {
    int y;
    int x;
  }
}
class AbsSpinner {
  class RecycleBin {
    int mScrapHeap;
  }
  class SavedState {
    int CREATOR;
    int position;
    int selectedId;
  }
  int mTouchFrame;
  int mDataSetObserver;
  int mRecycler;
  int mSpinnerPadding;
  int mSelectionBottomPadding;
  int mSelectionRightPadding;
  int mSelectionTopPadding;
  int mSelectionLeftPadding;
  int mWidthMeasureSpec;
  int mHeightMeasureSpec;
  int mAdapter;
}
class AbsSeekBar {
  int mIsDragging;
  int mTouchDownX;
  int mScaledTouchSlop;
  int mDisabledAlpha;
  int NO_ALPHA;
  int mKeyProgressIncrement;
  int mIsUserSeekable;
  int mTouchProgressOffset;
  int mThumbOffset;
  int mThumb;
}
class AbsListView {
  class RecycleBin {
    int mTransientStateViews;
    int mSkippedScrap;
    int mCurrentScrap;
    int mViewTypeCount;
    int mScrapViews;
    int mActiveViews;
    int mFirstActivePosition;
    int mRecyclerListener;
  }
  class RecyclerListener {
  }
  class LayoutParams {
    int itemId;
    int scrappedFromPosition;
    int forceAdd;
    int recycledHeaderFooter;
    int viewType;
  }
  class MultiChoiceModeWrapper {
    int mWrapped;
  }
  class MultiChoiceModeListener {
  }
  class AdapterDataSetObserver {
  }
  class PositionScroller {
    int mOffsetFromTop;
    int mExtraScroll;
    int mScrollDuration;
    int mLastSeenPos;
    int mBoundPos;
    int mTargetPos;
    int mMode;
    int MOVE_OFFSET;
    int MOVE_UP_BOUND;
    int MOVE_DOWN_BOUND;
    int MOVE_UP_POS;
    int MOVE_DOWN_POS;
    int SCROLL_DURATION;
  }
  class FlingRunnable {
    int FLYWHEEL_TIMEOUT;
    int mCheckFlywheel;
    int mLastFlingY;
    int mScroller;
  }
  class CheckForTap {
  }
  class CheckForKeyLongPress {
  }
  class CheckForLongPress {
  }
  class PerformClick {
    int mClickMotionPosition;
  }
  class WindowRunnnable {
    int mOriginalAttachCount;
  }
  class ListItemAccessibilityDelegate {
  }
  class SavedState {
    int CREATOR;
    int checkIdState;
    int checkState;
    int checkedItemCount;
    int inActionMode;
    int filter;
    int height;
    int position;
    int viewTop;
    int firstId;
    int selectedId;
  }
  class SelectionBoundsAdjuster {
  }
  class OnScrollListener {
    int SCROLL_STATE_FLING;
    int SCROLL_STATE_TOUCH_SCROLL;
    int SCROLL_STATE_IDLE;
  }
  int sLinearInterpolator;
  int mLastHandledItemCount;
  int mIsAttached;
  int mLastAccessibilityScrollEventToIndex;
  int mLastAccessibilityScrollEventFromIndex;
  int mAccessibilityDelegate;
  int mGlowPaddingRight;
  int mGlowPaddingLeft;
  int mForceTranscriptScroll;
  int mDirection;
  int mLastPositionDistanceGuess;
  int mFirstPositionDistanceGuess;
  int mEdgeGlowBottom;
  int mEdgeGlowTop;
  int mOverflingDistance;
  int mOverscrollDistance;
  int INVALID_POINTER;
  int mActivePointerId;
  int mPopupHidden;
  int mIsScrap;
  int mVelocityScale;
  int mMaximumVelocity;
  int mMinimumVelocity;
  int mPositionScrollAfterLayout;
  int mClearScrollingCache;
  int mPublicInputConnection;
  int mDefInputConnection;
  int mDensityScale;
  int mTouchSlop;
  int mGlobalLayoutListenerAddedFilter;
  int mFastScroller;
  int mLastScrollState;
  int mIsChildViewEnabled;
  int mCacheColorHint;
  int mTranscriptMode;
  int mTouchModeReset;
  int mPerformClick;
  int mPendingCheckForKeyLongPress;
  int mPendingCheckForTap;
  int mPendingCheckForLongPress;
  int mFlingStrictSpan;
  int mScrollStrictSpan;
  int mFlingProfilingStarted;
  int PROFILE_FLINGING;
  int mScrollProfilingStarted;
  int PROFILE_SCROLLING;
  int mLastTouchMode;
  int TOUCH_MODE_OFF;
  int TOUCH_MODE_ON;
  int TOUCH_MODE_UNKNOWN;
  int CHECK_POSITION_SEARCH_DISTANCE;
  int OVERSCROLL_LIMIT_DIVISOR;
  int mOverscrollMax;
  int mContextMenuInfo;
  int mResurrectToPosition;
  int mTouchFrame;
  int mFiltered;
  int mTextFilterEnabled;
  int mSmoothScrollbarEnabled;
  int mTextFilter;
  int mPopup;
  int mOnScrollListener;
  int mFastScrollEnabled;
  int mScrollingCacheEnabled;
  int mStackFromBottom;
  int mSelectedTop;
  int mPositionScroller;
  int mFlingRunnable;
  int mVelocityTracker;
  int mMotionCorrection;
  int mLastY;
  int mTouchMode;
  int mMotionY;
  int mMotionX;
  int mMotionViewNewTop;
  int mMotionViewOriginalTop;
  int mMotionPosition;
  int mCachingActive;
  int mCachingStarted;
  int mScrollDown;
  int mScrollUp;
  int mWidthMeasureSpec;
  int mListPadding;
  int mSelectionBottomPadding;
  int mSelectionRightPadding;
  int mSelectionTopPadding;
  int mSelectionLeftPadding;
  int mRecycler;
  int mSelectorRect;
  int mSelectorPosition;
  int mSelector;
  int mDrawSelectorOnTop;
  int mDeferNotifyDataSetChanged;
  int mAdapterHasStableIds;
  int mRemoteAdapter;
  int mAdapter;
  int mDataSetObserver;
  int mLayoutMode;
  int mCheckedIdStates;
  int mCheckStates;
  int mCheckedItemCount;
  int mMultiChoiceModeCallback;
  int mChoiceActionMode;
  int mChoiceMode;
  int CHOICE_MODE_MULTIPLE_MODAL;
  int CHOICE_MODE_MULTIPLE;
  int CHOICE_MODE_SINGLE;
  int CHOICE_MODE_NONE;
  int LAYOUT_MOVE_SELECTION;
  int LAYOUT_SYNC;
  int LAYOUT_SPECIFIC;
  int LAYOUT_FORCE_BOTTOM;
  int LAYOUT_SET_SELECTION;
  int LAYOUT_FORCE_TOP;
  int LAYOUT_NORMAL;
  int TOUCH_MODE_OVERFLING;
  int TOUCH_MODE_OVERSCROLL;
  int TOUCH_MODE_FLING;
  int TOUCH_MODE_SCROLL;
  int TOUCH_MODE_DONE_WAITING;
  int TOUCH_MODE_TAP;
  int TOUCH_MODE_DOWN;
  int TOUCH_MODE_REST;
  int TRANSCRIPT_MODE_ALWAYS_SCROLL;
  int TRANSCRIPT_MODE_NORMAL;
  int TRANSCRIPT_MODE_DISABLED;
  int TAG;
}
