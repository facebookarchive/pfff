package android.webkit;
class ZoomManagerTest {
  int zoomManager;
}
class ZoomManager {
  class PostScale {
    int mInPortraitMode;
    int mInZoomOverviewBeforeSizeChange;
    int mUpdateTextWrap;
  }
  class ScaleDetectorListener {
    int mAccumulatedSpan;
  }
  class FocusMovementQueue {
    int mIndex;
    int mSize;
    int mSum;
    int mQueue;
    int QUEUE_CAPACITY;
  }
  int mInHWAcceleratedZoom;
  int mHardwareAccelerated;
  int mPinchToZoomAnimating;
  int mScaleDetector;
  int mAllowPanAndScale;
  int mSupportMultiTouch;
  int ZOOM_ANIMATION_LENGTH;
  int mZoomStart;
  int mInitialScrollY;
  int mInitialScrollX;
  int mInvFinalZoomScale;
  int mInvInitialZoomScale;
  int mZoomScale;
  int MINIMUM_SCALE_WITHOUT_JITTER;
  int MINIMUM_SCALE_INCREMENT;
  int mInitialScale;
  int mInvActualScale;
  int mActualScale;
  int MIN_DOUBLE_TAP_SCALE_INCREMENT;
  int mDoubleTapZoomFactor;
  int mDisplayDensity;
  int mInvDefaultScale;
  int mDefaultScale;
  int mTextWrapScale;
  int mAnchorY;
  int mAnchorX;
  int mFocusMovementQueue;
  int mFocusY;
  int mFocusX;
  int mZoomCenterY;
  int mZoomCenterX;
  int mInvZoomOverviewWidth;
  int mZoomOverviewWidth;
  int mInZoomOverview;
  int mInitialZoomOverview;
  int mMinZoomScaleFixed;
  int mMinZoomScale;
  int mMaxZoomScale;
  int mDefaultMinZoomScale;
  int mDefaultMaxZoomScale;
  int DEFAULT_MIN_ZOOM_SCALE_FACTOR;
  int DEFAULT_MAX_ZOOM_SCALE_FACTOR;
  int mExternalZoomControl;
  int mEmbeddedZoomControl;
  int mCallbackProxy;
  int mWebView;
  int LOGTAG;
}
class ZoomControlExternal {
  class ExtendedZoomControls {
    int mPlusMinusZoomControls;
  }
  int mWebView;
  int mPrivateHandler;
  int mZoomControlRunnable;
  int mZoomControls;
  int ZOOM_CONTROLS_TIMEOUT;
}
class ZoomControlEmbedded {
  class ZoomListener {
  }
  int mZoomButtonsController;
  int mWebView;
  int mZoomManager;
}
class ZoomControlBase {
}
class WebkitTest {
  int LOGTAG;
}
class WebViewProvider {
  class ScrollDelegate {
  }
  class ViewDelegate {
  }
}
class WebViewInputDispatcher {
  class TouchStream {
    int mLastEvent;
  }
  class DispatchEventQueue {
    int mTail;
    int mHead;
  }
  class DispatchEvent {
    int mWebKitScale;
    int mWebKitYOffset;
    int mWebKitXOffset;
    int mTimeoutTime;
    int mFlags;
    int mEventType;
    int mEvent;
    int mNext;
  }
  class WebKitHandler {
    int MSG_DISPATCH_WEBKIT_EVENTS;
  }
  class UiHandler {
    int MSG_HIDE_TAP_HIGHLIGHT;
    int MSG_SHOW_TAP_HIGHLIGHT;
    int MSG_CLICK;
    int MSG_LONG_PRESS;
    int MSG_WEBKIT_TIMEOUT;
    int MSG_DISPATCH_UI_EVENTS;
  }
  class WebKitCallbacks {
  }
  class UiCallbacks {
  }
  int FLAG_WEBKIT_TRANSFORMED_EVENT;
  int FLAG_WEBKIT_TIMEOUT;
  int FLAG_WEBKIT_IN_PROGRESS;
  int FLAG_PRIVATE;
  int EVENT_TYPE_HIT_TEST;
  int EVENT_TYPE_DOUBLE_TAP;
  int EVENT_TYPE_CLICK;
  int EVENT_TYPE_LONG_PRESS;
  int EVENT_TYPE_SCROLL;
  int EVENT_TYPE_HOVER;
  int EVENT_TYPE_TOUCH;
  int PRESSED_STATE_DURATION;
  int DOUBLE_TAP_TIMEOUT;
  int LONG_PRESS_TIMEOUT;
  int TAP_TIMEOUT;
  int WEBKIT_TIMEOUT_MILLIS;
  int mUiDispatchScheduled;
  int mUiHandler;
  int mUiCallbacks;
  int mUiTouchStream;
  int mUiDispatchEventQueue;
  int mWebKitTimeoutTime;
  int mWebKitTimeoutScheduled;
  int mWebKitDispatchScheduled;
  int mWebKitHandler;
  int mWebKitCallbacks;
  int mWebKitTouchStream;
  int mWebKitDispatchEventQueue;
  int mDoubleTapSlopSquared;
  int mTouchSlopSquared;
  int mInitialDownY;
  int mInitialDownX;
  int mIsTapCandidate;
  int mIsDoubleTapCandidate;
  int mPostLastWebKitScale;
  int mPostLastWebKitYOffset;
  int mPostLastWebKitXOffset;
  int mPostHideTapHighlightScheduled;
  int mPostShowTapHighlightScheduled;
  int mPostClickScheduled;
  int mPostLongPressScheduled;
  int mPostDoNotSendTouchEventsToWebKitUntilNextGesture;
  int mPostSendTouchEventsToWebKit;
  int mPostTouchStream;
  int mDispatchEventPoolSize;
  int mDispatchEventPool;
  int MAX_DISPATCH_EVENT_POOL_SIZE;
  int mLock;
  int ENABLE_EVENT_BATCHING;
  int DEBUG;
  int TAG;
}
class WebViewFragment {
  int mIsWebViewAvailable;
  int mWebView;
}
class WebViewFactoryProvider {
  class Statics {
  }
}
class WebViewFactory {
  int sProviderInstance;
  int DEBUG;
  int LOGTAG;
  int DEFAULT_WEB_VIEW_FACTORY;
}
class WebViewDatabaseClassic {
  int mInitialized;
  int HTTPAUTH_PASSWORD_COL;
  int HTTPAUTH_USERNAME_COL;
  int HTTPAUTH_REALM_COL;
  int HTTPAUTH_HOST_COL;
  int FORMDATA_VALUE_COL;
  int FORMDATA_NAME_COL;
  int FORMDATA_URLID_COL;
  int FORMURL_URL_COL;
  int PASSWORD_PASSWORD_COL;
  int PASSWORD_USERNAME_COL;
  int PASSWORD_HOST_COL;
  int ID_PROJECTION;
  int ID_COL;
  int TABLE_HTTPAUTH_ID;
  int TABLE_FORMDATA_ID;
  int TABLE_FORMURL_ID;
  int TABLE_PASSWORD_ID;
  int mTableNames;
  int mHttpAuthLock;
  int mFormLock;
  int mPasswordLock;
  int sDatabase;
  int sInstance;
  int DATABASE_VERSION;
  int CACHE_DATABASE_FILE;
  int DATABASE_FILE;
  int LOGTAG;
}
class WebViewDatabase {
  int LOGTAG;
}
class WebViewCore {
  class ShowRectData {
    int mYPercentInView;
    int mYPercentInDoc;
    int mXPercentInView;
    int mXPercentInDoc;
    int mContentHeight;
    int mContentWidth;
    int mHeight;
    int mWidth;
    int mTop;
    int mLeft;
  }
  int mRepaintScheduled;
  int m_drawWasSkipped;
  int m_skipDrawFlag;
  int m_skipDrawFlagLock;
  int mLastDrawData;
  class DrawData {
    int mFocusSizeChanged;
    int mFirstLayoutForNonStandardLoad;
    int mViewState;
    int mMinPrefWidth;
    int mContentSize;
    int mViewSize;
    int mBaseLayer;
  }
  class ViewState {
    int mShouldStartScrolledRight;
    int mIsRestored;
    int mMobileSite;
    int mScrollY;
    int mScrollX;
    int mDefaultScale;
    int mTextWrapScale;
    int mViewScale;
    int mMaxScale;
    int mMinScale;
  }
  int mFirstLayoutForNonStandardLoad;
  int mInitialViewState;
  int mDrawIsPaused;
  int mDrawIsScheduled;
  int mCurrentViewScale;
  int mCurrentViewHeight;
  int mCurrentViewWidth;
  class EventHub {
    int LAST_PACKAGE_MSG_ID;
    int FIRST_PACKAGE_MSG_ID;
    int mSavedPriority;
    int mTid;
    int mDestroying;
    int mBlockMessages;
    int mMessages;
    int mHandler;
    int SAVE_VIEW_STATE;
    int SET_INITIAL_FOCUS;
    int KEY_PRESS;
    int FIND_NEXT;
    int FIND_ALL;
    int TRUST_STORAGE_UPDATED;
    int SELECT_ALL;
    int SELECT_WORD_AT;
    int SELECT_TEXT;
    int INSERT_TEXT;
    int DELETE_TEXT;
    int COPY_TEXT;
    int DESTROY;
    int SCROLL_LAYER;
    int HEARTBEAT;
    int NOTIFY_ANIMATION_STARTED;
    int PLUGIN_SURFACE_READY;
    int EXECUTE_JS;
    int PROXY_CHANGED;
    int AUTOFILL_FORM;
    int SET_USE_MOCK_DEVICE_ORIENTATION;
    int MODIFY_SELECTION;
    int REMOVE_PACKAGE_NAME;
    int ADD_PACKAGE_NAME;
    int ADD_PACKAGE_NAMES;
    int SET_NETWORK_TYPE;
    int HIDE_FULLSCREEN;
    int POPULATE_VISITED_LINKS;
    int GEOLOCATION_PERMISSIONS_PROVIDE;
    int CONTENT_INVALIDATE_ALL;
    int SET_JS_FLAGS;
    int DUMP_RENDERTREE;
    int DUMP_DOMTREE;
    int REQUEST_DOC_AS_TEXT;
    int REQUEST_EXT_REPRESENTATION;
    int CLEAR_SSL_PREF_TABLE;
    int REMOVE_JS_INTERFACE;
    int SAVE_WEBARCHIVE;
    int FREE_MEMORY;
    int ON_RESUME;
    int ON_PAUSE;
    int SET_ACTIVE;
    int LOAD_DATA;
    int ADD_JS_INTERFACE;
    int REQUEST_CURSOR_HREF;
    int SET_MOVE_MOUSE;
    int CLEAR_CONTENT;
    int POST_URL;
    int WEBKIT_DRAW;
    int DELETE_SURROUNDING_TEXT;
    int SAVE_DOCUMENT_STATE;
    int SET_BACKGROUND_COLOR;
    int MESSAGE_RELAY;
    int SINGLE_LISTBOX_CHOICE;
    int LISTBOX_CHOICES;
    int DELETE_SELECTION;
    int DOC_HAS_IMAGES;
    int SET_NETWORK_STATE;
    int SET_GLOBAL_BOUNDS;
    int PASS_TO_JS;
    int REPLACE_TEXT;
    int SET_SELECTION;
    int CLEAR_HISTORY;
    int CLEAR_CACHE;
    int RESUME_TIMERS;
    int PAUSE_TIMERS;
    int RESTORE_STATE;
    int SET_SCROLL_OFFSET;
    int GO_BACK_FORWARD;
    int VIEW_SIZE_CHANGED;
    int KEY_UP;
    int KEY_DOWN;
    int RELOAD;
    int STOP_LOADING;
    int LOAD_URL;
    int SCROLL_TEXT_INPUT;
    int REVEAL_SELECTION;
  }
  class SaveViewStateRequest {
    int mCallback;
    int mStream;
  }
  class FindAllRequest {
    int mMatchIndex;
    int mMatchCount;
    int mSearchText;
  }
  int HandlerDebugString;
  class GeolocationPermissionsData {
    int mRemember;
    int mAllow;
    int mOrigin;
  }
  class TouchEventData {
    int mNativeResult;
    int mSequence;
    int mNativeLayerRect;
    int mNativeLayer;
    int mMotionEvent;
    int mReprocess;
    int mMetaState;
    int mActionIndex;
    int mPointsInView;
    int mPoints;
    int mIds;
    int mAction;
  }
  int TOUCH_FLAG_PREVENT_DEFAULT;
  int TOUCH_FLAG_HIT_HANDLER;
  int ACTION_DOUBLETAP;
  int ACTION_LONGPRESS;
  class TextFieldInitData {
    int mContentRect;
    int mNodeLayerId;
    int mContentBounds;
    int mMaxLength;
    int mLabel;
    int mName;
    int mIsAutoCompleteEnabled;
    int mIsTextFieldPrev;
    int mIsTextFieldNext;
    int mIsSpellCheckEnabled;
    int mType;
    int mText;
    int mFieldPointer;
  }
  class AutoFillData {
    int mPreview;
    int mQueryId;
  }
  class WebKitHitTest {
    int mHitTestMovedMouse;
    int mHitTestSlop;
    int mHitTestY;
    int mHitTestX;
    int mHasFocus;
    int mEnclosingParentRects;
    int mTapHighlightColor;
    int mEditable;
    int mTouchRects;
    int mTitle;
    int mAltDisplayString;
    int mImageUrl;
    int mAnchorText;
    int mIntentUrl;
    int mLinkUrl;
  }
  class TouchHighlightData {
    int mNativeLayerRect;
    int mNativeLayer;
    int mSlop;
    int mY;
    int mX;
  }
  class TouchUpData {
    int mNativeLayerRect;
    int mNativeLayer;
    int mY;
    int mX;
    int mNode;
    int mFrame;
    int mMoveGeneration;
  }
  class TextSelectionData {
    int mSelectionReason;
    int mSelectTextPtr;
    int mEnd;
    int mStart;
    int REASON_SELECT_WORD;
    int REASON_ACCESSIBILITY_INJECTOR;
    int REASON_UNKNOWN;
  }
  class ReplaceTextData {
    int mTextGeneration;
    int mNewEnd;
    int mNewStart;
    int mReplace;
  }
  class PostUrlData {
    int mPostData;
    int mUrl;
  }
  class GetUrlData {
    int mExtraHeaders;
    int mUrl;
  }
  class MotionUpData {
    int mY;
    int mX;
    int mBounds;
    int mNode;
    int mFrame;
  }
  class JSKeyData {
    int mEvent;
    int mCurrentText;
  }
  class JSInterfaceData {
    int mInterfaceName;
    int mObject;
  }
  class BaseUrlData {
    int mHistoryUrl;
    int mEncoding;
    int mMimeType;
    int mData;
    int mBaseUrl;
  }
  class WebCoreThread {
    int RESUME_PRIORITY;
    int REDUCE_PRIORITY;
    int INITIALIZE;
  }
  int sWebCoreHandler;
  int mEventHub;
  int THREAD_NAME;
  int sShouldMonitorWebCoreThread;
  int mTextSelectionChangeReason;
  int mChromeCanFocusDirection;
  int mHighUsageDeltaMb;
  int mHighMemoryUsageThresholdMb;
  int mLowMemoryUsageThresholdMb;
  int mDeviceOrientationService;
  int mDeviceMotionService;
  int mDeviceMotionAndOrientationManager;
  int mRestoredY;
  int mRestoredX;
  int mRestoredTextWrapScale;
  int mRestoredScale;
  int mIsRestored;
  int mViewportDensityDpi;
  int mViewportUserScalable;
  int mViewportMaximumScale;
  int mViewportMinimumScale;
  int mViewportInitialScale;
  int mViewportHeight;
  int mViewportWidth;
  int mJavascriptInterfaces;
  int mBrowserFrame;
  int mNativeClass;
  int mContext;
  int mSettings;
  int mCallbackProxy;
  int mWebViewClassic;
  int LOGTAG;
}
class WebViewClient {
  int ERROR_TOO_MANY_REQUESTS;
  int ERROR_FILE_NOT_FOUND;
  int ERROR_FILE;
  int ERROR_BAD_URL;
  int ERROR_FAILED_SSL_HANDSHAKE;
  int ERROR_UNSUPPORTED_SCHEME;
  int ERROR_REDIRECT_LOOP;
  int ERROR_TIMEOUT;
  int ERROR_IO;
  int ERROR_CONNECT;
  int ERROR_PROXY_AUTHENTICATION;
  int ERROR_AUTHENTICATION;
  int ERROR_UNSUPPORTED_AUTH_SCHEME;
  int ERROR_HOST_LOOKUP;
  int ERROR_UNKNOWN;
}
class WebViewClassic {
  int mListBoxMessage;
  class InvokeListBox {
    class SingleDataSetObserver {
      int mAdapter;
      int mListView;
      int mCheckedId;
    }
    class MyArrayListAdapter {
    }
    class Container {
      int mId;
      int mEnabled;
      int mString;
      int OPTION_ENABLED;
      int OPTION_DISABLED;
      int OPTGROUP;
    }
    int mContainers;
    int mSelection;
    int mSelectedArray;
    int mMultiple;
  }
  int mAverageSwapFps;
  int mLastSwapTime;
  class PageSwapDelegate {
  }
  class FocusTransitionDrawable {
    int mTranslate;
    int mMaxAlpha;
    int mPaint;
    int mWebView;
    int mProgress;
    int mNewRegion;
    int mPreviousRegion;
  }
  int mFocusTransition;
  class PrivateHandler {
  }
  int mLoadedPicture;
  int mDelaySetPicture;
  int mMapTrackballToArrowKeys;
  int mHandleAlphaAnimator;
  int mHandleAlpha;
  int mLastCursorBounds;
  int mLastCursorTime;
  int mTrackballUpTime;
  int mTrackballDown;
  int mSelectY;
  int mSelectX;
  int SELECT_SCROLL;
  int SELECT_CURSOR_OFFSET;
  int TRACKBALL_MULTIPLIER;
  int TRACKBALL_MOVE_COUNT;
  int TRACKBALL_SCROLL_COUNT;
  int TRACKBALL_SCALE;
  int TRACKBALL_WAIT;
  int TRACKBALL_TIMEOUT;
  int TRACKBALL_KEY_TIMEOUT;
  int mSelectionStarted;
  int mShowTextSelectionExtra;
  int mSelectingText;
  int mTrackballYMove;
  int mTrackballXMove;
  int mTrackballRemainsY;
  int mTrackballRemainsX;
  int mTrackballLastTime;
  int mTrackballFirstTime;
  int DRAG_LAYER_FINGER_DISTANCE;
  int DRAG_LAYER_INVERSE_DENSITY_SQUARED;
  int MMA_WEIGHT_N;
  int ANGLE_HORIZ;
  int ANGLE_VERT;
  int VSLOPE_TO_BREAK_SNAP;
  int VSLOPE_TO_START_SNAP;
  int HSLOPE_TO_BREAK_SNAP;
  int HSLOPE_TO_START_SNAP;
  int mTempVisibleRect;
  int mTempVisibleRectOffset;
  int mGotCenterDown;
  class RequestFormData {
    int mWebSettings;
    int mAutoComplete;
    int mAutoFillable;
    int mUpdateMessage;
    int mUrl;
    int mName;
  }
  int mHistoryHeight;
  int mHistoryWidth;
  int mHistoryPicture;
  int mDrawHistory;
  class SelectionHandleAlpha {
    int mAlpha;
  }
  int mScrollFilter;
  int mZoomFilter;
  int SCROLL_BITS;
  int ZOOM_BITS;
  int mSelectCallback;
  int mOrientation;
  int mFindRequest;
  int mFindIsUp;
  int mFindCallback;
  class ViewSizeData {
    int mIgnoreHeight;
    int mScale;
    int mAnchorY;
    int mAnchorX;
    int mTextWrapWidth;
    int mActualViewHeight;
    int mHeightWidthRatio;
    int mHeight;
    int mWidth;
  }
  int mTempContentVisibleRect;
  int mGlobalVisibleOffset;
  int mScrollOffset;
  int mGlobalVisibleRect;
  int mVisibleRect;
  int mLastGlobalRect;
  int mLastVisibleRectSent;
  int NO_LEFTEDGE;
  class SaveWebArchiveMessage {
    int mResultFile;
    int mCallback;
    int mAutoname;
    int mBasename;
  }
  class DestroyNativeRunnable {
    int mNativePtr;
  }
  int mCachedOverlappingActionModeHeight;
  class TitleBarDelegate {
  }
  class PackageListener {
  }
  int sGoogleApps;
  int sPackageInstallationReceiverAdded;
  int sProxyReceiver;
  class ProxyReceiver {
  }
  int sTrustStorageListener;
  class TrustStorageListener {
  }
  int mContext;
  int mWebViewPrivate;
  int mWebView;
  class Factory {
  }
  class FocusNodeHref {
    int SRC;
    int URL;
    int TITLE;
  }
  int mResumeMsg;
  int mFindListener;
  int mPictureListener;
  int mPictureUpdatePausedForFocusChange;
  int mInputDispatcher;
  int mSentAutoScrollMessage;
  int mScrollingLayerBounds;
  int mMaxAutoScrollY;
  int mMinAutoScrollY;
  int mMaxAutoScrollX;
  int mMinAutoScrollX;
  int mAutoScrollY;
  int mAutoScrollX;
  int SELECT_SCROLL_INTERVAL;
  int mBackgroundColor;
  int SCHEME_GEO;
  int SCHEME_MAILTO;
  int SCHEME_TEL;
  int sNotificationsEnabled;
  int mAutoFillData;
  int mLastTouchUpTime;
  int mLogEvent;
  int mKeysPressed;
  int mOverScrollGlow;
  int mOverflingDistance;
  int mOverscrollDistance;
  int mVerticalScrollBarMode;
  int mHorizontalScrollBarMode;
  int SCROLLBAR_ALWAYSON;
  int SCROLLBAR_ALWAYSOFF;
  int SCROLLBAR_AUTO;
  int DRAW_EXTRAS_CURSOR_RING;
  int DRAW_EXTRAS_SELECTION;
  int DRAW_EXTRAS_NONE;
  int mSnapPositive;
  int SNAP_Y;
  int SNAP_X;
  int SNAP_LOCK;
  int SNAP_NONE;
  int mSnapScrollMode;
  int mSendScrollEvent;
  int mInitialScaleInPercent;
  int sMaxViewportWidth;
  int DEFAULT_VIEWPORT_WIDTH;
  int HandlerPackageDebugString;
  int HandlerPrivateDebugString;
  int LAST_PACKAGE_MSG_ID;
  int FIRST_PACKAGE_MSG_ID;
  int UPDATE_CONTENT_BOUNDS;
  int SHOW_CARET_HANDLE;
  int EDIT_TEXT_SIZE_CHANGED;
  int SCROLL_EDIT_TEXT;
  int AUTOFILL_FORM;
  int FOCUS_NODE_CHANGED;
  int RELOCATE_AUTO_COMPLETE_POPUP;
  int KEY_PRESS;
  int CLEAR_CARET_HANDLE;
  int REPLACE_TEXT;
  int INIT_EDIT_FIELD;
  int COPY_TO_CLIPBOARD;
  int EXIT_FULLSCREEN_VIDEO;
  int UPDATE_ZOOM_DENSITY;
  int ENTER_FULLSCREEN_VIDEO;
  int SCREEN_ON;
  int AUTOFILL_COMPLETE;
  int SET_AUTOFILLABLE;
  int SAVE_WEBARCHIVE_FINISHED;
  int HIT_TEST_RESULT;
  int SELECTION_STRING_CHANGED;
  int SET_SCROLLBAR_MODES;
  int CENTER_FIT_RECT;
  int UPDATE_MATCH_COUNT;
  int HIDE_FULLSCREEN;
  int SHOW_FULLSCREEN;
  int REQUEST_KEYBOARD;
  int INVAL_RECT_MSG_ID;
  int WEBCORE_NEED_TOUCH_EVENTS;
  int PREVENT_TOUCH_ID;
  int LONG_PRESS_CENTER;
  int SHOW_RECT_MSG_ID;
  int UPDATE_TEXT_SELECTION_MSG_ID;
  int CLEAR_TEXT_ENTRY;
  int TAKE_FOCUS;
  int UPDATE_ZOOM_RANGE;
  int UPDATE_TEXTFIELD_TEXT_MSG_ID;
  int WEBCORE_INITIALIZED_MSG_ID;
  int NEW_PICTURE_MSG_ID;
  int SCROLL_TO_MSG_ID;
  int LAST_PRIVATE_MSG_ID;
  int FIRST_PRIVATE_MSG_ID;
  int SCROLL_SELECT_TEXT;
  int PREVENT_DEFAULT_TIMEOUT;
  int DRAG_HELD_MOTIONLESS;
  int REQUEST_FORM_DATA;
  int RELEASE_SINGLE_TAP;
  int SWITCH_TO_LONGPRESS;
  int SWITCH_TO_SHORTPRESS;
  int NEVER_REMEMBER_PASSWORD;
  int REMEMBER_PASSWORD;
  int mHardwareAccelSkia;
  int mBlockWebkitViewMessages;
  int mHTML5VideoViewProxy;
  int mShowTapHighlight;
  int mTouchHighlightY;
  int mTouchHighlightX;
  int mTouchCrossHairColor;
  int TOUCH_HIGHLIGHT_ELAPSE_TIME;
  int DEBUG_TOUCH_HIGHLIGHT;
  int mTouchHightlightPaint;
  int mTouchHighlightRegion;
  int HIGHLIGHT_COLOR;
  int HANDLE_ID_RIGHT;
  int HANDLE_ID_LEFT;
  int mIsCaretSelection;
  int mSelectDraggingTextQuad;
  int mSelectDraggingOffset;
  int mSelectDraggingCursor;
  int mSelectCursorRightTextQuad;
  int mSelectCursorRightLayerId;
  int mSelectCursorRight;
  int mSelectCursorLeftTextQuad;
  int mSelectCursorLeftLayerId;
  int mSelectCursorLeft;
  int mSelectHandleCenterOffset;
  int mSelectHandleRightOffset;
  int mSelectHandleLeftOffset;
  int mSelectHandleCenter;
  int mSelectHandleRight;
  int mSelectHandleLeft;
  int CARET_HANDLE_STAMINA_MS;
  int mAccessibilityInjector;
  int mHeldMotionless;
  int MOTIONLESS_IGNORE;
  int MOTIONLESS_TRUE;
  int MOTIONLESS_PENDING;
  int MOTIONLESS_FALSE;
  int mWrapContent;
  int mOverScrollBorder;
  int mOverScrollBackground;
  int mInOverScrollMode;
  int mEditTextScroller;
  int mScroller;
  int MAX_DURATION;
  int STD_SPEED;
  int mOverlayVerticalScrollbar;
  int mOverlayHorizontalScrollbar;
  int mContentHeight;
  int mContentWidth;
  int mLastActualHeightSent;
  int mLastHeightSent;
  int mLastWidthSent;
  int mHeightCanMeasure;
  int mWidthCanMeasure;
  int PAGE_SCROLL_OVERLAP;
  int MOTIONLESS_TIME;
  int MIN_FLING_TIME;
  int LONG_PRESS_TIMEOUT;
  int TAP_TIMEOUT;
  int mNavSlop;
  int mDoubleTapSlopSquare;
  int mTouchSlopSquare;
  int mFocusedNode;
  int mInitialHitTestResult;
  int mIsPaused;
  int mDrawCursorRing;
  int mTouchInEditText;
  int mConfirmMove;
  int TOUCH_DRAG_TEXT_MODE;
  int TOUCH_DRAG_LAYER_MODE;
  int TOUCH_PINCH_DRAG;
  int TOUCH_DONE_MODE;
  int TOUCH_DOUBLE_TAP_MODE;
  int TOUCH_SHORTPRESS_MODE;
  int TOUCH_SHORTPRESS_START_MODE;
  int TOUCH_DRAG_MODE;
  int TOUCH_DRAG_START_MODE;
  int TOUCH_INIT_MODE;
  int mTouchMode;
  int MINIMUM_VELOCITY_RATIO_FOR_ACCELERATION;
  int mScrollingLayerRect;
  int mCurrentScrollingLayerId;
  int mLastVelY;
  int mLastVelX;
  int mLastVelocity;
  int mMaximumFling;
  int mVelocityTracker;
  int mCurrentTouchInterval;
  int TOUCH_SENT_INTERVAL;
  int mLastSentTouchTime;
  int mLastTouchTime;
  int mAverageAngle;
  int mStartTouchY;
  int mStartTouchX;
  int mLastTouchY;
  int mLastTouchX;
  int mFullScreenHolder;
  int mViewManager;
  int mTextGeneration;
  int mPrivateHandler;
  int mWebViewCore;
  int mNativeClass;
  int mCertificate;
  int mDatabase;
  int mCallbackProxy;
  class OnTrimMemoryListener {
    int sInstance;
  }
  int mLastEditScroll;
  int mIsBatchingTextChanges;
  int mBatchedTextChanges;
  int mIsEditingText;
  int mEditTextLayerId;
  int mEditTextContent;
  int mEditTextContentBounds;
  int mAutoCompletePopup;
  int mPasteWindow;
  int mFieldPointer;
  int mInputConnection;
  int mIsWebViewVisible;
  int mVisibleContentRect;
  int mScreenRect;
  int mInvScreenRect;
  int mZoomManager;
  int LOGTAG;
  int mListBoxDialog;
  int mAutoRedraw;
  int EDIT_RECT_BUFFER;
  int TEXT_SCROLL_FIRST_SCROLL_MS;
  int TEXT_SCROLL_RATE;
  int AUTO_REDRAW_HACK;
  class PastePopupWindow {
    int mPasteTextView;
    int mContentView;
  }
  class WebViewInputConnection {
    int mBatchLevel;
    int mName;
    int mIsAutoCompleteEnabled;
    int mIsAutoFillable;
    int mMaxLength;
    int mHint;
    int mImeOptions;
    int mInputType;
    int mIsKeySentByMe;
    int mKeyCharacterMap;
  }
}
class WebView {
}
class WebTextView {
  int FORM_NOT_AUTOFILLABLE;
  int URL;
  int TELEPHONE;
  int NUMBER;
  int EMAIL;
  int SEARCH;
  int PASSWORD;
  int TEXT_AREA;
  int NORMAL_TEXT_FIELD;
  int LOGTAG;
}
class WebSyncManager {
  class SyncHandler {
  }
  int LOGTAG;
  int mStartSyncRefCount;
  int mDataBase;
  int mHandler;
  int mThreadName;
  int mSyncThread;
  int SYNC_LATER_INTERVAL;
  int SYNC_NOW_INTERVAL;
  int SYNC_MESSAGE;
}
class WebStorageClassic {
  int mUIHandler;
  int mHandler;
  int mOrigins;
  int QUOTA;
  int USAGE;
  int CALLBACK;
  int ORIGIN;
  int ORIGINS;
  int RETURN_QUOTA_ORIGIN;
  int RETURN_USAGE_ORIGIN;
  int RETURN_ORIGINS;
  int GET_QUOTA_ORIGIN;
  int GET_USAGE_ORIGIN;
  int GET_ORIGINS;
  int DELETE_ALL;
  int DELETE_ORIGIN;
  int SET_QUOTA_ORIGIN;
  int UPDATE;
  int sWebStorage;
}
class WebStorage {
  class Origin {
    int mUsage;
    int mQuota;
    int mOrigin;
  }
  class QuotaUpdater {
  }
}
class WebSettingsClassic {
  int ACCEPT_LANG_FOR_US_LOCALE;
  int sLockForLocaleSettings;
  int sLocale;
  int IPHONE_USERAGENT;
  int DESKTOP_USERAGENT;
  class EventHandler {
    int mHandler;
    int SET_DOUBLE_TAP_TOAST_COUNT;
    int PRIORITY;
    int SYNC;
  }
  int DOUBLE_TAP_TOAST_COUNT;
  int PREF_FILE;
  int mDoubleTapToastCount;
  int mUseWebViewBackgroundForOverscroll;
  int mAutoFillProfile;
  class AutoFillProfile {
    int mPhoneNumber;
    int mCountry;
    int mZipCode;
    int mState;
    int mCity;
    int mAddressLine2;
    int mAddressLine1;
    int mCompanyName;
    int mEmailAddress;
    int mFullName;
    int mUniqueId;
  }
  int mPasswordEchoEnabled;
  int mForceUserScalable;
  int mEnableSmoothTransition;
  int mLoadWithOverviewMode;
  int mAllowContentAccess;
  int mAllowFileAccess;
  int mDisplayZoomControls;
  int mBuiltInZoomControls;
  int mSupportZoom;
  int mNavDump;
  int mNeedInitialFocus;
  int mLightTouchEnabled;
  int mSavePassword;
  int mAutoFillEnabled;
  int mSaveFormData;
  int mDoubleTapZoom;
  int mOverrideCacheMode;
  int mRenderPriority;
  int mDefaultZoom;
  int mGeolocationDatabasePath;
  int mDatabasePathHasBeenSet;
  int mDatabasePath;
  int mAppCachePath;
  int mAppCacheMaxSize;
  int mLinkPrefetchEnabled;
  int mXSSAuditorEnabled;
  int mGeolocationEnabled;
  int mWorkersEnabled;
  int mDomStorageEnabled;
  int mDatabaseEnabled;
  int mAppCacheEnabled;
  int mSyntheticLinksEnabled;
  int mPrivateBrowsingEnabled;
  int mMaximumDecodedImageSize;
  int mShrinksStandaloneImagesToFit;
  int mSupportMultipleWindows;
  int mUseWideViewport;
  int mUseDoubleTree;
  int mJavaScriptCanOpenWindowsAutomatically;
  int mPluginState;
  int mShowVisualIndicator;
  int mHardwareAccelSkia;
  int mAllowFileAccessFromFileURLs;
  int mAllowUniversalAccessFromFileURLs;
  int mJavaScriptEnabled;
  int mBlockNetworkLoads;
  int mBlockNetworkImage;
  int mLoadsImagesAutomatically;
  int mPageCacheCapacity;
  int mDefaultFixedFontSize;
  int mDefaultFontSize;
  int mMinimumLogicalFontSize;
  int mMinimumFontSize;
  int mAcceptLanguage;
  int mUseDefaultUserAgent;
  int mUserAgent;
  int mDefaultTextEncoding;
  int mFantasyFontFamily;
  int mCursiveFontFamily;
  int mSerifFontFamily;
  int mSansSerifFontFamily;
  int mFixedFontFamily;
  int mStandardFontFamily;
  int mTextSize;
  int mContext;
  int mLayoutAlgorithm;
  int mEventHandler;
  int mSyncPending;
  int mBrowserFrame;
  int mWebView;
  int PREVIOUS_VERSION;
}
class WebResourceResponse {
  int mInputStream;
  int mEncoding;
  int mMimeType;
}
class WebIconDatabaseClassic {
  class EventHandler {
    class IconResult {
      int mListener;
      int mIcon;
      int mUrl;
    }
    int mMessages;
    int mHandler;
    int ICON_RESULT;
    int BULK_REQUEST_ICON;
    int RELEASE_ICON;
    int RETAIN_ICON;
    int REQUEST_ICON;
    int REMOVE_ALL;
    int CLOSE;
    int OPEN;
  }
  int mEventHandler;
  int sIconDatabase;
  int LOGTAG;
}
class WebIconDatabase {
  class IconListener {
  }
}
class WebHistoryItem {
  int mCustomData;
  int mTouchIconUrlServerDefault;
  int mTouchIconUrlFromLink;
  int mFlattenedData;
  int mFavicon;
  int mNativeBridge;
  int mId;
  int sNextId;
}
class WebCoreThreadWatchdog {
  class PageNotRespondingRunnable {
    int mWatchdogHandler;
    int mContext;
  }
  int sInstance;
  int mWebViews;
  int mPaused;
  int mHandler;
  int mWebCoreThreadHandler;
  int SUBSEQUENT_TIMEOUT_PERIOD;
  int TIMEOUT_PERIOD;
  int HEARTBEAT_PERIOD;
  int TIMED_OUT;
  int IS_ALIVE;
}
class WebChromeClient {
  class CustomViewCallback {
  }
}
class WebBackForwardListClient {
}
class WebBackForwardList {
  int mCallbackProxy;
  int mClearPending;
  int mArray;
  int mCurrentIndex;
}
class ViewStateSerializer {
  int VERSION;
  int WORKING_STREAM_STORAGE;
}
class ViewManager {
  class ChildView {
    int mView;
    int height;
    int width;
    int y;
    int x;
  }
  int MAX_SURFACE_DIMENSION;
  int MAX_SURFACE_AREA;
  int mZoomInProgress;
  int mReadyToDraw;
  int mHidden;
  int mChildren;
  int mWebView;
}
class ValueCallback {
}
class UrlInterceptRegistryTest {
  class MockUrlInterceptHandler {
    int mUrl;
    int mData;
  }
}
class UrlInterceptRegistry {
  int mHandlerList;
  int mDisabled;
  int LOGTAG;
}
class UrlInterceptHandler {
}
class URLUtil {
  int CONTENT_DISPOSITION_PATTERN;
  int CONTENT_BASE;
  int PROXY_BASE;
  int FILE_BASE;
  int RESOURCE_BASE;
  int ASSET_BASE;
  int LOGTAG;
}
class SslErrorHandler {
}
class SslClientCertLookupTable {
  int denied;
  int certificateChains;
  int privateKeys;
  int sTable;
}
class SslCertLookupTable {
  int table;
  int sTable;
}
class SelectActionModeCallback {
  int mIsTextSelected;
  int mActionMode;
  int mWebView;
}
class SearchBoxImpl {
  int mEventCallbacks;
  int mNextEventId;
  int mSupportedCallback;
  int mCallbackProxy;
  int mWebViewCore;
  int mListeners;
  int IS_SUPPORTED_SCRIPT;
  int EVENT_CANCEL;
  int EVENT_RESIZE;
  int EVENT_SUBMIT;
  int EVENT_CHANGE;
  int DISPATCH_EVENT_SCRIPT;
  int SET_DIMENSIONS_SCRIPT;
  int SET_SELECTION_SCRIPT;
  int SET_VERBATIM_SCRIPT;
  int SET_QUERY_SCRIPT;
  int JS_BRIDGE;
  int JS_INTERFACE_NAME;
  int TAG;
}
class SearchBox {
  class IsSupportedCallback {
  }
  class SearchBoxListener {
  }
}
class QuadF {
  int p4;
  int p3;
  int p2;
  int p1;
}
class PluginStub {
}
class PluginManager {
  int SIGNATURES;
  int SIGNATURE_1;
  int mPackageInfoCache;
  int mContext;
  int mInstance;
  int TYPE_NATIVE;
  int PLUGIN_TYPE;
  int PLUGIN_SYSTEM_LIB;
  int LOGTAG;
  int PLUGIN_PERMISSION;
  int PLUGIN_ACTION;
}
class PluginList {
  int mPlugins;
}
class PluginFullScreenHolder {
  int mCallback;
  class CustomFrameLayout {
  }
  int mContentView;
  int mLayout;
  int mOrientation;
  int mNpp;
  int mWebView;
}
class PluginData {
  int mStatusCode;
  int mHeaders;
  int mContentLength;
  int mStream;
}
class Plugin {
  class DefaultClickHandler {
    int mDialog;
  }
  int mHandler;
  int mDescription;
  int mFileName;
  int mPath;
  int mName;
  class PreferencesClickHandler {
  }
}
class OverScrollGlow {
  int mOverScrollDeltaY;
  int mOverScrollDeltaX;
  int mEdgeGlowRight;
  int mEdgeGlowLeft;
  int mEdgeGlowBottom;
  int mEdgeGlowTop;
  int mHostView;
}
class MustOverrideException {
}
class MockGeolocation {
  int sMockGeolocation;
}
class MimeTypeMap {
  int sMimeTypeMap;
}
class L10nUtils {
  int mStrings;
  int mApplicationContext;
  int mIdsArray;
}
class KeyStoreHandler {
  int mMimeType;
  int mDataBuilder;
  int LOGTAG;
}
class JsResult {
  int mResult;
  int mReceiver;
  class ResultReceiver {
  }
}
class JsPromptResult {
  int mStringResult;
}
class JniUtil {
  int ANDROID_CONTENT;
  int sContext;
  int sCacheDirectory;
  int sDatabaseDirectory;
  int LOGTAG;
}
class JWebCoreJavaBridge {
  int mContentUriToFilePathMap;
  int REFRESH_PLUGINS;
  int sCurrentMainWebView;
  int mHasDeferredTimers;
  int mTimerPaused;
  int mHasInstantTimer;
  int mNativeBridge;
  int LOGTAG;
  int FUNCPTR_MESSAGE;
  int TIMER_MESSAGE;
}
class HttpAuthHandler {
}
class HTML5VideoViewProxy {
  class PosterDownloader {
    int mHandler;
    int mHeaders;
    int mStatusCode;
    int mRequestHandle;
    int mPosterBytes;
    int mProxy;
    int mUrl;
    int mQueueRefCount;
    int mRequestQueue;
  }
  class VideoPlayer {
    int mBaseLayer;
    int isVideoSelfEnded;
    int mHTML5VideoView;
    int mCurrentProxy;
  }
  int mSeekPosition;
  int mPosterDownloader;
  int mPoster;
  int mWebView;
  int mWebCoreHandler;
  int mNativePointer;
  int TIMEUPDATE;
  int RESTORESTATE;
  int STOPFULLSCREEN;
  int PAUSED;
  int POSTER_FETCHED;
  int ENDED;
  int PREPARED;
  int BUFFERING_END;
  int BUFFERING_START;
  int LOAD_DEFAULT_POSTER;
  int ERROR;
  int PAUSE;
  int SEEK;
  int PLAY;
  int LOGTAG;
}
class HTML5VideoView {
  int mStartWhenPrepared;
  int mPlayerBuffering;
  class TimeupdateTask {
    int mProxy;
  }
  int mSkipPrepare;
  int TIMEUPDATE_PERIOD;
  int mPauseDuringPreparing;
  int mTimer;
  int mHeaders;
  int mUri;
  int mCurrentState;
  int mPlayer;
  int mVideoLayerId;
  int mSaveSeekTime;
  int mProxy;
  int STATE_RESETTED;
  int STATE_PLAYING;
  int STATE_PREPARED;
  int STATE_PREPARING;
  int STATE_INITIALIZED;
  int HIDE_URL_LOGS;
  int COOKIE;
  int LOGTAG;
}
class HTML5VideoInline {
  int mVideoLayerUsingSurfaceTexture;
  int mTextureNames;
  int mSurfaceTexture;
}
class HTML5VideoFullScreen {
  class FullScreenMediaController {
    int mVideoView;
  }
  int mBufferingUpdateListener;
  int mCallback;
  int mSizeChangedListener;
  int mSHCallback;
  int mVideoHeight;
  int mVideoWidth;
  int mLayout;
  int mProgressView;
  int mCurrentBufferPercentage;
  int mCanPause;
  int mCanSeekForward;
  int mCanSeekBack;
  int mSurfaceHolder;
  int mMediaController;
  int mFullScreenMode;
  int FULLSCREEN_SURFACECREATED;
  int FULLSCREEN_SURFACECREATING;
  int FULLSCREEN_OFF;
  int mVideoSurfaceView;
  class VideoSurfaceView {
  }
}
class HTML5Audio {
  class IsPrivateBrowsingEnabledGetter {
    int mIsPrivateBrowsingEnabled;
    int mIsReady;
  }
  class TimeupdateTask {
  }
  int mTimer;
  int TIMEUPDATE_PERIOD;
  int HIDE_URL_LOGS;
  int COOKIE;
  int TIMEUPDATE;
  int mContext;
  int mProcessingOnEnd;
  int mLoopEnabled;
  int mAskToPlay;
  int mUrl;
  int mState;
  int ERROR;
  int STOPPED;
  int PAUSED;
  int COMPLETE;
  int STARTED;
  int PREPARED;
  int INITIALIZED;
  int IDLE;
  int mIsPrivateBrowsingEnabledGetter;
  int mNativePointer;
  int mMediaPlayer;
  int LOGTAG;
}
class GeolocationService {
  int mIsGpsProviderAvailable;
  int mIsNetworkProviderAvailable;
  int mIsRunning;
  int mIsGpsEnabled;
  int mLocationManager;
  int mNativeObject;
  int TAG;
}
class GeolocationPermissionsClassic {
  int sInstance;
  int ALLOWED;
  int CALLBACK;
  int ORIGIN;
  int ORIGINS;
  int RETURN_ALLOWED;
  int RETURN_ORIGINS;
  int CLEAR_ALL;
  int ALLOW;
  int CLEAR;
  int GET_ALLOWED;
  int GET_ORIGINS;
  int mQueuedMessages;
  int mUIHandler;
  int mHandler;
}
class GeolocationPermissions {
  class Callback {
  }
}
class FindActionModeCallback {
  class NoAction {
  }
  int mGlobalVisibleOffset;
  int mGlobalVisibleRect;
  int mActionMode;
  int mActiveMatchIndex;
  int mNumberOfMatches;
  int mMatchesFound;
  int mResources;
  int mInput;
  int mWebView;
  int mMatches;
  int mEditText;
  int mCustomView;
}
class DownloadListener {
}
class DeviceOrientationService {
  int DELTA_DEGRESS;
  int mHaveSentErrorEvent;
  int mGamma;
  int mBeta;
  int mAlpha;
  int mContext;
  int mSensorManager;
  int mHandler;
  int mIsRunning;
  int mManager;
  int mMagneticFieldVector;
  int mGravityVector;
}
class DeviceMotionService {
  int INTERVAL_MILLIS;
  int mLastAcceleration;
  int mUpdateRunnable;
  int mHaveSentErrorEvent;
  int mContext;
  int mSensorManager;
  int mHandler;
  int mIsRunning;
  int mManager;
}
class DeviceMotionAndOrientationManager {
  int mWebViewCore;
}
class DebugFlags {
  int MEASURE_PAGE_SWAP_FPS;
  int WEB_VIEW_CORE;
  int WEB_VIEW;
  int WEB_SYNC_MANAGER;
  int WEB_SETTINGS;
  int WEB_BACK_FORWARD_LIST;
  int URL_UTIL;
  int STREAM_LOADER;
  int SSL_ERROR_HANDLER;
  int NETWORK;
  int LOAD_LISTENER;
  int J_WEB_CORE_JAVA_BRIDGE;
  int FRAME_LOADER;
  int COOKIE_SYNC_MANAGER;
  int COOKIE_MANAGER;
  int CALLBACK_PROXY;
  int CACHE_MANAGER;
  int BROWSER_FRAME;
}
class DateSorter {
  int NUM_DAYS_AGO;
  int mLabels;
  int mBins;
  int DAY_COUNT;
  int LOGTAG;
}
class CookieSyncManager {
  int sRef;
}
class CookieManagerClassic {
  int mPendingCookieOperations;
  int LOGTAG;
  int sRef;
}
class CookieManager {
}
class ConsoleMessage {
  int mLineNumber;
  int mSourceId;
  int mMessage;
  int mLevel;
  class MessageLevel {
    int DEBUG;
    int ERROR;
    int WARNING;
    int LOG;
    int TIP;
  }
}
class ClientCertRequestHandler {
  int mTable;
  int mHostAndPort;
  int mHandle;
  int mBrowserFrame;
}
class CertTool {
  int sCertificateTypeMap;
  int MD5_WITH_RSA;
  int LOGTAG;
}
class CallbackProxy {
  class UploadFile {
    int mValue;
  }
  class UploadFileMessageData {
    int mCapture;
    int mAcceptType;
    int mCallback;
  }
  int mWebCoreIdleTime;
  int mWebCoreThreadTime;
  int PERF_PROBE;
  class JsResultReceiver {
    int mJsResult;
    int mTriedToNotifyBeforeReady;
    int mReady;
  }
  class ResultTransport {
    int mResult;
  }
  int NOTIFY;
  int PROCEEDED_AFTER_SSL_ERROR;
  int SEARCHBOX_DISPATCH_COMPLETE_CALLBACK;
  int SEARCHBOX_IS_SUPPORTED_CALLBACK;
  int CLIENT_CERT_REQUEST;
  int AUTO_LOGIN;
  int NOTIFY_SEARCHBOX_LISTENERS;
  int SET_INSTALLABLE_WEBAPP;
  int AUTH_CREDENTIALS;
  int HISTORY_INDEX_CHANGED;
  int ADD_HISTORY_ITEM;
  int OPEN_FILE_CHOOSER;
  int GET_VISITED_HISTORY;
  int RECEIVED_TOUCH_ICON_URL;
  int GEOLOCATION_PERMISSIONS_HIDE_PROMPT;
  int GEOLOCATION_PERMISSIONS_SHOW_PROMPT;
  int ADD_MESSAGE_TO_CONSOLE;
  int JS_TIMEOUT;
  int REACHED_APPCACHE_MAXSIZE;
  int EXCEEDED_DATABASE_QUOTA;
  int SWITCH_OUT_HISTORY;
  int RECEIVED_CERTIFICATE;
  int SCALE_CHANGED;
  int REQUEST_FOCUS;
  int PAGE_FINISHED;
  int RESEND_POST_DATA;
  int REPORT_ERROR;
  int DOWNLOAD_FILE;
  int ASYNC_KEYEVENTS;
  int JS_UNLOAD;
  int JS_PROMPT;
  int JS_CONFIRM;
  int JS_ALERT;
  int SAVE_PASSWORD;
  int CLOSE_WINDOW;
  int CREATE_WINDOW;
  int LOAD_RESOURCE;
  int UPDATE_VISITED;
  int PROGRESS;
  int SSL_ERROR;
  int AUTH_REQUEST;
  int OVERRIDE_URL;
  int RECEIVED_TITLE;
  int RECEIVED_ICON;
  int PAGE_STARTED;
  int mBlockMessages;
  int mContext;
  int mWebBackForwardListClient;
  int mBackForwardList;
  int mLatestProgress;
  int mProgressUpdatePending;
  int mDownloadListener;
  int mWebView;
  int mWebChromeClient;
  int mWebViewClient;
  int LOGTAG;
}
class CacheManager {
  class CacheResult {
    int outFile;
    int outStream;
    int inStream;
    int crossDomain;
    int contentdisposition;
    int encoding;
    int location;
    int mimeType;
    int etag;
    int lastModified;
    int localPath;
    int expiresString;
    int expires;
    int contentLength;
    int httpStatusCode;
  }
  int mBaseDir;
  int HEADER_KEY_IFNONEMATCH;
  int HEADER_KEY_IFMODIFIEDSINCE;
  int LOGTAG;
}
class ByteArrayBuilder {
  class Chunk {
    int mLength;
    int mArray;
  }
  int mChunks;
  int sQueue;
  int sPool;
  int DEFAULT_CAPACITY;
}
class BrowserFrame {
  int FILE_UPLOAD_NO_FILE_CHOSEN;
  int SUBMIT_LABEL;
  int RESET_LABEL;
  int FILE_UPLOAD_LABEL;
  int DRAWABLEDIR;
  int LOADERROR;
  int NODOMAIN;
  int POLICY_IGNORE;
  int POLICY_USE;
  int sConfigCallback;
  class ConfigCallback {
    int mWindowManager;
    int mHandlers;
  }
  int sJavaBridge;
  int mNativeFrame;
  int TRANSITION_SWITCH_THRESHOLD;
  int FRAME_LOADTYPE_REPLACE;
  int FRAME_LOADTYPE_REDIRECT;
  int FRAME_LOADTYPE_SAME;
  int FRAME_LOADTYPE_RELOADALLOWINGSTALEDATA;
  int FRAME_LOADTYPE_RELOAD;
  int FRAME_LOADTYPE_INDEXEDBACKFORWARD;
  int FRAME_LOADTYPE_FORWARD;
  int FRAME_LOADTYPE_BACK;
  int FRAME_LOADTYPE_STANDARD;
  int POLICY_FUNCTION;
  int ORIENTATION_CHANGED;
  int FRAME_COMPLETED;
  int mSearchBox;
  int mKeyStoreHandler;
  int mRemovedJavaScriptObjects;
  int mJavaScriptObjects;
  int mIsMainFrame;
  int mOrientation;
  int mBlockMessages;
  int mCommitted;
  int mFirstLayoutDone;
  int mLoadType;
  int mLoadInitFromJava;
  int mWebViewCore;
  int mContext;
  int mSettings;
  int mCallbackProxy;
  int MAX_OUTSTANDING_REQUESTS;
  int LOGTAG;
}
class AutoCompletePopup {
  int mWebView;
  int mInputConnection;
  int mAnchor;
  int mAdapter;
  int mText;
  int mFilter;
  int mPopup;
  int mQueryId;
  int mHandler;
  int mIsAutoFillProfileSet;
  int AUTOFILL_FORM;
  class AnchorView {
  }
}
class AccessibilityInjectorTestActivity {
  int mWebView;
}
class AccessibilityInjectorTest {
  class MockAccessibilityService {
    int mIsServiceInfoSet;
  }
  class Worker {
    int mHandler;
    int mWorkerLock;
  }
  int sReceivedSelectionString;
  int sDefaultKeyBindings;
  int mWebView;
  int mWorker;
  int sExecutedTestCount;
  int sIsAccessibilityServiceReady;
  int sInstance;
  int TEST_KEY_DINDINGS;
  int sTestLock;
  int SELECTION_STRING_UNKNOWN;
  int APPLE_SPAN_SUFFIX;
  int APPLE_SPAN_PREFIX;
  int META_STATE_ALT_LEFT_ON;
  int TEST_CASE_COUNT;
  int TIMEOUT_ENABLE_ACCESSIBILITY_AND_MOCK_SERVICE;
  int TIMEOUT_WAIT_FOR_SELECTION_STRING;
}
class AccessibilityInjectorFallback {
  class AccessibilityWebContentKeyBinding {
    int mActionSequence;
    int mKeyCodeAndModifiers;
    int THIRD_ARGUMENT_MASK;
    int THIRD_ARGUMENT_OFFSET;
    int SECOND_ARGUMENT_MASK;
    int SECOND_ARGUMENT_OFFSET;
    int FIRST_ARGUMENT_MASK;
    int FIRST_ARGUMENT_OFFSET;
    int ACTION_MASK;
    int ACTION_OFFSET;
    int KEY_CODE_MASK;
    int KEY_CODE_OFFSET;
    int MODIFIERS_MASK;
    int MODIFIERS_OFFSET;
  }
  int mLastDirection;
  int mIsLastSelectionStringNull;
  int mLastDownEventHandled;
  int mCurrentAxis;
  int mScheduledEventStack;
  int mWebViewInternal;
  int mWebView;
  int sBindings;
  int NAVIGATION_DIRECTION_FORWARD;
  int NAVIGATION_DIRECTION_BACKWARD;
  int NAVIGATION_AXIS_DEFAULT_WEB_VIEW_BEHAVIOR;
  int NAVIGATION_AXIS_DOCUMENT;
  int NAVIGATION_AXIS_PARENT_FIRST_CHILD;
  int NAVIGATION_AXIS_SIBLING;
  int NAVIGATION_AXIS_HEADING;
  int NAVIGATION_AXIS_SENTENCE;
  int NAVIGATION_AXIS_WORD;
  int NAVIGATION_AXIS_CHARACTER;
  int ACTION_TRAVERSE_DEFAULT_WEB_VIEW_BEHAVIOR_AXIS;
  int ACTION_PERFORM_AXIS_TRANSITION;
  int ACTION_TRAVERSE_GIVEN_AXIS;
  int ACTION_TRAVERSE_CURRENT_AXIS;
  int ACTION_SET_CURRENT_AXIS;
  int DEBUG;
  int LOG_TAG;
}
class AccessibilityInjector {
  class CallbackHandler {
    int mResultId;
    int mResult;
    int mInterfaceName;
    int mResultLock;
    int mResultIdCounter;
    int RESULT_TIMEOUT;
    int JAVASCRIPT_ACTION_TEMPLATE;
  }
  int ACCESSIBILITY_ANDROIDVOX_TEMPLATE;
  int ACCESSIBILITY_SCREEN_READER_JAVASCRIPT_TEMPLATE;
  int ALIAS_TRAVERSAL_JS_INTERFACE;
  int ALIAS_TTS_JS_INTERFACE;
  int ACCESSIBILITY_SCRIPT_INJECTION_PROVIDED;
  int ACCESSIBILITY_SCRIPT_INJECTION_OPTED_OUT;
  int ACCESSIBILITY_SCRIPT_INJECTION_UNDEFINED;
  int mAccessibilityScriptInjected;
  int mAccessibilityJSONObject;
  int mAccessibilityInjectorFallback;
  int mAccessibilityManager;
  int mCallback;
  int mTextToSpeech;
  int mWebView;
  int mContext;
  int mWebViewClassic;
}
