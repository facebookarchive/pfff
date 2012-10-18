package android.content.res;
class XmlResourceParser {
}
class XmlBlock {
  int mOpenCount;
  int mOpen;
  int mStrings;
  int mNative;
  int mAssets;
  class Parser {
    int mEventType;
    int mDepth;
    int mDecNextDepth;
    int mStarted;
    int mBlock;
    int mParseState;
  }
  int DEBUG;
}
class TypedArray_Delegate {
}
class TypedArray {
  int mValue;
  int mLength;
  int mIndices;
  int mData;
  int mRsrcs;
  int mXml;
  int mResources;
}
class StringBlock {
  class Height {
    int sProportion;
    int mSize;
  }
  class StyleIDs {
    int marqueeId;
    int listItemId;
    int strikeId;
    int supId;
    int subId;
    int smallId;
    int bigId;
    int ttId;
    int underlineId;
    int italicId;
    int boldId;
  }
  int mStyleIDs;
  int mSparseStrings;
  int mStrings;
  int mOwnsNative;
  int mUseSparse;
  int mNative;
  int localLOGV;
  int TAG;
}
class Resources_Theme_Delegate {
}
class Resources {
  class Theme {
    int mTheme;
    int mAssets;
  }
  class NotFoundException {
  }
  int mCompatibilityInfo;
  int mPluralRule;
  int mMetrics;
  int mConfiguration;
  int mAssets;
  int mCachedXmlBlocks;
  int mCachedXmlBlockIds;
  int mLastCachedXmlBlockIndex;
  int mLastRetrievedAttrs;
  int mCachedStyledAttributes;
  int mPreloading;
  int mColorDrawableCache;
  int mColorStateListCache;
  int mDrawableCache;
  int mTmpConfig;
  int mTmpValue;
  int mPreloaded;
  int sPreloadedColorDrawables;
  int sPreloadedColorStateLists;
  int sPreloadedDrawables;
  int mSystem;
  int mSync;
  int ID_OTHER;
  int TRACE_FOR_MISS_PRELOAD;
  int TRACE_FOR_PRELOAD;
  int DEBUG_ATTRIBUTES_CACHE;
  int DEBUG_CONFIG;
  int DEBUG_LOAD;
  int TAG;
}
class ObbScanner {
}
class ObbInfo {
  int CREATOR;
  int salt;
  int flags;
  int version;
  int packageName;
  int filename;
  int OBB_OVERLAY;
}
class Configuration {
  int CREATOR;
  int seq;
  int layoutDirection;
  int compatSmallestScreenWidthDp;
  int compatScreenHeightDp;
  int compatScreenWidthDp;
  int smallestScreenWidthDp;
  int SMALLEST_SCREEN_WIDTH_DP_UNDEFINED;
  int screenHeightDp;
  int SCREEN_HEIGHT_DP_UNDEFINED;
  int screenWidthDp;
  int SCREEN_WIDTH_DP_UNDEFINED;
  int uiMode;
  int UI_MODE_NIGHT_YES;
  int UI_MODE_NIGHT_NO;
  int UI_MODE_NIGHT_UNDEFINED;
  int UI_MODE_NIGHT_MASK;
  int UI_MODE_TYPE_APPLIANCE;
  int UI_MODE_TYPE_TELEVISION;
  int UI_MODE_TYPE_CAR;
  int UI_MODE_TYPE_DESK;
  int UI_MODE_TYPE_NORMAL;
  int UI_MODE_TYPE_UNDEFINED;
  int UI_MODE_TYPE_MASK;
  int orientation;
  int ORIENTATION_SQUARE;
  int ORIENTATION_LANDSCAPE;
  int ORIENTATION_PORTRAIT;
  int ORIENTATION_UNDEFINED;
  int navigationHidden;
  int NAVIGATIONHIDDEN_YES;
  int NAVIGATIONHIDDEN_NO;
  int NAVIGATIONHIDDEN_UNDEFINED;
  int navigation;
  int NAVIGATION_WHEEL;
  int NAVIGATION_TRACKBALL;
  int NAVIGATION_DPAD;
  int NAVIGATION_NONAV;
  int NAVIGATION_UNDEFINED;
  int hardKeyboardHidden;
  int HARDKEYBOARDHIDDEN_YES;
  int HARDKEYBOARDHIDDEN_NO;
  int HARDKEYBOARDHIDDEN_UNDEFINED;
  int keyboardHidden;
  int KEYBOARDHIDDEN_SOFT;
  int KEYBOARDHIDDEN_YES;
  int KEYBOARDHIDDEN_NO;
  int KEYBOARDHIDDEN_UNDEFINED;
  int keyboard;
  int KEYBOARD_12KEY;
  int KEYBOARD_QWERTY;
  int KEYBOARD_NOKEYS;
  int KEYBOARD_UNDEFINED;
  int touchscreen;
  int TOUCHSCREEN_FINGER;
  int TOUCHSCREEN_STYLUS;
  int TOUCHSCREEN_NOTOUCH;
  int TOUCHSCREEN_UNDEFINED;
  int screenLayout;
  int SCREENLAYOUT_COMPAT_NEEDED;
  int SCREENLAYOUT_LONG_YES;
  int SCREENLAYOUT_LONG_NO;
  int SCREENLAYOUT_LONG_UNDEFINED;
  int SCREENLAYOUT_LONG_MASK;
  int SCREENLAYOUT_SIZE_XLARGE;
  int SCREENLAYOUT_SIZE_LARGE;
  int SCREENLAYOUT_SIZE_NORMAL;
  int SCREENLAYOUT_SIZE_SMALL;
  int SCREENLAYOUT_SIZE_UNDEFINED;
  int SCREENLAYOUT_SIZE_MASK;
  int userSetLocale;
  int locale;
  int mnc;
  int mcc;
  int fontScale;
}
class CompatibilityInfo {
  int CREATOR;
  class Translator {
    int mTouchableAreaBuffer;
    int mVisibleInsetsBuffer;
    int mContentInsetsBuffer;
    int applicationInvertedScale;
    int applicationScale;
  }
  int applicationInvertedScale;
  int applicationScale;
  int applicationDensity;
  int NEEDS_SCREEN_COMPAT;
  int NEVER_NEEDS_COMPAT;
  int ALWAYS_NEEDS_COMPAT;
  int SCALING_REQUIRED;
  int mCompatibilityFlags;
  int MAXIMUM_ASPECT_RATIO;
  int DEFAULT_NORMAL_SHORT_DIMENSION;
  int DEFAULT_COMPATIBILITY_INFO;
}
class ColorStateList {
  int CREATOR;
  int sCache;
  int EMPTY;
  int mDefaultColor;
  int mColors;
  int mStateSpecs;
}
class BridgeTypedArray {
  int mIsFramework;
  int mNames;
  int mResourceData;
  int mPlatformFile;
  int mContext;
  int mBridgeResources;
}
class BridgeResources {
  class NinePatchInputStream {
    int mFakeMarkSupport;
  }
  int mPlatformResourceFlag;
  int mProjectCallback;
  int mContext;
}
class BridgeAssetManager {
}
class AssetManager {
  int STYLE_DENSITY;
  int STYLE_CHANGING_CONFIGURATIONS;
  int STYLE_RESOURCE_ID;
  int STYLE_ASSET_COOKIE;
  int STYLE_DATA;
  int STYLE_TYPE;
  int STYLE_NUM_ENTRIES;
  class AssetInputStream {
    int mMarkPos;
    int mLength;
    int mAsset;
  }
  int mRefStacks;
  int mOpen;
  int mNumRefs;
  int mStringBlocks;
  int mNObject;
  int mObject;
  int mOffsets;
  int mValue;
  int sSystem;
  int sSync;
  int DEBUG_REFS;
  int localLOGV;
  int TAG;
  int ACCESS_BUFFER;
  int ACCESS_STREAMING;
  int ACCESS_RANDOM;
  int ACCESS_UNKNOWN;
}
class AssetFileDescriptor {
  int CREATOR;
  class AutoCloseOutputStream {
    int mRemaining;
  }
  class AutoCloseInputStream {
    int mRemaining;
  }
  int mLength;
  int mStartOffset;
  int mFd;
  int UNKNOWN_LENGTH;
}
