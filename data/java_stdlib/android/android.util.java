package android.util;
class XmlPullAttributes {
  int mParser;
}
class Xml {
  class Encoding {
    int ISO_8859_1;
    int UTF_16;
    int UTF_8;
    int US_ASCII;
    int expatName;
  }
  class XmlSerializerFactory {
    int instance;
    int TYPE;
  }
  int FEATURE_RELAXED;
}
class TypedValue {
  int FRACTION_UNIT_STRS;
  int DIMENSION_UNIT_STRS;
  int RADIX_MULTS;
  int MANTISSA_MULT;
  int density;
  int changingConfigurations;
  int resourceId;
  int assetCookie;
  int data;
  int string;
  int type;
  int DENSITY_NONE;
  int DENSITY_DEFAULT;
  int COMPLEX_MANTISSA_MASK;
  int COMPLEX_MANTISSA_SHIFT;
  int COMPLEX_RADIX_0p23;
  int COMPLEX_RADIX_8p15;
  int COMPLEX_RADIX_16p7;
  int COMPLEX_RADIX_23p0;
  int COMPLEX_RADIX_MASK;
  int COMPLEX_RADIX_SHIFT;
  int COMPLEX_UNIT_FRACTION_PARENT;
  int COMPLEX_UNIT_FRACTION;
  int COMPLEX_UNIT_MM;
  int COMPLEX_UNIT_IN;
  int COMPLEX_UNIT_PT;
  int COMPLEX_UNIT_SP;
  int COMPLEX_UNIT_DIP;
  int COMPLEX_UNIT_PX;
  int COMPLEX_UNIT_MASK;
  int COMPLEX_UNIT_SHIFT;
  int TYPE_LAST_INT;
  int TYPE_LAST_COLOR_INT;
  int TYPE_INT_COLOR_RGB4;
  int TYPE_INT_COLOR_ARGB4;
  int TYPE_INT_COLOR_RGB8;
  int TYPE_INT_COLOR_ARGB8;
  int TYPE_FIRST_COLOR_INT;
  int TYPE_INT_BOOLEAN;
  int TYPE_INT_HEX;
  int TYPE_INT_DEC;
  int TYPE_FIRST_INT;
  int TYPE_FRACTION;
  int TYPE_DIMENSION;
  int TYPE_FLOAT;
  int TYPE_STRING;
  int TYPE_ATTRIBUTE;
  int TYPE_REFERENCE;
  int TYPE_NULL;
}
class TrustedTime {
}
class TouchModeFlexibleAsserts {
  int MAX_DELAY_MILLIS;
  int MAX_ATTEMPTS;
}
class TimingLogger {
  int mSplitLabels;
  int mSplits;
  int mDisabled;
  int mLabel;
  int mTag;
}
class TimeUtilsTest {
}
class TimeUtils {
  int sFormatStr;
  int sFormatSync;
  int SECONDS_PER_DAY;
  int SECONDS_PER_HOUR;
  int SECONDS_PER_MINUTE;
  int HUNDRED_DAY_FIELD_LEN;
  int sLastUniqueCountry;
  int sLastUniqueZoneOffsets;
  int sLastUniqueLockObj;
  int sLastCountry;
  int sLastZones;
  int sLastLockObj;
  int TAG;
  int DBG;
}
class TimeFormatException {
}
class SynchronizedPool {
  int mLock;
  int mPool;
}
class StringBuilderPrinter {
  int mBuilder;
}
class StateSetTest {
}
class StateSet {
  int NOTHING;
  int WILD_CARD;
}
class SparseLongArray {
  int mSize;
  int mValues;
  int mKeys;
}
class SparseIntArray {
  int mSize;
  int mValues;
  int mKeys;
}
class SparseBooleanArray {
  int mSize;
  int mValues;
  int mKeys;
}
class SparseArray {
  int mSize;
  int mValues;
  int mKeys;
  int mGarbage;
  int DELETED;
}
class Slog {
}
class Singleton {
  int mInstance;
}
class ScrollViewScenario {
  class Params {
    int mBottomPadding;
    int mTopPadding;
    int mViewFactories;
  }
  class ViewFactoryBase {
    int mHeightRatio;
  }
  class ViewFactory {
  }
  int mScrollView;
  int mLinearLayout;
}
class ReflectiveProperty {
  int mField;
  int mGetter;
  int mSetter;
  int PREFIX_SET;
  int PREFIX_IS;
  int PREFIX_GET;
}
class Property {
  int mType;
  int mName;
}
class Printer {
}
class PrintWriterPrinter {
  int mPW;
}
class PrintStreamPrinter {
  int mPS;
}
class PrefixPrinter {
  int mPrefix;
  int mPrinter;
}
class Pools {
}
class PoolableManager {
}
class Poolable {
}
class Pool {
}
class PatternsTest {
}
class Patterns {
  int PHONE;
  int EMAIL_ADDRESS;
  int DOMAIN_NAME;
  int IP_ADDRESS;
  int WEB_URL;
  int GOOD_IRI_CHAR;
  int TOP_LEVEL_DOMAIN_STR_FOR_WEB_URL;
  int TOP_LEVEL_DOMAIN;
  int TOP_LEVEL_DOMAIN_STR;
}
class Pair {
  int second;
  int first;
}
class NtpTrustedTime {
  int mCachedNtpCertainty;
  int mCachedNtpElapsedRealtime;
  int mCachedNtpTime;
  int mHasCache;
  int mTimeout;
  int mServer;
  int sSingleton;
  int LOGD;
  int TAG;
}
class NoSuchPropertyException {
}
class MonthDisplayHelperTest {
}
class MonthDisplayHelper {
  int mOffset;
  int mNumDaysInPrevMonth;
  int mNumDaysInMonth;
  int mCalendar;
  int mWeekStartDay;
}
class MathUtils {
  int RAD_TO_DEG;
  int DEG_TO_RAD;
  int sRandom;
}
class MalformedJsonException {
  int serialVersionUID;
}
class LruCacheTest {
  int expectedEvictionCount;
  int expectedMissCount;
  int expectedHitCount;
  int expectedPutCount;
  int expectedCreateCount;
}
class LruCache {
  int missCount;
  int hitCount;
  int evictionCount;
  int createCount;
  int putCount;
  int maxSize;
  int size;
  int map;
}
class LongSparseArray {
  int mSize;
  int mValues;
  int mKeys;
  int mGarbage;
  int DELETED;
}
class Log_Delegate {
}
class LogWriter {
  int mBuilder;
  int mBuffer;
  int mTag;
  int mPriority;
}
class LogTest {
  class PerformanceTest {
    int ITERATIONS;
  }
  int LOG_TAG;
  int PROPERTY_TAG;
}
class LogPrinter {
  int mBuffer;
  int mTag;
  int mPriority;
}
class Log {
  int LOG_ID_SYSTEM;
  int LOG_ID_EVENTS;
  int LOG_ID_RADIO;
  int LOG_ID_MAIN;
  int sWtfHandler;
  class TerribleFailureHandler {
  }
  class TerribleFailure {
  }
  int ASSERT;
  int ERROR;
  int WARN;
  int INFO;
  int DEBUG;
  int VERBOSE;
}
class LocaleUtil {
  int HEBR_SCRIPT_SUBTAG;
  int ARAB_SCRIPT_SUBTAG;
}
class LocalLog {
  int mNow;
  int mMaxLines;
  int mLog;
}
class ListUtil {
  int mInstrumentation;
  int mListView;
}
class ListScenario {
  class MyAdapter {
  }
  class Params {
    int mConnectAdapter;
    int mFooterViewCount;
    int mHeaderFocusable;
    int mHeaderViewCount;
    int mMustFillScreen;
    int mStackFromBottom;
    int mIncludeHeader;
    int mUnselectableItems;
    int mOverrideItemScreenSizeFactors;
    int mFadingEdgeScreenSizeFactor;
    int mItemScreenSizeFactor;
    int mStartingSelectionPosition;
    int mItemsFocusable;
    int mNumItems;
  }
  int mLinearLayout;
  int mFooterViewCount;
  int mHeadersFocusable;
  int mHeaderViewCount;
  int mConvertMisses;
  int mLongClickedPosition;
  int mClickedPosition;
  int mStackFromBottom;
  int mUnselectableItems;
  int mIncludeHeader;
  int mScreenHeight;
  int mOverrideItemScreenSizeFactors;
  int mItemScreenSizeFactor;
  int mStartingSelectionPosition;
  int mItemsFocusable;
  int mNumItems;
  int mHeaderTextView;
  int mListView;
}
class ListItemFactory {
  class Slot {
    int Right;
    int Middle;
    int Left;
  }
}
class KeyUtils {
}
class JsonWriterTest {
}
class JsonWriter {
  int lenient;
  int separator;
  int indent;
  int stack;
  int out;
}
class JsonToken {
  int END_DOCUMENT;
  int NULL;
  int BOOLEAN;
  int NUMBER;
  int STRING;
  int NAME;
  int END_OBJECT;
  int BEGIN_OBJECT;
  int END_ARRAY;
  int BEGIN_ARRAY;
}
class JsonScope {
  int CLOSED;
  int NONEMPTY_DOCUMENT;
  int EMPTY_DOCUMENT;
  int NONEMPTY_OBJECT;
  int DANGLING_NAME;
  int EMPTY_OBJECT;
  int NONEMPTY_ARRAY;
  int EMPTY_ARRAY;
}
class JsonReaderTest {
  int READER_BUFFER_SIZE;
}
class JsonReader {
  int skipping;
  int valueLength;
  int valuePos;
  int value;
  int name;
  int token;
  int stack;
  int bufferStartColumn;
  int bufferStartLine;
  int limit;
  int pos;
  int buffer;
  int lenient;
  int in;
  int stringPool;
  int FALSE;
  int TRUE;
}
class InternalSelectionView {
  int mLabel;
  int mDesiredHeight;
  int mEstimatedPixelHeight;
  int mSelectedRow;
  int mNumRows;
  int mTempRect;
  int mTextPaint;
  int mPainter;
}
class IntProperty {
}
class GridScenario {
  class MyAdapter {
  }
  class Params {
    int mVerticalSpacing;
    int mStretchMode;
    int mNumColumns;
    int mColumnWidth;
    int mMustFillScreen;
    int mStackFromBottom;
    int mOverrideItemScreenSizeFactors;
    int mItemScreenSizeFactor;
    int mStartingSelectionPosition;
    int mNumItems;
  }
  int mVerticalSpacing;
  int mStretchMode;
  int mNumColumns;
  int mColumnWidth;
  int mStackFromBottom;
  int mScreenHeight;
  int mOverrideItemScreenSizeFactors;
  int mItemScreenSizeFactor;
  int mStartingSelectionPosition;
  int mNumItems;
  int mGridView;
}
class FloatProperty {
}
class FloatMath_Delegate {
}
class FloatMathTest {
}
class FloatMath {
}
class FinitePool {
  int mPoolCount;
  int mRoot;
  int mInfinite;
  int mLimit;
  int mManager;
  int LOG_TAG;
}
class FastImmutableArraySet {
  class FastIterator {
    int mIndex;
    int mContents;
  }
  int mContents;
  int mIterator;
}
class EventLogTags {
  class Description {
    int mName;
    int mTag;
  }
}
class EventLog {
  class Event {
    int LIST_TYPE;
    int STRING_TYPE;
    int LONG_TYPE;
    int INT_TYPE;
    int DATA_START;
    int TAG_OFFSET;
    int PAYLOAD_START;
    int NANOSECONDS_OFFSET;
    int SECONDS_OFFSET;
    int THREAD_OFFSET;
    int PROCESS_OFFSET;
    int LENGTH_OFFSET;
    int mBuffer;
  }
  int sTagNames;
  int sTagCodes;
  int TAG_PATTERN;
  int COMMENT_PATTERN;
  int TAGS_FILE;
  int TAG;
}
class DisplayMetrics {
  int noncompatYdpi;
  int noncompatXdpi;
  int noncompatScaledDensity;
  int noncompatDensity;
  int noncompatHeightPixels;
  int noncompatWidthPixels;
  int ydpi;
  int xdpi;
  int scaledDensity;
  int densityDpi;
  int density;
  int heightPixels;
  int widthPixels;
  int DENSITY_DEVICE;
  int DENSITY_DEFAULT;
  int DENSITY_XXHIGH;
  int DENSITY_XHIGH;
  int DENSITY_HIGH;
  int DENSITY_TV;
  int DENSITY_MEDIUM;
  int DENSITY_LOW;
}
class DebugUtils {
}
class DayOfMonthCursorTest {
}
class DayOfMonthCursor {
  int mColumn;
  int mRow;
}
class Config {
  int LOGD;
  int LOGV;
  int PROFILE;
  int RELEASE;
  int DEBUG;
}
class CharsetUtils {
  int sVendorShiftJisMap;
  int VENDOR_SOFTBANK;
  int VENDOR_KDDI;
  int VENDOR_DOCOMO;
}
class BridgeXmlPullAttributes {
  int mPlatformFile;
  int mContext;
}
class Base64Test {
  int lipsum;
  int BYTES;
  int TAG;
}
class Base64OutputStream {
  int EMPTY;
  int bpos;
  int buffer;
  int flags;
  int coder;
}
class Base64InputStream {
  int outputEnd;
  int outputStart;
  int inputBuffer;
  int eof;
  int BUFFER_SIZE;
  int EMPTY;
  int coder;
}
class Base64DataException {
}
class Base64 {
  class Encoder {
    int alphabet;
    int do_cr;
    int do_newline;
    int do_padding;
    int count;
    int tailLen;
    int tail;
    int ENCODE_WEBSAFE;
    int ENCODE;
    int LINE_GROUPS;
  }
  class Decoder {
    int alphabet;
    int value;
    int state;
    int EQUALS;
    int SKIP;
    int DECODE_WEBSAFE;
    int DECODE;
  }
  class Coder {
    int op;
    int output;
  }
  int NO_CLOSE;
  int URL_SAFE;
  int CRLF;
  int NO_WRAP;
  int NO_PADDING;
  int DEFAULT;
}
class AttributeSet {
}
class AndroidRuntimeException {
}
class AndroidException {
}
