package java.util;
class WeakHashMap {
  class HashIterator {
    int type;
    int nextKey;
    int nextEntry;
    int currentEntry;
    int expectedModCount;
    int position;
  }
  class Entry {
    class Type {
    }
    int next;
    int value;
    int isNull;
    int hash;
  }
  int modCount;
  int threshold;
  int loadFactor;
  int elementData;
  int elementCount;
  int referenceQueue;
  int DEFAULT_SIZE;
}
class Vector {
  int DEFAULT_SIZE;
  int capacityIncrement;
  int elementData;
  int elementCount;
  int serialVersionUID;
}
class UnsafeArrayList {
  int size;
  int array;
  int elementType;
}
class UnknownFormatFlagsException {
  int flags;
  int serialVersionUID;
}
class UnknownFormatConversionException {
  int s;
  int serialVersionUID;
}
class UUID {
  int hash;
  int node;
  int clockSequence;
  int timestamp;
  int version;
  int variant;
  int leastSigBits;
  int mostSigBits;
  int rng;
  int serialVersionUID;
}
class TreeSet {
  int descendingSet;
  int backingMap;
  int serialVersionUID;
}
class TreeMap {
  class SubMap {
    int toEnd;
    int fromStart;
    int toKey;
    int fromKey;
    int serialVersionUID;
  }
  class AscendingSubMap {
    int serialVersionUID;
  }
  class DescendingSubMap {
    int reverseComparator;
    int serialVersionUID;
  }
  class NavigableSubMap {
    int hiInclusive;
    int loInclusive;
    int toEnd;
    int fromStart;
    int hi;
    int lo;
    int m;
    int serialVersionUID;
  }
  int serialVersionUID;
  class BoundedMap {
    class BoundedKeySet {
    }
    class BoundedEntrySet {
    }
    class BoundedIterator {
    }
    int keySet;
    int entrySet;
    int toBound;
    int to;
    int fromBound;
    int from;
    int ascending;
  }
  class Bound {
    int NO_BOUND;
    int EXCLUSIVE;
    int INCLUSIVE;
  }
  class KeySet {
  }
  class EntrySet {
  }
  class MapIterator {
    int expectedModCount;
    int last;
    int next;
  }
  class Node {
    int height;
    int value;
    int key;
    int right;
    int left;
    int parent;
  }
  int keySet;
  int entrySet;
  class Relation {
    int HIGHER;
    int CEILING;
    int CREATE;
    int EQUAL;
    int FLOOR;
    int LOWER;
  }
  int modCount;
  int size;
  int root;
  int comparator;
  int NATURAL_ORDER;
}
class TooManyListenersException {
  int serialVersionUID;
}
class TimerTask {
  int scheduledTime;
  int fixedRate;
  int period;
  int when;
  int cancelled;
  int lock;
}
class Timer {
  int finalizer;
  int impl;
  int timerId;
  class FinalizerHelper {
    int impl;
  }
  class TimerImpl {
    int tasks;
    int finished;
    int cancelled;
    class TimerHeap {
      int deletedCancelledNumber;
      int size;
      int timers;
      int DEFAULT_HEAP_SIZE;
    }
  }
}
class TimeZone {
  int ID;
  int defaultTimeZone;
  int GMT;
  int LONG;
  int SHORT;
  int serialVersionUID;
}
class TimSort {
  int DEBUG;
  int runLen;
  int runBase;
  int stackSize;
  int tmp;
  int INITIAL_TMP_STORAGE_LENGTH;
  int minGallop;
  int MIN_GALLOP;
  int c;
  int a;
  int MIN_MERGE;
}
class StringTokenizer {
  int position;
  int returnDelimiters;
  int delimiters;
  int string;
}
class Stack {
  int serialVersionUID;
}
class SortedSet {
}
class SortedMap {
}
class SimpleTimeZone {
  int serialPersistentFields;
  int dstSavings;
  int useDaylight;
  int WALL_TIME;
  int STANDARD_TIME;
  int UTC_TIME;
  int DOW_LE_DOM_MODE;
  int DOW_GE_DOM_MODE;
  int DOW_IN_MONTH_MODE;
  int DOM_MODE;
  int endMode;
  int startMode;
  int endTime;
  int endDayOfWeek;
  int endDay;
  int endMonth;
  int startTime;
  int startDayOfWeek;
  int startDay;
  int startMonth;
  int startYear;
  int rawOffset;
  int serialVersionUID;
}
class Set {
}
class ServiceLoader {
  class ServiceIterator {
    int queue;
    int isRead;
    int services;
    int service;
    int classLoader;
  }
  int services;
  int classLoader;
  int service;
}
class ServiceConfigurationError {
  int serialVersionUID;
}
class Scanner {
  class DataType {
    int FLOAT;
    int INT;
  }
  int cachehasNextIndex;
  int cacheHasNextValue;
  int inputExhausted;
  int decimalFormat;
  int matchSuccessful;
  int lastIOException;
  int closed;
  int bufferLength;
  int preStartIndex;
  int findStartIndex;
  int locale;
  int integerRadix;
  int matcher;
  int delimiter;
  int buffer;
  int input;
  int DEFAULT_TRUNK_SIZE;
  int DEFAULT_RADIX;
  int DIPLOID;
  int ANY_PATTERN;
  int LINE_PATTERN;
  int MULTI_LINE_TERMINATOR;
  int LINE_TERMINATOR;
  int BOOLEAN_PATTERN;
  int DEFAULT_DELIMITER;
}
class ResourceBundle {
  class Control {
    int format;
    int FORMAT_DEFAULT_CONTROL;
    int FORMAT_CLASS_CONTROL;
    int FORMAT_PROPERTIES_CONTROL;
    int TTL_NO_EXPIRATION_CONTROL;
    int TTL_DONT_CACHE;
    int FORMAT_PROPERTIES;
    int FORMAT_CLASS;
    int FORMAT_DEFAULT;
    int JAVAPROPERTIES;
    int JAVACLASS;
    int listProperties;
    int listClass;
    int listDefault;
  }
  class SimpleControl {
  }
  class NoFallbackControl {
    int NOFALLBACK_FORMAT_DEFAULT_CONTROL;
    int NOFALLBACK_FORMAT_CLASS_CONTROL;
    int NOFALLBACK_FORMAT_PROPERTIES_CONTROL;
  }
  int cacheLocale;
  int cache;
  int MISSINGBASE;
  int MISSING;
  class MissingBundle {
  }
  int lastLoadTime;
  int locale;
  int parent;
  int EMPTY_STRING;
  int UNDER_SCORE;
}
class RandomAccess {
}
class Random {
  int nextNextGaussian;
  int seed;
  int haveNextNextGaussian;
  int multiplier;
  int serialVersionUID;
}
class Queue {
}
class PropertyResourceBundle {
  int resources;
}
class PropertyPermission {
}
class Properties {
  int IGNORE;
  int KEY_DONE;
  int CONTINUE;
  int UNICODE;
  int SLASH;
  int NONE;
  int defaults;
  int PROP_DTD;
  int PROP_DTD_NAME;
  int builder;
  int serialVersionUID;
}
class PriorityQueue {
  class PriorityIterator {
    int allowRemove;
    int currentIndex;
  }
  int elements;
  int comparator;
  int size;
  int DEFAULT_CAPACITY_RATIO;
  int DEFAULT_INIT_CAPACITY_RATIO;
  int DEFAULT_CAPACITY;
  int serialVersionUID;
}
class Observer {
}
class Observable {
  int changed;
  int observers;
}
class NoSuchElementException {
  int serialVersionUID;
}
class NavigableSet {
}
class NavigableMap {
}
class MissingResourceException {
  int key;
  int className;
  int serialVersionUID;
}
class MissingFormatWidthException {
  int s;
  int serialVersionUID;
}
class MissingFormatArgumentException {
  int s;
  int serialVersionUID;
}
class MiniEnumSet {
  class MiniEnumSetIterator {
    int last;
    int mask;
    int currentBits;
  }
  int bits;
  int enums;
  int size;
  int MAX_ELEMENTS;
}
class MapEntry {
  class Type {
  }
  int value;
  int key;
}
class Map {
  class Entry {
  }
}
class Locale {
  int serialPersistentFields;
  int cachedToStringResult;
  int variantCode;
  int languageCode;
  int countryCode;
  int defaultLocale;
  int US;
  int UK;
  int TRADITIONAL_CHINESE;
  int TAIWAN;
  int SIMPLIFIED_CHINESE;
  int ROOT;
  int PRC;
  int KOREAN;
  int KOREA;
  int JAPANESE;
  int JAPAN;
  int ITALY;
  int ITALIAN;
  int GERMANY;
  int GERMAN;
  int FRENCH;
  int FRANCE;
  int ENGLISH;
  int CHINESE;
  int CHINA;
  int CANADA_FRENCH;
  int CANADA;
  int serialVersionUID;
}
class ListResourceBundle {
  int table;
}
class ListIterator {
}
class List {
}
class LinkedList {
  class ReverseLinkIterator {
    int canRemove;
    int link;
    int list;
    int expectedModCount;
  }
  class LinkIterator {
    int lastLink;
    int link;
    int list;
    int expectedModCount;
    int pos;
  }
  class Link {
    int next;
    int previous;
    int data;
  }
  int voidLink;
  int size;
  int serialVersionUID;
}
class LinkedHashSet {
  int serialVersionUID;
}
class LinkedHashMap {
  int serialVersionUID;
  class EntryIterator {
  }
  class ValueIterator {
  }
  class KeyIterator {
  }
  class LinkedHashIterator {
    int expectedModCount;
    int lastReturned;
    int next;
  }
  class LinkedEntry {
    int prv;
    int nxt;
  }
  int accessOrder;
  int header;
}
class Iterator {
}
class InvalidPropertiesFormatException {
  int serialVersionUID;
}
class InputMismatchException {
  int serialVersionUID;
}
class IllegalFormatWidthException {
  int w;
  int serialVersionUID;
}
class IllegalFormatPrecisionException {
  int p;
  int serialVersionUID;
}
class IllegalFormatFlagsException {
  int flags;
  int serialVersionUID;
}
class IllegalFormatException {
  int serialVersionUID;
}
class IllegalFormatConversionException {
  int arg;
  int c;
  int serialVersionUID;
}
class IllegalFormatCodePointException {
  int c;
  int serialVersionUID;
}
class IdentityHashMap {
  class IdentityHashMapEntrySet {
    int associatedMap;
  }
  class IdentityHashMapIterator {
    int canRemove;
    int type;
    int expectedModCount;
    int associatedMap;
    int lastPosition;
    int position;
  }
  class IdentityHashMapEntry {
    int map;
  }
  int NULL_OBJECT;
  int modCount;
  int loadFactor;
  int DEFAULT_MAX_SIZE;
  int threshold;
  int size;
  int elementData;
  int serialVersionUID;
}
class HugeEnumSet {
  class HugeEnumSetIterator {
    int last;
    int mask;
    int index;
    int currentBits;
  }
  int size;
  int bits;
  int enums;
  int BIT_IN_LONG;
}
class Hashtable {
  int serialPersistentFields;
  int serialVersionUID;
  class EntrySet {
  }
  class Values {
  }
  class KeySet {
  }
  int CHARS_PER_ENTRY;
  class ValueEnumeration {
  }
  class KeyEnumeration {
  }
  class EntryIterator {
  }
  class ValueIterator {
  }
  class KeyIterator {
  }
  class HashIterator {
    int expectedModCount;
    int lastEntryReturned;
    int nextEntry;
    int nextIndex;
  }
  class HashtableEntry {
    int next;
    int hash;
    int value;
    int key;
  }
  int values;
  int entrySet;
  int keySet;
  int threshold;
  int modCount;
  int size;
  int table;
  int DEFAULT_LOAD_FACTOR;
  int EMPTY_TABLE;
  int MAXIMUM_CAPACITY;
  int MINIMUM_CAPACITY;
}
class HashSet {
  int backingMap;
  int serialVersionUID;
}
class HashMap {
  int serialPersistentFields;
  int serialVersionUID;
  class EntrySet {
  }
  class Values {
  }
  class KeySet {
  }
  class EntryIterator {
  }
  class ValueIterator {
  }
  class KeyIterator {
  }
  class HashIterator {
    int expectedModCount;
    int lastEntryReturned;
    int nextEntry;
    int nextIndex;
  }
  class HashMapEntry {
    int next;
    int hash;
    int value;
    int key;
  }
  int values;
  int entrySet;
  int keySet;
  int threshold;
  int modCount;
  int size;
  int entryForNullKey;
  int table;
  int DEFAULT_LOAD_FACTOR;
  int EMPTY_TABLE;
  int MAXIMUM_CAPACITY;
  int MINIMUM_CAPACITY;
}
class GregorianCalendar {
  int lastYearSkew;
  int currentYearSkew;
  int lastMidnightMillis;
  int nextMidnightMillis;
  int cachedFields;
  int isCached;
  int leastMaximums;
  int minimums;
  int maximums;
  int DaysInYear;
  int DaysInMonth;
  int julianSkew;
  int changeYear;
  int gregorianCutover;
  int defaultGregorianCutover;
  int AD;
  int BC;
  int serialVersionUID;
}
class Grego {
  int DAYS_BEFORE;
  int MONTH_LENGTH;
  int JULIAN_1970_CE;
  int JULIAN_1_CE;
  int MILLIS_PER_DAY;
  int MILLIS_PER_HOUR;
  int MILLIS_PER_MINUTE;
  int MILLIS_PER_SECOND;
  int MAX_MILLIS;
  int MIN_MILLIS;
}
class FormatterClosedException {
  int serialVersionUID;
}
class Formatter {
  class FormatSpecifierParser {
    int i;
    int startIndex;
    int length;
    int format;
  }
  class FormatToken {
    int strFlags;
    int width;
    int precision;
    int dateSuffix;
    int conversionType;
    int flagZero;
    int flagSpace;
    int flagSharp;
    int flagPlus;
    int flagParenthesis;
    int flagMinus;
    int flagComma;
    int argIndex;
    int FLAG_ZERO;
    int DEFAULT_PRECISION;
    int FLAGS_UNSET;
    int UNSET;
    int LAST_ARGUMENT_INDEX;
  }
  int cachedDecimalFormat;
  class CachedDecimalFormat {
    int currentPattern;
    int currentLocaleData;
    int decimalFormat;
  }
  int localeData;
  int lastIOException;
  int formatToken;
  int closed;
  int arg;
  int locale;
  int out;
  class BigDecimalLayoutForm {
    int DECIMAL_FLOAT;
    int SCIENTIFIC;
  }
  int ZEROS;
}
class FormattableFlags {
  int ALTERNATE;
  int UPPERCASE;
  int LEFT_JUSTIFY;
}
class Formattable {
}
class FormatFlagsConversionMismatchException {
  int c;
  int f;
  int serialVersionUID;
}
class EventObject {
  int source;
  int serialVersionUID;
}
class EventListenerProxy {
  int listener;
}
class EventListener {
}
class Enumeration {
}
class EnumSet {
  class SerializationProxy {
    int elements;
    int elementType;
    int serialVersionUID;
  }
  int elementClass;
  int serialVersionUID;
}
class EnumMap {
  class EnumMapEntrySet {
    int enumMap;
  }
  class EnumMapEntryIterator {
  }
  class EnumMapValueCollection {
    int enumMap;
  }
  class EnumMapKeySet {
    int enumMap;
  }
  class EnumMapIterator {
    int type;
    int enumMap;
    int prePosition;
    int position;
  }
  class Entry {
    int ordinal;
    int enumMap;
  }
  int entrySet;
  int enumSize;
  int mappingsCount;
  int hasMapping;
  int values;
  int keys;
  int keyType;
  int serialVersionUID;
}
class EmptyStackException {
  int serialVersionUID;
}
class DuplicateFormatFlagsException {
  int flags;
  int serialVersionUID;
}
class DualPivotQuicksort {
  int NUM_BYTE_VALUES;
  int NUM_CHAR_VALUES;
  int NUM_SHORT_VALUES;
  int COUNTING_SORT_THRESHOLD_FOR_SHORT_OR_CHAR;
  int COUNTING_SORT_THRESHOLD_FOR_BYTE;
  int INSERTION_SORT_THRESHOLD;
}
class Dictionary {
}
class Deque {
}
class Date {
  int milliseconds;
  int creationYear;
  int serialVersionUID;
}
class Currency {
  int currencyCode;
  int localesToCurrencies;
  int codesToCurrencies;
  int serialVersionUID;
}
class ConcurrentModificationException {
  int serialVersionUID;
}
class Comparator {
}
class ComparableTimSort {
  int DEBUG;
  int runLen;
  int runBase;
  int stackSize;
  int tmp;
  int INITIAL_TMP_STORAGE_LENGTH;
  int minGallop;
  int MIN_GALLOP;
  int a;
  int MIN_MERGE;
}
class Collections {
  class CheckedSortedMap {
    int sm;
    int serialVersionUID;
  }
  class CheckedSortedSet {
    int ss;
    int serialVersionUID;
  }
  class CheckedMap {
    class CheckedEntrySet {
      class CheckedEntryIterator {
        int valueType;
        int i;
      }
      int valueType;
      int s;
    }
    class CheckedEntry {
      int valueType;
      int e;
    }
    int valueType;
    int keyType;
    int m;
    int serialVersionUID;
  }
  class CheckedSet {
    int serialVersionUID;
  }
  class CheckedRandomAccessList {
    int serialVersionUID;
  }
  class CheckedList {
    int l;
    int serialVersionUID;
  }
  class CheckedListIterator {
    int type;
    int i;
  }
  class CheckedCollection {
    int type;
    int c;
    int serialVersionUID;
  }
  class AsLIFOQueue {
    int q;
    int serialVersionUID;
  }
  class SetFromMap {
    int backingSet;
    int m;
    int serialVersionUID;
  }
  class UnmodifiableSortedSet {
    int ss;
    int serialVersionUID;
  }
  class UnmodifiableSortedMap {
    int sm;
    int serialVersionUID;
  }
  class UnmodifiableSet {
    int serialVersionUID;
  }
  class UnmodifiableMap {
    class UnmodifiableEntrySet {
      class UnmodifiableMapEntry {
        int mapEntry;
      }
      int serialVersionUID;
    }
    int m;
    int serialVersionUID;
  }
  class UnmodifiableList {
    int list;
    int serialVersionUID;
  }
  class UnmodifiableRandomAccessList {
    int serialVersionUID;
  }
  class UnmodifiableCollection {
    int c;
    int serialVersionUID;
  }
  class SynchronizedSortedSet {
    int ss;
    int serialVersionUID;
  }
  class SynchronizedSortedMap {
    int sm;
    int serialVersionUID;
  }
  class SynchronizedSet {
    int serialVersionUID;
  }
  class SynchronizedMap {
    int mutex;
    int m;
    int serialVersionUID;
  }
  class SynchronizedList {
    int list;
    int serialVersionUID;
  }
  class SynchronizedRandomAccessList {
    int serialVersionUID;
  }
  class SynchronizedCollection {
    int mutex;
    int c;
    int serialVersionUID;
  }
  class SingletonMap {
    int v;
    int k;
    int serialVersionUID;
  }
  class SingletonList {
    int element;
    int serialVersionUID;
  }
  class SingletonSet {
    int element;
    int serialVersionUID;
  }
  class ReverseComparator2 {
    int cmp;
    int serialVersionUID;
  }
  class ReverseComparator {
    int serialVersionUID;
    int INSTANCE;
  }
  int EMPTY_MAP;
  int EMPTY_SET;
  int EMPTY_LIST;
  class EmptyMap {
    int serialVersionUID;
  }
  class EmptySet {
    int serialVersionUID;
  }
  class EmptyList {
    int serialVersionUID;
  }
  class CopiesList {
    int element;
    int n;
    int serialVersionUID;
  }
  int EMPTY_ENUMERATION;
  int EMPTY_ITERATOR;
}
class Collection {
}
class Calendar {
  int serialPersistentFields;
  int FIELD_NAMES;
  int LONG;
  int SHORT;
  int ALL_STYLES;
  int PM;
  int AM;
  int FIELD_COUNT;
  int DST_OFFSET;
  int ZONE_OFFSET;
  int MILLISECOND;
  int SECOND;
  int MINUTE;
  int HOUR_OF_DAY;
  int HOUR;
  int AM_PM;
  int DAY_OF_WEEK_IN_MONTH;
  int DAY_OF_WEEK;
  int DAY_OF_YEAR;
  int DAY_OF_MONTH;
  int DATE;
  int WEEK_OF_MONTH;
  int WEEK_OF_YEAR;
  int MONTH;
  int YEAR;
  int ERA;
  int SATURDAY;
  int FRIDAY;
  int THURSDAY;
  int WEDNESDAY;
  int TUESDAY;
  int MONDAY;
  int SUNDAY;
  int UNDECIMBER;
  int DECEMBER;
  int NOVEMBER;
  int OCTOBER;
  int SEPTEMBER;
  int AUGUST;
  int JULY;
  int JUNE;
  int MAY;
  int APRIL;
  int MARCH;
  int FEBRUARY;
  int JANUARY;
  int zone;
  int minimalDaysInFirstWeek;
  int firstDayOfWeek;
  int lenient;
  int lastDateFieldSet;
  int lastTimeFieldSet;
  int time;
  int isTimeSet;
  int isSet;
  int fields;
  int areFieldsSet;
  int serialVersionUID;
}
class BitSet {
  int longCount;
  int bits;
  int ALL_ONES;
  int serialVersionUID;
}
class Arrays {
  class ArrayList {
    int a;
    int serialVersionUID;
  }
}
class ArrayList {
  int serialVersionUID;
  class ArrayListIterator {
    int expectedModCount;
    int removalIndex;
    int remaining;
  }
  int array;
  int size;
  int MIN_CAPACITY_INCREMENT;
}
class ArrayDeque {
  int serialVersionUID;
  class DescendingIterator {
    int lastRet;
    int fence;
    int cursor;
  }
  class DeqIterator {
    int lastRet;
    int fence;
    int cursor;
  }
  int MIN_INITIAL_CAPACITY;
  int tail;
  int head;
  int elements;
}
class AbstractSet {
}
class AbstractSequentialList {
}
class AbstractQueue {
}
class AbstractMap {
  class SimpleEntry {
    int value;
    int key;
    int serialVersionUID;
  }
  class SimpleImmutableEntry {
    int value;
    int key;
    int serialVersionUID;
  }
  int valuesCollection;
  int keySet;
}
class AbstractList {
  class SubAbstractList {
    class SubAbstractListIterator {
      int end;
      int start;
      int iterator;
      int subList;
    }
    int size;
    int offset;
    int fullList;
  }
  class SubAbstractListRandomAccess {
  }
  class FullListIterator {
  }
  class SimpleListIterator {
    int lastPosition;
    int expectedModCount;
    int pos;
  }
  int modCount;
}
class AbstractCollection {
}
