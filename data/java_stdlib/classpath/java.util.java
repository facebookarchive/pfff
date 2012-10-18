package java.util;
class VMTimeZone {
}
class WeakHashMap {
  int buckets;
  int theEntrySet;
  class WeakBucket {
    class WeakEntry {
      int key;
    }
    int slot;
    int next;
    int value;
  }
  class WeakEntrySet {
  }
  int modCount;
  int threshold;
  int loadFactor;
  int size;
  int queue;
  int NULL_KEY;
  int DEFAULT_LOAD_FACTOR;
  int DEFAULT_CAPACITY;
}
class Vector {
  int capacityIncrement;
  int elementCount;
  int elementData;
  int serialVersionUID;
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
  int r;
  int leastSigBits;
  int mostSigBits;
  int serialVersionUID;
}
class TreeSet {
  int map;
  int serialVersionUID;
}
class TreeMap {
  class NavigableEntrySet {
  }
  class EntrySet {
  }
  class DescendingSet {
    int set;
  }
  class NavigableKeySet {
  }
  class KeySet {
  }
  class DescendingMap {
    int map;
    int values;
    int nKeys;
    int keys;
    int entries;
  }
  class SubMap {
    class NavigableEntrySet {
    }
    class EntrySet {
    }
    class NavigableKeySet {
    }
    class KeySet {
    }
    int nKeys;
    int descendingMap;
    int entries;
    int maxKey;
    int minKey;
  }
  class TreeIterator {
    int max;
    int next;
    int last;
    int knownMod;
    int type;
  }
  class Node {
    int parent;
    int right;
    int left;
    int color;
  }
  int comparator;
  int modCount;
  int nKeys;
  int descendingMap;
  int entries;
  int size;
  int root;
  int nil;
  int BLACK;
  int RED;
  int serialVersionUID;
}
class TooManyListenersException {
  int serialVersionUID;
}
class TimerTask {
  int fixed;
  int period;
  int lastExecutionTime;
  int scheduled;
}
class Timer {
  int canceled;
  int thread;
  int scheduler;
  int queue;
  int nr;
  class Scheduler {
    int queue;
  }
  class TaskQueue {
    int elements;
    int heap;
    int nullOnEmpty;
    int DEFAULT_SIZE;
  }
}
class TimeZone {
  int timezones0;
  int aliases0;
  int availableIDs;
  int zoneinfo_dir;
  int serialVersionUID;
  int defaultZone0;
  int ID;
  int LONG;
  int SHORT;
}
class StringTokenizer {
  int retDelims;
  int delim;
  int len;
  int str;
  int pos;
}
class Stack {
  int serialVersionUID;
}
class SortedSet {
}
class SortedMap {
}
class SimpleTimeZone {
  int UTC_TIME;
  int WALL_TIME;
  int STANDARD_TIME;
  int serialVersionUID;
  int serialVersionOnStream;
  int monthArr;
  int monthLength;
  int endTimeMode;
  int endTime;
  int endDayOfWeek;
  int endDay;
  int endMode;
  int endMonth;
  int startTimeMode;
  int startTime;
  int startDayOfWeek;
  int startDay;
  int startMonth;
  int startMode;
  int DOW_LE_DOM_MODE;
  int DOW_GE_DOM_MODE;
  int DOW_IN_MONTH_MODE;
  int DOM_MODE;
  int startYear;
  int dstSavings;
  int useDaylight;
  int rawOffset;
}
class Set {
}
class ServiceLoader {
  int serviceIt;
  int cache;
  int loader;
  int spi;
}
class ServiceConfigurationError {
  int serialVersionUID;
}
class Scanner {
  int dfs;
  int useLocale;
  int df;
  int actFormat;
  int matchValid;
  int doSkipp;
  int skipped;
  int needInput;
  int procesedChars;
  int lastResult;
  int last_transparent;
  int last_anchor;
  int last_RegionEnd;
  int last_RegionStart;
  int lastPatternHash;
  int lastNextPos;
  int lastFoundPresent;
  int lastFound;
  int isClosed;
  int rbcSource;
  int readableSource;
  int actResult;
  int myMatcher;
  int charsetName;
  int tmpBuffer;
  int actPos;
  int p;
  int actLocale;
  int currentRadix;
  int actBuffer;
  int MAX_PREFIX;
  int MIN_BUF_LEN;
  int MaxBufferLen;
  int bIS;
  int lastIOException;
  int NEW_LINE;
  int BIG_INTEGER;
  int DEFAULT_PATTERN;
  int DEFAULT_PATTERN_S;
  int IS_NOT;
  int NOT_BOOLEAN;
  int NOT_BYTE;
  int NOT_DOUBLE;
  int NOT_INT;
  int ERR_PREFIX;
  int NOT_LONG;
}
class ResourceBundle {
  int nullEntry;
  int lookupKey;
  class BundleKey {
    int hashcode;
    int classLoader;
    int locale;
    int baseName;
    int defaultLocale;
  }
  int bundleCache;
  int locale;
  int parent;
  int CACHE_SIZE;
}
class RandomAccess {
}
class Random {
  int serialVersionUID;
  int seed;
  int nextNextGaussian;
  int haveNextNextGaussian;
}
class PropertyResourceBundle {
  int properties;
}
class PropertyPermissionCollection {
  int all_allowed;
  int permissions;
  int serialVersionUID;
}
class PropertyPermission {
  int actionStrings;
  int actions;
  int WRITE;
  int READ;
  int serialVersionUID;
  int serialPersistentFields;
}
class Properties {
  int serialVersionUID;
  int defaults;
}
class PriorityQueue {
  int comparator;
  int storage;
  int used;
  int serialVersionUID;
  int DEFAULT_CAPACITY;
}
class Observer {
}
class Observable {
  int observers;
  int changed;
}
class NoSuchElementException {
  int serialVersionUID;
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
class Map {
  class Entry {
  }
}
class Locale {
  int countryCache;
  int languageCache;
  int defaultLocale;
  int localeMap;
  int availableLocales;
  int hashcode;
  int variant;
  int country;
  int language;
  int serialVersionUID;
  int ROOT;
  int CANADA_FRENCH;
  int CANADA;
  int US;
  int UK;
  int TAIWAN;
  int PRC;
  int CHINA;
  int KOREA;
  int JAPAN;
  int ITALY;
  int GERMANY;
  int FRANCE;
  int TRADITIONAL_CHINESE;
  int SIMPLIFIED_CHINESE;
  int CHINESE;
  int KOREAN;
  int JAPANESE;
  int ITALIAN;
  int GERMAN;
  int FRENCH;
  int ENGLISH;
}
class ListResourceBundle {
}
class ListIterator {
}
class List {
}
class LinkedList {
  class LinkedListItr {
    int position;
    int lastReturned;
    int previous;
    int next;
    int knownMod;
  }
  class Entry {
    int previous;
    int next;
    int data;
  }
  int size;
  int last;
  int first;
  int serialVersionUID;
}
class LinkedHashSet {
  int serialVersionUID;
}
class LinkedHashMap {
  class LinkedHashEntry {
    int succ;
    int pred;
  }
  int accessOrder;
  int root;
  int serialVersionUID;
}
class Iterator {
}
class InvalidPropertiesFormatException {
  int serialVersionUID;
}
class InputMismatchException {
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
  class IdentityEntry {
    int knownMod;
    int loc;
  }
  class IdentityIterator {
    int loc;
    int count;
    int knownMod;
    int type;
  }
  int threshold;
  int entries;
  int modCount;
  int table;
  int size;
  int serialVersionUID;
  int nullslot;
  int DEFAULT_CAPACITY;
}
class Hashtable {
  class ValueEnumerator {
    int enumerator;
  }
  class KeyEnumerator {
    int enumerator;
  }
  class EntryEnumerator {
    int next;
    int idx;
    int count;
  }
  class ValueIterator {
    int iterator;
  }
  class KeyIterator {
    int iterator;
  }
  class EntryIterator {
    int next;
    int last;
    int idx;
    int count;
    int knownMod;
  }
  class HashEntry {
    int next;
  }
  int entries;
  int values;
  int keys;
  int size;
  int modCount;
  int buckets;
  int loadFactor;
  int threshold;
  int serialVersionUID;
  int DEFAULT_LOAD_FACTOR;
  int DEFAULT_CAPACITY;
}
class HashSet {
  int map;
  int serialVersionUID;
}
class HashMap {
  class HashIterator {
    int next;
    int last;
    int idx;
    int count;
    int knownMod;
    int type;
  }
  class HashEntry {
    int next;
  }
  int entries;
  int size;
  int modCount;
  int buckets;
  int loadFactor;
  int threshold;
  int serialVersionUID;
  int DEFAULT_LOAD_FACTOR;
  int DEFAULT_CAPACITY;
}
class GregorianCalendar {
  int maximums;
  int minimums;
  int EPOCH_DAYS;
  int serialVersionUID;
  int gregorianCutover;
  int AD;
  int BC;
}
class FormatterClosedException {
  int serialVersionUID;
}
class Formatter {
  class BigDecimalLayoutForm {
    int SCIENTIFIC;
    int DECIMAL_FLOAT;
  }
  int lineSeparator;
  int FLAGS;
  int fmtLocale;
  int length;
  int index;
  int format;
  int ioException;
  int closed;
  int locale;
  int out;
}
class FormattableFlags {
  int PAREN;
  int COMMA;
  int ZERO;
  int SPACE;
  int PLUS;
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
  int enumClass;
  int cardinality;
  int store;
  int serialVersionUID;
}
class EnumMap {
  int emptySlot;
  int entries;
  int enumClass;
  int cardinality;
  int store;
  int serialVersionUID;
}
class EmptyStackException {
  int serialVersionUID;
}
class DuplicateFormatFlagsException {
  int flags;
  int serialVersionUID;
}
class Dictionary {
}
class Date {
  int monthNames;
  int weekNames;
  int time;
  int serialVersionUID;
}
class Currency {
  int cache;
  int countryMap;
  int fractionDigits;
  int currencyCode;
  int properties;
  int serialVersionUID;
}
class ConcurrentModificationException {
  int serialVersionUID;
}
class Comparator {
}
class Collections {
  class MapSet {
    int map;
  }
  class LIFOQueue {
    int deque;
  }
  class CheckedSortedSet {
    int ss;
    int serialVersionUID;
  }
  class CheckedSortedMap {
    int sm;
    int serialVersionUID;
  }
  class CheckedSet {
    int serialVersionUID;
  }
  class CheckedMap {
    class CheckedEntrySet {
      int valueType;
      int keyType;
    }
    int values;
    int keys;
    int entries;
    int valueType;
    int keyType;
    int m;
    int serialVersionUID;
  }
  class CheckedListIterator {
    int li;
  }
  class CheckedRandomAccessList {
    int serialVersionUID;
  }
  class CheckedList {
    int list;
    int serialVersionUID;
  }
  class CheckedIterator {
    int type;
    int i;
  }
  class CheckedCollection {
    int type;
    int c;
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
      int serialVersionUID;
      class UnmodifiableMapEntry {
        int e;
      }
    }
    int values;
    int keys;
    int entries;
    int m;
    int serialVersionUID;
  }
  class UnmodifiableListIterator {
    int li;
  }
  class UnmodifiableRandomAccessList {
    int serialVersionUID;
  }
  class UnmodifiableList {
    int list;
    int serialVersionUID;
  }
  class UnmodifiableIterator {
    int i;
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
    int values;
    int keys;
    int entries;
    int mutex;
    int m;
    int serialVersionUID;
  }
  class SynchronizedListIterator {
    int li;
  }
  class SynchronizedRandomAccessList {
    int serialVersionUID;
  }
  class SynchronizedList {
    int list;
    int serialVersionUID;
  }
  class SynchronizedIterator {
    int i;
    int mutex;
  }
  class SynchronizedCollection {
    int mutex;
    int c;
    int serialVersionUID;
  }
  class SingletonMap {
    int entries;
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
  int defaultRandom;
  class ReverseComparator {
    int serialVersionUID;
  }
  int rcInstance;
  class CopiesList {
    int element;
    int n;
    int serialVersionUID;
  }
  class EmptyMap {
    int serialVersionUID;
  }
  int EMPTY_MAP;
  class EmptyList {
    int serialVersionUID;
  }
  int EMPTY_LIST;
  class EmptySet {
    int serialVersionUID;
  }
  int EMPTY_SET;
  int LARGE_LIST_SIZE;
}
class Collection {
}
class Calendar {
  int fieldNames;
  int ctorArgTypes;
  int cache;
  int properties;
  int bundleName;
  int serialVersionUID;
  int serialVersionOnStream;
  int explicitDSTOffset;
  int minimalDaysInFirstWeek;
  int firstDayOfWeek;
  int lenient;
  int calendarClassName;
  int zone;
  int areFieldsSet;
  int isTimeSet;
  int time;
  int isSet;
  int fields;
  int LONG;
  int SHORT;
  int ALL_STYLES;
  int PM;
  int AM;
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
  int SATURDAY;
  int FRIDAY;
  int THURSDAY;
  int WEDNESDAY;
  int TUESDAY;
  int MONDAY;
  int SUNDAY;
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
}
class BitSet {
  int bits;
  int LONG_MASK;
  int serialVersionUID;
}
class Arrays {
  class ArrayList {
    int a;
    int serialVersionUID;
  }
}
class ArrayList {
  int data;
  int size;
  int DEFAULT_CAPACITY;
  int serialVersionUID;
}
class AbstractSet {
}
class AbstractSequentialList {
}
class AbstractMap {
  class SimpleEntry {
    int value;
    int key;
    int serialVersionUID;
  }
  int values;
  int keys;
  int ENTRIES;
  int VALUES;
  int KEYS;
  class SimpleImmutableEntry {
    int value;
    int key;
    int serialVersionUID;
  }
}
class AbstractList {
  class RandomAccessSubList {
  }
  class SubList {
    int size;
    int offset;
    int backingList;
  }
  int modCount;
}
class AbstractCollection {
}
class Queue {
}
class NavigableSet {
}
class NavigableMap {
}
class Deque {
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
class AbstractQueue {
}
