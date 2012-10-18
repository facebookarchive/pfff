package java.lang;
class Void {
  int TYPE;
}
class VirtualMachineError {
  int serialVersionUID;
}
class VerifyError {
  int serialVersionUID;
}
class VMThread {
  int STATE_MAP;
  int vmData;
  int thread;
}
class VMClassLoader {
}
class UnsupportedOperationException {
  int serialVersionUID;
}
class UnsupportedClassVersionError {
  int serialVersionUID;
}
class UnsatisfiedLinkError {
  int serialVersionUID;
}
class UnsafeByteSequence {
  int count;
  int bytes;
}
class UnknownError {
  int serialVersionUID;
}
class TypeNotPresentException {
  int typeName;
  int serialVersionUID;
}
class Throwable {
  int stackTrace;
  int stackState;
  int suppressedExceptions;
  int cause;
  int detailMessage;
  int serialVersionUID;
}
class ThreadLocal {
  class Values {
    int clean;
    int maximumLoad;
    int tombstones;
    int size;
    int mask;
    int table;
    int TOMBSTONE;
    int INITIAL_SIZE;
  }
  int hash;
  int hashCounter;
  int reference;
}
class ThreadGroup {
  int mMain;
  int mSystem;
  int isDestroyed;
  int isDaemon;
  int groups;
  int threads;
  int threadRefs;
  int parent;
  int maxPriority;
  int name;
}
class ThreadDeath {
  int serialVersionUID;
}
class Thread {
  class UncaughtExceptionHandler {
  }
  int parkBlocker;
  int parkState;
  int hasBeenStarted;
  int defaultUncaughtHandler;
  int uncaughtHandler;
  int contextClassLoader;
  int interruptActions;
  int inheritableValues;
  int localValues;
  int id;
  int count;
  int target;
  int stackSize;
  int priority;
  int name;
  int daemon;
  int group;
  int vmThread;
  int NORM_PRIORITY;
  int MIN_PRIORITY;
  int MAX_PRIORITY;
  class State {
    int TERMINATED;
    int TIMED_WAITING;
    int WAITING;
    int BLOCKED;
    int RUNNABLE;
    int NEW;
  }
  class ParkState {
    int PARKED;
    int PREEMPTIVELY_UNPARKED;
    int UNPARKED;
  }
  int NANOS_PER_MILLI;
}
class System {
  class SystemEnvironment {
    int map;
  }
  int systemProperties;
  int lineSeparator;
  int err;
  int out;
  int in;
}
class StringToReal {
  class StringExponentPair {
    int zero;
    int infinity;
    int negative;
    int e;
    int s;
  }
}
class StringIndexOutOfBoundsException {
  int serialVersionUID;
}
class StringBuilder {
  int serialVersionUID;
}
class StringBuffer {
  int serialPersistentFields;
  int serialVersionUID;
}
class String {
  int hashCode;
  int count;
  int offset;
  int value;
  int ASCII;
  int CASE_INSENSITIVE_ORDER;
  class CaseInsensitiveComparator {
    int serialVersionUID;
  }
  int REPLACEMENT_CHAR;
  int serialVersionUID;
}
class StrictMath {
  int PI;
  int E;
}
class StackTraceElement {
  int lineNumber;
  int fileName;
  int methodName;
  int declaringClass;
  int NATIVE_LINE_NUMBER;
  int serialVersionUID;
}
class StackOverflowError {
  int serialVersionUID;
}
class Short {
  int SMALL_VALUES;
  int TYPE;
  int SIZE;
  int MIN_VALUE;
  int MAX_VALUE;
  int value;
  int serialVersionUID;
}
class SecurityManager {
  int inCheck;
}
class SecurityException {
  int serialVersionUID;
}
class RuntimePermission {
}
class RuntimeException {
  int serialVersionUID;
}
class Runtime {
  int tracingMethods;
  int shuttingDown;
  int finalizeOnExit;
  int shutdownHooks;
  int mLibPaths;
  int mRuntime;
}
class Runnable {
}
class RealToString {
  int digitCount;
  int digits;
  int firstK;
  int invLogOfTenBaseTwo;
  int INSTANCE;
}
class Readable {
}
class ProcessManager {
  class ProcessOutputStream {
    int fd;
  }
  class ProcessInputStream {
    int fd;
  }
  int instance;
  class ProcessReferenceQueue {
  }
  class ProcessReference {
    int processId;
  }
  class ProcessImpl {
    int exitValueMutex;
    int exitValue;
    int outputStream;
    int inputStream;
    int errorStream;
    int pid;
  }
  int referenceQueue;
  int processReferences;
}
class ProcessBuilder {
  int redirectErrorStream;
  int environment;
  int directory;
  int command;
}
class Process {
}
class Package {
  int sealBase;
  int implVendor;
  int implVersion;
  int implTitle;
  int specVendor;
  int specVersion;
  int specTitle;
  int name;
  int NO_ANNOTATIONS;
}
class OutOfMemoryError {
  int serialVersionUID;
}
class Object {
}
class NumberFormatException {
  int serialVersionUID;
}
class Number {
  int serialVersionUID;
}
class NullPointerException {
  int serialVersionUID;
}
class NoSuchMethodException {
  int serialVersionUID;
}
class NoSuchMethodError {
  int serialVersionUID;
}
class NoSuchFieldException {
  int serialVersionUID;
}
class NoSuchFieldError {
  int serialVersionUID;
}
class NoClassDefFoundError {
  int serialVersionUID;
}
class NegativeArraySizeException {
  int serialVersionUID;
}
class Math {
  int random;
  int PI;
  int E;
}
class Long {
  int SMALL_VALUES;
  int SIZE;
  int TYPE;
  int MIN_VALUE;
  int MAX_VALUE;
  int value;
  int serialVersionUID;
}
class LinkageError {
  int serialVersionUID;
}
class Iterable {
}
class InterruptedException {
  int serialVersionUID;
}
class InternalError {
  int serialVersionUID;
}
class IntegralToString {
  int UPPER_CASE_DIGITS;
  int DIGITS;
  int MOD_10_TABLE;
  int ONES;
  int TENS;
  int SMALL_NEGATIVE_VALUES;
  int SMALL_NONNEGATIVE_VALUES;
  int BUFFER;
}
class Integer {
  int SMALL_VALUES;
  int TYPE;
  int NTZ_TABLE;
  int SIZE;
  int MIN_VALUE;
  int MAX_VALUE;
  int value;
  int serialVersionUID;
}
class InstantiationException {
  int serialVersionUID;
}
class InstantiationError {
  int serialVersionUID;
}
class InheritableThreadLocal {
}
class IndexOutOfBoundsException {
  int serialVersionUID;
}
class IncompatibleClassChangeError {
  int serialVersionUID;
}
class IllegalThreadStateException {
  int serialVersionUID;
}
class IllegalStateException {
  int serialVersionUID;
}
class IllegalMonitorStateException {
  int serialVersionUID;
}
class IllegalArgumentException {
  int serialVersionUID;
}
class IllegalAccessException {
  int serialVersionUID;
}
class IllegalAccessError {
  int serialVersionUID;
}
class HexStringParser {
  int abandonedNumber;
  int mantissa;
  int exponent;
  int sign;
  int MANTISSA_MASK;
  int MIN_EXPONENT;
  int MAX_EXPONENT;
  int EXPONENT_BASE;
  int MANTISSA_WIDTH;
  int EXPONENT_WIDTH;
  int PATTERN;
  int HEX_PATTERN;
  int FLOAT_TYPE_SUFFIX;
  int BINARY_EXPONENT;
  int HEX_SIGNIFICANT;
  int MAX_SIGNIFICANT_LENGTH;
  int HEX_RADIX;
  int FLOAT_MANTISSA_WIDTH;
  int FLOAT_EXPONENT_WIDTH;
  int DOUBLE_MANTISSA_WIDTH;
  int DOUBLE_EXPONENT_WIDTH;
}
class Float {
  int SIZE;
  int TYPE;
  int MIN_EXPONENT;
  int MAX_EXPONENT;
  int MIN_NORMAL;
  int NEGATIVE_INFINITY;
  int POSITIVE_INFINITY;
  int NaN;
  int MIN_VALUE;
  int MAX_VALUE;
  int value;
  int serialVersionUID;
  int MANTISSA_MASK;
  int EXPONENT_MASK;
  int SIGN_MASK;
  int NON_MANTISSA_BITS;
  int MANTISSA_BITS;
  int EXPONENT_BITS;
  int EXPONENT_BIAS;
}
class ExceptionInInitializerError {
  int exception;
  int serialVersionUID;
}
class Exception {
  int serialVersionUID;
}
class Error {
  int serialVersionUID;
}
class EnumConstantNotPresentException {
  int constantName;
  int enumType;
  int serialVersionUID;
}
class Enum {
  int ordinal;
  int name;
  int sharedConstantsCache;
  int serialVersionUID;
}
class Double {
  int SIZE;
  int TYPE;
  int MIN_EXPONENT;
  int MAX_EXPONENT;
  int MIN_NORMAL;
  int NEGATIVE_INFINITY;
  int POSITIVE_INFINITY;
  int NaN;
  int MIN_VALUE;
  int MAX_VALUE;
  int value;
  int serialVersionUID;
  int MANTISSA_MASK;
  int EXPONENT_MASK;
  int SIGN_MASK;
  int NON_MANTISSA_BITS;
  int MANTISSA_BITS;
  int EXPONENT_BITS;
  int EXPONENT_BIAS;
}
class Daemons {
  class FinalizerWatchdogDaemon {
    int INSTANCE;
  }
  class FinalizerDaemon {
    int finalizingStartedNanos;
    int finalizingObject;
    int queue;
    int INSTANCE;
  }
  class ReferenceQueueDaemon {
    int INSTANCE;
  }
  class Daemon {
    int thread;
  }
  int MAX_FINALIZE_MILLIS;
  int NANOS_PER_MILLI;
}
class Compiler {
}
class Comparable {
}
class Cloneable {
}
class CloneNotSupportedException {
  int serialVersionUID;
}
class ClassNotFoundException {
  int ex;
  int serialVersionUID;
}
class BootClassLoader {
  int instance;
}
class TwoEnumerationsInOne {
  int second;
  int first;
}
class ClassLoader {
  int packages;
  int parent;
  class SystemClassLoader {
    int loader;
  }
}
class ClassFormatError {
  int serialVersionUID;
}
class ClassCircularityError {
  int serialVersionUID;
}
class ClassCastException {
  int serialVersionUID;
}
class Class {
  int name;
  int serialVersionUID;
}
class Character {
  int SMALL_VALUES;
  class UnicodeBlock {
    int BLOCKS;
    int SUPPLEMENTARY_PRIVATE_USE_AREA_B;
    int SUPPLEMENTARY_PRIVATE_USE_AREA_A;
    int VARIATION_SELECTORS_SUPPLEMENT;
    int TAGS;
    int CJK_COMPATIBILITY_IDEOGRAPHS_SUPPLEMENT;
    int CJK_UNIFIED_IDEOGRAPHS_EXTENSION_B;
    int MATHEMATICAL_ALPHANUMERIC_SYMBOLS;
    int TAI_XUAN_JING_SYMBOLS;
    int MUSICAL_SYMBOLS;
    int BYZANTINE_MUSICAL_SYMBOLS;
    int CYPRIOT_SYLLABARY;
    int OSMANYA;
    int SHAVIAN;
    int DESERET;
    int UGARITIC;
    int GOTHIC;
    int OLD_ITALIC;
    int AEGEAN_NUMBERS;
    int LINEAR_B_IDEOGRAMS;
    int LINEAR_B_SYLLABARY;
    int SPECIALS;
    int HALFWIDTH_AND_FULLWIDTH_FORMS;
    int ARABIC_PRESENTATION_FORMS_B;
    int SMALL_FORM_VARIANTS;
    int CJK_COMPATIBILITY_FORMS;
    int COMBINING_HALF_MARKS;
    int VARIATION_SELECTORS;
    int ARABIC_PRESENTATION_FORMS_A;
    int ALPHABETIC_PRESENTATION_FORMS;
    int CJK_COMPATIBILITY_IDEOGRAPHS;
    int PRIVATE_USE_AREA;
    int LOW_SURROGATES;
    int HIGH_PRIVATE_USE_SURROGATES;
    int HIGH_SURROGATES;
    int HANGUL_SYLLABLES;
    int YI_RADICALS;
    int YI_SYLLABLES;
    int CJK_UNIFIED_IDEOGRAPHS;
    int YIJING_HEXAGRAM_SYMBOLS;
    int CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A;
    int CJK_COMPATIBILITY;
    int ENCLOSED_CJK_LETTERS_AND_MONTHS;
    int KATAKANA_PHONETIC_EXTENSIONS;
    int BOPOMOFO_EXTENDED;
    int KANBUN;
    int HANGUL_COMPATIBILITY_JAMO;
    int BOPOMOFO;
    int KATAKANA;
    int HIRAGANA;
    int CJK_SYMBOLS_AND_PUNCTUATION;
    int IDEOGRAPHIC_DESCRIPTION_CHARACTERS;
    int KANGXI_RADICALS;
    int CJK_RADICALS_SUPPLEMENT;
    int MISCELLANEOUS_SYMBOLS_AND_ARROWS;
    int SUPPLEMENTAL_MATHEMATICAL_OPERATORS;
    int MISCELLANEOUS_MATHEMATICAL_SYMBOLS_B;
    int SUPPLEMENTAL_ARROWS_B;
    int BRAILLE_PATTERNS;
    int SUPPLEMENTAL_ARROWS_A;
    int MISCELLANEOUS_MATHEMATICAL_SYMBOLS_A;
    int DINGBATS;
    int MISCELLANEOUS_SYMBOLS;
    int GEOMETRIC_SHAPES;
    int BLOCK_ELEMENTS;
    int BOX_DRAWING;
    int ENCLOSED_ALPHANUMERICS;
    int OPTICAL_CHARACTER_RECOGNITION;
    int CONTROL_PICTURES;
    int MISCELLANEOUS_TECHNICAL;
    int MATHEMATICAL_OPERATORS;
    int ARROWS;
    int NUMBER_FORMS;
    int LETTERLIKE_SYMBOLS;
    int COMBINING_MARKS_FOR_SYMBOLS;
    int CURRENCY_SYMBOLS;
    int SUPERSCRIPTS_AND_SUBSCRIPTS;
    int GENERAL_PUNCTUATION;
    int GREEK_EXTENDED;
    int LATIN_EXTENDED_ADDITIONAL;
    int PHONETIC_EXTENSIONS;
    int KHMER_SYMBOLS;
    int TAI_LE;
    int LIMBU;
    int MONGOLIAN;
    int KHMER;
    int TAGBANWA;
    int BUHID;
    int HANUNOO;
    int TAGALOG;
    int RUNIC;
    int OGHAM;
    int UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS;
    int CHEROKEE;
    int ETHIOPIC;
    int HANGUL_JAMO;
    int GEORGIAN;
    int MYANMAR;
    int TIBETAN;
    int LAO;
    int THAI;
    int SINHALA;
    int MALAYALAM;
    int KANNADA;
    int TELUGU;
    int TAMIL;
    int ORIYA;
    int GUJARATI;
    int GURMUKHI;
    int BENGALI;
    int DEVANAGARI;
    int THAANA;
    int SYRIAC;
    int ARABIC;
    int HEBREW;
    int ARMENIAN;
    int CYRILLIC_SUPPLEMENTARY;
    int CYRILLIC;
    int GREEK;
    int COMBINING_DIACRITICAL_MARKS;
    int SPACING_MODIFIER_LETTERS;
    int IPA_EXTENSIONS;
    int LATIN_EXTENDED_B;
    int LATIN_EXTENDED_A;
    int LATIN_1_SUPPLEMENT;
    int BASIC_LATIN;
    int SURROGATES_AREA;
  }
  class Subset {
    int name;
  }
  int DIRECTIONALITY;
  int SIZE;
  int MAX_CODE_POINT;
  int MIN_CODE_POINT;
  int MIN_SUPPLEMENTARY_CODE_POINT;
  int MAX_SURROGATE;
  int MIN_SURROGATE;
  int MAX_LOW_SURROGATE;
  int MIN_LOW_SURROGATE;
  int MAX_HIGH_SURROGATE;
  int MIN_HIGH_SURROGATE;
  int DIRECTIONALITY_POP_DIRECTIONAL_FORMAT;
  int DIRECTIONALITY_RIGHT_TO_LEFT_OVERRIDE;
  int DIRECTIONALITY_RIGHT_TO_LEFT_EMBEDDING;
  int DIRECTIONALITY_LEFT_TO_RIGHT_OVERRIDE;
  int DIRECTIONALITY_LEFT_TO_RIGHT_EMBEDDING;
  int DIRECTIONALITY_OTHER_NEUTRALS;
  int DIRECTIONALITY_WHITESPACE;
  int DIRECTIONALITY_SEGMENT_SEPARATOR;
  int DIRECTIONALITY_PARAGRAPH_SEPARATOR;
  int DIRECTIONALITY_BOUNDARY_NEUTRAL;
  int DIRECTIONALITY_NONSPACING_MARK;
  int DIRECTIONALITY_COMMON_NUMBER_SEPARATOR;
  int DIRECTIONALITY_ARABIC_NUMBER;
  int DIRECTIONALITY_EUROPEAN_NUMBER_TERMINATOR;
  int DIRECTIONALITY_EUROPEAN_NUMBER_SEPARATOR;
  int DIRECTIONALITY_EUROPEAN_NUMBER;
  int DIRECTIONALITY_RIGHT_TO_LEFT_ARABIC;
  int DIRECTIONALITY_RIGHT_TO_LEFT;
  int DIRECTIONALITY_LEFT_TO_RIGHT;
  int DIRECTIONALITY_UNDEFINED;
  int FINAL_QUOTE_PUNCTUATION;
  int INITIAL_QUOTE_PUNCTUATION;
  int OTHER_SYMBOL;
  int MODIFIER_SYMBOL;
  int CURRENCY_SYMBOL;
  int MATH_SYMBOL;
  int OTHER_PUNCTUATION;
  int CONNECTOR_PUNCTUATION;
  int END_PUNCTUATION;
  int START_PUNCTUATION;
  int DASH_PUNCTUATION;
  int SURROGATE;
  int PRIVATE_USE;
  int FORMAT;
  int CONTROL;
  int PARAGRAPH_SEPARATOR;
  int LINE_SEPARATOR;
  int SPACE_SEPARATOR;
  int OTHER_NUMBER;
  int LETTER_NUMBER;
  int DECIMAL_DIGIT_NUMBER;
  int COMBINING_SPACING_MARK;
  int ENCLOSING_MARK;
  int NON_SPACING_MARK;
  int OTHER_LETTER;
  int MODIFIER_LETTER;
  int TITLECASE_LETTER;
  int LOWERCASE_LETTER;
  int UPPERCASE_LETTER;
  int UNASSIGNED;
  int TYPE;
  int MAX_RADIX;
  int MIN_RADIX;
  int MAX_VALUE;
  int MIN_VALUE;
  int value;
  int serialVersionUID;
}
class CharSequence {
}
class CaseMapper {
  int GREEK_SMALL_FINAL_SIGMA;
  int GREEK_CAPITAL_SIGMA;
  int LATIN_CAPITAL_I_WITH_DOT;
  int upperValues2;
  int upperValues;
}
class Byte {
  int VALUES;
  int TYPE;
  int SIZE;
  int MIN_VALUE;
  int MAX_VALUE;
  int value;
  int serialVersionUID;
}
class Boolean {
  int FALSE;
  int TRUE;
  int TYPE;
  int value;
  int serialVersionUID;
}
class AutoCloseable {
}
class AssertionError {
  int serialVersionUID;
}
class ArrayStoreException {
  int serialVersionUID;
}
class ArrayIndexOutOfBoundsException {
  int serialVersionUID;
}
class ArithmeticException {
  int serialVersionUID;
}
class Appendable {
}
class AbstractStringBuilder {
  int shared;
  int count;
  int value;
  int INITIAL_CAPACITY;
}
class AbstractMethodError {
  int serialVersionUID;
}
