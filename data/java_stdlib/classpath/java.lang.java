package java.lang;
class VMThrowable {
  int vmdata;
}
class VMThread {
  int vmdata;
  int running;
  int thread;
}
class VMSystem {
}
class VMString {
  int internTable;
}
class VMRuntime {
}
class VMProcess {
  class ProcessThread {
    int activeMap;
    int MAX_REAP_DELAY;
  }
  int redirect;
  int exitValue;
  int stderr;
  int stdout;
  int stdin;
  int pid;
  int exception;
  int dir;
  int env;
  int cmd;
  int state;
  int reapedExitValue;
  int reapedPid;
  int workList;
  int processThread;
  int TERMINATED;
  int RUNNING;
  int INITIAL;
}
class VMObject {
}
class VMMath {
}
class VMFloat {
}
class VMDouble {
}
class VMCompiler {
}
class VMClassLoader {
  int instrumenter;
  int bootjars;
  int definedPackages;
}
class VMClass {
}
class Void {
  int TYPE;
}
class VirtualMachineError {
  int serialVersionUID;
}
class VerifyError {
  int serialVersionUID;
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
class UnknownError {
  int serialVersionUID;
}
class TypeNotPresentException {
  int typeName;
  int serialVersionUID;
}
class Throwable {
  int vmState;
  class StaticData {
    int nl;
  }
  int stackTrace;
  int cause;
  int detailMessage;
  int serialVersionUID;
}
class ThreadLocalMap {
  class Entry {
    int value;
  }
  int count;
  int hashMask;
  int entries;
  int deletedEntry;
  int TARGET_OCCUPANCY;
  int MAX_OCCUPANCY;
  int LOG_INITIAL_SIZE;
}
class ThreadLocal {
  int fastHash;
  int nextHashBase;
  int sentinel;
}
class ThreadGroup {
  int maxpri;
  int daemon_flag;
  int groups;
  int threads;
  int name;
  int parent;
  int had_uncaught_exception;
  int root;
}
class ThreadDeath {
  int serialVersionUID;
}
class Thread {
  class State {
    int WAITING;
    int TIMED_WAITING;
    int TERMINATED;
    int RUNNABLE;
    int NEW;
    int BLOCKED;
    int serialVersionUID;
  }
  class UncaughtExceptionHandler {
  }
  int exceptionHandler;
  int locals;
  int defaultHandler;
  int totalThreadsCreated;
  int numAnonymousThreadsCreated;
  int parkBlocker;
  int threadId;
  int contextClassLoaderIsSystemClassLoader;
  int contextClassLoader;
  int stillborn;
  int stacksize;
  int priority;
  int daemon;
  int name;
  int runnable;
  int group;
  int vmThread;
  int MAX_PRIORITY;
  int NORM_PRIORITY;
  int MIN_PRIORITY;
}
class System {
  class EnvironmentSet {
  }
  class EnvironmentMap {
    int values;
    int keys;
    int entries;
  }
  class EnvironmentCollection {
    int c;
  }
  int environmentMap;
  int err;
  int LINE_SEPARATOR;
  int out;
  int in;
}
class StringIndexOutOfBoundsException {
  int serialVersionUID;
}
class StringBuilder {
  int serialVersionUID;
}
class StringBuffer {
  int shared;
  int serialVersionUID;
}
class String {
  int CASE_INSENSITIVE_ORDER;
  class CaseInsensitiveComparator {
    int serialVersionUID;
  }
  int offset;
  int cachedHashCode;
  int count;
  int value;
  int upperSpecial;
  int upperExpand;
  int serialVersionUID;
}
class StrictMath {
  int EXPM1_Q5;
  int EXPM1_Q4;
  int EXPM1_Q3;
  int EXPM1_Q2;
  int EXPM1_Q1;
  int CBRT_G;
  int CBRT_F;
  int CBRT_E;
  int CBRT_D;
  int CBRT_C;
  int CBRT_B2;
  int CBRT_B1;
  int AT10;
  int AT9;
  int AT8;
  int AT7;
  int AT6;
  int AT5;
  int AT4;
  int AT3;
  int AT2;
  int AT1;
  int AT0;
  int ATAN_1_5L;
  int ATAN_1_5H;
  int ATAN_0_5L;
  int ATAN_0_5H;
  int QS4;
  int QS3;
  int QS2;
  int QS1;
  int PS5;
  int PS4;
  int PS3;
  int PS2;
  int PS1;
  int PS0;
  int T12;
  int T11;
  int T10;
  int T9;
  int T8;
  int T7;
  int T6;
  int T5;
  int T4;
  int T3;
  int T2;
  int T1;
  int T0;
  int C6;
  int C5;
  int C4;
  int C3;
  int C2;
  int C1;
  int S6;
  int S5;
  int S4;
  int S3;
  int S2;
  int S1;
  int OVT;
  int DP_L;
  int DP_H;
  int P5;
  int P4;
  int P3;
  int P2;
  int P1;
  int L6;
  int L5;
  int L4;
  int L3;
  int L2;
  int L1;
  int LG7;
  int LG6;
  int LG5;
  int LG4;
  int LG3;
  int LG2;
  int LG1;
  int INV_LN2_L;
  int INV_LN2_H;
  int INV_LN2;
  int LN2_L;
  int LN2_H;
  int LN2;
  int CP_L;
  int CP_H;
  int CP;
  int EXP_LIMIT_L;
  int EXP_LIMIT_H;
  int SQRT_3;
  int SQRT_2;
  int SQRT_1_5;
  int PIO2_3L;
  int PIO2_3;
  int PIO2_2L;
  int PIO2_2;
  int PIO2_1L;
  int PIO2_1;
  int PI_L;
  int PI_OVER_TWO;
  int TWO_OVER_PI;
  int TWO_1023;
  int TWO_66;
  int TWO_64;
  int TWO_60;
  int TWO_57;
  int TWO_54;
  int TWO_52;
  int TWO_49;
  int TWO_31;
  int TWO_29;
  int TWO_28;
  int TWO_27;
  int TWO_24;
  int TWO_20;
  int TWO_16;
  int PI;
  int E;
  int rand;
}
class StackTraceElement {
  int isNative;
  int methodName;
  int declaringClass;
  int lineNumber;
  int fileName;
  int serialVersionUID;
}
class StackOverflowError {
  int serialVersionUID;
}
class Short {
  int value;
  int shortCache;
  int MAX_CACHE;
  int MIN_CACHE;
  int SIZE;
  int TYPE;
  int MAX_VALUE;
  int MIN_VALUE;
  int serialVersionUID;
}
class SecurityManager {
  int inCheck;
  int current;
}
class SecurityException {
  int serialVersionUID;
}
class RuntimePermission {
  int serialVersionUID;
}
class RuntimeException {
  int serialVersionUID;
}
class Runtime {
  int current;
  int shutdownHooks;
  int exitSequence;
  int libpath;
}
class Runnable {
}
class ReflectiveOperationException {
  int serialVersionUID;
}
class Readable {
}
class ProcessBuilder {
  int redirect;
  int environment;
  int command;
  int directory;
}
class Process {
}
class Package {
  int loader;
  int sealed;
  int specVersion;
  int specVendor;
  int specTitle;
  int implVersion;
  int implVendor;
  int implTitle;
  int name;
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
  int digits;
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
  int PI;
  int E;
  int rand;
}
class Long {
  int value;
  int longCache;
  int MAX_CACHE;
  int MIN_CACHE;
  int SIZE;
  int TYPE;
  int MAX_VALUE;
  int MIN_VALUE;
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
class Integer {
  int value;
  int intCache;
  int MAX_CACHE;
  int MIN_CACHE;
  int SIZE;
  int TYPE;
  int MAX_VALUE;
  int MIN_VALUE;
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
class Float {
  int value;
  int ONE;
  int ZERO;
  int SIZE;
  int TYPE;
  int NaN;
  int POSITIVE_INFINITY;
  int NEGATIVE_INFINITY;
  int MIN_VALUE;
  int MAX_VALUE;
  int serialVersionUID;
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
  int serialVersionUID;
}
class Double {
  int value;
  int ONE;
  int ZERO;
  int TYPE;
  int SIZE;
  int NaN;
  int POSITIVE_INFINITY;
  int NEGATIVE_INFINITY;
  int MIN_VALUE;
  int MAX_VALUE;
  int serialVersionUID;
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
class ClassLoader {
  int vmdata;
  int classAssertionStatus;
  int packageAssertionStatus;
  int defaultAssertionStatus;
  class StaticData {
    int systemClassAssertionStatus;
    int systemPackageAssertionStatus;
    int defaultProtectionDomain;
    int systemClassLoader;
  }
  int initialized;
  int parent;
  int definedPackages;
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
  class MethodKey {
    int hash;
    int returnType;
    int params;
    int name;
  }
  int constructor;
  int vmdata;
  class StaticData {
    int unknownProtectionDomain;
  }
  int pd;
  int signers;
  int ENUM;
  int ANNOTATION;
  int SYNTHETIC;
  int serialVersionUID;
}
class Character {
  int MIRROR_MASK;
  int NO_BREAK_MASK;
  int TYPE_MASK;
  int title;
  int direction;
  int lower;
  int upper;
  int numValue;
  int data;
  int blocks;
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
  int OTHER_SYMBOL;
  int MODIFIER_SYMBOL;
  int CURRENCY_SYMBOL;
  int MATH_SYMBOL;
  int OTHER_PUNCTUATION;
  int FINAL_QUOTE_PUNCTUATION;
  int INITIAL_QUOTE_PUNCTUATION;
  int END_PUNCTUATION;
  int START_PUNCTUATION;
  int DASH_PUNCTUATION;
  int CONNECTOR_PUNCTUATION;
  int OTHER_LETTER;
  int MODIFIER_LETTER;
  int UNASSIGNED;
  int PRIVATE_USE;
  int SURROGATE;
  int FORMAT;
  int CONTROL;
  int PARAGRAPH_SEPARATOR;
  int LINE_SEPARATOR;
  int SPACE_SEPARATOR;
  int OTHER_NUMBER;
  int LETTER_NUMBER;
  int DECIMAL_DIGIT_NUMBER;
  int ENCLOSING_MARK;
  int COMBINING_SPACING_MARK;
  int NON_SPACING_MARK;
  int TITLECASE_LETTER;
  int LOWERCASE_LETTER;
  int UPPERCASE_LETTER;
  int charCache;
  int MAX_CACHE;
  int SIZE;
  int TYPE;
  int MIN_SUPPLEMENTARY_CODE_POINT;
  int MAX_SURROGATE;
  int MIN_SURROGATE;
  int MAX_LOW_SURROGATE;
  int MIN_LOW_SURROGATE;
  int MAX_HIGH_SURROGATE;
  int MIN_HIGH_SURROGATE;
  int MAX_CODE_POINT;
  int MIN_CODE_POINT;
  int MAX_VALUE;
  int MIN_VALUE;
  int MAX_RADIX;
  int MIN_RADIX;
  int serialVersionUID;
  int value;
  class UnassignedCharacters {
  }
  class PrivateUseCharacters {
  }
  class UnicodeBlock {
    int sets;
    int SURROGATES_AREA;
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
    class NameType {
      int CONSTANT;
      int NO_SPACES;
      int CANONICAL;
    }
    int canonicalName;
    int end;
    int start;
  }
  class Subset {
    int name;
  }
}
class CharSequence {
}
class Byte {
  int value;
  int byteCache;
  int SIZE;
  int TYPE;
  int MAX_VALUE;
  int MIN_VALUE;
  int serialVersionUID;
}
class Boolean {
  int value;
  int TYPE;
  int FALSE;
  int TRUE;
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
class AbstractStringBuffer {
  int DEFAULT_CAPACITY;
  int value;
  int count;
}
class AbstractMethodError {
  int serialVersionUID;
}
