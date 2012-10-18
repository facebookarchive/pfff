package java.text;
class StringCharacterIterator {
  int index;
  int end;
  int begin;
  int text;
}
class SimpleDateFormat {
  int RFC822_TIMEZONE_FIELD;
  int standardChars;
  int serialVersionUID;
  int serialVersionOnStream;
  int pattern;
  int defaultCentury;
  int defaultCenturyStart;
  int formatData;
  int tokens;
  class CompiledField {
    int character;
    int size;
    int field;
  }
}
class RuleBasedCollator {
  int SPECIAL_UNKNOWN_SEQ;
  int inverseAccentComparison;
  int last_tertiary_value;
  int last_primary_value;
  int prefix_tree;
  int ce_table;
  int rules;
  class CollationSorter {
    int expansionOrdering;
    int ignore;
    int offset;
    int hashText;
    int textElement;
    int comparisonType;
    int INVERSE_SECONDARY;
    int RESET;
    int EQUAL;
    int GREATERT;
    int GREATERS;
    int GREATERP;
  }
  class CollationElement {
    int expansion;
    int ignore;
    int equality;
    int tertiary;
    int secondary;
    int primary;
    int key;
  }
}
class ParsePosition {
  int error_index;
  int index;
}
class ParseException {
  int errorOffset;
  int serialVersionUID;
}
class NumberFormat {
  int serialVersionUID;
  int serialVersionOnStream;
  int parseIntegerOnly;
  int minIntegerDigits;
  int minimumIntegerDigits;
  int minFractionDigits;
  int minimumFractionDigits;
  int maxIntegerDigits;
  int maximumIntegerDigits;
  int maxFractionDigits;
  int maximumFractionDigits;
  int groupingUsed;
  class Field {
    int allFields;
    int EXPONENT_SIGN;
    int CURRENCY;
    int PERMILLE;
    int PERCENT;
    int EXPONENT_SYMBOL;
    int GROUPING_SEPARATOR;
    int SIGN;
    int DECIMAL_SEPARATOR;
    int EXPONENT;
    int FRACTION;
    int INTEGER;
    int serialVersionUID;
  }
  int FRACTION_FIELD;
  int INTEGER_FIELD;
}
class MessageFormat {
  int leader;
  int elements;
  int locale;
  int pattern;
  class Field {
    int ARGUMENT;
    int serialVersionUID;
  }
  int serialVersionUID;
  class MessageFormatElement {
    int trailer;
    int style;
    int type;
    int formatClass;
    int format;
    int setFormat;
    int argNumber;
  }
}
class Format {
  class Field {
    int serialVersionUID;
  }
  int serialVersionUID;
}
class FieldPosition {
  int field_attribute;
  int end;
  int begin;
  int field_id;
}
class DecimalFormatSymbols {
  int serialVersionUID;
  int currency;
  int locale;
  int zeroDigit;
  int serialVersionOnStream;
  int perMill;
  int percent;
  int patternSeparator;
  int NaN;
  int monetarySeparator;
  int minusSign;
  int intlCurrencySymbol;
  int infinity;
  int groupingSeparator;
  int exponential;
  int digit;
  int decimalSeparator;
  int currencySymbol;
}
class DecimalFormat {
  int attributes;
  int hasFractionalPattern;
  int hasNegativePrefix;
  int maxIntegerDigitsExponent;
  int useExponentialNotation;
  int symbols;
  int positiveSuffix;
  int positivePrefix;
  int negativeSuffix;
  int negativePrefix;
  int negativePatternMultiplier;
  int multiplier;
  int exponentRound;
  int minExponentDigits;
  int groupingSize;
  int groupingSeparatorInPattern;
  int showDecimalSeparator;
  int decimalSeparatorAlwaysShown;
  int useCurrencySeparator;
  int parseBigDecimal;
  int nonLocalizedSymbols;
  int DEFAULT_FRACTION_DIGITS;
  int DEFAULT_INTEGER_DIGITS;
  int serialVersionUID;
}
class DateFormatSymbols {
  int timeFormats;
  int dateFormats;
  int formatPrefixes;
  int serialVersionUID;
  int zoneStrings;
  int runtimeZoneStrings;
  int weekdays;
  int shortWeekdays;
  int shortMonths;
  int months;
  int localPatternChars;
  int eras;
  int ampms;
  int dataCache;
  class DFSData {
    int runtimeZoneStrings;
    int timeFormats;
    int dateFormats;
    int shortWeekdays;
    int weekdays;
    int shortMonths;
    int months;
    int localPatternChars;
    int eras;
    int ampms;
  }
  int FIELD_SEP;
  int ZONE_SEP;
  int properties;
}
class DateFormat {
  class Field {
    int allFields;
    int TIME_ZONE;
    int HOUR0;
    int HOUR1;
    int AM_PM;
    int WEEK_OF_MONTH;
    int WEEK_OF_YEAR;
    int DAY_OF_WEEK_IN_MONTH;
    int DAY_OF_YEAR;
    int DAY_OF_WEEK;
    int MILLISECOND;
    int SECOND;
    int MINUTE;
    int HOUR_OF_DAY0;
    int HOUR_OF_DAY1;
    int DAY_OF_MONTH;
    int MONTH;
    int YEAR;
    int ERA;
    int calendarField;
    int serialVersionUID;
  }
  int TIMEZONE_FIELD;
  int HOUR0_FIELD;
  int HOUR1_FIELD;
  int AM_PM_FIELD;
  int WEEK_OF_MONTH_FIELD;
  int WEEK_OF_YEAR_FIELD;
  int DAY_OF_WEEK_IN_MONTH_FIELD;
  int DAY_OF_YEAR_FIELD;
  int DAY_OF_WEEK_FIELD;
  int MILLISECOND_FIELD;
  int SECOND_FIELD;
  int MINUTE_FIELD;
  int HOUR_OF_DAY0_FIELD;
  int HOUR_OF_DAY1_FIELD;
  int DATE_FIELD;
  int MONTH_FIELD;
  int YEAR_FIELD;
  int ERA_FIELD;
  int DEFAULT;
  int SHORT;
  int MEDIUM;
  int LONG;
  int FULL;
  int numberFormat;
  int calendar;
  int serialVersionUID;
}
class Collator {
  int strength;
  int decmp;
  int FULL_DECOMPOSITION;
  int CANONICAL_DECOMPOSITION;
  int NO_DECOMPOSITION;
  int IDENTICAL;
  int TERTIARY;
  int SECONDARY;
  int PRIMARY;
}
class CollationKey {
  int key;
  int originalText;
  int collator;
}
class CollationElementIterator {
  int textIndexes;
  int textDecomposition;
  int textIndex;
  int index;
  int text;
  int collator;
  int NULLORDER;
}
class ChoiceFormat {
  int serialVersionUID;
  int exponentBits;
  int mantissaBits;
  int choiceLimits;
  int choiceFormats;
}
class CharacterIterator {
  int DONE;
}
class BreakIterator {
  int DONE;
}
class Bidi {
  int resultFlags;
  int runs;
  int formatterIndices;
  int levels;
  int types;
  int baseEmbedding;
  int flags;
  int length;
  int embeddingOffset;
  int embeddings;
  int textOffset;
  int text;
  int RTOL;
  int LTOR;
  int DIRECTION_RIGHT_TO_LEFT;
  int DIRECTION_LEFT_TO_RIGHT;
  int DIRECTION_DEFAULT_RIGHT_TO_LEFT;
  int DIRECTION_DEFAULT_LEFT_TO_RIGHT;
}
class AttributedStringIterator {
  int restricts;
  int attribs;
  int ci;
}
class AttributedString {
  int attribs;
  int sci;
  class AttributeRange {
    int endIndex;
    int beginIndex;
    int attribs;
  }
}
class AttributedCharacterIterator {
  class Attribute {
    int name;
    int INPUT_METHOD_SEGMENT;
    int READING;
    int LANGUAGE;
    int serialVersionUID;
  }
}
class Annotation {
  int attrib;
}
