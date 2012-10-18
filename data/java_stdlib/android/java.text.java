package java.text;
class StringCharacterIterator {
  int offset;
  int end;
  int start;
  int string;
}
class SimpleDateFormat {
  int serialPersistentFields;
  int defaultCenturyStart;
  int creationYear;
  int formatData;
  int pattern;
  int STAND_ALONE_DAY_OF_WEEK_FIELD;
  int STAND_ALONE_MONTH_FIELD;
  int RFC_822_TIMEZONE_FIELD;
  int PATTERN_CHARS;
  int serialVersionUID;
}
class RuleBasedCollator {
}
class RuleBasedBreakIterator {
}
class ParsePosition {
  int errorIndex;
  int currentPosition;
}
class ParseException {
  int errorOffset;
  int serialVersionUID;
}
class NumberFormat {
  class Field {
    int CURRENCY;
    int PERMILLE;
    int PERCENT;
    int GROUPING_SEPARATOR;
    int DECIMAL_SEPARATOR;
    int EXPONENT_SYMBOL;
    int EXPONENT_SIGN;
    int EXPONENT;
    int FRACTION;
    int INTEGER;
    int SIGN;
    int serialVersionUID;
  }
  int serialPersistentFields;
  int minimumFractionDigits;
  int maximumFractionDigits;
  int minimumIntegerDigits;
  int maximumIntegerDigits;
  int parseIntegerOnly;
  int groupingUsed;
  int FRACTION_FIELD;
  int INTEGER_FIELD;
  int serialVersionUID;
}
class Normalizer {
  class Form {
    int NFKC;
    int NFKD;
    int NFC;
    int NFD;
  }
}
class MessageFormat {
  class Field {
    int ARGUMENT;
    int serialVersionUID;
  }
  int serialPersistentFields;
  class FieldContainer {
    int value;
    int attribute;
    int end;
    int start;
  }
  int maxArgumentIndex;
  int maxOffset;
  int formats;
  int argumentNumbers;
  int strings;
  int locale;
  int serialVersionUID;
}
class Format {
  class Field {
    int serialVersionUID;
  }
  int serialVersionUID;
}
class FieldPosition {
  int myAttribute;
  int endIndex;
  int beginIndex;
  int myField;
}
class DecimalFormatSymbols {
  int serialPersistentFields;
  int exponentSeparator;
  int locale;
  int currency;
  int intlCurrencySymbol;
  int currencySymbol;
  int NaN;
  int infinity;
  int minusSign;
  int monetarySeparator;
  int perMill;
  int percent;
  int patternSeparator;
  int groupingSeparator;
  int decimalSeparator;
  int digit;
  int zeroDigit;
  int serialVersionUID;
}
class DecimalFormat {
  int serialPersistentFields;
  int NEGATIVE_ZERO_DOUBLE;
  int roundingMode;
  int finalizerGuardian;
  int dform;
  int symbols;
  int serialVersionUID;
}
class DateFormatSymbols {
  int locale;
  int customZoneStrings;
  int zoneStrings;
  int shortStandAloneWeekdays;
  int longStandAloneWeekdays;
  int shortStandAloneMonths;
  int longStandAloneMonths;
  int weekdays;
  int shortWeekdays;
  int shortMonths;
  int months;
  int eras;
  int ampms;
  int localPatternChars;
  int serialVersionUID;
}
class DateFormat {
  class Field {
    int calendarField;
    int TIME_ZONE;
    int HOUR1;
    int HOUR0;
    int AM_PM;
    int WEEK_OF_MONTH;
    int WEEK_OF_YEAR;
    int DAY_OF_WEEK_IN_MONTH;
    int DAY_OF_YEAR;
    int DAY_OF_MONTH;
    int DAY_OF_WEEK;
    int MILLISECOND;
    int SECOND;
    int MINUTE;
    int HOUR_OF_DAY1;
    int HOUR_OF_DAY0;
    int MONTH;
    int YEAR;
    int ERA;
    int table;
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
  int SHORT;
  int MEDIUM;
  int LONG;
  int FULL;
  int DEFAULT;
  int numberFormat;
  int calendar;
  int serialVersionUID;
}
class Collator {
  int icuColl;
  int IDENTICAL;
  int TERTIARY;
  int SECONDARY;
  int PRIMARY;
  int FULL_DECOMPOSITION;
  int CANONICAL_DECOMPOSITION;
  int NO_DECOMPOSITION;
}
class CollationKey {
  int source;
}
class CollationElementIterator {
  int icuIterator;
  int NULLORDER;
}
class ChoiceFormat {
  int choiceFormats;
  int choiceLimits;
  int serialVersionUID;
}
class CharacterIterator {
  int DONE;
}
class BreakIterator {
  int wrapped;
  int DONE;
}
class Bidi {
  int UBiDiDirection_UBIDI_MIXED;
  int UBiDiDirection_UBIDI_RTL;
  int UBiDiDirection_UBIDI_LTR;
  int UBIDI_LEVEL_OVERRIDE;
  int unidirectional;
  int direction;
  int runs;
  int offsetLevel;
  int length;
  int baseLevel;
  class Run {
    int level;
    int limit;
    int start;
  }
  int DIRECTION_RIGHT_TO_LEFT;
  int DIRECTION_LEFT_TO_RIGHT;
  int DIRECTION_DEFAULT_RIGHT_TO_LEFT;
  int DIRECTION_DEFAULT_LEFT_TO_RIGHT;
}
class AttributedString {
  class AttributedIterator {
    int attributesAllowed;
    int attrString;
    int offset;
    int end;
    int begin;
  }
  class Range {
    int value;
    int end;
    int start;
  }
  int attributeMap;
  int text;
}
class AttributedCharacterIterator {
  class Attribute {
    int name;
    int READING;
    int LANGUAGE;
    int INPUT_METHOD_SEGMENT;
    int serialVersionUID;
  }
}
class Annotation {
  int value;
}
