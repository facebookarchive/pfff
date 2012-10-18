package gnu.xml.validation.datatype;
class WhiteSpaceFacet {
  int fixed;
  int value;
  int COLLAPSE;
  int REPLACE;
  int PRESERVE;
}
class UnsignedShortType {
  int LENGTH;
  int MAX_VALUE;
  int CONSTRAINING_FACETS;
}
class UnsignedLongType {
  int LENGTH;
  int MAX_VALUE;
  int CONSTRAINING_FACETS;
}
class UnsignedIntType {
  int LENGTH;
  int MAX_VALUE;
  int CONSTRAINING_FACETS;
}
class UnsignedByteType {
  int LENGTH;
  int MAX_VALUE;
  int CONSTRAINING_FACETS;
}
class UnionSimpleType {
  int memberTypes;
}
class TypeLibraryFactory {
}
class TypeLibrary {
  int byName;
  int POSITIVE_INTEGER;
  int UNSIGNED_BYTE;
  int UNSIGNED_SHORT;
  int UNSIGNED_INT;
  int UNSIGNED_LONG;
  int NON_NEGATIVE_INTEGER;
  int BYTE;
  int SHORT;
  int INT;
  int LONG;
  int NEGATIVE_INTEGER;
  int NON_POSITIVE_INTEGER;
  int INTEGER;
  int ENTITIES;
  int ENTITY;
  int IDREFS;
  int IDREF;
  int ID;
  int NCNAME;
  int NAME;
  int NMTOKENS;
  int NMTOKEN;
  int LANGUAGE;
  int TOKEN;
  int NORMALIZED_STRING;
  int NOTATION;
  int QNAME;
  int ANY_URI;
  int BASE64_BINARY;
  int HEX_BINARY;
  int G_MONTH;
  int G_DAY;
  int G_MONTH_DAY;
  int G_YEAR;
  int G_YEAR_MONTH;
  int DATE;
  int TIME;
  int DATE_TIME;
  int DURATION;
  int DOUBLE;
  int FLOAT;
  int DECIMAL;
  int BOOLEAN;
  int STRING;
  int ANY_SIMPLE_TYPE;
}
class TypeBuilder {
  int type;
}
class Type {
  int name;
  int ANY_TYPE;
}
class TotalDigitsFacet {
  int fixed;
  int value;
}
class TokenType {
  int CONSTRAINING_FACETS;
}
class TimeType {
  int CONSTRAINING_FACETS;
  class Time {
    int seconds;
    int minutes;
  }
}
class StringType {
  int CONSTRAINING_FACETS;
}
class SimpleType {
  int annotation;
  int baseType;
  int fundamentalFacets;
  int facets;
  int variety;
  int ID_TYPE_IDREFS;
  int ID_TYPE_IDREF;
  int ID_TYPE_ID;
  int ID_TYPE_NULL;
  int UNION;
  int LIST;
  int ATOMIC;
  int ANY;
}
class ShortType {
  int LENGTH;
  int MIN_VALUE;
  int MAX_VALUE;
  int CONSTRAINING_FACETS;
}
class QNameType {
  int CONSTRAINING_FACETS;
}
class PositiveIntegerType {
  int CONSTRAINING_FACETS;
}
class PatternFacet {
  int value;
}
class NotationType {
  int CONSTRAINING_FACETS;
}
class NormalizedStringType {
  int CONSTRAINING_FACETS;
}
class NonPositiveIntegerType {
  int CONSTRAINING_FACETS;
}
class NonNegativeIntegerType {
  int CONSTRAINING_FACETS;
}
class NegativeIntegerType {
  int CONSTRAINING_FACETS;
}
class NameType {
  int CONSTRAINING_FACETS;
}
class NMTokensType {
  int CONSTRAINING_FACETS;
}
class NMTokenType {
  int CONSTRAINING_FACETS;
}
class NCNameType {
  int CONSTRAINING_FACETS;
}
class MinLengthFacet {
  int fixed;
  int value;
}
class MinInclusiveFacet {
  int fixed;
  int value;
}
class MinExclusiveFacet {
  int fixed;
  int value;
}
class MaxLengthFacet {
  int fixed;
  int value;
}
class MaxInclusiveFacet {
  int fixed;
  int value;
}
class MaxExclusiveFacet {
  int fixed;
  int value;
}
class LongType {
  int LENGTH;
  int MIN_VALUE;
  int MAX_VALUE;
  int CONSTRAINING_FACETS;
}
class ListSimpleType {
  int itemType;
}
class LengthFacet {
  int fixed;
  int value;
}
class LanguageType {
  int PATTERN;
  int CONSTRAINING_FACETS;
}
class IntegerType {
  int CONSTRAINING_FACETS;
}
class IntType {
  int LENGTH;
  int MIN_VALUE;
  int MAX_VALUE;
  int CONSTRAINING_FACETS;
}
class IDType {
  int CONSTRAINING_FACETS;
}
class IDRefsType {
  int CONSTRAINING_FACETS;
}
class IDRefType {
  int CONSTRAINING_FACETS;
}
class HexBinaryType {
  int CONSTRAINING_FACETS;
  int HEX;
}
class GYearType {
  int CONSTRAINING_FACETS;
  class GYear {
    int year;
  }
}
class GYearMonthType {
  int CONSTRAINING_FACETS;
  class GYearMonth {
    int month;
    int year;
  }
}
class GMonthType {
  int CONSTRAINING_FACETS;
  class GMonth {
    int month;
  }
}
class GMonthDayType {
  int CONSTRAINING_FACETS;
  class GMonthDay {
    int day;
    int month;
  }
}
class GDayType {
  int CONSTRAINING_FACETS;
  class GDay {
    int day;
  }
}
class FractionDigitsFacet {
  int fixed;
  int value;
}
class FloatType {
  int SPECIAL;
  int CONSTRAINING_FACETS;
}
class Facet {
  int annotation;
  int type;
  int FRACTION_DIGITS;
  int TOTAL_DIGITS;
  int MIN_INCLUSIVE;
  int MIN_EXCLUSIVE;
  int MAX_EXCLUSIVE;
  int MAX_INCLUSIVE;
  int WHITESPACE;
  int ENUMERATION;
  int PATTERN;
  int MAX_LENGTH;
  int MIN_LENGTH;
  int LENGTH;
}
class EnumerationFacet {
  int value;
}
class EntityType {
  int CONSTRAINING_FACETS;
}
class EntitiesType {
  int CONSTRAINING_FACETS;
}
class DurationType {
  int CONSTRAINING_FACETS;
  class Duration {
    int seconds;
    int minutes;
    int days;
    int months;
    int years;
  }
}
class DoubleType {
  int SPECIAL;
  int CONSTRAINING_FACETS;
}
class DecimalType {
  int CONSTRAINING_FACETS;
}
class DateType {
  int CONSTRAINING_FACETS;
}
class DateTimeType {
  int CONSTRAINING_FACETS;
}
class ByteType {
  int LENGTH;
  int MIN_VALUE;
  int MAX_VALUE;
  int CONSTRAINING_FACETS;
}
class BooleanType {
  int VALUE_SPACE;
  int CONSTRAINING_FACETS;
}
class Base64BinaryType {
  int CONSTRAINING_FACETS;
  int B04;
  int B16;
  int B64;
}
class AtomicSimpleType {
}
class AnyURIType {
  int CONSTRAINING_FACETS;
}
class AnyType {
}
class AnySimpleType {
}
class Annotation {
  int documentation;
}
