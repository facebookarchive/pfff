package gnu.java.security.der;
class DERWriter {
}
class DERValue {
  int encoded;
  int value;
  int length;
  int tag;
  int constructed;
  int tagClass;
}
class DERReader {
  int encBuf;
  int in;
}
class DEREncodingException {
}
class DER {
  int GENERALIZED_TIME;
  int UTC_TIME;
  int BMP_STRING;
  int UNIVERSAL_STRING;
  int UTF8_STRING;
  int GENERAL_STRING;
  int ISO646_STRING;
  int GRAPHIC_STRING;
  int IA5_STRING;
  int VIDEOTEX_STRING;
  int T61_STRING;
  int PRINTABLE_STRING;
  int NUMERIC_STRING;
  int CONSTRUCTED_VALUE;
  int SET;
  int SEQUENCE;
  int RELATIVE_OID;
  int ENUMERATED;
  int REAL;
  int OBJECT_IDENTIFIER;
  int NULL;
  int OCTET_STRING;
  int BIT_STRING;
  int INTEGER;
  int BOOLEAN;
  int ANY;
  int CONSTRUCTED;
  int PRIVATE;
  int CONTEXT;
  int APPLICATION;
  int UNIVERSAL;
}
class BitString {
  int boolVal;
  int ignoredBits;
  int externBytes;
  int bytes;
}
