package org.apache.harmony.security.asn1;
class ObjectIdentifier {
  int soid;
  int oid;
}
class DerOutputStream {
  int val;
  int len;
  int index;
  int initSize;
}
class DerInputStream {
  int UNUSED_BITS_MASK;
}
class BitString {
  int unusedBits;
  int bytes;
  int RESET_MASK;
  int SET_MASK;
}
class BerOutputStream {
  int content;
  int length;
  int offset;
  int encoded;
}
class BerInputStream {
  int pool;
  int isIndefinedLength;
  int isVerify;
  int oidElement;
  int times;
  int choiceIndex;
  int contentOffset;
  int tagOffset;
  int content;
  int length;
  int tag;
  int INDEFINIT_LENGTH;
  int BUF_INCREASE_SIZE;
  int offset;
  int buffer;
  int in;
}
class ASN1ValueCollection {
  int type;
}
class ASN1UTCTime {
  int UTC_PATTERN;
  int ASN1;
  int UTC_LOCAL_HMS;
  int UTC_LOCAL_HM;
  int UTC_HMS;
  int UTC_HM;
}
class ASN1TypeCollection {
  int DEFAULT;
  int OPTIONAL;
  int type;
}
class ASN1Type {
  int constrId;
  int id;
}
class ASN1Time {
}
class ASN1StringType {
  int UTF8STRING;
  int UNIVERSALSTRING;
  int TELETEXSTRING;
  int PRINTABLESTRING;
  int GENERALSTRING;
  int IA5STRING;
  int BMPSTRING;
}
class ASN1SetOf {
}
class ASN1Set {
}
class ASN1SequenceOf {
}
class ASN1Sequence {
}
class ASN1Primitive {
}
class ASN1Oid {
  int STRING_OID;
  int ASN1;
}
class ASN1OctetString {
  int ASN1;
}
class ASN1Integer {
  int ASN1;
}
class ASN1Implicit {
  int taggingType;
  int type;
  int TAGGING_STRING;
  int TAGGING_CONSTRUCTED;
  int TAGGING_PRIMITIVE;
}
class ASN1GeneralizedTime {
  int GEN_PATTERN;
  int ASN1;
}
class ASN1Explicit {
  int type;
}
class ASN1Exception {
  int serialVersionUID;
}
class ASN1Enumerated {
  int ASN1;
}
class ASN1Constructed {
}
class ASN1Constants {
  int TAG_CHOICE;
  int TAG_ANY;
  int TAG_C_GENERALIZEDTIME;
  int TAG_C_UTCTIME;
  int TAG_C_SETOF;
  int TAG_C_SET;
  int TAG_C_SEQUENCEOF;
  int TAG_C_SEQUENCE;
  int TAG_C_UTF8STRING;
  int TAG_C_OCTETSTRING;
  int TAG_C_BITSTRING;
  int TAG_BMPSTRING;
  int TAG_UNIVERSALSTRING;
  int TAG_GENERALSTRING;
  int TAG_ISO646STRING;
  int TAG_VISIBLESTRING;
  int TAG_GRAPHICSTRING;
  int TAG_GENERALIZEDTIME;
  int TAG_UTCTIME;
  int TAG_IA5STRING;
  int TAG_VIDEOTEXSTRING;
  int TAG_T61STRING;
  int TAG_TELETEXSTRING;
  int TAG_PRINTABLESTRING;
  int TAG_NUMERICSTRING;
  int TAG_SETOF;
  int TAG_SET;
  int TAG_SEQUENCEOF;
  int TAG_SEQUENCE;
  int TAG_RELATIVEOID;
  int TAG_UTF8STRING;
  int TAG_EMBEDDEDPDV;
  int TAG_ENUM;
  int TAG_REAL;
  int TAG_INSTANCEOF;
  int TAG_EXTERNAL;
  int TAG_OBJDESCRIPTOR;
  int TAG_OID;
  int TAG_NULL;
  int TAG_OCTETSTRING;
  int TAG_BITSTRING;
  int TAG_INTEGER;
  int TAG_BOOLEAN;
  int PC_CONSTRUCTED;
  int PC_PRIMITIVE;
  int CLASS_PRIVATE;
  int CLASS_CONTEXTSPECIFIC;
  int CLASS_APPLICATION;
  int CLASS_UNIVERSAL;
}
class ASN1Choice {
  int identifiers;
  int type;
}
class ASN1Boolean {
  int ASN1;
}
class ASN1BitString {
  class ASN1NamedBitList {
    int maxBits;
    int minBits;
    int INDEFINITE_SIZE;
    int emptyString;
    int SET_MASK;
  }
  int ASN1;
}
class ASN1Any {
  int ASN1;
}
