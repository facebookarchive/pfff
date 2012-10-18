package org.kxml2.io;
class KXmlSerializerTest {
  int NAMESPACE;
}
class KXmlSerializer {
  int encoding;
  int unicode;
  int indent;
  int nspStack;
  int nspCounts;
  int elementStack;
  int depth;
  int auto;
  int pending;
  int writer;
  int WRITE_BUFFER_SIZE;
}
class KXmlParser {
  class ContentSource {
    int limit;
    int position;
    int buffer;
    int next;
  }
  class ValueContext {
    int ENTITY_DECLARATION;
    int TEXT;
    int ATTRIBUTE;
  }
  int DOUBLE_QUOTE;
  int SINGLE_QUOTE;
  int stringPool;
  int unresolved;
  int error;
  int attributes;
  int attributeCount;
  int degenerated;
  int text;
  int name;
  int prefix;
  int namespace;
  int isWhitespace;
  int type;
  int bufferStartColumn;
  int bufferStartLine;
  int limit;
  int position;
  int buffer;
  int nextContentSource;
  int encoding;
  int reader;
  int nspCounts;
  int nspStack;
  int elementStack;
  int depth;
  int defaultAttributes;
  int documentEntities;
  int bufferCapture;
  int keepNamespaceAttributes;
  int relaxed;
  int processNsp;
  int processDocDecl;
  int publicId;
  int systemId;
  int rootElementName;
  int standalone;
  int version;
  int location;
  int XML_DECLARATION;
  int ILLEGAL_TYPE;
  int UNEXPECTED_EOF;
  int FIXED;
  int IMPLIED;
  int REQUIRED;
  int NOTATION;
  int NDATA;
  int ANY;
  int EMPTY;
  int START_NOTATION;
  int START_ENTITY;
  int START_ATTLIST;
  int START_ELEMENT;
  int PUBLIC;
  int SYSTEM;
  int START_DOCTYPE;
  int END_PROCESSING_INSTRUCTION;
  int START_PROCESSING_INSTRUCTION;
  int END_CDATA;
  int START_CDATA;
  int COMMENT_DOUBLE_DASH;
  int END_COMMENT;
  int START_COMMENT;
  int PARAMETER_ENTITY_REF;
  int NOTATIONDECL;
  int ATTLISTDECL;
  int ENTITYDECL;
  int ELEMENTDECL;
  int DEFAULT_ENTITIES;
  int FEATURE_RELAXED;
  int PROPERTY_LOCATION;
  int PROPERTY_XMLDECL_STANDALONE;
  int PROPERTY_XMLDECL_VERSION;
}
