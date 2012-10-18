package gnu.xml.aelfred2;
class XmlReader {
  int active;
  int isValidating;
  int filter;
  int aelfred2;
  class FatalErrorHandler {
  }
}
class XmlParser {
  class Input {
    int reader;
    int column;
    int currentByteCount;
    int is;
    int readBufferOverflow;
    int encoding;
    int line;
    int readBufferLength;
    int readBufferPos;
    int readBuffer;
    int externalEntity;
    int sourceType;
  }
  class ElementDecl {
    int attributes;
    int contentModel;
    int contentType;
  }
  class AttributeDecl {
    int defaultValue;
    int enumeration;
    int valueType;
    int value;
    int type;
  }
  class EntityInfo {
    int notationName;
    int value;
    int ids;
    int type;
  }
  class ExternalIdentifiers {
    int baseUri;
    int systemId;
    int publicId;
  }
  int isDirtyCurrentElement;
  int endDelimCDATA;
  int endDelimPI;
  int startDelimPI;
  int endDelimComment;
  int startDelimComment;
  int xmlVersion;
  int XML_11;
  int XML_10;
  int inCDATA;
  int sawCR;
  int tagAttributePos;
  int tagAttributes;
  int symbolTable;
  int SYMBOL_TABLE_LENGTH;
  int doReport;
  int peIsError;
  int expandPE;
  int inLiteral;
  int entityStack;
  int currentElementContent;
  int currentElement;
  int skippedPE;
  int notationInfo;
  int entityInfo;
  int elementInfo;
  int docIsStandalone;
  int nameBufferPos;
  int nameBuffer;
  int NAME_BUFFER_INITIAL;
  int dataBufferPos;
  int dataBuffer;
  int DATA_BUFFER_INITIAL;
  int rawReadBuffer;
  int READ_BUFFER_MAX;
  int readBufferOverflow;
  int readBufferLength;
  int readBufferPos;
  int readBuffer;
  int scratch;
  int currentByteCount;
  int encoding;
  int externalEntity;
  int inputStack;
  int sourceType;
  int column;
  int line;
  int is;
  int reader;
  int handler;
  int uriWarnings;
  int CONTEXT_LITERAL;
  int CONTEXT_NORMAL;
  int LIT_PUBID;
  int LIT_DISABLE_EREF;
  int LIT_DISABLE_CREF;
  int LIT_DISABLE_PE;
  int LIT_ATTRIBUTE;
  int LIT_NORMALIZE;
  int LIT_ENTITY_REF;
  int INPUT_READER;
  int INPUT_STREAM;
  int INPUT_INTERNAL;
  int INPUT_NONE;
  int ATTRIBUTE_DEFAULT_FIXED;
  int ATTRIBUTE_DEFAULT_REQUIRED;
  int ATTRIBUTE_DEFAULT_IMPLIED;
  int ATTRIBUTE_DEFAULT_SPECIFIED;
  int ATTRIBUTE_DEFAULT_UNDECLARED;
  int ENCODING_ASCII;
  int ENCODING_UCS_4_3412;
  int ENCODING_UCS_4_2143;
  int ENCODING_UCS_4_4321;
  int ENCODING_UCS_4_1234;
  int ENCODING_UCS_2_21;
  int ENCODING_UCS_2_12;
  int ENCODING_ISO_8859_1;
  int ENCODING_UTF_8;
  int ENCODING_EXTERNAL;
  int ENTITY_TEXT;
  int ENTITY_NDATA;
  int ENTITY_INTERNAL;
  int ENTITY_UNDECLARED;
  int CONTENT_ELEMENTS;
  int CONTENT_MIXED;
  int CONTENT_EMPTY;
  int CONTENT_ANY;
  int CONTENT_UNDECLARED;
  int USE_CHEATS;
}
class SAXDriver {
  class Attribute {
    int specified;
    int localName;
    int nameSpace;
    int value;
    int name;
  }
  class Adapter {
    int docHandler;
  }
  int PROPERTY;
  int FEATURE;
  int prefixStack;
  int nsTemp;
  int attributes;
  int attributeCount;
  int stringInterning;
  int useResolver2;
  int resolveAll;
  int extPE;
  int extGE;
  int xmlNames;
  int namespaces;
  int attributesList;
  int entityStack;
  int elementName;
  int lexicalHandler;
  int declHandler;
  int errorHandler;
  int dtdHandler;
  int contentHandler;
  int resolver2;
  int entityResolver;
  int parser;
  int base;
}
class JAXPFactory {
  class JaxpParser {
    int parser;
    int ae2;
  }
  int flags;
}
