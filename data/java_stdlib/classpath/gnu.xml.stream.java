package gnu.xml.stream;
class XMLStreamWriterImpl {
  int hasXML11RestrictedChars;
  int xml11;
  int count;
  int namespaces;
  int emptyElement;
  int inStartElement;
  int elements;
  int namespaceContext;
  int prefixDefaulting;
  int encoding;
  int writer;
}
class XMLParser {
  class Input {
    int SIGNATURE_UTF_8_BOM;
    int SIGNATURE_UTF_8;
    int SIGNATURE_UCS_2_21_NOBOM;
    int SIGNATURE_UCS_2_12_NOBOM;
    int SIGNATURE_UCS_2_21;
    int SIGNATURE_UCS_2_12;
    int SIGNATURE_UCS_4_3412;
    int SIGNATURE_UCS_4_2143;
    int SIGNATURE_UCS_4_4321;
    int SIGNATURE_UCS_4_1234;
    int xml11;
    int inputEncoding;
    int encodingDetected;
    int initialized;
    int unicodeReader;
    int reader;
    int in;
    int normalize;
    int report;
    int name;
    int systemId;
    int publicId;
    int markOffset;
    int offset;
    int markColumn;
    int column;
    int markLine;
    int line;
  }
  class AttributeDecl {
    int external;
    int values;
    int enumeration;
    int valueType;
    int value;
    int type;
  }
  class MixedContentModel {
    int names;
  }
  class ContentParticle {
    int content;
    int max;
    int min;
  }
  class ElementContentModel {
    int regex;
    int or;
    int contentParticles;
  }
  class AnyContentModel {
  }
  class EmptyContentModel {
  }
  class ContentModel {
    int external;
    int text;
    int type;
    int max;
    int min;
    int MIXED;
    int ELEMENT;
    int ANY;
    int EMPTY;
  }
  class ExternalIds {
    int notationName;
    int systemId;
    int publicId;
  }
  class Doctype {
    int anon;
    int externalNotations;
    int externalEntities;
    int entries;
    int pis;
    int comments;
    int notations;
    int entities;
    int attlists;
    int elements;
    int systemId;
    int publicId;
    int rootName;
  }
  class Attribute {
    int localName;
    int prefix;
    int value;
    int specified;
    int type;
    int name;
  }
  int PREDEFINED_ENTITIES;
  int TEST_END_CDATA;
  int TEST_END_PI;
  int TEST_END_COMMENT;
  int TEST_KET;
  int TEST_NOTATION_DECL;
  int TEST_ENTITY_DECL;
  int TEST_ATTLIST_DECL;
  int TEST_ELEMENT_DECL;
  int TEST_DOCTYPE_DECL;
  int TEST_XML_DECL;
  int TEST_CDATA;
  int TEST_PI;
  int TEST_COMMENT;
  int TEST_END_ELEMENT;
  int TEST_START_ELEMENT;
  int resolver;
  int reporter;
  int extendedEventTypes;
  int baseAware;
  int namespaceAware;
  int supportDTD;
  int externalEntities;
  int replaceERefs;
  int coalescing;
  int stringInterning;
  int validating;
  int peIsError;
  int expandPE;
  int doctype;
  int xmlStandalone;
  int xmlEncoding;
  int xmlVersion;
  int piData;
  int piTarget;
  int idrefs;
  int ids;
  int validationStack;
  int currentContentModel;
  int tmpBuf;
  int literalBuf;
  int nmtokenBuf;
  int buf;
  int attrs;
  int bases;
  int namespaces;
  int stack;
  int event;
  int state;
  int endEntityStack;
  int startEntityStack;
  int inputStack;
  int input;
  int END_ENTITY;
  int START_ENTITY;
  int ATTRIBUTE_DEFAULT_FIXED;
  int ATTRIBUTE_DEFAULT_REQUIRED;
  int ATTRIBUTE_DEFAULT_IMPLIED;
  int ATTRIBUTE_DEFAULT_SPECIFIED;
  int ATTRIBUTE_DEFAULT_UNDECLARED;
  int LIT_PUBID;
  int LIT_DISABLE_EREF;
  int LIT_DISABLE_CREF;
  int LIT_DISABLE_PE;
  int LIT_ATTRIBUTE;
  int LIT_NORMALIZE;
  int LIT_ENTITY_REF;
  int MISC;
  int EMPTY_ELEMENT;
  int CONTENT;
  int PROLOG;
  int INIT;
}
class XMLOutputFactoryImpl {
  int prefixDefaulting;
}
class XMLInputFactoryImpl {
  int stringInterning;
  int baseAware;
  int xIncludeAware;
  int supportDTD;
  int externalEntities;
  int replacingEntityReferences;
  int coalescing;
  int namespaceAware;
  int validating;
  int allocator;
  int reporter;
  int resolver;
}
class XMLEventWriterImpl {
  int writer;
}
class XMLEventReaderImpl {
  int peekEvent;
  int systemId;
  int allocator;
  int reader;
}
class XMLEventImpl {
  int location;
}
class XMLEventFactoryImpl {
  int location;
}
class XMLEventAllocatorImpl {
  int entityDeclarations;
}
class XIncludeFilter {
  int builder;
  int seenFallback;
  int inFallback;
  int inInclude;
  int len;
  int buf;
  int includedText;
  int lookahead;
  int backtracking;
  int seen;
  int walker;
  int current;
  int snapshotIndex;
  int result;
  int included;
  int event;
  int href;
  int expandERefs;
  int validating;
  int namespaceAware;
  int systemId;
  int SHOW_FLAGS;
  int XINCLUDE_NS_URI;
}
class UnicodeReader {
  int in;
}
class StartElementImpl {
  int namespaceContext;
  int namespaces;
  int attributes;
  int name;
}
class StartDocumentImpl {
  int encodingDeclared;
  int standaloneDeclared;
  int xmlStandalone;
  int xmlVersion;
  int encoding;
  int systemId;
}
class SAXParserFactory {
  int features;
  int FEATURE_NAMES;
}
class SAXParser {
  int baseURI;
  int xmlEncoding;
  int xmlStandalone;
  int xmlVersion;
  int encoding;
  int reader;
  int parser;
  int baseAware;
  int supportDTD;
  int externalEntities;
  int replaceERefs;
  int coalescing;
  int stringInterning;
  int xIncludeAware;
  int namespaceAware;
  int validating;
  int lexicalHandler;
  int errorHandler;
  int entityResolver;
  int dtdHandler;
  int declHandler;
  int contentHandler;
}
class ProcessingInstructionImpl {
  int data;
  int target;
}
class NotationDeclarationImpl {
  int systemId;
  int publicId;
  int name;
}
class NamespaceImpl {
  int specified;
  int uri;
  int prefix;
}
class FilteredStreamReader {
  int filter;
  int reader;
}
class FilteredEventReader {
  int filter;
}
class EntityReferenceImpl {
  int name;
  int decl;
}
class EntityDeclarationImpl {
  int baseUri;
  int replacementText;
  int notationName;
  int name;
  int systemId;
  int publicId;
}
class EndElementImpl {
  int namespaces;
  int name;
}
class EndDocumentImpl {
}
class DTDImpl {
  int entities;
  int notations;
  int impl;
  int body;
}
class CommentImpl {
  int text;
}
class CharactersImpl {
  int ignorableWhitespace;
  int cdata;
  int whitespace;
  int data;
}
class CRLFReader {
  int in;
  int doReset;
  int LF;
  int CR;
}
class BufferedReader {
  int bufferSize;
  int marklimit;
  int markpos;
  int count;
  int pos;
  int buf;
  int in;
  int DEFAULT_BUFFER_SIZE;
}
class AttributeImpl {
  int specified;
  int type;
  int value;
  int name;
}
