package org.apache.harmony.xml;
class ExpatReader {
  class Feature {
    int EXTERNAL_PARAMETER_ENTITIES;
    int EXTERNAL_GENERAL_ENTITIES;
    int STRING_INTERNING;
    int NAMESPACE_PREFIXES;
    int NAMESPACES;
    int VALIDATION;
    int BASE_URI;
  }
  int LEXICAL_HANDLER_PROPERTY;
  int processNamespacePrefixes;
  int processNamespaces;
  int lexicalHandler;
  int errorHandler;
  int entityResolver;
  int dtdHandler;
  int contentHandler;
}
class ExpatParser {
  class EntityParser {
    int depth;
  }
  class ParseException {
  }
  class CurrentAttributes {
  }
  class ExpatLocator {
  }
  class ClonedAttributes {
    int length;
    int pointer;
    int parserPointer;
    int EMPTY;
  }
  int TIMEOUT;
  int CHARACTER_ENCODING;
  int DEFAULT_ENCODING;
  int OUTSIDE_START_ELEMENT;
  int attributes;
  int encoding;
  int systemId;
  int publicId;
  int xmlReader;
  int locator;
  int attributePointer;
  int attributeCount;
  int inStartElement;
  int pointer;
  int BUFFER_SIZE;
}
class ExpatException {
}
class ExpatAttributes {
  int CDATA;
}
