package org.xml.sax.helpers;
class XMLReaderFactory {
  int property;
}
class XMLReaderAdapter {
  class AttributesAdapter {
    int attributes;
  }
  int qAtts;
  int documentHandler;
  int xmlReader;
}
class XMLFilterImpl {
  int errorHandler;
  int contentHandler;
  int dtdHandler;
  int entityResolver;
  int locator;
  int parent;
}
class ParserFactory {
}
class ParserAdapter {
  class AttributeListAdapter {
    int qAtts;
  }
  int errorHandler;
  int contentHandler;
  int dtdHandler;
  int entityResolver;
  int locator;
  int uris;
  int prefixes;
  int namespaces;
  int atts;
  int parser;
  int nameParts;
  int parsing;
  int attAdapter;
  int nsSupport;
  int XMLNS_URIs;
  int NAMESPACE_PREFIXES;
  int NAMESPACES;
  int FEATURES;
}
class NewInstance {
}
class NamespaceSupport {
  class Context {
    int parent;
    int declSeen;
    int declarations;
    int declsOK;
    int defaultNS;
    int attributeNameTable;
    int elementNameTable;
    int uriTable;
    int prefixTable;
  }
  int namespaceDeclUris;
  int contextPos;
  int currentContext;
  int contexts;
  int EMPTY_ENUMERATION;
  int NSDECL;
  int XMLNS;
}
class LocatorImpl {
  int columnNumber;
  int lineNumber;
  int systemId;
  int publicId;
}
class DefaultHandler {
}
class AttributesImpl {
  int data;
  int length;
}
class AttributeListImpl {
  int values;
  int types;
  int names;
}
