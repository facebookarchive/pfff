package org.apache.harmony.xml.dom;
class TextImpl {
}
class ProcessingInstructionImpl {
  int data;
  int target;
}
class NotationImpl {
  int systemID;
  int publicID;
  int notationName;
}
class NodeListImpl {
  int children;
}
class NodeImpl {
  class UserData {
    int handler;
    int value;
  }
  int document;
  int NULL_TYPE_INFO;
  int EMPTY_LIST;
}
class LeafNodeImpl {
  int index;
  int parent;
}
class InnerNodeImpl {
  int children;
}
class EntityReferenceImpl {
  int name;
}
class EntityImpl {
  int systemID;
  int publicID;
  int notationName;
}
class ElementImpl {
  class ElementAttrNamedNodeMapImpl {
  }
  int attributes;
  int localName;
  int prefix;
  int namespaceURI;
  int namespaceAware;
}
class DocumentTypeImpl {
  int systemId;
  int publicId;
  int qualifiedName;
}
class DocumentImpl {
  int nodeToUserData;
  int strictErrorChecking;
  int xmlStandalone;
  int xmlVersion;
  int xmlEncoding;
  int inputEncoding;
  int documentUri;
  int domConfiguration;
  int domImplementation;
}
class DocumentFragmentImpl {
}
class DOMImplementationImpl {
  int instance;
}
class DOMErrorImpl {
  int type;
  int severity;
  int NULL_DOM_LOCATOR;
}
class DOMConfigurationImpl {
  class BooleanParameter {
  }
  class FixedParameter {
    int onlyValue;
  }
  class Parameter {
  }
  int wellFormed;
  int validate;
  int splitCdataSections;
  int schemaType;
  int schemaLocation;
  int namespaces;
  int errorHandler;
  int entities;
  int datatypeNormalization;
  int comments;
  int cdataSections;
  int PARAMETERS;
}
class CommentImpl {
}
class CharacterDataImpl {
  int buffer;
}
class CDATASectionImpl {
}
class AttrImpl {
  int value;
  int localName;
  int prefix;
  int namespaceURI;
  int namespaceAware;
  int isId;
  int ownerElement;
}
