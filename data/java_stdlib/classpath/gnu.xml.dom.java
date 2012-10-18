package gnu.xml.dom;
class JAXPFactory {
  class JAXPBuilder {
    int impl;
    int producer;
    int consumer;
  }
  int secureProcessing;
  int pf;
  int FEATURE;
  int PROPERTY;
}
class ImplementationSource {
  int implementations;
  int DIGITS;
}
class ImplementationList {
  int list;
}
class DomXPathResult {
  int iterator;
  int type;
  int value;
}
class DomXPathNSResolver {
  int node;
}
class DomXPathExpression {
  int resolver;
  int expression;
  int doc;
}
class DomText {
}
class DomProcessingInstruction {
  int data;
  int target;
}
class DomNsNode {
  int localName;
  int prefix;
  int namespace;
  int name;
}
class DomNotation {
}
class DomNodeIterator {
  int current;
  int walk;
  int entityReferenceExpansion;
  int filter;
  int whatToShow;
  int root;
}
class DomNode {
  class DomEventException {
  }
  class ListenerRecord {
    int useCapture;
    int listener;
    int type;
  }
  class LiveNodeList {
    int lastIndex;
    int current;
    int elementName;
    int elementURI;
    int matchAnyName;
    int matchAnyURI;
  }
  class ShadowList {
    int liveList;
  }
  int userDataHandlers;
  int userData;
  int nListeners;
  int listeners;
  int readonly;
  int nodeType;
  int length;
  int depth;
  int index;
  int last;
  int first;
  int next;
  int previous;
  int parent;
  int owner;
  int mutationEvent;
  int eventDataLock;
  int notificationSet;
  int ancestors;
  int dispatchDataLock;
  int lockNode;
  int reportMutations;
  int NOTIFICATIONS_INIT;
  int ANCESTORS_INIT;
  int NKIDS_DELTA;
}
class DomNamedNodeMap {
  int readonly;
  int length;
  int first;
  int type;
  int owner;
}
class DomNSResolverContext {
  int resolver;
}
class DomIterator {
  int expandEntityReferences;
  int filter;
  int whatToShow;
  int root;
  int done;
  int right;
  int reference;
}
class DomImpl {
}
class DomExtern {
  int systemId;
  int publicId;
  int name;
}
class DomEvent {
  class DomUIEvent {
    int detail;
    int view;
  }
  class DomMutationEvent {
    int attrChange;
    int attrName;
    int newValue;
    int prevValue;
    int relatedNode;
  }
  int doDefault;
  int stop;
  int timeStamp;
  int cancelable;
  int bubbles;
  int eventPhase;
  int currentNode;
  int target;
  int type;
}
class DomEntityReference {
  int name;
}
class DomEntity {
  int notation;
}
class DomElement {
  int xmlSpace;
  int attributes;
  int userIdAttrs;
}
class DomDocumentFragment {
}
class DomDocumentConfiguration {
  int splitCdataSections;
  int namespaceDeclarations;
  int errorHandler;
  int entities;
  int elementContentWhitespace;
  int comments;
  int cdataSections;
  int SUPPORTED_PARAMETERS;
}
class DomDocumentBuilderFactory {
  int secureProcessing;
  int ls;
  int impl;
}
class DomDocumentBuilder {
  int parser;
  int ls;
  int impl;
}
class DomDocument {
  int systemId;
  int standalone;
  int version;
  int encoding;
  int inputEncoding;
  int config;
  int building;
  int defaultAttributes;
  int checkingWellformedness;
  int checkingCharacters;
  int implementation;
}
class DomDoctype {
  int ids;
  int elements;
  int subset;
  int implementation;
  int entities;
  int notations;
}
class DomDOMException {
  int value;
  int node;
  int data;
}
class DomComment {
}
class DomCharacterData {
  int text;
  int CHILD_NODES;
  class EmptyNodeList {
  }
}
class DomCDATASection {
}
class DomAttr {
  int value;
  int specified;
}
class DTDElementTypeInfo {
  int idAttrName;
  int attributes;
  int model;
  int name;
}
class DTDAttributeTypeInfo {
  int value;
  int mode;
  int type;
  int name;
  int elementName;
}
class Consumer {
  class Backdoor {
  }
}
