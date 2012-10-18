package gnu.xml.pipeline;
class XsltFilter {
}
class XIncludeFilter {
  class Scrubber {
  }
  int savingPrefixes;
  int inclusions;
  int locator;
  int uris;
  int ignoreCount;
  int extEntities;
}
class WellFormednessFilter {
  int dtdState;
  int startedCDATA;
  int elementStack;
  int startedDoc;
}
class ValidationConsumer {
  class ChildrenRecognizer {
    int flags;
    int next;
    int name;
    int components;
    int consumer;
  }
  int nodeCount;
  int F_LOOPNEXT;
  int F_LOOPHEAD;
  class MixedRecognizer {
    int permitted;
  }
  class EmptyRecognizer {
  }
  class Recognizer {
    int type;
  }
  int ANY;
  class AttributeInfo {
    int value;
    int mode;
    int type;
  }
  class ElementInfo {
    int recognizer;
    int attributes;
    int model;
    int name;
  }
  int types;
  int fakeRootName;
  int uDeferred;
  int unparsed;
  int nDeferred;
  int notations;
  int ids;
  int elements;
  int disableReset;
  int disableDeclarations;
  int contentStack;
  int rootName;
  int warnNonDeterministic;
}
class TextConsumer {
}
class TeeConsumer {
  int lexRest;
  int lexFirst;
  int declRest;
  int declFirst;
  int docRest;
  int docFirst;
  int rest;
  int first;
}
class PipelineFactory {
  class Pipeline {
    int next;
    int rest;
    int stage;
  }
  class Stage {
    int param;
    int id;
  }
  int builtinStages;
  int index;
  int tokens;
}
class NSFilter {
  int prefixRoot;
  int usedDefault;
  int attributes;
  int nsTemp;
  int pushedContext;
  int elementStack;
  int nsStack;
}
class LinkFilter {
  int siteRestricted;
  int baseURI;
  int vector;
}
class EventFilter {
  int xincClass;
  int wfClass;
  int validClass;
  int nsClass;
  int loaded;
  int LEXICAL_HANDLER;
  int DECL_HANDLER;
  int PROPERTY_URI;
  int FEATURE_URI;
  int errHandler;
  int next;
  int locator;
  int declNext;
  int declHandler;
  int lexNext;
  int lexHandler;
  int dtdNext;
  int dtdHandler;
  int docNext;
  int docHandler;
}
class EventConsumer {
}
class DomConsumer {
  class Handler {
    int xmlnsURI;
    int attributes;
    int recreatedAttrs;
    int currentEntity;
    int inDTD;
    int mergeCDATA;
    int inCDATA;
    int top;
    int locator;
    int isL2;
    int document;
    int impl;
    int consumer;
  }
  int next;
  int errHandler;
  int handler;
  int hidingReferences;
  int hidingWhitespace;
  int hidingComments;
  int hidingCDATA;
  int domImpl;
}
class CallFilter {
  class Requestor {
  }
  int errHandler;
  int conn;
  int target;
  int next;
  int req;
}
