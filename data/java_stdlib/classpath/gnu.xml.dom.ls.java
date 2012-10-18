package gnu.xml.dom.ls;
class WriterOutputStream {
  int encoding;
  int writer;
}
class SAXEventSink {
  int interrupted;
  int inDTD;
  int inCDATA;
  int locator;
  int pending;
  int entityCtx;
  int ctx;
  int doc;
  int reader;
  int coalescing;
  int ignoreComments;
  int expandEntityReferences;
  int ignoreWhitespace;
  int namespaceAware;
  int PREDEFINED_ENTITIES;
  int XMLNS_PREFIX;
  int XMLNS_URI;
}
class ReaderInputStream {
  int pos_marked;
  int extra_marked;
  int pos;
  int extra;
  int encoding;
  int reader;
}
class FilteredSAXEventSink {
  int rejecting;
  int decisions;
  int nodes;
  int whatToShow;
  int filter;
}
class DomLSSerializer {
  int serializer;
  int filter;
  int SUPPORTED_PARAMETERS;
}
class DomLSParser {
  int errorHandler;
  int entityResolver;
  int xIncludeAware;
  int validating;
  int coalescing;
  int ignoreComments;
  int expandEntityReferences;
  int ignoreWhitespace;
  int namespaceAware;
  int reader;
  int factory;
  int eventSink;
  int schemaType;
  int async;
  int filter;
  int SUPPORTED_PARAMETERS;
}
class DomLSOutput {
  int encoding;
  int systemId;
  int out;
}
class DomLSInput {
  int certifiedText;
  int encoding;
  int baseURI;
  int publicId;
  int systemId;
  int in;
}
class DomLSException {
}
