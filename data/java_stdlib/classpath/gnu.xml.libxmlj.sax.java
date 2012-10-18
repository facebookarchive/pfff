package gnu.xml.libxmlj.sax;
class XMLName {
  int prefix;
  int qName;
  int localName;
  int uri;
  int XML_URI;
}
class StringArrayAttributes {
  int values;
  int keys;
  int len;
}
class Namespaces {
  int stack;
}
class GnomeXMLReader {
  int base;
  int seenStartDocument;
  int seenFatalError;
  int ns;
  int locator;
  int lexicalHandler;
  int declarationHandler;
  int errorHandler;
  int entityResolver;
  int dtdHandler;
  int contentHandler;
  int validation;
  int namespacePrefixes;
  int namespaces;
  int standalone;
  int RECOGNIZED_PROPERTIES;
  int PROPERTIES_PREFIX;
  int RECOGNIZED_FEATURES;
  int FEATURES_PREFIX;
}
class GnomeSAXParserFactory {
  int features;
}
class GnomeSAXParser {
  int validating;
  int namespaceAware;
}
class GnomeLocator {
  int loc;
  int ctx;
}
