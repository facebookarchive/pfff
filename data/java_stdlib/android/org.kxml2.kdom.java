package org.kxml2.kdom;
class Node {
  int types;
  int children;
  int DOCDECL;
  int COMMENT;
  int PROCESSING_INSTRUCTION;
  int IGNORABLE_WHITESPACE;
  int ENTITY_REF;
  int CDSECT;
  int TEXT;
  int ELEMENT;
  int DOCUMENT;
}
class Element {
  int prefixes;
  int parent;
  int attributes;
  int name;
  int namespace;
}
class Document {
  int standalone;
  int encoding;
  int rootIndex;
}
