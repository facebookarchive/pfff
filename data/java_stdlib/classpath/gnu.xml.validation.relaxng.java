package gnu.xml.validation.relaxng;
class ValuePattern {
  int value;
  int ns;
  int type;
  int datatypeLibrary;
}
class TextPattern {
  int INSTANCE;
}
class RefPattern {
  int name;
}
class RELAXNGSchemaFactory {
  int errorHandler;
  int resourceResolver;
}
class Pattern {
}
class Param {
  int value;
  int name;
}
class OneOrMorePattern {
  int pattern;
}
class NotAllowedPattern {
  int INSTANCE;
}
class NameNameClass {
  int name;
  int ns;
}
class NameClass {
}
class NSNameNameClass {
  int exceptNameClass;
  int ns;
}
class ListPattern {
  int pattern;
}
class InterleavePattern {
  int pattern2;
  int pattern1;
}
class GroupPattern {
  int pattern2;
  int pattern1;
}
class GrammarValidator {
  int resourceResolver;
  int errorHandler;
  int grammar;
}
class GrammarException {
}
class Grammar {
  int defines;
  int start;
}
class FullSyntaxBuilder {
  int datatypeLibraries;
  int refCount;
  int urls;
  int PATTERN_ELEMENTS;
  int STRIPPED_ATTRIBUTES;
  int VOCABULARY;
}
class EmptyPattern {
  int INSTANCE;
}
class ElementPattern {
  int pattern;
  int nameClass;
}
class Define {
  int element;
  int name;
}
class DataPattern {
  int exceptPattern;
  int params;
  int datatypeLibrary;
  int type;
}
class ChoicePattern {
  int pattern2;
  int pattern1;
}
class ChoiceNameClass {
  int name2;
  int name1;
}
class AttributePattern {
  int pattern;
  int nameClass;
}
class AnyNameNameClass {
  int exceptNameClass;
}
