package org.objectweb.asm.xml;
class SAXFieldAdapter {
  int h;
}
class SAXCodeAdapter {
  int labelNames;
}
class SAXClassAdapter {
  int singleDocument;
}
class SAXAnnotationAdapter {
  int elementName;
}
class SAXAdapter {
  int h;
}
class Processor {
  class ZipEntryElement {
    int zos;
  }
  class SingleDocElement {
    int os;
  }
  class EntryElement {
  }
  class OutputSlicingHandler {
    int subdocumentHandler;
    int subdocument;
    int isXml;
    int entryElement;
    int subdocumentHandlerFactory;
    int subdocumentRoot;
  }
  class InputSlicingHandler {
    int subdocumentHandler;
    int subdocument;
    int subdocumentHandlerFactory;
    int rootHandler;
    int subdocumentRoot;
  }
  class SAXWriter {
    int ident;
    int openElement;
    int optimizeEmptyElements;
    int w;
    int OFF;
  }
  class SubdocumentHandlerFactory {
    int subdocumentHandler;
  }
  class TransformerHandlerFactory {
    int outputHandler;
    int templates;
    int saxtf;
  }
  class ASMContentHandlerFactory {
    int computeMax;
    int os;
  }
  class SAXWriterFactory {
    int optimizeEmptyElements;
    int w;
  }
  class ContentHandlerFactory {
  }
  class ProtectedInputStream {
    int is;
  }
  int n;
  int computeMax;
  int xslt;
  int output;
  int input;
  int outRepresentation;
  int inRepresentation;
  int SINGLE_XML_NAME;
  int SINGLE_XML;
  int MULTI_XML;
  int BYTECODE;
}
class ASMContentHandler {
  class Opcode {
    int type;
    int opcode;
  }
  class AnnotationDefaultRule {
  }
  class AnnotationValueArrayRule {
  }
  class AnnotationValueAnnotationRule {
  }
  class AnnotationValueEnumRule {
  }
  class AnnotationValueRule {
  }
  class AnnotationParameterRule {
  }
  class AnnotationRule {
  }
  class MaxRule {
  }
  class OpcodesRule {
  }
  class LocalVarRule {
  }
  class LineNumberRule {
  }
  class TryCatchRule {
  }
  class LabelRule {
  }
  class LookupSwitchLabelRule {
  }
  class LookupSwitchRule {
  }
  class TableSwitchLabelRule {
  }
  class TableSwitchRule {
  }
  class ExceptionsRule {
  }
  class ExceptionRule {
  }
  class MethodRule {
  }
  class FieldRule {
  }
  class InnerClassRule {
  }
  class OuterClassRule {
  }
  class InterfacesRule {
  }
  class InterfaceRule {
  }
  class SourceRule {
  }
  class ClassRule {
  }
  class Rule {
  }
  class RuleSet {
    int rpatterns;
    int lpatterns;
    int rules;
  }
  int OPCODES;
  class OpcodeGroup {
    int INSN_MULTIANEWARRAY;
    int INSN_IINC;
    int INSN_LDC;
    int INSN_JUMP;
    int INSN_METHOD;
    int INSN_FIELD;
    int INSN_TYPE;
    int INSN_VAR;
    int INSN_INT;
    int INSN;
  }
  int RULES;
  int BASE;
  int labels;
  int cw;
  int os;
  int computeMax;
  int match;
  int stack;
}
