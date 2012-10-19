package org.objectweb.asm.util;
class TraceSignatureVisitor {
  int separator;
  int arrayStack;
  int argumentStack;
  int exceptions;
  int returnType;
  int seenInterface;
  int seenParameter;
  int seenInterfaceBound;
  int seenFormalParameter;
  int isInterface;
  int declaration;
}
class TraceMethodVisitor {
  int labelNames;
  int ltab;
  int tab3;
  int tab2;
  int mv;
}
class TraceFieldVisitor {
  int fv;
}
class TraceClassVisitor {
  int pw;
  int cv;
}
class TraceAnnotationVisitor {
  int valueNumber;
  int av;
}
class TraceAbstractVisitor {
  int tab;
  int PARAMETERS_DECLARATION;
  int CLASS_DECLARATION;
  int TYPE_DECLARATION;
  int CLASS_SIGNATURE;
  int METHOD_SIGNATURE;
  int METHOD_DESCRIPTOR;
  int FIELD_SIGNATURE;
  int FIELD_DESCRIPTOR;
  int INTERNAL_NAME;
}
class CheckMethodAdapter {
  int TYPE;
  int labels;
  int endMethod;
  int endCode;
  int startCode;
}
class CheckFieldAdapter {
  int end;
  int fv;
}
class CheckClassAdapter {
  int end;
  int outer;
  int source;
  int start;
}
class CheckAnnotationAdapter {
  int end;
  int named;
  int av;
}
class AbstractVisitor {
  int buf;
  int text;
  int TYPES;
  int OPCODES;
}
class ASMifierMethodVisitor {
}
class ASMifierFieldVisitor {
}
class ASMifierClassVisitor {
  int pw;
  int ACCESS_INNER;
  int ACCESS_FIELD;
  int ACCESS_CLASS;
}
class ASMifierAnnotationVisitor {
  int id;
}
class ASMifierAbstractVisitor {
  int labelNames;
  int name;
}
