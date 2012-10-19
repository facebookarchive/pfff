package org.objectweb.asm.tree.analysis;
class Value {
}
class Subroutine {
  int callers;
  int access;
  int start;
}
class SmallSet {
  int EMPTY_SET;
  int e2;
  int e1;
}
class SimpleVerifier {
  int isInterface;
  int currentClassInterfaces;
  int currentSuperClass;
  int currentClass;
}
class Interpreter {
}
class IntMap {
  int values;
  int keys;
  int size;
}
class Frame {
  int top;
  int locals;
  int values;
}
class DataflowValue {
  int insns;
  int size;
}
class DataflowInterpreter {
}
class BasicVerifier {
}
class BasicValue {
  int type;
  int RETURNADDRESS_VALUE;
  int REFERENCE_VALUE;
  int DOUBLE_VALUE;
  int LONG_VALUE;
  int FLOAT_VALUE;
  int INT_VALUE;
  int UNINITIALIZED_VALUE;
}
class BasicInterpreter {
}
class AnalyzerException {
}
class Analyzer {
  int jsr;
  int top;
  int queue;
  int queued;
  int subroutines;
  int frames;
  int handlers;
  int indexes;
  int n;
  int interpreter;
}
