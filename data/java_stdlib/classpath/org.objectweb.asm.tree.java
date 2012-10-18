package org.objectweb.asm.tree;
class VarInsnNode {
  int var;
}
class TypeInsnNode {
  int desc;
}
class TryCatchBlockNode {
  int type;
  int handler;
  int end;
  int start;
}
class TableSwitchInsnNode {
  int labels;
  int dflt;
  int max;
  int min;
}
class MultiANewArrayInsnNode {
  int dims;
  int desc;
}
class MethodNode {
  int lineNumbers;
  int localVariables;
  int maxLocals;
  int maxStack;
  int tryCatchBlocks;
  int instructions;
  int invisibleParameterAnnotations;
  int visibleParameterAnnotations;
  int annotationDefault;
  int exceptions;
  int signature;
  int desc;
  int name;
  int access;
}
class MethodInsnNode {
  int desc;
  int name;
  int owner;
}
class MemberNode {
  int attrs;
  int invisibleAnnotations;
  int visibleAnnotations;
}
class LookupSwitchInsnNode {
  int labels;
  int keys;
  int dflt;
}
class LocalVariableNode {
  int index;
  int end;
  int start;
  int signature;
  int desc;
  int name;
}
class LineNumberNode {
  int start;
  int line;
}
class LdcInsnNode {
  int cst;
}
class LabelNode {
  int label;
}
class JumpInsnNode {
  int label;
}
class IntInsnNode {
  int operand;
}
class InsnNode {
  int INSNS;
}
class InnerClassNode {
  int access;
  int innerName;
  int outerName;
  int name;
}
class IincInsnNode {
  int incr;
  int var;
}
class FieldNode {
  int value;
  int signature;
  int desc;
  int name;
  int access;
}
class FieldInsnNode {
  int desc;
  int name;
  int owner;
}
class ClassNode {
  int methods;
  int fields;
  int innerClasses;
  int outerMethodDesc;
  int outerMethod;
  int outerClass;
  int sourceDebug;
  int sourceFile;
  int interfaces;
  int superName;
  int signature;
  int name;
  int access;
  int version;
}
class AnnotationNode {
  int values;
  int desc;
}
class AbstractInsnNode {
  int opcode;
  int MULTIANEWARRAY_INSN;
  int LOOKUPSWITCH_INSN;
  int TABLESWITCH_INSN;
  int IINC_INSN;
  int LDC_INSN;
  int LABEL;
  int JUMP_INSN;
  int METHOD_INSN;
  int FIELD_INSN;
  int TYPE_INSN;
  int VAR_INSN;
  int INT_INSN;
  int INSN;
}
