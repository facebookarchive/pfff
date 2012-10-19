package org.objectweb.asm.attrs;
class StackMapType {
  int object;
  int offset;
  int type;
  int ITEM_NAMES;
  int ITEM_Uninitialized;
  int ITEM_Object;
  int ITEM_UninitializedThis;
  int ITEM_Null;
  int ITEM_Long;
  int ITEM_Double;
  int ITEM_Float;
  int ITEM_Integer;
  int ITEM_Top;
}
class StackMapTableAttribute {
  int frames;
  int MAX_SHORT;
  int FULL_FRAME;
  int APPEND_FRAME;
  int SAME_FRAME_EXTENDED;
  int CHOP_FRAME;
  int SAME_LOCALS_1_STACK_ITEM_FRAME_EXTENDED;
  int RESERVED;
  int SAME_LOCALS_1_STACK_ITEM_FRAME;
  int SAME_FRAME;
}
class StackMapFrame {
  int stack;
  int locals;
  int label;
}
class StackMapAttribute {
  int frames;
  int MAX_SIZE;
}
