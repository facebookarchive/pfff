package gnu.java.beans.encoder.elements;
class StringReference {
  int string;
}
class StaticMethodInvocation {
  int methodName;
  int className;
}
class StaticFieldAccess {
  int fieldName;
  int className;
}
class PrimitiveInstantiation {
  int valueAsString;
  int primitiveName;
}
class ObjectReference {
  int id;
}
class ObjectInstantiation {
  int className;
}
class NullObject {
}
class MethodInvocation {
  int methodName;
}
class List_Set {
}
class List_Get {
}
class Element {
  int objectId;
  int children;
}
class ClassResolution {
  int className;
}
class Array_Set {
  int indexAsString;
}
class Array_Get {
  int indexAsString;
}
class ArrayInstantiation {
  int lengthAsString;
  int className;
}
