package gnu.classpath.jdwp.id;
class ThreadId {
  int typeClass;
}
class ThreadGroupId {
  int typeClass;
}
class StringId {
  int typeClass;
}
class ReferenceTypeId {
}
class ObjectId {
  int _handle;
  int typeClass;
}
class NullObjectId {
  int typeClass;
}
class JdwpId {
  int _reference;
  int _tag;
  int _id;
  int SIZE;
}
class InterfaceReferenceTypeId {
}
class ClassReferenceTypeId {
}
class ClassObjectId {
  int typeClass;
}
class ClassLoaderId {
  int typeClass;
}
class ArrayReferenceTypeId {
}
class ArrayId {
}
