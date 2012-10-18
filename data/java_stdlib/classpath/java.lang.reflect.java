package java.lang.reflect;
class VMProxy {
  int HAVE_NATIVE_GENERATE_PROXY_CLASS;
  int HAVE_NATIVE_GET_PROXY_DATA;
  int HAVE_NATIVE_GET_PROXY_CLASS;
}
class VMMethod {
  int m;
  int slot;
  int name;
  int clazz;
}
class VMField {
  int f;
  int slot;
  int name;
  int clazz;
}
class VMConstructor {
  int cons;
  int slot;
  int clazz;
}
class VMArray {
}
class WildcardType {
}
class UndeclaredThrowableException {
  int undeclaredThrowable;
  int serialVersionUID;
}
class TypeVariable {
}
class Type {
}
class ReflectPermission {
  int serialVersionUID;
}
class Proxy {
  class ClassFactory {
    int methods;
    int qualName;
    int poolEntries;
    int stream;
    int pool;
    int CHECKCAST;
    int ATHROW;
    int ANEWARRAY;
    int NEW;
    int INVOKEINTERFACE;
    int INVOKESPECIAL;
    int INVOKEVIRTUAL;
    int GETFIELD;
    int GETSTATIC;
    int RETURN;
    int ARETURN;
    int DRETURN;
    int FRETURN;
    int LRETURN;
    int IRETURN;
    int SWAP;
    int DUP_X1;
    int DUP;
    int AASTORE;
    int AALOAD;
    int ALOAD_1;
    int ALOAD_0;
    int ILOAD_0;
    int ILOAD;
    int SIPUSH;
    int BIPUSH;
    int ICONST_0;
    int ACONST_NULL;
    int INVOKE_SIG;
    int CTOR_SIG;
    int INTERFACE;
    int METHOD;
    int FIELD;
  }
  class ProxyData {
    int id;
    int count;
    int exceptions;
    int methods;
    int interfaces;
    int pack;
  }
  class ProxySignature {
    int exceptions;
    int method;
    int coreMethods;
  }
  class ProxyType {
    int interfaces;
    int loader;
  }
  int h;
  int proxyClasses;
  int serialVersionUID;
}
class ParameterizedType {
}
class Modifier {
  int ENUM;
  int SYNTHETIC;
  int VARARGS;
  int BRIDGE;
  int ALL_FLAGS;
  int SUPER;
  int STRICT;
  int ABSTRACT;
  int INTERFACE;
  int NATIVE;
  int TRANSIENT;
  int VOLATILE;
  int SYNCHRONIZED;
  int FINAL;
  int STATIC;
  int PROTECTED;
  int PRIVATE;
  int PUBLIC;
}
class Method {
  int m;
  int p;
  int METHOD_MODIFIERS;
}
class Member {
  int PUBLIC;
  int DECLARED;
}
class MalformedParameterizedTypeException {
  int serialVersionUID;
}
class InvocationTargetException {
  int target;
  int serialVersionUID;
}
class InvocationHandler {
}
class GenericSignatureFormatError {
  int serialVersionUID;
}
class GenericDeclaration {
}
class GenericArrayType {
}
class Field {
  int f;
  int p;
  int FIELD_MODIFIERS;
}
class Constructor {
  int cons;
  int p;
  int CONSTRUCTOR_MODIFIERS;
}
class Array {
}
class AnnotatedElement {
}
class AccessibleObject {
  int flag;
}
