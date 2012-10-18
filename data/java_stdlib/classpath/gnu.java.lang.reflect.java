package gnu.java.lang.reflect;
class TypeSignature {
}
class TypeImpl {
}
class MethodSignatureParser {
  int throwsSigs;
  int retType;
  int argTypes;
  int typeParameters;
}
class GenericSignatureParser {
  int pos;
  int signature;
  int container;
  int loader;
}
class WildcardTypeImpl {
  int upper;
  int lower;
}
class UnresolvedTypeVariable {
  int name;
  int decl;
}
class GenericArrayTypeImpl {
  int componentType;
}
class ParameterizedTypeImpl {
  int typeArgs;
  int owner;
  int rawType;
  int loader;
  int rawTypeName;
}
class TypeVariableImpl {
  int name;
  int bounds;
  int decl;
}
class FieldSignatureParser {
  int type;
}
class ClassSignatureParser {
  int interfaceTypes;
  int superclassType;
  int typeParameters;
}
