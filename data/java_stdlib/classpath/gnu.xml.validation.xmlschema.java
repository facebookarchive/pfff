package gnu.xml.validation.xmlschema;
class XMLSchemaValidatorHandler {
  int attributes;
  int context;
  int resourceResolver;
  int errorHandler;
  int contentHandler;
  int loc;
  int typeLibrary;
  int namespaceSupport;
  int typeInfoProvider;
  int schema;
}
class XMLSchemaValidator {
  int resourceResolver;
  int errorHandler;
  int schema;
}
class XMLSchemaTypeInfoProvider {
  int handler;
}
class XMLSchemaTypeInfo {
}
class XMLSchemaSchemaFactory {
  int errorHandler;
  int resourceResolver;
}
class XMLSchemaElementTypeInfo {
  int nil;
  int type;
  int decl;
  int schema;
}
class XMLSchemaBuilder {
  int typeLibrary;
  int schema;
}
class XMLSchemaAttributeTypeInfo {
  int specified;
  int id;
  int type;
  int decl;
  int schema;
}
class XMLSchema {
  int types;
  int attributeDeclarations;
  int elementDeclarations;
  int elementFormQualified;
  int attributeFormQualified;
  int blockDefault;
  int finalDefault;
  int version;
  int targetNamespace;
  int CONTENT_ELEMENT_ONLY;
  int CONTENT_MIXED;
  int CONTENT_SIMPLE;
  int CONTENT_EMPTY;
  int CONSTRAINT_FIXED;
  int CONSTRAINT_DEFAULT;
  int CONSTRAINT_NONE;
  int ABSENT;
  int LOCAL;
  int GLOBAL;
  int BLOCK_ALL;
  int BLOCK_SUBSTITUTION;
  int BLOCK_RESTRICTION;
  int BLOCK_EXTENSION;
  int BLOCK_NONE;
  int FINAL_ALL;
  int FINAL_UNION;
  int FINAL_LIST;
  int FINAL_RESTRICTION;
  int FINAL_EXTENSION;
  int FINAL_NONE;
}
class ValidationException {
}
class Particle {
  int term;
  int maxOccurs;
  int minOccurs;
}
class ElementDeclaration {
  int annotation;
  int isAbstract;
  int disallowedSubstitutions;
  int substitutionGroupExclusions;
  int substitutionGroup;
  int nillable;
  int value;
  int type;
  int parent;
  int scope;
  int datatype;
  int name;
}
class ComplexType {
  int annotations;
  int prohibitedSubstitutions;
  int contentModel;
  int contentType;
  int attributeWildcard;
  int attributeUses;
  int isAbstract;
  int finality;
  int derivationMethod;
  int baseType;
}
class AttributeUse {
  int declaration;
  int value;
  int type;
  int required;
}
class AttributeDeclaration {
  int annotation;
  int datatype;
  int name;
  int value;
  int type;
  int scope;
  int FIXED;
  int DEFAULT;
  int NONE;
}
class AnyAttribute {
  int annotation;
  int processContents;
  int namespace;
  int SKIP;
  int LAX;
  int STRICT;
}
