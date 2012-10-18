package javax.naming.directory;
class SearchResult {
  int attrs;
  int serialVersionUID;
}
class SearchControls {
  int attributesToReturn;
  int countLimit;
  int returnObj;
  int derefLink;
  int timeLimit;
  int searchScope;
  int SUBTREE_SCOPE;
  int ONELEVEL_SCOPE;
  int OBJECT_SCOPE;
  int serialVersionUID;
}
class SchemaViolationException {
  int serialVersionUID;
}
class NoSuchAttributeException {
  int serialVersionUID;
}
class ModificationItem {
  int attr;
  int mod_op;
  int serialVersionUID;
}
class InvalidSearchFilterException {
  int serialVersionUID;
}
class InvalidSearchControlsException {
  int serialVersionUID;
}
class InvalidAttributesException {
  int serialVersionUID;
}
class InvalidAttributeValueException {
  int serialVersionUID;
}
class InvalidAttributeIdentifierException {
  int serialVersionUID;
}
class InitialDirContext {
}
class DirContext {
  int REMOVE_ATTRIBUTE;
  int REPLACE_ATTRIBUTE;
  int ADD_ATTRIBUTE;
}
class BasicAttributes {
  class BasicAttributesEnumeration {
    int where;
  }
  int attributes;
  int ignoreCase;
  int serialVersionUID;
}
class BasicAttribute {
  class BasicAttributeEnumeration {
    int where;
  }
  int values;
  int ordered;
  int attrID;
  int serialVersionUID;
}
class Attributes {
}
class AttributeModificationException {
  int unexecs;
  int serialVersionUID;
}
class AttributeInUseException {
  int serialVersionUID;
}
class Attribute {
  int serialVersionUID;
}
