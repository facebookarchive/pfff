package javax.imageio.metadata;
class IIOMetadataNode {
  class IIONodeList {
    int children;
  }
  class IIONamedNodeMap {
    int attrs;
  }
  int obj;
  int parent;
  int children;
  int attrs;
  int name;
}
class IIOMetadataFormatImpl {
  class NodeObjectArray {
    int arrayMaxLength;
    int arrayMinLength;
  }
  class NodeObjectBounded {
    int maxInclusive;
    int minInclusive;
    int maxValue;
    int minValue;
  }
  class NodeObjectEnumerated {
    int enumeratedValues;
  }
  class NodeObject {
    int valueType;
    int defaultValue;
    int required;
    int classType;
    int owner;
  }
  class IIOMetadataNodeAttrList {
    int listMaxLength;
    int listMinLength;
  }
  class IIOMetadataNodeAttrBounded {
    int maxInclusive;
    int minInclusive;
    int maxValue;
    int minValue;
  }
  class IIOMetadataNodeAttrEnumerated {
    int enumeratedValues;
  }
  class IIOMetadataNodeAttr {
    int defaultValue;
    int required;
    int dataType;
    int name;
    int owner;
  }
  int resourceBaseName;
  int childRanges;
  int childPolicies;
  int nodes;
  int rootName;
  int standardMetadataFormatName;
}
class IIOMetadataFormat {
  int VALUE_RANGE_MIN_MAX_INCLUSIVE;
  int VALUE_RANGE_MIN_INCLUSIVE_MASK;
  int VALUE_RANGE_MIN_INCLUSIVE;
  int VALUE_RANGE_MAX_INCLUSIVE_MASK;
  int VALUE_RANGE_MAX_INCLUSIVE;
  int VALUE_RANGE;
  int VALUE_NONE;
  int VALUE_LIST;
  int VALUE_ENUMERATION;
  int VALUE_ARBITRARY;
  int DATATYPE_STRING;
  int DATATYPE_INTEGER;
  int DATATYPE_FLOAT;
  int DATATYPE_DOUBLE;
  int DATATYPE_BOOLEAN;
  int CHILD_POLICY_SOME;
  int CHILD_POLICY_SEQUENCE;
  int CHILD_POLICY_REPEAT;
  int CHILD_POLICY_MAX;
  int CHILD_POLICY_EMPTY;
  int CHILD_POLICY_CHOICE;
  int CHILD_POLICY_ALL;
}
class IIOMetadataController {
}
class IIOMetadata {
  int standardFormatSupported;
  int nativeMetadataFormatName;
  int nativeMetadataFormatClassName;
  int extraMetadataFormatNames;
  int extraMetadataFormatClassNames;
  int defaultController;
  int controller;
}
class IIOInvalidTreeException {
  int offendingNode;
  int serialVersionUID;
}
