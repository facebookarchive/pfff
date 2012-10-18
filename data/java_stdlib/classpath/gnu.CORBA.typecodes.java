package gnu.CORBA.typecodes;
class StringTypeCode {
  int len;
  int serialVersionUID;
}
class RecursiveTypeCode {
  int the_id;
  int serialVersionUID;
}
class RecordTypeCode {
  int default_index;
  int discriminator_type;
  int members;
  class Field {
    int visibility;
    int type;
    int name;
    int label;
  }
  int serialVersionUID;
}
class PrimitiveTypeCode {
  int kind;
  int serialVersionUID;
}
class GeneralTypeCode {
  int type_modifier;
  int len;
  int content_type;
  int concrete_base_type;
  int name;
  int id;
  int lengthAllowed;
  int UNSET;
  int serialVersionUID;
}
class FixedTypeCode {
  int scale;
  int digits;
  int serialVersionUID;
}
class ArrayTypeCode {
  int length;
  int of;
  int serialVersionUID;
}
class AliasTypeCode {
  int aliasFor;
  int name;
  int id;
  int serialVersionUID;
}
