package java.lang.annotation;
class RetentionPolicy {
  int SOURCE;
  int RUNTIME;
  int CLASS;
  int serialVersionUID;
}
class IncompleteAnnotationException {
  int elementName;
  int annotationType;
}
class ElementType {
  int TYPE;
  int PARAMETER;
  int PACKAGE;
  int METHOD;
  int LOCAL_VARIABLE;
  int FIELD;
  int CONSTRUCTOR;
  int ANNOTATION_TYPE;
  int serialVersionUID;
}
class AnnotationTypeMismatchException {
  int foundType;
  int element;
  int serialVersionUID;
}
class AnnotationFormatError {
  int serialVersionUID;
}
class Annotation {
}
