package java.lang.annotation;
class RetentionPolicy {
  int RUNTIME;
  int CLASS;
  int SOURCE;
}
class IncompleteAnnotationException {
  int elementName;
  int annotationType;
  int serialVersionUID;
}
class ElementType {
  int PACKAGE;
  int ANNOTATION_TYPE;
  int LOCAL_VARIABLE;
  int CONSTRUCTOR;
  int PARAMETER;
  int METHOD;
  int FIELD;
  int TYPE;
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
