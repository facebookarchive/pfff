package gnu.classpath.jdwp.event.filters;
class ThreadOnlyFilter {
  int _tid;
}
class StepFilter {
  int _depth;
  int _size;
  int _tid;
}
class LocationOnlyFilter {
  int _location;
}
class InstanceOnlyFilter {
  int _instance;
}
class IEventFilter {
}
class FieldOnlyFilter {
  int _fieldId;
  int _refId;
}
class ExceptionOnlyFilter {
  int _uncaught;
  int _caught;
  int _refId;
}
class CountFilter {
  int _count;
}
class ConditionalFilter {
}
class ClassOnlyFilter {
  int _id;
}
class ClassMatchFilter {
  int _pattern;
}
class ClassExcludeFilter {
}
