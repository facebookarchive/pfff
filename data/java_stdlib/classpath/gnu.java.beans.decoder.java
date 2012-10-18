package gnu.java.beans.decoder;
class VoidHandler {
}
class StringHandler {
}
class StaticMethodContext {
  int methodName;
  int klass;
  int arguments;
}
class SimpleHandler {
  int context;
}
class ShortHandler {
}
class PropertyContext {
  int methodCalled;
  int prefix;
  int propertyName;
  int argument;
}
class PersistenceParser {
  class ArrayHandlerCreator {
  }
  class NullHandlerCreator {
  }
  class ClassHandlerCreator {
  }
  class VoidHandlerCreator {
  }
  class ObjectHandlerCreator {
  }
  class JavaHandlerCreator {
  }
  class StringHandlerCreator {
  }
  class CharHandlerCreator {
  }
  class DoubleHandlerCreator {
  }
  class FloatHandlerCreator {
  }
  class LongHandlerCreator {
  }
  class IntHandlerCreator {
  }
  class ShortHandlerCreator {
  }
  class ByteHandlerCreator {
  }
  class BooleanHandlerCreator {
  }
  class Creator {
  }
  int decoder;
  int objects;
  int javaHandler;
  int currentHandler;
  int handlerCreators;
  int skipElement;
  int exceptionListener;
}
class ObjectHandler {
}
class ObjectContext {
}
class NullHandler {
}
class MethodFinder {
  int typeMapping;
}
class MethodContext {
  int methodName;
  int arguments;
}
class LongHandler {
}
class JavaHandler {
  int classLoader;
  int objectMap;
  int context;
}
class IntHandler {
}
class IndexContext {
  int isSetter;
  int index;
  int argument;
  int result;
}
class GrowableArrayContext {
  int length;
  int array;
  int klass;
  int INITIAL_SIZE;
}
class FloatHandler {
}
class ElementHandler {
}
class DummyHandler {
}
class DummyContext {
}
class DoubleHandler {
}
class DecoderContext {
  int objects;
  int decoder;
}
class Context {
}
class ConstructorContext {
  int klass;
  int arguments;
}
class ClassHandler {
}
class CharHandler {
}
class ByteHandler {
}
class BooleanHandler {
}
class AssemblyException {
}
class ArrayHandler {
  int typeMap;
}
class ArrayContext {
  int array;
}
class AbstractObjectContext {
  int object;
}
class AbstractElementHandler {
  int allowsSubelements;
  int buffer;
  int hasFailed;
  int parent;
  int context;
}
class AbstractCreatableObjectContext {
}
class AbstractContext {
  int id;
  int isStatement;
}
