package gnu.classpath.jdwp.util;
class VariableTable {
  int names;
  int sigs;
  int lengths;
  int slot;
  int lineCI;
  int slots;
  int argCnt;
}
class Signature {
}
class NullObject {
}
class MonitorInfo {
  int waiters;
  int owner;
  int entryCount;
}
class MethodResult {
  int thrownException;
  int returnedValue;
}
class Location {
  int index;
  int method;
}
class LineTable {
  int lineCI;
  int lineNum;
  int end;
  int start;
}
class JdwpString {
}
