package org.json;
class SelfUseTest {
  int tokener;
  int array;
  int object;
  int tokenerNextValueCalls;
  int tokenerNextCalls;
  int arrayOptTypeCalls;
  int arrayOptCalls;
  int arrayGetCalls;
  int arrayPutCalls;
  int objectOptTypeCalls;
  int objectOptCalls;
  int objectGetCalls;
  int objectPutCalls;
}
class ParsingTest {
}
class JSONTokenerTest {
}
class JSONTokener {
  int pos;
  int in;
}
class JSONStringerTest {
}
class JSONStringer {
  int indent;
  int stack;
  class Scope {
    int NULL;
    int NONEMPTY_OBJECT;
    int DANGLING_KEY;
    int EMPTY_OBJECT;
    int NONEMPTY_ARRAY;
    int EMPTY_ARRAY;
  }
  int out;
}
class JSONObjectTest {
}
class JSONObject {
  int nameValuePairs;
  int NULL;
  int NEGATIVE_ZERO;
}
class JSONException {
}
class JSONArrayTest {
}
class JSONArray {
  int values;
}
class JSON {
}
