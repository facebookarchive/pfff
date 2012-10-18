package android.filterfw;
class MffEnvironment {
  int mContext;
}
class GraphEnvironment {
  class GraphHandle {
    int mSyncRunner;
    int mAsyncRunner;
    int mGraph;
  }
  int mGraphs;
  int mGraphReader;
  int MODE_SYNCHRONOUS;
  int MODE_ASYNCHRONOUS;
}
class FilterFunctionEnvironment {
}
