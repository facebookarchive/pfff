package android.filterfw.io;
class TextGraphReader {
  class ConnectCommand {
    int mTargetName;
    int mTargetFilter;
    int mSourcePort;
    int mSourceFilter;
  }
  class InitFilterCommand {
    int mParams;
  }
  class AllocateFilterCommand {
    int mFilterName;
    int mClassName;
  }
  class AddLibraryCommand {
    int mLibraryName;
  }
  class ImportPackageCommand {
    int mPackageName;
  }
  class Command {
  }
  int mFactory;
  int mSettings;
  int mBoundReferences;
  int mCurrentGraph;
  int mCurrentFilter;
  int mCommands;
}
class PatternScanner {
  int mStartOfLine;
  int mLineNo;
  int mOffset;
  int mIgnorePattern;
  int mInput;
}
class GraphReader {
  int mReferences;
}
class GraphIOException {
}
