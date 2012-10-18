package java.util.logging;
class XMLFormatter {
  int indent;
}
class StreamHandler {
  int writerNotInitialized;
  int writer;
  int os;
}
class SocketHandler {
  int socket;
  int DEFAULT_FORMATTER;
  int DEFAULT_LEVEL;
}
class SimpleFormatter {
}
class MemoryHandler {
  int cursor;
  int buffer;
  int manager;
  int push;
  int size;
  int target;
  int DEFAULT_SIZE;
}
class LoggingPermission {
}
class LoggingMXBean {
}
class Logger {
  int dalvikLogHandler;
  int androidTag;
  int children;
  int isNamed;
  int notifyParentHandlers;
  int handlers;
  int resourceBundle;
  int resourceBundleName;
  int filter;
  int levelIntVal;
  int levelObjVal;
  int parent;
  int name;
  int EMPTY_HANDLERS_ARRAY;
  int global;
  int GLOBAL_LOGGER_NAME;
  int GENERAL_LOG_HANDLER;
}
class LogRecord {
  int sourceInitialized;
  int parameters;
  int resourceBundle;
  int resourceBundleName;
  int loggerName;
  int thrown;
  int millis;
  int threadID;
  int message;
  int sourceMethodName;
  int sourceClassName;
  int sequenceNumber;
  int level;
  int initThreadId;
  int currentThreadId;
  int currentSequenceNumber;
  int MINOR;
  int MAJOR;
  int serialVersionUID;
}
class LogManager {
  int listeners;
  int props;
  int loggers;
  int LOGGING_MXBEAN_NAME;
  int manager;
  int perm;
}
class Level {
  int rb;
  int resourceBundleName;
  int value;
  int name;
  int ALL;
  int FINEST;
  int FINER;
  int FINE;
  int CONFIG;
  int INFO;
  int WARNING;
  int SEVERE;
  int OFF;
  int levels;
  int serialVersionUID;
}
class Handler {
  int prefix;
  int filter;
  int formatter;
  int level;
  int encoding;
  int errorMan;
  int DEFAULT_LEVEL;
}
class Formatter {
}
class Filter {
}
class FileHandler {
  class MeasureOutputStream {
    int length;
    int wrapped;
  }
  int uniqueID;
  int fileName;
  int lock;
  int files;
  int output;
  int manager;
  int pattern;
  int append;
  int limit;
  int count;
  int allLocks;
  int DEFAULT_PATTERN;
  int DEFAULT_APPEND;
  int DEFAULT_LIMIT;
  int DEFAULT_COUNT;
  int LCK_EXT;
}
class ErrorManager {
  int called;
  int FAILURES;
  int FORMAT_FAILURE;
  int OPEN_FAILURE;
  int CLOSE_FAILURE;
  int FLUSH_FAILURE;
  int WRITE_FAILURE;
  int GENERIC_FAILURE;
}
class ConsoleHandler {
}
