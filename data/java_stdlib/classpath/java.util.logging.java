package java.util.logging;
class XMLFormatter {
  int iso8601;
  int lineSep;
}
class StreamHandler {
  int STATE_CLOSED;
  int STATE_PUBLISHED;
  int STATE_FRESH;
  int streamState;
  int writer;
  int out;
}
class SocketHandler {
}
class SimpleFormatter {
  int lineSep;
  int dateFormat;
}
class MemoryHandler {
  int target;
  int pushLevel;
  int numPublished;
  int position;
  int buffer;
}
class LoggingPermission {
  int serialVersionUID;
}
class LoggingMXBean {
}
class Logger {
  int parent;
  int level;
  int useParentHandlers;
  int anonymous;
  int handlers;
  int handlerList;
  int filter;
  int resourceBundle;
  int resourceBundleName;
  int name;
  int lock;
  int global;
  int root;
}
class LogRecord {
  int lastSeqNum;
  int serialVersionUID;
  int bundle;
  int parameters;
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
}
class LogManager {
  int controlPermission;
  int CONFIG_PROPERTY;
  int MANAGER_PROPERTY;
  int pcs;
  int properties;
  int loggers;
  int loggingBean;
  int logManager;
  int LOGGING_MXBEAN_NAME;
}
class Level {
  int serialVersionUID;
  int resourceBundleName;
  int value;
  int name;
  int knownLevels;
  int ALL;
  int FINEST;
  int FINER;
  int FINE;
  int CONFIG;
  int INFO;
  int WARNING;
  int SEVERE;
  int OFF;
}
class Handler {
  int encoding;
  int errorManager;
  int level;
  int filter;
  int formatter;
}
class Formatter {
}
class Filter {
}
class FileHandler {
  class ostr {
  }
  int logFiles;
  int written;
  int append;
  int pattern;
  int count;
  int limit;
  int DEFAULT_APPEND;
  int APPEND_KEY;
  int DEFAULT_COUNT;
  int COUNT_KEY;
  int DEFAULT_LIMIT;
  int LIMIT_KEY;
  int DEFAULT_PATTERN;
  int PATTERN_KEY;
  int PROPERTY_PREFIX;
}
class ErrorManager {
  int everUsed;
  int FORMAT_FAILURE;
  int OPEN_FAILURE;
  int CLOSE_FAILURE;
  int FLUSH_FAILURE;
  int WRITE_FAILURE;
  int GENERIC_FAILURE;
}
class ConsoleHandler {
}
