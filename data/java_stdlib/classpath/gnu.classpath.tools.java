package gnu.classpath.tools;
class StringToolkit {
}
class NotifyingInputStreamReader {
  int listeners;
  int flushed;
  int decodingFinished;
  int allInputConsumed;
  int columnNumber;
  int lineNumber;
  int readBuffer;
  int charBuffer;
  int byteBuffer;
  int decoder;
  int in;
  int DEFAULT_OUTPUT_BUFFER_SIZE;
  int DEFAULT_INPUT_BUFFER_SIZE;
}
class MalformedInputListener {
}
class MalformedInputEvent {
  int length;
  int columnNumber;
  int lineNumber;
}
class IOToolkit {
}
class FileSystemClassLoader {
  class JarStreamInfo {
    int jarEntry;
    int jarFile;
    int file;
  }
  class FileStreamInfo {
    int file;
  }
  class StreamInfo {
  }
  int pathComponents;
}
