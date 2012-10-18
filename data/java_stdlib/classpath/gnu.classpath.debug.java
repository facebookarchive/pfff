package gnu.classpath.debug;
class TeeWriter {
  int sink;
  int out;
}
class TeeReader {
  int out;
  int in;
}
class TeeOutputStream {
  int sink;
  int out;
}
class TeeInputStream {
  int out;
  int in;
}
class SystemLogger {
  int SYSTEM;
}
class Simple1LineFormatter {
  int threadFormat;
  int dateFormat;
  int LS;
  int SPACES_6;
  int SPACES_32;
  int THREAD_PATTERN;
  int DAT_PATTERN;
}
class PreciseFilter {
  int enabled;
  int GLOBAL;
}
class Component {
  int endIndex;
  int startIndex;
  int IPP;
  int POLICY;
  int X509;
  int CRYPTO;
  int SSL_DELEGATED_TASK;
  int SSL_KEY_EXCHANGE;
  int SSL_RECORD_LAYER;
  int SSL_HANDSHAKE;
  int SSL;
  int EVERYTHING;
}
