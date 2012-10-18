package gnu.java.util.prefs;
class NodeWriter {
  int subtree;
  int bw;
  int prefs;
}
class NodeReader {
  int factory;
  int line;
  int br;
}
class MemoryBasedPreferences {
  int entries;
  int isUser;
}
class MemoryBasedFactory {
  int userPreferences;
  int systemPreferences;
}
class GConfBasedPreferences {
  int isUser;
  int node;
  int DEFAULT_SYSTEM_ROOT;
  int DEFAULT_USER_ROOT;
  int backend;
  int PERMISSION;
}
class GConfBasedFactory {
  int userPreferences;
  int systemPreferences;
}
class FileBasedPreferences {
  int properties;
  int dataFile;
  int directory;
  int DATA_FILE;
}
class FileBasedFactory {
  int userPreferences;
  int systemPreferences;
}
