package java.util.prefs;
class XMLParser {
  int indent;
  int builder;
  int XML_VERSION;
  int FILE_PREFS;
  int DOCTYPE;
  int HEADER;
  int PREFS_DTD;
  int PREFS_DTD_NAME;
}
class PreferencesFactory {
}
class Preferences {
  int factory;
  int MAX_VALUE_LENGTH;
  int MAX_NAME_LENGTH;
  int MAX_KEY_LENGTH;
}
class PreferenceChangeListener {
}
class PreferenceChangeEvent {
  int value;
  int key;
  int node;
  int serialVersionUID;
}
class NodeSet {
  int list;
}
class NodeChangeListener {
}
class NodeChangeEvent {
  int child;
  int parent;
  int serialVersionUID;
}
class InvalidPreferencesFormatException {
  int serialVersionUID;
}
class FilePreferencesImpl {
  int updated;
  int removed;
  int dir;
  int prefsFile;
  int prefs;
  int path;
  int SYSTEM_HOME;
  int USER_HOME;
  int PREFS_FILE_NAME;
}
class FilePreferencesFactoryImpl {
  int SYSTEM_ROOT;
  int USER_ROOT;
}
class BackingStoreException {
  int serialVersionUID;
}
class AbstractPreferences {
  class NodeRemoveEvent {
    int serialVersionUID;
  }
  class NodeAddEvent {
    int serialVersionUID;
  }
  class EventDispatcher {
  }
  int root;
  int isRemoved;
  int parentPref;
  int nodeName;
  int preferenceChangeListeners;
  int nodeChangeListeners;
  int cachedNode;
  int newNode;
  int lock;
  int userNode;
  int dispatcher;
  int events;
}
