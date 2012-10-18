package java.util.prefs;
class PreferencesFactory {
}
class Preferences {
  int MAX_VALUE_LENGTH;
  int MAX_KEY_LENGTH;
  int MAX_NAME_LENGTH;
  int factory;
  int prefsPermission;
  int defaultFactoryClass;
}
class PreferenceChangeListener {
}
class PreferenceChangeEvent {
  int newValue;
  int key;
  int serialVersionUID;
}
class NodeChangeListener {
}
class NodeChangeEvent {
  int child;
  int serialVersionUID;
}
class InvalidPreferencesFormatException {
  int serialVersionUID;
}
class BackingStoreException {
  int serialVersionUID;
}
class AbstractPreferences {
  int preferenceListeners;
  int nodeListeners;
  int childCache;
  int removed;
  int name;
  int parent;
  int newNode;
  int lock;
}
