package com.android.providers.settings;
class SettingsProvider {
  class SettingsCache {
    int mCacheFullyMatchesDisk;
    int mCacheName;
  }
  class SettingsFileObserver {
    int mPath;
    int mIsDirty;
  }
  int sObserverInstance;
  class SqlArguments {
    int args;
    int where;
    int table;
  }
  int mBackupManager;
  int mOpenHelper;
  int TOO_LARGE_TO_CACHE_MARKER;
  int NULL_SETTING;
  int MAX_CACHE_ENTRY_SIZE;
  int sKnownMutationsInFlight;
  int sSecureCache;
  int sSystemCache;
  int MAX_CACHE_ENTRIES;
  int COLUMN_VALUE;
  int TABLE_OLD_FAVORITES;
  int TABLE_FAVORITES;
  int LOCAL_LOGV;
  int TAG;
}
class SettingsHelper {
  int mPowerManager;
  int mContentService;
  int mAudioManager;
  int mContext;
  int TAG;
}
class SettingsBackupAgent {
  class WifiNetworkSettings {
    int mNetworks;
    int mKnownNetworks;
  }
  class Network {
    int rawLines;
    int key_mgmt;
    int ssid;
  }
  int mWifiConfigFile;
  int mWfm;
  int mSettingsHelper;
  int STAGE_FILE;
  int KEY_WIFI_CONFIG;
  int KEY_WIFI_SUPPLICANT;
  int FILE_WIFI_SUPPLICANT_TEMPLATE;
  int FILE_WIFI_SUPPLICANT;
  int PROJECTION;
  int COLUMN_VALUE;
  int COLUMN_NAME;
  int TAG;
  int EMPTY_DATA;
  int INTEGER_BYTE_COUNT;
  int FULL_BACKUP_VERSION;
  int STATE_SIZE;
  int STATE_WIFI_CONFIG;
  int STATE_WIFI_SUPPLICANT;
  int STATE_LOCALE;
  int STATE_SECURE;
  int STATE_SYSTEM;
  int STATE_VERSION;
  int STATE_VERSION_1_SIZE;
  int STATE_VERSION_1;
  int KEY_LOCALE;
  int KEY_SECURE;
  int KEY_SYSTEM;
  int DEBUG_BACKUP;
  int DEBUG;
}
class DatabaseHelper {
  int mValidTables;
  int mContext;
  int DATABASE_VERSION;
  int DATABASE_NAME;
  int TAG;
}
