package android.app.backup;
class WallpaperBackupHelper {
  int mDesiredMinHeight;
  int mDesiredMinWidth;
  int mKeys;
  int mFiles;
  int mContext;
  int STAGE_FILE;
  int WALLPAPER_INFO_KEY;
  int WALLPAPER_IMAGE_KEY;
  int WALLPAPER_INFO;
  int WALLPAPER_IMAGE;
  int DEBUG;
  int TAG;
}
class SharedPreferencesBackupHelper {
  int mPrefGroups;
  int mContext;
  int DEBUG;
  int TAG;
}
class RestoreSet {
  int CREATOR;
  int token;
  int device;
  int name;
}
class RestoreSession {
  class RestoreObserverWrapper {
    int MSG_RESTORE_SETS_AVAILABLE;
    int MSG_RESTORE_FINISHED;
    int MSG_UPDATE;
    int MSG_RESTORE_STARTING;
    int mAppObserver;
    int mHandler;
  }
  int mObserver;
  int mBinder;
  int mContext;
  int TAG;
}
class RestoreObserver {
}
class FullBackupDataOutput {
  int mData;
}
class FullBackupAgent {
}
class FullBackup {
  int CONF_TOKEN_INTENT_EXTRA;
  int FULL_RESTORE_INTENT_ACTION;
  int FULL_BACKUP_INTENT_ACTION;
  int SHARED_PREFIX;
  int APPS_PREFIX;
  int SHARED_STORAGE_TOKEN;
  int CACHE_TREE_TOKEN;
  int SHAREDPREFS_TREE_TOKEN;
  int DATABASE_TREE_TOKEN;
  int DATA_TREE_TOKEN;
  int ROOT_TREE_TOKEN;
  int OBB_TREE_TOKEN;
  int APK_TREE_TOKEN;
  int TAG;
}
class FileBackupHelperBase {
  int mExceptionLogged;
  int mContext;
  int mPtr;
  int TAG;
}
class FileBackupHelper {
  int mFiles;
  int mFilesDir;
  int mContext;
  int DEBUG;
  int TAG;
}
class BackupManager {
  int sService;
  int mContext;
  int TAG;
}
class BackupHelperDispatcher {
  int mHelpers;
  class Header {
    int keyPrefix;
    int chunkSize;
  }
  int TAG;
}
class BackupHelper {
}
class BackupDataOutput {
  int mBackupWriter;
}
class BackupDataInputStream {
  int mOneByte;
  int mData;
  int dataSize;
  int key;
}
class BackupDataInput {
  class EntityHeader {
    int dataSize;
    int key;
  }
  int mHeaderReady;
  int mHeader;
  int mBackupReader;
}
class BackupAgentHelper {
  int mDispatcher;
  int TAG;
}
class BackupAgent {
  class BackupServiceBinder {
    int TAG;
  }
  int mBinder;
  int TYPE_SYMLINK;
  int TYPE_DIRECTORY;
  int TYPE_FILE;
  int TYPE_EOF;
  int DEBUG;
  int TAG;
}
class AbsoluteFileBackupHelper {
  int mFiles;
  int mContext;
  int DEBUG;
  int TAG;
}
