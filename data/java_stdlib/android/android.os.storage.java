package android.os.storage;
class StorageVolume {
  int CREATOR;
  int EXTRA_STORAGE_VOLUME;
  int mMaxFileSize;
  int mStorageId;
  int mAllowMassStorage;
  int mMtpReserveSpace;
  int mEmulated;
  int mRemovable;
  int mDescriptionId;
  int mPath;
}
class StorageResultCode {
  int OperationFailedStorageBusy;
  int OperationFailedStorageMounted;
  int OperationFailedStorageNotMounted;
  int OperationFailedMediaCorrupt;
  int OperationFailedMediaBlank;
  int OperationFailedNoMedia;
  int OperationFailedInternalError;
  int OperationSucceeded;
}
class StorageManagerIntegrationTest {
  int mFile;
  int LOG_TAG;
}
class StorageManagerBaseTest {
  class ObbListener {
    int mState;
    int mDone;
    int mOfficialPath;
    int LOG_TAG;
  }
  class MountingObbThread {
    int mOfficialObbFilePath;
    int mPathToContentsFile;
    int mObbFilePath;
    int mFileOpenOnObb;
    int mStop;
  }
  int SAMPLE2_TEXT;
  int SAMPLE1_TEXT;
  int DONT_FORCE;
  int FORCE;
  int OBB_FILE_3_BAD_PACKAGENAME;
  int OBB_FILE_3_ENCRYPTED;
  int OBB_FILE_3_PASSWORD;
  int OBB_FILE_2_UNSIGNED;
  int OBB_FILE_1_ENCRYPTED;
  int OBB_FILE_1_PASSWORD;
  int OBB_FILE_3;
  int OBB_FILE_2;
  int OBB_FILE_1_CONTENTS_1;
  int OBB_FILE_1;
  int WAIT_TIME_INCR;
  int MAX_WAIT_TIME;
  int LOG_TAG;
  int mSm;
  int mContext;
}
class StorageManager {
  class ListenerDelegate {
    int mHandler;
    int mStorageEventListener;
  }
  class StorageStateChangedStorageEvent {
    int newState;
    int oldState;
    int path;
  }
  class UmsConnectionChangedStorageEvent {
    int available;
  }
  class StorageEvent {
    int mMessage;
    int EVENT_OBB_STATE_CHANGED;
    int EVENT_STORAGE_STATE_CHANGED;
    int EVENT_UMS_CONNECTION_CHANGED;
  }
  class ObbStateChangedStorageEvent {
    int state;
    int path;
  }
  class ObbListenerDelegate {
    int nonce;
    int mHandler;
    int mObbEventListenerRef;
  }
  class ObbActionListener {
    int mListeners;
  }
  int mObbActionListener;
  class MountServiceBinderListener {
  }
  int mNextNonce;
  int mListeners;
  int mBinderListener;
  int mTgtLooper;
  int mMountService;
  int TAG;
}
class StorageListener {
  int doneFlag;
  int mTargetState;
  int TAG;
  int localLOGV;
}
class StorageEventListener {
}
class OnObbStateChangeListener {
  int ERROR_PERMISSION_DENIED;
  int ERROR_ALREADY_MOUNTED;
  int ERROR_NOT_MOUNTED;
  int ERROR_COULD_NOT_UNMOUNT;
  int ERROR_COULD_NOT_MOUNT;
  int ERROR_INTERNAL;
  int UNMOUNTED;
  int MOUNTED;
}
class MountServiceListener {
}
class IObbActionListener {
  class Stub {
    int TRANSACTION_onObbResult;
    class Proxy {
      int mRemote;
    }
    int DESCRIPTOR;
  }
}
class IMountShutdownObserver {
  class Stub {
    int TRANSACTION_onShutDownComplete;
    class Proxy {
      int mRemote;
    }
    int DESCRIPTOR;
  }
}
class IMountServiceListener {
  class Stub {
    int TRANSACTION_onStorageStateChanged;
    int TRANSACTION_onUsbMassStorageConnectionChanged;
    class Proxy {
      int mRemote;
    }
    int DESCRIPTOR;
  }
}
class IMountService {
  int ENCRYPTION_STATE_ERROR_INCOMPLETE;
  int ENCRYPTION_STATE_ERROR_UNKNOWN;
  int ENCRYPTION_STATE_OK;
  int ENCRYPTION_STATE_NONE;
  class Stub {
    int TRANSACTION_fixPermissionsSecureContainer;
    int TRANSACTION_verifyEncryptionPassword;
    int TRANSACTION_getEncryptionState;
    int TRANSACTION_getSecureContainerFilesystemPath;
    int TRANSACTION_getVolumeList;
    int TRANSACTION_changeEncryptionPassword;
    int TRANSACTION_encryptStorage;
    int TRANSACTION_decryptStorage;
    int TRANSACTION_isExternalStorageEmulated;
    int TRANSACTION_getMountedObbPath;
    int TRANSACTION_isObbMounted;
    int TRANSACTION_unmountObb;
    int TRANSACTION_mountObb;
    int TRANSACTION_finishMediaUpdate;
    int TRANSACTION_shutdown;
    int TRANSACTION_getSecureContainerList;
    int TRANSACTION_getSecureContainerPath;
    int TRANSACTION_renameSecureContainer;
    int TRANSACTION_isSecureContainerMounted;
    int TRANSACTION_unmountSecureContainer;
    int TRANSACTION_mountSecureContainer;
    int TRANSACTION_destroySecureContainer;
    int TRANSACTION_finalizeSecureContainer;
    int TRANSACTION_createSecureContainer;
    int TRANSACTION_getVolumeState;
    int TRANSACTION_getStorageUsers;
    int TRANSACTION_formatVolume;
    int TRANSACTION_unmountVolume;
    int TRANSACTION_mountVolume;
    int TRANSACTION_isUsbMassStorageEnabled;
    int TRANSACTION_setUsbMassStorageEnabled;
    int TRANSACTION_isUsbMassStorageConnected;
    int TRANSACTION_unregisterListener;
    int TRANSACTION_registerListener;
    int DESCRIPTOR;
    class Proxy {
      int mRemote;
    }
  }
}
class AsecTests {
  class ShutdownObserver {
    int statusCode;
    int doneFlag;
  }
  class MultipleStorageLis {
    int count;
  }
  class StorageListener {
    int doneFlag;
    int path;
    int newState;
    int oldState;
  }
  int WAIT_TIME_INCR;
  int MAX_WAIT_TIME;
  int FS_EXT4;
  int FS_FAT;
  int TAG;
  int localLOGV;
  int SECURE_CONTAINER_PREFIX;
}
