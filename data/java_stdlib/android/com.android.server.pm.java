package com.android.server.pm;
class UserManagerTest {
  int mUserManager;
}
class UserManager {
  int mBaseUserPath;
  int mInstaller;
  int mUserIds;
  int mUserListFile;
  int mUsersDir;
  int mUsers;
  int USER_LIST_FILENAME;
  int USER_INFO_DIR;
  int LOG_TAG;
  int TAG_USER;
  int TAG_USERS;
  int ATTR_ID;
  int ATTR_FLAGS;
  int TAG_NAME;
}
class ShutdownThread {
  class CloseDialogReceiver {
    int dialog;
    int mContext;
  }
  int mHandler;
  int mScreenWakeLock;
  int mCpuWakeLock;
  int mPowerManager;
  int mContext;
  int mActionDone;
  int mActionDoneSync;
  int sInstance;
  int REBOOT_SAFEMODE_PROPERTY;
  int SHUTDOWN_ACTION_PROPERTY;
  int mRebootReason;
  int mRebootSafeMode;
  int mReboot;
  int sIsStarted;
  int sIsStartedGuard;
  int SHUTDOWN_VIBRATE_MS;
  int MAX_RADIO_WAIT_TIME;
  int MAX_SHUTDOWN_WAIT_TIME;
  int MAX_BROADCAST_TIME;
  int PHONE_STATE_POLL_SLEEP_MSEC;
  int TAG;
}
class SharedUserSetting {
  int signatures;
  int packages;
  int userId;
  int name;
}
class Settings {
  int FLAG_DUMP_SPEC;
  int mSystemDir;
  int mPendingPackages;
  int mReadMessages;
  int mRenamedPackages;
  int mPackagesToBeCleaned;
  int mPermissionTrees;
  int mPermissions;
  int mPastSignatures;
  int mOtherUserIds;
  int mUserIds;
  int mSharedUsers;
  int mPreferredActivities;
  int mVerifierDeviceIdentity;
  int mReadExternalStorageEnforced;
  int mExternalSdkPlatform;
  int mInternalSdkPlatform;
  int mDisabledSysPackages;
  int mPackages;
  int mBackupStoppedPackagesFilename;
  int mStoppedPackagesFilename;
  int mPackageListFilename;
  int mBackupSettingsFilename;
  int mSettingsFilename;
  int ATTR_STOPPED;
  int ATTR_ENABLED;
  int ATTR_NOT_LAUNCHED;
  int ATTR_NAME;
  int TAG_PACKAGE;
  int TAG_PACKAGE_RESTRICTIONS;
  int TAG_ENABLED_COMPONENTS;
  int TAG_DISABLED_COMPONENTS;
  int TAG_ITEM;
  int ATTR_ENFORCEMENT;
  int TAG_READ_EXTERNAL_STORAGE;
  int DEBUG_STOPPED;
  int TAG;
}
class PreferredActivity {
  int mPref;
  int DEBUG_FILTERS;
  int TAG;
}
class PendingPackage {
  int sharedId;
}
class PackageVerificationStateTest {
  int SUFFICIENT_UID_2;
  int SUFFICIENT_UID_1;
  int REQUIRED_UID;
}
class PackageVerificationState {
  int mRequiredVerificationPassed;
  int mRequiredVerificationComplete;
  int mSufficientVerificationPassed;
  int mSufficientVerificationComplete;
  int mRequiredVerifierUid;
  int mSufficientVerifierUids;
  int mArgs;
}
class PackageVerificationResponse {
  int callerUid;
  int code;
}
class PackageSignatures {
  int mSignatures;
}
class PackageSettingBase {
  int installerPackageName;
  int origPackage;
  int installStatus;
  int enabled;
  int enabledComponents;
  int disabledComponents;
  int notLaunched;
  int stopped;
  int haveGids;
  int permissionsFixed;
  int signatures;
  int uidError;
  int versionCode;
  int lastUpdateTime;
  int firstInstallTime;
  int timeStamp;
  int nativeLibraryPathString;
  int resourcePathString;
  int resourcePath;
  int codePathString;
  int codePath;
  int realName;
  int name;
  int PKG_INSTALL_INCOMPLETE;
  int PKG_INSTALL_COMPLETE;
}
class PackageSetting {
  int sharedUser;
  int pkg;
  int appId;
}
class PackageManagerSettingsTests {
  int PREFIX;
  int TAG;
  int localLOGV;
  int PACKAGE_NAME_1;
  int PACKAGE_NAME_3;
  int PACKAGE_NAME_2;
}
class PackageManagerService {
  int mMediaMounted;
  int SD_ENCRYPTION_ALGORITHM;
  int SD_ENCRYPTION_KEYSTORE_NAME;
  int DEBUG_SD_INSTALL;
  class DumpState {
    int mSharedUser;
    int mTitlePrinted;
    int mOptions;
    int mTypes;
    int OPTION_SHOW_FILTERS;
    int DUMP_PREFERRED_XML;
    int DUMP_PREFERRED;
    int DUMP_VERIFIERS;
    int DUMP_PROVIDERS;
    int DUMP_MESSAGES;
    int DUMP_SHARED_USERS;
    int DUMP_PACKAGES;
    int DUMP_PERMISSIONS;
    int DUMP_RESOLVERS;
    int DUMP_FEATURES;
    int DUMP_LIBS;
  }
  class ClearStorageConnection {
    int mContainerService;
  }
  class PackageRemovedInfo {
    int args;
    int isRemovedPackageSystemUpdate;
    int removedUid;
    int uid;
    int removedPackage;
  }
  class PackageInstalledInfo {
    int removedInfo;
    int returnCode;
    int pkg;
    int uid;
    int name;
  }
  class AsecInstallArgs {
    int libraryPath;
    int resourcePath;
    int packagePath;
    int cid;
    int PUBLIC_RES_FILE_NAME;
    int RES_FILE_NAME;
  }
  class FileInstallArgs {
    int created;
    int libraryPath;
    int resourceFileName;
    int codeFileName;
    int installDir;
  }
  class InstallArgs {
    int manifestDigest;
    int installerPackageName;
    int packageURI;
    int flags;
    int observer;
  }
  class MoveParams {
    int mRet;
    int uid;
    int targetArgs;
    int srcArgs;
    int packageName;
    int flags;
    int observer;
  }
  class InstallParams {
    int encryptionParams;
    int mTempPackage;
    int mRet;
    int mArgs;
    int manifestDigest;
    int verificationURI;
    int installerPackageName;
    int mPackageURI;
    int flags;
    int observer;
  }
  class MeasureParams {
    int mObserver;
    int mSuccess;
    int mStats;
  }
  class HandlerParams {
    int mRetries;
    int MAX_RETRIES;
  }
  class AppDirObserver {
    int mIsRom;
    int mRootDir;
  }
  int mProviderInitOrderSorter;
  int mResolvePrioritySorter;
  class ServiceIntentResolver {
    int mFlags;
    int mServices;
  }
  class ActivityIntentResolver {
    int mFlags;
    int mActivities;
  }
  int UPDATE_PERMISSIONS_REPLACE_ALL;
  int UPDATE_PERMISSIONS_REPLACE_PKG;
  int UPDATE_PERMISSIONS_ALL;
  int DEX_OPT_FAILED;
  int DEX_OPT_DEFERRED;
  int DEX_OPT_PERFORMED;
  int DEX_OPT_SKIPPED;
  class PackageHandler {
    int mPendingInstalls;
    int mBound;
  }
  int mRequiredVerifierPackage;
  int mNextInstallToken;
  int mRunningInstalls;
  class PostInstallData {
    int res;
    int args;
  }
  class DefaultContainerConnection {
  }
  int mDefContainerConn;
  int mDirtyUsers;
  int sUserManager;
  int BROADCAST_DELAY;
  int WRITE_SETTINGS_DELAY;
  int CHECK_PENDING_VERIFICATION;
  int PACKAGE_VERIFIED;
  int WRITE_PACKAGE_RESTRICTIONS;
  int WRITE_SETTINGS;
  int UPDATED_MEDIA_STATUS;
  int MCS_GIVE_UP;
  int MCS_RECONNECT;
  int POST_INSTALL;
  int FIND_INSTALL_LOC;
  int START_CLEANING_PACKAGE;
  int MCS_UNBIND;
  int INIT_COPY;
  int END_COPY;
  int MCS_BOUND;
  int SEND_PENDING_BROADCAST;
  int mContainerService;
  int mPendingBroadcasts;
  int mPlatformPackage;
  int mResolveComponentName;
  int mResolveInfo;
  int mResolveActivity;
  int mAndroidApplication;
  int mHasSystemUidErrors;
  int mSafeMode;
  int mSystemReady;
  int mPendingVerificationToken;
  int mDeferredDexOpt;
  int mPendingVerification;
  int mProtectedBroadcasts;
  int mTransferedPackages;
  int mPermissionGroups;
  int mInstrumentation;
  int mProviders;
  int mProvidersByComponent;
  int mServices;
  int mReceivers;
  int mActivities;
  int mAvailableFeatures;
  int mTmpSharedLibraries;
  int mSharedLibraries;
  int mSystemPermissions;
  int mGlobalGids;
  int mRestoredSettings;
  int mSettings;
  int mPackages;
  int mOutPermissions;
  int mLastScanError;
  int mScanningPath;
  int mAppDirs;
  int mInstallLock;
  int mDrmAppPrivateInstallDir;
  int mDalvikCacheDir;
  int mAppInstallDir;
  int mVendorAppDir;
  int mSystemAppDir;
  int mFrameworkDir;
  int mInstaller;
  int mDrmAppInstallObserver;
  int mAppInstallObserver;
  int mVendorInstallObserver;
  int mSystemInstallObserver;
  int mFrameworkInstallObserver;
  int mAsecInternalPath;
  int mUserAppDataDir;
  int mAppDataDir;
  int mSeparateProcesses;
  int mDefParseFlags;
  int mMetrics;
  int mNoDexOpt;
  int mOnlyCore;
  int mFactoryTest;
  int mContext;
  int mSdkCodename;
  int mSdkVersion;
  int mHandler;
  int mHandlerThread;
  int mTempContainerPrefix;
  int LIB_DIR_NAME;
  int PACKAGE_MIME_TYPE;
  int DEFAULT_CONTAINER_COMPONENT;
  int DEFAULT_CONTAINER_PACKAGE;
  int DEFAULT_VERIFICATION_TIMEOUT;
  int DEFAULT_VERIFY_ENABLE;
  int REMOVE_CHATTY;
  int SCAN_BOOTING;
  int SCAN_DEFER_DEX;
  int SCAN_UPDATE_TIME;
  int SCAN_NO_PATHS;
  int SCAN_NEW_INSTALL;
  int SCAN_UPDATE_SIGNATURE;
  int SCAN_FORCE_DEX;
  int SCAN_NO_DEX;
  int SCAN_MONITOR;
  int INSTALL_PACKAGE_SUFFIX;
  int OBSERVER_EVENTS;
  int ADD_EVENTS;
  int REMOVE_EVENTS;
  int GET_CERTIFICATES;
  int NFC_UID;
  int LOG_UID;
  int RADIO_UID;
  int DEBUG_VERIFY;
  int DEBUG_APP_DIR_OBSERVER;
  int DEBUG_PACKAGE_SCANNING;
  int DEBUG_INTENT_MATCHING;
  int DEBUG_PACKAGE_INFO;
  int DEBUG_SHOW_INFO;
  int DEBUG_REMOVE;
  int DEBUG_INSTALL;
  int DEBUG_UPGRADE;
  int DEBUG_PREFERRED;
  int DEBUG_SETTINGS;
  int TAG;
}
class Installer {
  int buflen;
  int buf;
  int mSocket;
  int mOut;
  int mIn;
  int LOCAL_DEBUG;
  int TAG;
}
class GrantedPermissions {
  int gids;
  int grantedPermissions;
  int pkgFlags;
}
class BasePermission {
  int gids;
  int uid;
  int pendingInfo;
  int perm;
  int protectionLevel;
  int type;
  int packageSetting;
  int sourcePackage;
  int name;
  int TYPE_DYNAMIC;
  int TYPE_BUILTIN;
  int TYPE_NORMAL;
}
