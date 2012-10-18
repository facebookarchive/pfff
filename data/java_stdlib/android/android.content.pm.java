package android.content.pm;
class XmlSerializerAndParser {
}
class VerifierInfo {
  int CREATOR;
  int publicKey;
  int packageName;
}
class VerifierDeviceIdentityTest {
  class MockRandom {
    int mNextLong;
  }
  int TEST_SUBSTITUTION_UNCORRECTED;
  int TEST_SUBSTITUTION_CORRECTED;
  int TEST_OVERFLOW_ENCODED;
  int TEST_NEGONE_ENCODED;
  int TEST_NEGONE;
  int TEST_ZERO_ENCODED;
  int TEST_ZERO;
  int TEST_MINVALUE_ENCODED;
  int TEST_MINVALUE;
  int TEST_MAXVALUE_ENCODED;
  int TEST_MAXVALUE;
  int TEST_2;
  int TEST_1_ENCODED_LOWERCASE;
  int TEST_1_ENCODED;
  int TEST_1;
}
class VerifierDeviceIdentity {
  int CREATOR;
  int SEPARATOR;
  int ENCODE;
  int mIdentityString;
  int mIdentity;
  int GROUP_SIZE;
  int LONG_SIZE;
}
class UserInfo {
  int CREATOR;
  int flags;
  int name;
  int id;
  int FLAG_GUEST;
  int FLAG_ADMIN;
  int FLAG_PRIMARY;
}
class Signature {
  int CREATOR;
  int mStringRef;
  int mHaveHashCode;
  int mHashCode;
  int mSignature;
}
class ServiceInfo {
  int CREATOR;
  int flags;
  int FLAG_ISOLATED_PROCESS;
  int FLAG_STOP_WITH_TASK;
  int permission;
}
class ResolveInfo {
  class DisplayNameComparator {
    int mPM;
    int sCollator;
  }
  int CREATOR;
  int system;
  int resolvePackageName;
  int icon;
  int nonLocalizedLabel;
  int labelRes;
  int isDefault;
  int specificIndex;
  int match;
  int preferredOrder;
  int priority;
  int filter;
  int serviceInfo;
  int activityInfo;
}
class RegisteredServicesCacheListener {
}
class RegisteredServicesCache {
  class ServiceInfo {
    int uid;
    int componentName;
    int type;
  }
  int mHandler;
  int mListener;
  int mPersistentServicesFile;
  int mPersistentServicesFileDidNotExist;
  int mServices;
  int mPersistentServices;
  int mServicesLock;
  int mReceiver;
  int mSerializerAndParser;
  int mAttributesName;
  int mMetaDataName;
  int mInterfaceName;
  int mContext;
  int TAG;
}
class ProviderInfo {
  int CREATOR;
  int isSyncable;
  int initOrder;
  int multiprocess;
  int pathPermissions;
  int uriPermissionPatterns;
  int grantUriPermissions;
  int writePermission;
  int readPermission;
  int authority;
}
class PermissionInfo {
  int CREATOR;
  int protectionLevel;
  int nonLocalizedDescription;
  int descriptionRes;
  int group;
  int PROTECTION_MASK_FLAGS;
  int PROTECTION_MASK_BASE;
  int PROTECTION_FLAG_DEVELOPMENT;
  int PROTECTION_FLAG_SYSTEM;
  int PROTECTION_SIGNATURE_OR_SYSTEM;
  int PROTECTION_SIGNATURE;
  int PROTECTION_DANGEROUS;
  int PROTECTION_NORMAL;
}
class PermissionGroupInfo {
  int CREATOR;
  int priority;
  int flags;
  int FLAG_PERSONAL_INFO;
  int nonLocalizedDescription;
  int descriptionRes;
}
class PathPermission {
  int CREATOR;
  int mWritePermission;
  int mReadPermission;
}
class ParceledListSlice {
  int CREATOR;
  int mIsLastSlice;
  int mNumItems;
  int mParcel;
  int MAX_IPC_SIZE;
}
class PackageStats {
  int CREATOR;
  int externalObbSize;
  int externalMediaSize;
  int externalCacheSize;
  int externalDataSize;
  int externalCodeSize;
  int cacheSize;
  int dataSize;
  int codeSize;
  int packageName;
}
class PackageParser {
  class ServiceIntentInfo {
    int service;
  }
  class ActivityIntentInfo {
    int activity;
  }
  class IntentInfo {
    int logo;
    int icon;
    int nonLocalizedLabel;
    int labelRes;
    int hasDefault;
  }
  class Instrumentation {
    int info;
  }
  class Provider {
    int syncable;
    int info;
  }
  class Service {
    int info;
  }
  class Activity {
    int info;
  }
  class PermissionGroup {
    int info;
  }
  class Permission {
    int group;
    int tree;
    int info;
  }
  class Component {
    int componentShortName;
    int componentName;
    int metaData;
    int className;
    int intents;
    int owner;
  }
  class Package {
    int manifestDigest;
    int installLocation;
    int reqFeatures;
    int configPreferences;
    int mOperationPending;
    int mExtras;
    int mDidDexOpt;
    int mScanPath;
    int mPreferredOrder;
    int mSignatures;
    int mSharedUserLabel;
    int mSharedUserId;
    int mVersionName;
    int mVersionCode;
    int mPath;
    int mAppMetaData;
    int mAdoptPermissions;
    int mRealPackage;
    int mOriginalPackages;
    int usesLibraryFiles;
    int usesOptionalLibraries;
    int usesLibraries;
    int protectedBroadcasts;
    int requestedPermissionsRequired;
    int requestedPermissions;
    int instrumentation;
    int services;
    int providers;
    int receivers;
    int activities;
    int permissionGroups;
    int permissions;
    int applicationInfo;
    int packageName;
  }
  int ANDROID_RESOURCES;
  int PARSE_IS_SYSTEM_DIR;
  int PARSE_ON_SDCARD;
  int PARSE_FORWARD_LOCK;
  int PARSE_IGNORE_PROCESSES;
  int PARSE_MUST_BE_APK;
  int PARSE_CHATTY;
  int PARSE_IS_SYSTEM;
  int TAG;
  int RIGID_PARSER;
  int mParseProviderArgs;
  int mParseServiceArgs;
  int mParseActivityAliasArgs;
  int mParseActivityArgs;
  int mParseInstrumentationArgs;
  class PackageLite {
    int verifiers;
    int installLocation;
    int packageName;
  }
  class ParseComponentArgs {
    int flags;
    int enabledRes;
    int descriptionRes;
    int processRes;
    int sepProcesses;
  }
  class ParsePackageItemArgs {
    int sa;
    int tag;
    int logoRes;
    int iconRes;
    int labelRes;
    int nameRes;
    int outError;
    int owner;
  }
  int PARSE_DEFAULT_INSTALL_LOCATION;
  int sCompatibilityModeEnabled;
  int mReadBuffer;
  int mSync;
  int mParseError;
  int SDK_CODENAME;
  int SDK_VERSION;
  int mOnlyCoreApps;
  int mSeparateProcesses;
  int mArchiveSourcePath;
  int SPLIT_PERMISSIONS;
  int NEW_PERMISSIONS;
  class SplitPermissionInfo {
    int targetSdk;
    int newPerms;
    int rootPerm;
  }
  class NewPermissionInfo {
    int fileVersion;
    int sdkVersion;
    int name;
  }
  int ANDROID_MANIFEST_FILENAME;
  int DEBUG_BACKUP;
  int DEBUG_PARSER;
  int DEBUG_JAR;
}
class PackageManagerTests {
  int SHARED2_CERT1_CERT2;
  int SHARED2_CERT2;
  int SHARED2_CERT1;
  int SHARED2_UNSIGNED;
  int SHARED1_CERT1_CERT2;
  int SHARED1_CERT2;
  int SHARED1_CERT1;
  int SHARED1_UNSIGNED;
  int APP2_CERT3;
  int APP2_CERT1_CERT2;
  int APP2_CERT2;
  int APP2_CERT1;
  int APP2_UNSIGNED;
  int APP1_CERT3;
  int APP1_CERT3_CERT4;
  int APP1_CERT1_CERT2;
  int APP1_CERT2;
  int APP1_CERT1;
  int APP1_UNSIGNED;
  int BASE_PERMISSIONS_SIGUSED;
  int BASE_PERMISSIONS_NOTUSED;
  int BASE_PERMISSIONS_USED;
  int BASE_PERMISSIONS_UNDEFINED;
  int BASE_PERMISSIONS_DEFINED;
  class PackageMoveObserver {
    int packageName;
    int doneFlag;
    int returnCode;
  }
  class MoveReceiver {
    int removed;
    int ADDED;
    int REMOVED;
    int INVALID;
    int pkgName;
  }
  class SdUnMountReceiver {
    int status;
    int pkgNames;
  }
  class SdMountReceiver {
    int status;
    int pkgNames;
  }
  class DeleteReceiver {
    int pkgName;
  }
  class DeleteObserver {
    int doneFlag;
    int succeeded;
  }
  class ReplaceReceiver {
    int update;
    int removed;
    int REPLACED;
    int ADDED;
    int REMOVED;
    int INVALID;
    int pkgName;
  }
  int PERM_NOTUSED;
  int PERM_USED;
  int PERM_UNDEFINED;
  int PERM_DEFINED;
  int PERM_PACKAGE;
  class InstallParams {
    int pkg;
    int packageURI;
  }
  int INSTALL_LOC_ERR;
  int INSTALL_LOC_SD;
  int INSTALL_LOC_INT;
  class InstallReceiver {
    int pkgName;
  }
  class GenericReceiver {
    int filter;
    int intent;
    int received;
    int doneFlag;
  }
  class PackageInstallObserver {
    int doneFlag;
    int returnCode;
  }
  int mOrigState;
  int APP_INSTALL_SDCARD;
  int APP_INSTALL_DEVICE;
  int APP_INSTALL_AUTO;
  int SECURE_CONTAINERS_PREFIX;
  int WAIT_TIME_INCR;
  int MAX_WAIT_TIME;
  int TAG;
  int localLOGV;
}
class PackageManagerStressHostTests {
  int MANY_APPS_APK_PREFIX;
  int MANY_APPS_PKG_PREFIX;
  int MANY_APPS_END;
  int MANY_APPS_START;
  int VERSIONED_APPS_END_VERSION;
  int VERSIONED_APPS_START_VERSION;
  int VERSIONED_APPS_PKG;
  int VERSIONED_APPS_FILENAME_PREFIX;
  int NO_LOC_PKG;
  int NO_LOC_APK;
  int EXTERNAL_LOC_PKG;
  int EXTERNAL_LOC_APK;
  int INTERNAL_LOC_PKG;
  int INTERNAL_LOC_APK;
  int AUTO_LOC_PKG;
  int AUTO_LOC_APK;
  int LARGE_APPS;
  class APK {
    int PACKAGENAME;
    int FILENAME;
  }
  int AppRepositoryPath;
  int MANY_APPS_DIRECTORY_NAME;
  int VERSIONED_APPS_DIRECTORY_NAME;
  int MISC_APPS_DIRECTORY_NAME;
  int LARGE_APPS_DIRECTORY_NAME;
  int mPMHostUtils;
  int LOG_TAG;
}
class PackageManagerHostTests {
  int SHARED_PERMS_DIFF_KEY_PKG;
  int SHARED_PERMS_DIFF_KEY_APK;
  int SHARED_PERMS_BT_PKG;
  int SHARED_PERMS_BT_APK;
  int SHARED_PERMS_FL_PKG;
  int SHARED_PERMS_FL_APK;
  int SHARED_PERMS_PKG;
  int SHARED_PERMS_APK;
  int VERSATILE_LOC_NONE_APK;
  int VERSATILE_LOC_AUTO_APK;
  int VERSATILE_LOC_EXTERNAL_APK;
  int VERSATILE_LOC_INTERNAL_APK;
  int VERSATILE_LOC_PKG;
  int ALL_PERMS_PKG;
  int ALL_PERMS_APK;
  int FL_PERMS_PKG;
  int FL_PERMS_APK;
  int UPDATE_EXT_TO_INT_LOC_PKG;
  int UPDATE_EXT_TO_INT_LOC_V2_INT_APK;
  int UPDATE_EXT_TO_INT_LOC_V1_EXT_APK;
  int UPDATE_EXTERNAL_LOC_PKG;
  int UPDATE_EXTERNAL_LOC_V2_NONE_APK;
  int UPDATE_EXTERNAL_LOC_V1_EXT_APK;
  int NO_LOC_PKG;
  int NO_LOC_APK;
  int NO_LOC_VERSION_PKG;
  int NO_LOC_VERSION_V2_APK;
  int NO_LOC_VERSION_V1_APK;
  int EXTERNAL_LOC_VERSION_PKG;
  int EXTERNAL_LOC_VERSION_V2_APK;
  int EXTERNAL_LOC_VERSION_V1_APK;
  int AUTO_LOC_VERSION_PKG;
  int AUTO_LOC_VERSION_V2_APK;
  int AUTO_LOC_VERSION_V1_APK;
  int EXTERNAL_LOC_PKG;
  int EXTERNAL_LOC_APK;
  int INTERNAL_LOC_PKG;
  int INTERNAL_LOC_APK;
  int AUTO_LOC_PKG;
  int AUTO_LOC_APK;
  int SIMPLE_PKG;
  int SIMPLE_APK;
  int sdcardAppPath;
  int deviceAppPath;
  int appPrivatePath;
  int mPMHostUtils;
  int LOG_TAG;
}
class PackageManagerHostTestUtils {
  class InstallReceiver {
    int mErrorMessage;
    int FAILURE_PATTERN;
    int SUCCESS_OUTPUT;
  }
  class CollectingTestRunListener {
    int mTestRunErrorMessage;
    int mAllTestsPassed;
  }
  class NullSyncProgressMonitor {
  }
  class CollectingOutputReceiver {
    int mOutputBuffer;
  }
  class InstallLocation {
    int SDCARD;
    int DEVICE;
  }
  class InstallLocPreference {
    int EXTERNAL;
    int INTERNAL;
    int AUTO;
  }
  int WAIT_FOR_APP_LAUNCH_POLL_TIME;
  int MAX_WAIT_FOR_APP_LAUNCH_TIME;
  int WAIT_FOR_DEVICE_POLL_TIME;
  int MAX_WAIT_FOR_DEVICE_TIME;
  int SDCARD_APP_PATH;
  int DEVICE_APP_PATH;
  int APP_PRIVATE_PATH;
  int mDevice;
  int LOG_TAG;
}
class PackageManager {
  int EXTRA_VERIFICATION_INSTALL_FLAGS;
  int EXTRA_VERIFICATION_INSTALLER_PACKAGE;
  int EXTRA_VERIFICATION_ID;
  int EXTRA_VERIFICATION_URI;
  int ACTION_CLEAN_EXTERNAL_STORAGE;
  int FEATURE_TELEVISION;
  int FEATURE_WIFI_DIRECT;
  int FEATURE_WIFI;
  int FEATURE_LIVE_WALLPAPER;
  int FEATURE_SCREEN_LANDSCAPE;
  int FEATURE_SCREEN_PORTRAIT;
  int FEATURE_FAKETOUCH_MULTITOUCH_JAZZHAND;
  int FEATURE_FAKETOUCH_MULTITOUCH_DISTINCT;
  int FEATURE_FAKETOUCH;
  int FEATURE_TOUCHSCREEN_MULTITOUCH_JAZZHAND;
  int FEATURE_TOUCHSCREEN_MULTITOUCH_DISTINCT;
  int FEATURE_TOUCHSCREEN_MULTITOUCH;
  int FEATURE_TOUCHSCREEN;
  int FEATURE_SIP_VOIP;
  int FEATURE_SIP;
  int FEATURE_USB_ACCESSORY;
  int FEATURE_USB_HOST;
  int FEATURE_TELEPHONY_GSM;
  int FEATURE_TELEPHONY_CDMA;
  int FEATURE_TELEPHONY;
  int FEATURE_SENSOR_PROXIMITY;
  int FEATURE_SENSOR_LIGHT;
  int FEATURE_SENSOR_GYROSCOPE;
  int FEATURE_SENSOR_COMPASS;
  int FEATURE_SENSOR_BAROMETER;
  int FEATURE_SENSOR_ACCELEROMETER;
  int FEATURE_NFC;
  int FEATURE_MICROPHONE;
  int FEATURE_LOCATION_NETWORK;
  int FEATURE_LOCATION_GPS;
  int FEATURE_LOCATION;
  int FEATURE_CAMERA_FRONT;
  int FEATURE_CAMERA_FLASH;
  int FEATURE_CAMERA_AUTOFOCUS;
  int FEATURE_CAMERA;
  int FEATURE_BLUETOOTH;
  int FEATURE_AUDIO_LOW_LATENCY;
  int VERIFICATION_REJECT;
  int VERIFICATION_ALLOW;
  int VERIFICATION_ALLOW_WITHOUT_SUFFICIENT;
  int MOVE_EXTERNAL_MEDIA;
  int MOVE_INTERNAL;
  int MOVE_FAILED_OPERATION_PENDING;
  int MOVE_FAILED_INTERNAL_ERROR;
  int MOVE_FAILED_INVALID_LOCATION;
  int MOVE_FAILED_FORWARD_LOCKED;
  int MOVE_FAILED_SYSTEM_PACKAGE;
  int MOVE_FAILED_DOESNT_EXIST;
  int MOVE_FAILED_INSUFFICIENT_STORAGE;
  int MOVE_SUCCEEDED;
  int DELETE_FAILED_DEVICE_POLICY_MANAGER;
  int DELETE_FAILED_INTERNAL_ERROR;
  int DELETE_SUCCEEDED;
  int DONT_DELETE_DATA;
  int INSTALL_FAILED_INTERNAL_ERROR;
  int INSTALL_PARSE_FAILED_MANIFEST_EMPTY;
  int INSTALL_PARSE_FAILED_MANIFEST_MALFORMED;
  int INSTALL_PARSE_FAILED_BAD_SHARED_USER_ID;
  int INSTALL_PARSE_FAILED_BAD_PACKAGE_NAME;
  int INSTALL_PARSE_FAILED_CERTIFICATE_ENCODING;
  int INSTALL_PARSE_FAILED_INCONSISTENT_CERTIFICATES;
  int INSTALL_PARSE_FAILED_NO_CERTIFICATES;
  int INSTALL_PARSE_FAILED_UNEXPECTED_EXCEPTION;
  int INSTALL_PARSE_FAILED_BAD_MANIFEST;
  int INSTALL_PARSE_FAILED_NOT_APK;
  int INSTALL_FAILED_UID_CHANGED;
  int INSTALL_FAILED_PACKAGE_CHANGED;
  int INSTALL_FAILED_VERIFICATION_FAILURE;
  int INSTALL_FAILED_VERIFICATION_TIMEOUT;
  int INSTALL_FAILED_MEDIA_UNAVAILABLE;
  int INSTALL_FAILED_INVALID_INSTALL_LOCATION;
  int INSTALL_FAILED_CONTAINER_ERROR;
  int INSTALL_FAILED_MISSING_FEATURE;
  int INSTALL_FAILED_CPU_ABI_INCOMPATIBLE;
  int INSTALL_FAILED_TEST_ONLY;
  int INSTALL_FAILED_NEWER_SDK;
  int INSTALL_FAILED_CONFLICTING_PROVIDER;
  int INSTALL_FAILED_OLDER_SDK;
  int INSTALL_FAILED_DEXOPT;
  int INSTALL_FAILED_REPLACE_COULDNT_DELETE;
  int INSTALL_FAILED_MISSING_SHARED_LIBRARY;
  int INSTALL_FAILED_SHARED_USER_INCOMPATIBLE;
  int INSTALL_FAILED_UPDATE_INCOMPATIBLE;
  int INSTALL_FAILED_NO_SHARED_USER;
  int INSTALL_FAILED_DUPLICATE_PACKAGE;
  int INSTALL_FAILED_INSUFFICIENT_STORAGE;
  int INSTALL_FAILED_INVALID_URI;
  int INSTALL_FAILED_INVALID_APK;
  int INSTALL_FAILED_ALREADY_EXISTS;
  int INSTALL_SUCCEEDED;
  int DONT_KILL_APP;
  int INSTALL_FROM_ADB;
  int INSTALL_INTERNAL;
  int INSTALL_EXTERNAL;
  int INSTALL_ALLOW_TEST;
  int INSTALL_REPLACE_EXISTING;
  int INSTALL_FORWARD_LOCK;
  int COMPONENT_ENABLED_STATE_DISABLED_USER;
  int COMPONENT_ENABLED_STATE_DISABLED;
  int COMPONENT_ENABLED_STATE_ENABLED;
  int COMPONENT_ENABLED_STATE_DEFAULT;
  int SIGNATURE_UNKNOWN_PACKAGE;
  int SIGNATURE_NO_MATCH;
  int SIGNATURE_SECOND_NOT_SIGNED;
  int SIGNATURE_FIRST_NOT_SIGNED;
  int SIGNATURE_NEITHER_SIGNED;
  int SIGNATURE_MATCH;
  int PERMISSION_DENIED;
  int PERMISSION_GRANTED;
  int MATCH_DEFAULT_ONLY;
  int GET_CONFIGURATIONS;
  int GET_UNINSTALLED_PACKAGES;
  int GET_PERMISSIONS;
  int GET_URI_PERMISSION_PATTERNS;
  int GET_SHARED_LIBRARY_FILES;
  int GET_DISABLED_COMPONENTS;
  int GET_GIDS;
  int GET_META_DATA;
  int GET_RESOLVED_FILTER;
  int GET_SIGNATURES;
  int GET_INTENT_FILTERS;
  int GET_INSTRUMENTATION;
  int GET_PROVIDERS;
  int GET_SERVICES;
  int GET_RECEIVERS;
  int GET_ACTIVITIES;
  class NameNotFoundException {
  }
}
class PackageItemInfo {
  class DisplayNameComparator {
    int mPM;
    int sCollator;
  }
  int metaData;
  int logo;
  int icon;
  int nonLocalizedLabel;
  int labelRes;
  int packageName;
  int name;
}
class PackageInfoLite {
  int CREATOR;
  int verifiers;
  int installLocation;
  int recommendedInstallLocation;
  int packageName;
}
class PackageInfo {
  int CREATOR;
  int installLocation;
  int INSTALL_LOCATION_PREFER_EXTERNAL;
  int INSTALL_LOCATION_INTERNAL_ONLY;
  int INSTALL_LOCATION_AUTO;
  int INSTALL_LOCATION_UNSPECIFIED;
  int reqFeatures;
  int configPreferences;
  int signatures;
  int REQUESTED_PERMISSION_GRANTED;
  int REQUESTED_PERMISSION_REQUIRED;
  int requestedPermissionsFlags;
  int requestedPermissions;
  int permissions;
  int instrumentation;
  int providers;
  int services;
  int receivers;
  int activities;
  int gids;
  int lastUpdateTime;
  int firstInstallTime;
  int applicationInfo;
  int sharedUserLabel;
  int sharedUserId;
  int versionName;
  int versionCode;
  int packageName;
}
class PackageHelperTests {
  int fullId2;
  int fullId;
  int mMs;
  int PREFIX;
  int TAG;
  int localLOGV;
}
class ManifestDigestTest {
  int MD5_DIGEST;
  int SHA1_DIGEST;
  int DIGEST_2_STR;
  int DIGEST_2;
  int DIGEST_1_STR;
  int DIGEST_1;
}
class ManifestDigest {
  int CREATOR;
  int TO_STRING_PREFIX;
  int DIGEST_TYPES;
  int mDigest;
}
class MacAuthenticatedInputStreamTest {
  int mTestStream1;
  int TEST_STRING_1_MAC_BROKEN;
  int TEST_STRING_1_MAC;
  int TEST_STRING_1;
  int HMAC_KEY_1;
}
class MacAuthenticatedInputStream {
  int mMac;
}
class LimitedLengthInputStreamTest {
  int mTestStream1;
  int TEST_STRING1;
}
class LimitedLengthInputStream {
  int mOffset;
  int mEnd;
}
class LabeledIntent {
  int CREATOR;
  int mIcon;
  int mNonLocalizedLabel;
  int mLabelRes;
  int mSourcePackage;
}
class InstrumentationInfo {
  int CREATOR;
  int functionalTest;
  int handleProfiling;
  int nativeLibraryDir;
  int dataDir;
  int publicSourceDir;
  int sourceDir;
  int targetPackage;
}
class FeatureInfo {
  int CREATOR;
  int flags;
  int FLAG_REQUIRED;
  int reqGlEsVersion;
  int GL_ES_VERSION_UNDEFINED;
  int name;
}
class ContainerEncryptionParamsTest {
  int DATA_END;
  int ENCRYPTED_START;
  int AUTHENTICATED_START;
  int MAC_TAG;
  int MAC_KEY;
  int MAC_KEY_BYTES;
  int MAC_ALGORITHM;
  int ENC_KEY;
  int ENC_KEY_BYTES;
  int ENC_PARAMS;
  int IV_BYTES;
  int ENC_ALGORITHM;
}
class ContainerEncryptionParams {
  int CREATOR;
  int mDataEnd;
  int mEncryptedDataStart;
  int mAuthenticatedDataStart;
  int mMacTag;
  int mMacKey;
  int mMacSpec;
  int mMacAlgorithm;
  int mEncryptionKey;
  int mEncryptionSpec;
  int mEncryptionAlgorithm;
  int MAC_PARAMS_NONE;
  int ENC_PARAMS_IV_PARAMETERS;
  int TO_STRING_PREFIX;
  int TAG;
}
class ConfigurationInfo {
  int CREATOR;
  int reqGlEsVersion;
  int GL_ES_VERSION_UNDEFINED;
  int reqInputFeatures;
  int INPUT_FEATURE_FIVE_WAY_NAV;
  int INPUT_FEATURE_HARD_KEYBOARD;
  int reqNavigation;
  int reqKeyboardType;
  int reqTouchScreen;
}
class ComponentTest {
  int TEST_CATEGORY;
  int DISABLED_APP_ENABLED_ACTIVITY_COMPONENTNAME;
  int DISABLED_APP_ENABLED_ACTIVITY_CLASSNAME;
  int ENABLED_PROVIDER_NAME;
  int ENABLED_PROVIDER_COMPONENTNAME;
  int ENABLED_PROVIDER_CLASSNAME;
  int ENABLED_RECEIVER_COMPONENTNAME;
  int ENABLED_RECEIVER_CLASSNAME;
  int DISABLED_RECEIVER_COMPONENTNAME;
  int DISABLED_RECEIVER_CLASSNAME;
  int ENABLED_SERVICE_COMPONENTNAME;
  int ENABLED_SERVICE_CLASSNAME;
  int DISABLED_PROVIDER_NAME;
  int DISABLED_PROVIDER_COMPONENTNAME;
  int DISABLED_PROVIDER_CLASSNAME;
  int DISABLED_SERVICE_COMPONENTNAME;
  int DISABLED_SERVICE_CLASSNAME;
  int ENABLED_ACTIVITY_COMPONENTNAME;
  int ENABLED_ACTIVITY_CLASSNAME;
  int DISABLED_ACTIVITY_COMPONENTNAME;
  int DISABLED_ACTIVITY_CLASSNAME;
  int DISABLED_PACKAGENAME;
  int ENABLED_PACKAGENAME;
  int mDisabledAppEnabledActivityIntent;
  int mEnabledReceiverIntent;
  int mDisabledReceiverIntent;
  int mEnabledServiceIntent;
  int mDisabledServiceIntent;
  int mEnabledActivityIntent;
  int mDisabledActivityIntent;
  int mPackageManager;
}
class ComponentInfo {
  int exported;
  int enabled;
  int descriptionRes;
  int processName;
  int applicationInfo;
}
class ApplicationInfo {
  int CREATOR;
  class DisplayNameComparator {
    int mPM;
    int sCollator;
  }
  int installLocation;
  int enabledSetting;
  int enabled;
  int targetSdkVersion;
  int uid;
  int nativeLibraryDir;
  int dataDir;
  int sharedLibraryFiles;
  int resourceDirs;
  int publicSourceDir;
  int sourceDir;
  int largestWidthLimitDp;
  int compatibleWidthLimitDp;
  int requiresSmallestWidthDp;
  int flags;
  int FLAG_CANT_SAVE_STATE;
  int FLAG_FORWARD_LOCK;
  int FLAG_SUPPORTS_RTL;
  int FLAG_STOPPED;
  int FLAG_LARGE_HEAP;
  int FLAG_SUPPORTS_XLARGE_SCREENS;
  int FLAG_EXTERNAL_STORAGE;
  int FLAG_RESTORE_ANY_VERSION;
  int FLAG_KILL_AFTER_RESTORE;
  int FLAG_ALLOW_BACKUP;
  int FLAG_VM_SAFE_MODE;
  int FLAG_SUPPORTS_SCREEN_DENSITIES;
  int FLAG_RESIZEABLE_FOR_SCREENS;
  int FLAG_SUPPORTS_LARGE_SCREENS;
  int FLAG_SUPPORTS_NORMAL_SCREENS;
  int FLAG_SUPPORTS_SMALL_SCREENS;
  int FLAG_TEST_ONLY;
  int FLAG_UPDATED_SYSTEM_APP;
  int FLAG_ALLOW_CLEAR_USER_DATA;
  int FLAG_ALLOW_TASK_REPARENTING;
  int FLAG_FACTORY_TEST;
  int FLAG_PERSISTENT;
  int FLAG_HAS_CODE;
  int FLAG_DEBUGGABLE;
  int FLAG_SYSTEM;
  int uiOptions;
  int backupAgentName;
  int manageSpaceActivityName;
  int theme;
  int descriptionRes;
  int className;
  int processName;
  int permission;
  int taskAffinity;
}
class AppCacheTest {
  class FreeStorageReceiver {
    int doneFlag;
    int ACTION_FREE;
  }
  class PackageStatsObserver {
    int doneFlag;
    int stats;
    int retValue;
  }
  class PackageDataObserver {
    int doneFlag;
    int retValue;
  }
  int ACTUAL_THRESHOLD;
  int THRESHOLD;
  int WAIT_TIME_INCR;
  int MAX_WAIT_TIME;
  int TAG;
  int localLOGV;
}
class ActivityInfo {
  int CREATOR;
  int parentActivityName;
  int UIOPTION_SPLIT_ACTION_BAR_WHEN_NARROW;
  int uiOptions;
  int softInputMode;
  int configChanges;
  int CONFIG_NATIVE_BITS;
  int CONFIG_FONT_SCALE;
  int CONFIG_SMALLEST_SCREEN_SIZE;
  int CONFIG_SCREEN_SIZE;
  int CONFIG_UI_MODE;
  int CONFIG_SCREEN_LAYOUT;
  int CONFIG_ORIENTATION;
  int CONFIG_NAVIGATION;
  int CONFIG_KEYBOARD_HIDDEN;
  int CONFIG_KEYBOARD;
  int CONFIG_TOUCHSCREEN;
  int CONFIG_LOCALE;
  int CONFIG_MNC;
  int CONFIG_MCC;
  int screenOrientation;
  int SCREEN_ORIENTATION_FULL_SENSOR;
  int SCREEN_ORIENTATION_REVERSE_PORTRAIT;
  int SCREEN_ORIENTATION_REVERSE_LANDSCAPE;
  int SCREEN_ORIENTATION_SENSOR_PORTRAIT;
  int SCREEN_ORIENTATION_SENSOR_LANDSCAPE;
  int SCREEN_ORIENTATION_NOSENSOR;
  int SCREEN_ORIENTATION_SENSOR;
  int SCREEN_ORIENTATION_BEHIND;
  int SCREEN_ORIENTATION_USER;
  int SCREEN_ORIENTATION_PORTRAIT;
  int SCREEN_ORIENTATION_LANDSCAPE;
  int SCREEN_ORIENTATION_UNSPECIFIED;
  int flags;
  int FLAG_IMMERSIVE;
  int FLAG_HARDWARE_ACCELERATED;
  int FLAG_FINISH_ON_CLOSE_SYSTEM_DIALOGS;
  int FLAG_NO_HISTORY;
  int FLAG_ALLOW_TASK_REPARENTING;
  int FLAG_EXCLUDE_FROM_RECENTS;
  int FLAG_STATE_NOT_NEEDED;
  int FLAG_ALWAYS_RETAIN_TASK_STATE;
  int FLAG_CLEAR_TASK_ON_LAUNCH;
  int FLAG_FINISH_ON_TASK_LAUNCH;
  int FLAG_MULTIPROCESS;
  int targetActivity;
  int taskAffinity;
  int permission;
  int launchMode;
  int LAUNCH_SINGLE_INSTANCE;
  int LAUNCH_SINGLE_TASK;
  int LAUNCH_SINGLE_TOP;
  int LAUNCH_MULTIPLE;
  int theme;
}
