package com.android.server.usb;
class UsbSettingsManager {
  int mPackageMonitor;
  class MyPackageMonitor {
  }
  class AccessoryFilter {
    int mVersion;
    int mModel;
    int mManufacturer;
  }
  class DeviceFilter {
    int mProtocol;
    int mSubclass;
    int mClass;
    int mProductId;
    int mVendorId;
  }
  int mLock;
  int mAccessoryPreferenceMap;
  int mDevicePreferenceMap;
  int mAccessoryPermissionMap;
  int mDevicePermissionMap;
  int mPackageManager;
  int mContext;
  int sSettingsFile;
  int DEBUG;
  int TAG;
}
class UsbService {
  int mSettingsManager;
  int mHostManager;
  int mDeviceManager;
  int mContext;
}
class UsbHostManager {
  int mSettingsManager;
  int mLock;
  int mContext;
  int mHostBlacklist;
  int mDevices;
  int LOG;
  int TAG;
}
class UsbDeviceManager {
  class UsbHandler {
    int mBootCompletedReceiver;
    int mAdbNotificationShown;
    int mUsbNotificationId;
    int mCurrentAccessory;
    int mDefaultFunctions;
    int mCurrentFunctions;
    int mConfigured;
    int mConnected;
  }
  int mUEventObserver;
  class AdbSettingsObserver {
  }
  int mAccessoryStrings;
  int mOemModeMap;
  int mAudioSourceEnabled;
  int mAdbEnabled;
  int mUseUsbNotification;
  int mHasUsbAccessory;
  int mNotificationManager;
  int mSettingsManager;
  int mContentResolver;
  int mContext;
  int mBootCompleted;
  int mHandler;
  int BOOT_MODE_PROPERTY;
  int UPDATE_DELAY;
  int AUDIO_MODE_SOURCE;
  int AUDIO_MODE_NONE;
  int MSG_BOOT_COMPLETED;
  int MSG_SYSTEM_READY;
  int MSG_SET_CURRENT_FUNCTIONS;
  int MSG_ENABLE_ADB;
  int MSG_UPDATE_STATE;
  int AUDIO_SOURCE_PCM_PATH;
  int RNDIS_ETH_ADDR_PATH;
  int MASS_STORAGE_FILE_PATH;
  int STATE_PATH;
  int FUNCTIONS_PATH;
  int ACCESSORY_START_MATCH;
  int USB_STATE_MATCH;
  int DEBUG;
  int TAG;
}
