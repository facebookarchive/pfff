package android.server;
class BluetoothPanProfileHandler {
  class BluetoothPanDevice {
    int mLocalRole;
    int mIface;
    int mIfaceAddr;
    int mState;
  }
  int mTetheringReceiver;
  int NAP_BRIDGE;
  int NAP_ROLE;
  int mBluetoothService;
  int mContext;
  int mTetheringOn;
  int mPanDevices;
  int sInstance;
  int BLUETOOTH_PREFIX_LENGTH;
  int BLUETOOTH_MAX_PAN_CONNECTIONS;
  int BLUETOOTH_IFACE_ADDR_START;
  int mMaxPanDevices;
  int mBluetoothIfaceAddresses;
  int DBG;
  int TAG;
}
class BluetoothInputProfileHandler {
  int mHidProfileState;
  int mInputDevices;
  int mBluetoothService;
  int mContext;
  int sInstance;
  int DBG;
  int TAG;
}
class BluetoothHealthProfileHandler {
  int mHandler;
  class HealthChannel {
    int mId;
    int mChannelType;
    int mState;
    int mConfig;
    int mDevice;
    int mChannelPath;
    int mMainChannel;
    int mChannelFd;
  }
  int sChannelId;
  int MESSAGE_CONNECT_CHANNEL;
  int MESSAGE_UNREGISTER_APPLICATION;
  int MESSAGE_REGISTER_APPLICATION;
  int mCallbacks;
  int mHealthDevices;
  int mHealthAppConfigs;
  int mHealthChannels;
  int mBluetoothService;
  int sInstance;
  int DBG;
  int TAG;
}
class BluetoothEventLoop {
  int mProfileServiceListener;
  int mHandler;
  int BLUETOOTH_PERM;
  int BLUETOOTH_ADMIN_PERM;
  int CREATE_DEVICE_FAILED;
  int CREATE_DEVICE_SUCCESS;
  int CREATE_DEVICE_ALREADY_EXISTS;
  int EVENT_AGENT_CANCEL;
  int EVENT_PAIRING_CONSENT_DELAYED_ACCEPT;
  int mWakeLock;
  int mContext;
  int mA2dp;
  int mBluetoothState;
  int mAdapter;
  int mBluetoothService;
  int mAuthorizationAgentRequestData;
  int mPasskeyAgentRequestData;
  int mInterrupted;
  int mStarted;
  int mThread;
  int mNativeData;
  int DBG;
  int TAG;
}
class BluetoothDeviceProperties {
  int mService;
  int mPropertiesMap;
  int TAG;
}
class BluetoothBondState {
  int mReceiver;
  int mProfileServiceListener;
  int mPairingRequestRcvd;
  int mHeadsetProxy;
  int mA2dpProxy;
  int mBluetoothInputProfileHandler;
  int mService;
  int mContext;
  int mPendingOutgoingBonding;
  int mAutoPairingDynamicAddressBlacklist;
  int mAutoPairingFixedPinZerosKeyboardList;
  int mAutoPairingPartialNameBlacklist;
  int mAutoPairingExactNameBlacklist;
  int mAutoPairingAddressBlacklist;
  int DYNAMIC_AUTO_PAIRING_BLACKLIST;
  int AUTO_PAIRING_BLACKLIST;
  int mPinAttempt;
  int mState;
  int DBG;
  int TAG;
}
class BluetoothAdapterStateMachine {
  class PerProcessState {
    int isTurningOn;
    int mCallback;
  }
  class BluetoothOn {
  }
  class Switching {
  }
  class HotOff {
  }
  class WarmUp {
  }
  class PowerOff {
  }
  int POWER_DOWN_TIMEOUT_TIME;
  int TURN_OFF_TIMEOUT_TIME;
  int PREPARE_BLUETOOTH_TIMEOUT_TIME;
  int DEVICES_DISCONNECT_TIMEOUT_TIME;
  int mDelayBroadcastStateOff;
  int mPublicState;
  int mPerProcessState;
  int mPowerOff;
  int mWarmUp;
  int mHotOff;
  int mSwitching;
  int mBluetoothOn;
  int mEventLoop;
  int mBluetoothService;
  int mContext;
  int POWER_DOWN_TIMEOUT;
  int TURN_OFF_TIMEOUT;
  int PREPARE_BLUETOOTH_TIMEOUT;
  int DEVICES_DISCONNECT_TIMEOUT;
  int TURN_COLD;
  int TURN_ON_CONTINUE;
  int AIRPLANE_MODE_OFF;
  int AIRPLANE_MODE_ON;
  int POWER_STATE_CHANGED;
  int SCAN_MODE_CHANGED;
  int ALL_DEVICES_DISCONNECTED;
  int SERVICE_RECORD_LOADED;
  int TURN_HOT;
  int PER_PROCESS_TURN_OFF;
  int PER_PROCESS_TURN_ON;
  int USER_TURN_OFF;
  int USER_TURN_ON;
  int DBG;
  int TAG;
}
class BluetoothAdapterProperties {
  int mService;
  int mContext;
  int mPropertiesMap;
  int TAG;
}
class BluetoothA2dpService {
  class IntentBroadcastHandler {
  }
  int mReceiver;
  int MSG_CONNECTION_STATE_CHANGED;
  int mWakeLock;
  int mIntentBroadcastHandler;
  int mPlayingA2dpDevice;
  int mTargetA2dpState;
  int mAdapter;
  int mBluetoothService;
  int mAudioManager;
  int mAudioDevices;
  int mIntentFilter;
  int mContext;
  int PROPERTY_STATE;
  int BLUETOOTH_ENABLED;
  int BLUETOOTH_PERM;
  int BLUETOOTH_ADMIN_PERM;
  int BLUETOOTH_A2DP_SERVICE;
  int DBG;
  int TAG;
}
