package android.bluetooth;
class HeadsetBase {
  int mWakeLock;
  int mAtParser;
  int mConnectTimestamp;
  int mDirection;
  int mTimeoutRemainingMs;
  int mEventThreadHandler;
  int mEventThreadInterrupted;
  int mEventThread;
  int mNativeData;
  int mRfcommChannel;
  int mAddress;
  int mRemoteDevice;
  int mAdapter;
  int sAtInputCount;
  int DIRECTION_OUTGOING;
  int DIRECTION_INCOMING;
  int RFCOMM_DISCONNECTED;
  int DBG;
  int TAG;
}
class BluetoothUuid {
  int RESERVED_UUIDS;
  int PBAP_PSE;
  int BNEP;
  int NAP;
  int PANU;
  int Hid;
  int ObexObjectPush;
  int AvrcpTarget;
  int AvrcpController;
  int Handsfree_AG;
  int Handsfree;
  int HSP_AG;
  int HSP;
  int AdvAudioDist;
  int AudioSource;
  int AudioSink;
}
class BluetoothTetheringDataTracker {
  int mProfileServiceListener;
  int sInstance;
  int mContext;
  int mCsHandler;
  int mIface;
  int mDevice;
  int mBluetoothPan;
  int mNetworkInfo;
  int mLinkCapabilities;
  int mLinkProperties;
  int mDefaultRouteSet;
  int mDefaultGatewayAddr;
  int mPrivateDnsRouteSet;
  int mTeardownRequested;
  int TAG;
  int NETWORKTYPE;
}
class BluetoothTestUtils {
  int mPan;
  int mInput;
  int mHeadset;
  int mA2dp;
  int mContext;
  int mOutputFile;
  int mTag;
  int mOutputWriter;
  int mReceivers;
  int mServiceListener;
  class StartStopScoReceiver {
    int STATE_DISCONNECTED_FLAG;
    int STATE_CONNECTED_FLAG;
  }
  class ConnectPanReceiver {
    int mRole;
  }
  class ConnectProfileReceiver {
    int mConnectionAction;
    int mProfile;
    int mDevice;
    int STATE_DISCONNECTING_FLAG;
    int STATE_CONNECTED_FLAG;
    int STATE_CONNECTING_FLAG;
    int STATE_DISCONNECTED_FLAG;
  }
  class PairReceiver {
    int mPin;
    int mPasskey;
    int mDevice;
    int STATE_NONE_FLAG;
    int STATE_BONDING_FLAG;
    int STATE_BONDED_FLAG;
  }
  class BluetoothReceiver {
    int STATE_TURNING_OFF_FLAG;
    int STATE_ON_FLAG;
    int STATE_TURNING_ON_FLAG;
    int STATE_OFF_FLAG;
    int SCAN_MODE_CONNECTABLE_DISCOVERABLE_FLAG;
    int SCAN_MODE_CONNECTABLE_FLAG;
    int SCAN_MODE_NONE_FLAG;
    int DISCOVERY_FINISHED_FLAG;
    int DISCOVERY_STARTED_FLAG;
  }
  class FlagReceiver {
    int mCompletedTime;
    int mFiredFlags;
    int mExpectedFlags;
  }
  int POLL_TIME;
  int CONNECT_PROXY_TIMEOUT;
  int START_STOP_SCO_TIMEOUT;
  int CONNECT_DISCONNECT_PROFILE_TIMEOUT;
  int PAIR_UNPAIR_TIMEOUT;
  int START_STOP_SCAN_TIMEOUT;
  int DISCOVERABLE_UNDISCOVERABLE_TIMEOUT;
  int ENABLE_DISABLE_TIMEOUT;
}
class BluetoothTestRunner {
  int sDevicePairPasskey;
  int sDevicePairPin;
  int sDeviceAddress;
  int sStartStopScoIterations;
  int sConnectPanIterations;
  int sConnectInputIterations;
  int sConnectA2dpIterations;
  int sConnectHeadsetIterations;
  int sPairIterations;
  int sEnablePanIterations;
  int sScanIterations;
  int sDiscoverableIterations;
  int sEnableIterations;
  int TAG;
}
class BluetoothStressTest {
  int mTestUtils;
  int SCO_SLEEP_TIME;
  int OUTPUT_FILE;
  int TAG;
}
class BluetoothSocket {
  class SdpHelper {
    int canceled;
    int channel;
    int device;
    int uuid;
    int service;
  }
  int mSocketData;
  int mLock;
  int mSocketState;
  class SocketState {
    int CLOSED;
    int CONNECTED;
    int INIT;
  }
  int mPort;
  int mSdp;
  int mOutputStream;
  int mInputStream;
  int mEncrypt;
  int mAuth;
  int mAddress;
  int mDevice;
  int mType;
  int EADDRINUSE;
  int EBADFD;
  int TYPE_L2CAP;
  int TYPE_SCO;
  int TYPE_RFCOMM;
  int MAX_RFCOMM_CHANNEL;
  int TAG;
}
class BluetoothServerSocket {
  int mChannel;
  int mMessage;
  int mHandler;
  int mSocket;
}
class BluetoothRebootStressTest {
  int mTestUtils;
  int OUTPUT_FILE;
  int TAG;
}
class BluetoothProfileState {
  class PendingCommandState {
  }
  class StableState {
  }
  int mBroadcastReceiver;
  int mStableState;
  int mPendingCommandState;
  int mPendingDevice;
  int mProfile;
  int TRANSITION_TO_STABLE;
  int HID;
  int A2DP;
  int HFP;
  int TAG;
  int DBG;
}
class BluetoothProfile {
  class ServiceListener {
  }
  int PRIORITY_UNDEFINED;
  int PRIORITY_OFF;
  int PRIORITY_ON;
  int PRIORITY_AUTO_CONNECT;
  int PBAP;
  int PAN;
  int INPUT_DEVICE;
  int HEALTH;
  int A2DP;
  int HEADSET;
  int STATE_DISCONNECTING;
  int STATE_CONNECTED;
  int STATE_CONNECTING;
  int STATE_DISCONNECTED;
  int EXTRA_PREVIOUS_STATE;
  int EXTRA_STATE;
}
class BluetoothPbap {
  int mConnection;
  class ServiceListener {
  }
  int RESULT_CANCELED;
  int RESULT_SUCCESS;
  int RESULT_FAILURE;
  int STATE_CONNECTED;
  int STATE_CONNECTING;
  int STATE_DISCONNECTED;
  int STATE_ERROR;
  int mServiceListener;
  int mContext;
  int mService;
  int PBAP_STATE_CHANGED_ACTION;
  int PBAP_PREVIOUS_STATE;
  int PBAP_STATE;
  int DBG;
  int TAG;
}
class BluetoothPan {
  int mService;
  int mAdapter;
  int mServiceListener;
  int PAN_OPERATION_SUCCESS;
  int PAN_OPERATION_GENERIC_FAILURE;
  int PAN_CONNECT_FAILED_ATTEMPT_FAILED;
  int PAN_CONNECT_FAILED_ALREADY_CONNECTED;
  int PAN_DISCONNECT_FAILED_NOT_CONNECTED;
  int LOCAL_PANU_ROLE;
  int LOCAL_NAP_ROLE;
  int EXTRA_LOCAL_ROLE;
  int ACTION_CONNECTION_STATE_CHANGED;
  int DBG;
  int TAG;
}
class BluetoothOutputStream {
  int mSocket;
}
class BluetoothInputStream {
  int mSocket;
}
class BluetoothInputDevice {
  int mService;
  int mAdapter;
  int mServiceListener;
  int INPUT_OPERATION_SUCCESS;
  int INPUT_OPERATION_GENERIC_FAILURE;
  int INPUT_CONNECT_FAILED_ATTEMPT_FAILED;
  int INPUT_CONNECT_FAILED_ALREADY_CONNECTED;
  int INPUT_DISCONNECT_FAILED_NOT_CONNECTED;
  int ACTION_CONNECTION_STATE_CHANGED;
  int DBG;
  int TAG;
}
class BluetoothHealthCallback {
  int TAG;
}
class BluetoothHealthAppConfiguration {
  int CREATOR;
  int mChannelType;
  int mRole;
  int mDataType;
  int mName;
}
class BluetoothHealth {
  int mAdapter;
  int mService;
  int mServiceListener;
  int APP_CONFIG_UNREGISTRATION_FAILURE;
  int APP_CONFIG_UNREGISTRATION_SUCCESS;
  int APP_CONFIG_REGISTRATION_FAILURE;
  int APP_CONFIG_REGISTRATION_SUCCESS;
  int STATE_CHANNEL_DISCONNECTING;
  int STATE_CHANNEL_CONNECTED;
  int STATE_CHANNEL_CONNECTING;
  int STATE_CHANNEL_DISCONNECTED;
  class BluetoothHealthCallbackWrapper {
    int mCallback;
  }
  int HEALTH_OPERATION_NOT_ALLOWED;
  int HEALTH_OPERATION_NOT_FOUND;
  int HEALTH_OPERATION_GENERIC_FAILURE;
  int HEALTH_OPERATION_INVALID_ARGS;
  int HEALTH_OPERATION_ERROR;
  int HEALTH_OPERATION_SUCCESS;
  int CHANNEL_TYPE_ANY;
  int CHANNEL_TYPE_STREAMING;
  int CHANNEL_TYPE_RELIABLE;
  int SINK_ROLE;
  int SOURCE_ROLE;
  int DBG;
  int TAG;
}
class BluetoothHeadset {
  int mConnection;
  int mAdapter;
  int mService;
  int mServiceListener;
  int mContext;
  int STATE_AUDIO_CONNECTED;
  int STATE_AUDIO_CONNECTING;
  int STATE_AUDIO_DISCONNECTED;
  int VENDOR_SPECIFIC_HEADSET_EVENT_COMPANY_ID_CATEGORY;
  int EXTRA_VENDOR_SPECIFIC_HEADSET_EVENT_ARGS;
  int AT_CMD_TYPE_ACTION;
  int AT_CMD_TYPE_BASIC;
  int AT_CMD_TYPE_SET;
  int AT_CMD_TYPE_TEST;
  int AT_CMD_TYPE_READ;
  int EXTRA_VENDOR_SPECIFIC_HEADSET_EVENT_CMD_TYPE;
  int EXTRA_VENDOR_SPECIFIC_HEADSET_EVENT_CMD;
  int ACTION_VENDOR_SPECIFIC_HEADSET_EVENT;
  int ACTION_AUDIO_STATE_CHANGED;
  int ACTION_CONNECTION_STATE_CHANGED;
  int DBG;
  int TAG;
}
class BluetoothDeviceProfileState {
  class IncomingHid {
    int mCommand;
    int mStatus;
  }
  class OutgoingHid {
    int mCommand;
    int mStatus;
  }
  class IncomingA2dp {
    int mCommand;
    int mStatus;
  }
  class OutgoingA2dp {
    int mCommand;
    int mStatus;
  }
  class IncomingHandsfree {
    int mCommand;
    int mStatus;
  }
  class OutgoingHandsfree {
    int mCommand;
    int mStatus;
  }
  class BondedDevice {
  }
  class PbapServiceListener {
  }
  int mBluetoothProfileServiceListener;
  int mBroadcastReceiver;
  int mPairingRequestRcvd;
  int mPowerManager;
  int mWakeLock;
  int mIncomingConnections;
  int mConnectionAccessReplyReceived;
  int mIncomingRejectTimer;
  int mA2dpState;
  int mHeadsetState;
  int mDevice;
  int BLUETOOTH_ADMIN_PERM;
  int mAutoConnectionPending;
  int mPbapServiceConnected;
  int mAdapter;
  int mPbap;
  int mPbapService;
  int mHeadsetService;
  int mA2dpService;
  int mService;
  int mContext;
  int mIncomingHid;
  int mOutgoingHid;
  int mOutgoingA2dp;
  int mIncomingA2dp;
  int mIncomingHandsfree;
  int mOutgoingHandsfree;
  int mBondedDevice;
  int ACCESS_AUTHORITY_CLASS;
  int ACCESS_AUTHORITY_PACKAGE;
  int MAX_INCOMING_REJECT_TIMER;
  int INIT_INCOMING_REJECT_TIMER;
  int CONNECTION_ACCESS_UNDEFINED;
  int CONNECTION_ACCESS_REQUEST_EXPIRY_TIMEOUT;
  int CONNECT_OTHER_PROFILES_DELAY;
  int CONNECTION_ACCESS_REQUEST_EXPIRY;
  int CONNECTION_ACCESS_REQUEST_REPLY;
  int CONNECT_OTHER_PROFILES;
  int TRANSITION_TO_STABLE;
  int AUTO_CONNECT_PROFILES;
  int UNPAIR;
  int DISCONNECT_PBAP_OUTGOING;
  int DISCONNECT_HID_INCOMING;
  int DISCONNECT_HID_OUTGOING;
  int DISCONNECT_A2DP_INCOMING;
  int DISCONNECT_A2DP_OUTGOING;
  int DISCONNECT_HFP_INCOMING;
  int DISCONNECT_HFP_OUTGOING;
  int CONNECT_HID_INCOMING;
  int CONNECT_HID_OUTGOING;
  int CONNECT_A2DP_INCOMING;
  int CONNECT_A2DP_OUTGOING;
  int CONNECT_HFP_INCOMING;
  int CONNECT_HFP_OUTGOING;
  int DBG;
  int TAG;
}
class BluetoothDevicePicker {
  int FILTER_TYPE_NAP;
  int FILTER_TYPE_PANU;
  int FILTER_TYPE_TRANSFER;
  int FILTER_TYPE_AUDIO;
  int FILTER_TYPE_ALL;
  int ACTION_LAUNCH;
  int ACTION_DEVICE_SELECTED;
  int EXTRA_LAUNCH_CLASS;
  int EXTRA_LAUNCH_PACKAGE;
  int EXTRA_FILTER_TYPE;
  int EXTRA_NEED_AUTH;
}
class BluetoothDevice {
  int CREATOR;
  int mAddress;
  int sService;
  int EXTRA_UUID;
  int PAIRING_VARIANT_OOB_CONSENT;
  int PAIRING_VARIANT_DISPLAY_PIN;
  int PAIRING_VARIANT_DISPLAY_PASSKEY;
  int PAIRING_VARIANT_CONSENT;
  int PAIRING_VARIANT_PASSKEY_CONFIRMATION;
  int PAIRING_VARIANT_PASSKEY;
  int PAIRING_VARIANT_PIN;
  int UNBOND_REASON_REMOVED;
  int UNBOND_REASON_REMOTE_AUTH_CANCELED;
  int UNBOND_REASON_REPEATED_ATTEMPTS;
  int UNBOND_REASON_AUTH_TIMEOUT;
  int UNBOND_REASON_DISCOVERY_IN_PROGRESS;
  int UNBOND_REASON_REMOTE_DEVICE_DOWN;
  int UNBOND_REASON_AUTH_CANCELED;
  int UNBOND_REASON_AUTH_REJECTED;
  int UNBOND_REASON_AUTH_FAILED;
  int BOND_SUCCESS;
  int EXTRA_ALWAYS_ALLOWED;
  int CONNECTION_ACCESS_NO;
  int CONNECTION_ACCESS_YES;
  int EXTRA_CONNECTION_ACCESS_RESULT;
  int EXTRA_CLASS_NAME;
  int EXTRA_PACKAGE_NAME;
  int REQUEST_TYPE_PHONEBOOK_ACCESS;
  int REQUEST_TYPE_PROFILE_CONNECTION;
  int EXTRA_ACCESS_REQUEST_TYPE;
  int ACTION_CONNECTION_ACCESS_CANCEL;
  int ACTION_CONNECTION_ACCESS_REPLY;
  int ACTION_CONNECTION_ACCESS_REQUEST;
  int ACTION_PAIRING_CANCEL;
  int ACTION_PAIRING_REQUEST;
  int ACTION_NAME_FAILED;
  int ACTION_UUID;
  int EXTRA_PAIRING_KEY;
  int EXTRA_PAIRING_VARIANT;
  int EXTRA_REASON;
  int BOND_BONDED;
  int BOND_BONDING;
  int BOND_NONE;
  int EXTRA_PREVIOUS_BOND_STATE;
  int EXTRA_BOND_STATE;
  int EXTRA_CLASS;
  int EXTRA_RSSI;
  int EXTRA_NAME;
  int EXTRA_DEVICE;
  int ACTION_BOND_STATE_CHANGED;
  int ACTION_ALIAS_CHANGED;
  int ACTION_NAME_CHANGED;
  int ACTION_ACL_DISCONNECTED;
  int ACTION_ACL_DISCONNECT_REQUESTED;
  int ACTION_ACL_CONNECTED;
  int ACTION_CLASS_CHANGED;
  int ACTION_DISAPPEARED;
  int ACTION_FOUND;
  int ERROR;
  int TAG;
}
class BluetoothClass {
  int PROFILE_NAP;
  int PROFILE_PANU;
  int PROFILE_HID;
  int PROFILE_OPP;
  int PROFILE_A2DP;
  int PROFILE_HEADSET;
  class Device {
    int PERIPHERAL_KEYBOARD_POINTING;
    int PERIPHERAL_POINTING;
    int PERIPHERAL_KEYBOARD;
    int PERIPHERAL_NON_KEYBOARD_NON_POINTING;
    int HEALTH_DATA_DISPLAY;
    int HEALTH_PULSE_RATE;
    int HEALTH_PULSE_OXIMETER;
    int HEALTH_GLUCOSE;
    int HEALTH_WEIGHING;
    int HEALTH_THERMOMETER;
    int HEALTH_BLOOD_PRESSURE;
    int HEALTH_UNCATEGORIZED;
    int TOY_GAME;
    int TOY_CONTROLLER;
    int TOY_DOLL_ACTION_FIGURE;
    int TOY_VEHICLE;
    int TOY_ROBOT;
    int TOY_UNCATEGORIZED;
    int WEARABLE_GLASSES;
    int WEARABLE_HELMET;
    int WEARABLE_JACKET;
    int WEARABLE_PAGER;
    int WEARABLE_WRIST_WATCH;
    int WEARABLE_UNCATEGORIZED;
    int AUDIO_VIDEO_VIDEO_GAMING_TOY;
    int AUDIO_VIDEO_VIDEO_CONFERENCING;
    int AUDIO_VIDEO_VIDEO_DISPLAY_AND_LOUDSPEAKER;
    int AUDIO_VIDEO_VIDEO_MONITOR;
    int AUDIO_VIDEO_CAMCORDER;
    int AUDIO_VIDEO_VIDEO_CAMERA;
    int AUDIO_VIDEO_VCR;
    int AUDIO_VIDEO_HIFI_AUDIO;
    int AUDIO_VIDEO_SET_TOP_BOX;
    int AUDIO_VIDEO_CAR_AUDIO;
    int AUDIO_VIDEO_PORTABLE_AUDIO;
    int AUDIO_VIDEO_HEADPHONES;
    int AUDIO_VIDEO_LOUDSPEAKER;
    int AUDIO_VIDEO_MICROPHONE;
    int AUDIO_VIDEO_HANDSFREE;
    int AUDIO_VIDEO_WEARABLE_HEADSET;
    int AUDIO_VIDEO_UNCATEGORIZED;
    int PHONE_ISDN;
    int PHONE_MODEM_OR_GATEWAY;
    int PHONE_SMART;
    int PHONE_CORDLESS;
    int PHONE_CELLULAR;
    int PHONE_UNCATEGORIZED;
    int COMPUTER_WEARABLE;
    int COMPUTER_PALM_SIZE_PC_PDA;
    int COMPUTER_HANDHELD_PC_PDA;
    int COMPUTER_LAPTOP;
    int COMPUTER_SERVER;
    int COMPUTER_DESKTOP;
    int COMPUTER_UNCATEGORIZED;
    class Major {
      int UNCATEGORIZED;
      int HEALTH;
      int TOY;
      int WEARABLE;
      int IMAGING;
      int PERIPHERAL;
      int AUDIO_VIDEO;
      int NETWORKING;
      int PHONE;
      int COMPUTER;
      int MISC;
      int BITMASK;
    }
    int BITMASK;
  }
  class Service {
    int INFORMATION;
    int TELEPHONY;
    int AUDIO;
    int OBJECT_TRANSFER;
    int CAPTURE;
    int RENDER;
    int NETWORKING;
    int POSITIONING;
    int LIMITED_DISCOVERABILITY;
    int BITMASK;
  }
  int CREATOR;
  int mClass;
  int ERROR;
}
class BluetoothAudioGateway {
  int MSG_INCOMING_HANDSFREE_CONNECTION;
  int MSG_INCOMING_HEADSET_CONNECTION;
  class IncomingConnectionInfo {
    int mRfcommChan;
    int mSocketFd;
    int mRemoteDevice;
    int mAdapter;
  }
  int mCallback;
  int SELECT_WAIT_TIMEOUT;
  int mInterrupted;
  int mConnectThead;
  int DEFAULT_HS_AG_CHANNEL;
  int DEFAULT_HF_AG_CHANNEL;
  int mAdapter;
  int mTimeoutRemainingMs;
  int mConnectingHandsfreeSocketFd;
  int mConnectingHandsfreeRfcommChannel;
  int mConnectingHandsfreeAddress;
  int mConnectingHeadsetSocketFd;
  int mConnectingHeadsetRfcommChannel;
  int mConnectingHeadsetAddress;
  int mHeadsetAgRfcommChannel;
  int mHandsfreeAgRfcommChannel;
  int mNativeData;
  int DBG;
  int TAG;
}
class BluetoothAssignedNumbers {
  int RIVIERAWAVES;
  int WICENTRIC;
  int STONESTREET_ONE;
  int REALTEK_SEMICONDUCTOR;
  int BELKIN_INTERNATIONAL;
  int RALINK_TECHNOLOGY;
  int EM_MICROELECTRONIC_MARIN;
  int NORDIC_SEMICONDUCTOR;
  int VIZIO;
  int HARMAN_INTERNATIONAL;
  int SONY_ERICSSON;
  int PLANTRONICS;
  int THREE_DIJOY;
  int FREE2MOVE;
  int J_AND_M;
  int TZERO_TECHNOLOGIES;
  int SIRF_TECHNOLOGY;
  int APT_LICENSING;
  int AVAGO;
  int STACCATO_COMMUNICATIONS;
  int APPLE;
  int CONTINENTAL_AUTOMOTIVE;
  int ACCEL_SEMICONDUCTOR;
  int THREE_DSP;
  int MARVELL;
  int BLUEGIGA;
  int MEDIATEK;
  int ATHEROS_COMMUNICATIONS;
  int SOCKET_MOBILE;
  int PARROT;
  int CONWISE_TECHNOLOGY;
  int INTEGRATED_SILICON_SOLUTION;
  int SEIKO_EPSON;
  int BLUETOOTH_SIG;
  int SYSTEMS_AND_CHIPS;
  int IPEXTREME;
  int RESEARCH_IN_MOTION;
  int GENNUM;
  int MATSUSHITA_ELECTRIC;
  int INTEGRATED_SYSTEM_SOLUTION;
  int TERAX;
  int MOBILIAN_CORPORATION;
  int RENESAS_TECHNOLOGY;
  int ECLIPSE;
  int CATC;
  int COMMIL;
  int RED_M;
  int SYNOPSYS;
  int ST_MICROELECTRONICS;
  int MEWTEL_TECHNOLOGY;
  int NORWOOD_SYSTEMS;
  int GCT_SEMICONDUCTOR;
  int MACRONIX;
  int TENOVIS;
  int SYMBOL_TECHNOLOGIES;
  int HITACHI;
  int RF_MICRO_DEVICES;
  int OPEN_INTERFACE;
  int C_TECHNOLOGIES;
  int PHILIPS_SEMICONDUCTORS;
  int ALCATEL;
  int WAVEPLUS_TECHNOLOGY;
  int NEC;
  int MANSELLA;
  int BANDSPEED;
  int AVM_BERLIN;
  int INVENTEL;
  int QUALCOMM;
  int CONEXANT_SYSTEMS;
  int SIGNIA_TECHNOLOGIES;
  int TTPCOM;
  int ROHDE_AND_SCHWARZ;
  int TRANSILICA;
  int NEWLOGIC;
  int KC_TECHNOLOGY;
  int RTX_TELECOM;
  int MITSUBISHI_ELECTRIC;
  int ATMEL;
  int ZEEVO;
  int WIDCOMM;
  int MITEL_SEMICONDUCTOR;
  int BROADCOM;
  int PARTHUS_TECHNOLOGIES;
  int TEXAS_INSTRUMENTS;
  int DIGIANSWER;
  int SILICON_WAVE;
  int CAMBRIDGE_SILICON_RADIO;
  int INFINEON_TECHNOLOGIES;
  int MOTOROLA;
  int LUCENT;
  int MICROSOFT;
  int THREECOM;
  int TOSHIBA;
  int IBM;
  int INTEL;
  int NOKIA_MOBILE_PHONES;
  int ERICSSON_TECHNOLOGY;
}
class BluetoothAdapter {
  class StateChangeCallbackWrapper {
    int mCallback;
  }
  class BluetoothStateChangeCallback {
  }
  class RfcommChannelPicker {
    int mUuid;
    int mChannels;
    int sRandom;
    int sChannels;
    int RESERVED_RFCOMM_CHANNELS;
  }
  int mServiceRecordHandler;
  int mService;
  int sAdapter;
  int ADDRESS_LENGTH;
  int BLUETOOTH_SERVICE;
  int STATE_DISCONNECTING;
  int STATE_CONNECTED;
  int STATE_CONNECTING;
  int STATE_DISCONNECTED;
  int EXTRA_PREVIOUS_CONNECTION_STATE;
  int EXTRA_CONNECTION_STATE;
  int ACTION_CONNECTION_STATE_CHANGED;
  int EXTRA_LOCAL_NAME;
  int ACTION_LOCAL_NAME_CHANGED;
  int ACTION_DISCOVERY_FINISHED;
  int ACTION_DISCOVERY_STARTED;
  int SCAN_MODE_CONNECTABLE_DISCOVERABLE;
  int SCAN_MODE_CONNECTABLE;
  int SCAN_MODE_NONE;
  int EXTRA_PREVIOUS_SCAN_MODE;
  int EXTRA_SCAN_MODE;
  int ACTION_SCAN_MODE_CHANGED;
  int ACTION_REQUEST_ENABLE;
  int EXTRA_DISCOVERABLE_DURATION;
  int ACTION_REQUEST_DISCOVERABLE;
  int STATE_TURNING_OFF;
  int STATE_ON;
  int STATE_TURNING_ON;
  int STATE_OFF;
  int EXTRA_PREVIOUS_STATE;
  int EXTRA_STATE;
  int ACTION_STATE_CHANGED;
  int ERROR;
  int DBG;
  int TAG;
}
class BluetoothA2dp {
  int mAdapter;
  int mService;
  int mServiceListener;
  int STATE_NOT_PLAYING;
  int STATE_PLAYING;
  int ACTION_PLAYING_STATE_CHANGED;
  int ACTION_CONNECTION_STATE_CHANGED;
  int DBG;
  int TAG;
}
class AtParser {
  int mLastInput;
  int mBasicHandlers;
  int mExtHandlers;
  int TYPE_TEST;
  int TYPE_SET;
  int TYPE_READ;
  int TYPE_ACTION;
}
class AtCommandResult {
  int mResponse;
  int mResultCode;
  int ERROR_STRING;
  int OK_STRING;
  int UNSOLICITED;
  int ERROR;
  int OK;
}
class AtCommandHandler {
}
