package android.net.wifi;
class WpsResult {
  int CREATOR;
  int pin;
  int status;
  class Status {
    int IN_PROGRESS;
    int FAILURE;
    int SUCCESS;
  }
}
class WpsInfo {
  int CREATOR;
  int pin;
  int BSSID;
  int setup;
  int INVALID;
  int LABEL;
  int KEYPAD;
  int DISPLAY;
  int PBC;
}
class WifiWatchdogStateMachine {
  class OnlineState {
  }
  class RssiMonitoringState {
  }
  class OnlineWatchState {
  }
  class WalledGardenCheckState {
    int mWalledGardenToken;
  }
  class ConnectedState {
  }
  class VerifyingLinkState {
  }
  class NotConnectedState {
  }
  class WatchdogEnabledState {
  }
  class WatchdogDisabledState {
  }
  class DefaultState {
  }
  int mWalledGardenNotificationShown;
  int sWifiOnly;
  int mLastWalledGardenCheckTime;
  int mLinkProperties;
  int mWifiInfo;
  int mWalledGardenUrl;
  int mWalledGardenTestEnabled;
  int mPoorNetworkDetectionEnabled;
  int mArpPingTimeoutMs;
  int mMinArpResponses;
  int mNumArpPings;
  int mWalledGardenIntervalMs;
  int mRssiFetchIntervalMs;
  int mRssiFetchToken;
  int mArpCheckIntervalMs;
  int mArpToken;
  int mOnlineState;
  int mRssiMonitoringState;
  int mOnlineWatchState;
  int mWalledGardenCheckState;
  int mConnectedState;
  int mVerifyingLinkState;
  int mNotConnectedState;
  int mWatchdogEnabledState;
  int mWatchdogDisabledState;
  int mDefaultState;
  int mWsmChannel;
  int mBroadcastReceiver;
  int mIntentFilter;
  int mWifiManager;
  int mContentResolver;
  int mContext;
  int FULL_ARP_CHECK;
  int SINGLE_ARP_CHECK;
  int RSSI_FETCH_FAILED;
  int RSSI_FETCH_SUCCEEDED;
  int RSSI_FETCH;
  int GOOD_LINK_DETECTED;
  int POOR_LINK_DETECTED;
  int CMD_RSSI_FETCH;
  int CMD_DELAYED_WALLED_GARDEN_CHECK;
  int CMD_ARP_CHECK;
  int EVENT_WATCHDOG_SETTINGS_CHANGE;
  int EVENT_WIFI_RADIO_STATE_CHANGE;
  int EVENT_RSSI_CHANGE;
  int EVENT_NETWORK_STATE_CHANGE;
  int EVENT_WATCHDOG_TOGGLED;
  int BASE;
  int WALLED_GARDEN_START_DELAY_MS;
  int WALLED_GARDEN_SOCKET_TIMEOUT_MS;
  int DEFAULT_WALLED_GARDEN_URL;
  int DEFAULT_ARP_PING_TIMEOUT_MS;
  int DEFAULT_MIN_ARP_RESPONSES;
  int DEFAULT_NUM_ARP_PINGS;
  int DEFAULT_WALLED_GARDEN_INTERVAL_MS;
  int DEFAULT_RSSI_FETCH_INTERVAL_MS;
  int DEFAULT_ARP_CHECK_INTERVAL_MS;
  int mCurrentSignalLevel;
  int mLastBssidAvoidedTime;
  int mMinIntervalArrayIndex;
  int MIN_INTERVAL_AVOID_BSSID_MS;
  int mRssiMonitorCount;
  int RSSI_MONITOR_COUNT;
  int RSSI_MONITOR_THRESHOLD;
  int RSSI_LEVEL_MONITOR;
  int WALLED_GARDEN_NOTIFICATION_ID;
  int TAG;
  int DBG;
}
class WifiStateTracker {
  class WifiStateReceiver {
  }
  int mWifiManager;
  int mWifiStateReceiver;
  int mContext;
  int mCsHandler;
  int mLastState;
  int mNetworkInfo;
  int mLinkCapabilities;
  int mLinkProperties;
  int mDefaultRouteSet;
  int mPrivateDnsRouteSet;
  int mTeardownRequested;
  int LOGV;
  int TAG;
  int NETWORKTYPE;
}
class WifiStateMachine {
  class SoftApStoppingState {
  }
  class TetheredState {
  }
  class TetheringState {
  }
  class SoftApStartedState {
  }
  class SoftApStartingState {
  }
  class WpsRunningState {
    int mSourceMessage;
  }
  class DisconnectedState {
    int mFrameworkScanIntervalMs;
    int mAlarmEnabled;
  }
  class DisconnectingState {
  }
  class ConnectedState {
  }
  class VerifyingLinkState {
  }
  class ObtainingIpState {
  }
  class L2ConnectedState {
  }
  class ConnectModeState {
  }
  class ScanModeState {
  }
  class DriverStoppedState {
  }
  class DriverStoppingState {
  }
  class DriverStartedState {
  }
  class DriverStartingState {
    int mTries;
  }
  class SupplicantStoppingState {
  }
  class SupplicantStartedState {
  }
  class SupplicantStartingState {
  }
  class DriverFailedState {
  }
  class DriverUnloadedState {
  }
  class DriverUnloadingState {
  }
  class DriverLoadedState {
  }
  class DriverLoadingState {
  }
  class InitialState {
  }
  class DefaultState {
  }
  int mBatteryStats;
  int mLastRunningWifiUids;
  int mRunningWifiUids;
  int mReportedRunning;
  int mIsRunning;
  int ACTION_DELAYED_DRIVER_STOP;
  int DRIVER_STOP_REQUEST;
  int DELAYED_STOP_COUNTER;
  int ACTION_START_SCAN;
  int SCAN_REQUEST;
  int mLastApEnableUid;
  int mLastEnableUid;
  int mWifiApState;
  int mWifiState;
  class TetherStateChange {
    int active;
    int available;
  }
  int mSoftApStoppingState;
  int mTetheredState;
  int mTetheringState;
  int mSoftApStartedState;
  int mSoftApStartingState;
  int mWpsRunningState;
  int mDisconnectedState;
  int mDisconnectingState;
  int mConnectedState;
  int mVerifyingLinkState;
  int mObtainingIpState;
  int mL2ConnectedState;
  int mConnectModeState;
  int mScanModeState;
  int mDriverStoppedState;
  int mDriverStoppingState;
  int mDriverStartedState;
  int mDriverStartingState;
  int mSupplicantStoppingState;
  int mSupplicantStartedState;
  int mSupplicantStartingState;
  int mDriverLoadedState;
  int mDriverLoadingState;
  int mDriverFailedState;
  int mDriverUnloadedState;
  int mDriverUnloadingState;
  int mInitialState;
  int mDefaultState;
  int MAX_RSSI;
  int MIN_RSSI;
  int mInDelayedStop;
  int mDelayedStopCounter;
  int mDriverStopDelayMs;
  int mLastEnableAllNetworksTime;
  int MIN_INTERVAL_ENABLE_ALL_NETWORKS_MS;
  int mSupplicantScanIntervalMs;
  int mDefaultFrameworkScanIntervalMs;
  int mPowerSaveEnabled;
  int DEFAULT_MAX_DHCP_RETRIES;
  int NOT_IN_ECM_STATE;
  int IN_ECM_STATE;
  int FAILURE;
  int SUCCESS;
  int SCAN_PASSIVE;
  int SCAN_ACTIVE;
  int SCAN_ONLY_MODE;
  int CONNECT_MODE;
  int CMD_DISABLE_P2P;
  int CMD_ENABLE_P2P;
  int CMD_RESET_SUPPLICANT_STATE;
  int CMD_ENABLE_BACKGROUND_SCAN;
  int CMD_SET_FREQUENCY_BAND;
  int MULTICAST_V4;
  int MULTICAST_V6;
  int CMD_NO_NETWORKS_PERIODIC_SCAN;
  int CMD_CLEAR_SUSPEND_OPTIMIZATIONS;
  int CMD_SET_SUSPEND_OPTIMIZATIONS;
  int CMD_STOP_PACKET_FILTERING;
  int CMD_START_PACKET_FILTERING;
  int CMD_RSSI_POLL;
  int CMD_ENABLE_RSSI_POLL;
  int CMD_SET_COUNTRY_CODE;
  int CMD_SET_HIGH_PERF_MODE;
  int CMD_REASSOCIATE;
  int CMD_RECONNECT;
  int CMD_DISCONNECT;
  int CMD_SET_SCAN_TYPE;
  int CMD_SET_SCAN_MODE;
  int CMD_START_SCAN;
  int CMD_GET_CONFIGURED_NETWORKS;
  int CMD_SAVE_CONFIG;
  int CMD_CLEAR_BLACKLIST;
  int CMD_BLACKLIST_NETWORK;
  int CMD_ENABLE_ALL_NETWORKS;
  int CMD_ENABLE_NETWORK;
  int CMD_REMOVE_NETWORK;
  int CMD_ADD_OR_UPDATE_NETWORK;
  int CMD_PING_SUPPLICANT;
  int CMD_BLUETOOTH_ADAPTER_STATE_CHANGE;
  int CMD_TETHER_NOTIFICATION_TIMED_OUT;
  int CMD_TETHER_STATE_CHANGE;
  int CMD_RESPONSE_AP_CONFIG;
  int CMD_REQUEST_AP_CONFIG;
  int CMD_SET_AP_CONFIG_COMPLETED;
  int CMD_SET_AP_CONFIG;
  int CMD_STOP_AP;
  int CMD_START_AP_FAILURE;
  int CMD_START_AP_SUCCESS;
  int CMD_START_AP;
  int CMD_DRIVER_START_TIMED_OUT;
  int CMD_DELAYED_STOP_DRIVER;
  int CMD_STOP_SUPPLICANT_FAILED;
  int CMD_STATIC_IP_FAILURE;
  int CMD_STATIC_IP_SUCCESS;
  int CMD_STOP_DRIVER;
  int CMD_START_DRIVER;
  int CMD_STOP_SUPPLICANT;
  int CMD_START_SUPPLICANT;
  int CMD_UNLOAD_DRIVER_FAILURE;
  int CMD_UNLOAD_DRIVER_SUCCESS;
  int CMD_LOAD_DRIVER_FAILURE;
  int CMD_LOAD_DRIVER_SUCCESS;
  int CMD_UNLOAD_DRIVER;
  int CMD_LOAD_DRIVER;
  int BASE;
  int EVENTLOG_SUPPLICANT_STATE_CHANGED;
  int EVENTLOG_WIFI_EVENT_HANDLED;
  int EVENTLOG_WIFI_STATE_CHANGED;
  int mWifiApConfigChannel;
  int mWifiP2pChannel;
  int mWifiP2pManager;
  int mReplyChannel;
  int mFilteringMulticastV4Packets;
  int mFrequencyBand;
  int mDriverStopIntent;
  int mScanIntent;
  int mAlarmManager;
  int mDhcpStateMachine;
  int mSupplicantStateTracker;
  int mNetworkInfo;
  int mWifiInfo;
  int mDhcpInfoInternal;
  int mContext;
  int mWakeLock;
  int mPeriodicScanToken;
  int mLinkProperties;
  int mDriverStartToken;
  int DRIVER_START_TIME_OUT_MSECS;
  int mTetherToken;
  int TETHER_NOTIFICATION_TIME_OUT_MSECS;
  int mSupplicantStopFailureToken;
  int mSupplicantRestartCount;
  int SUPPLICANT_RESTART_TRIES;
  int SUPPLICANT_RESTART_INTERVAL_MSECS;
  int POLL_RSSI_INTERVAL_MSECS;
  int mSuspendWakeLock;
  int mScreenFilter;
  int mScreenReceiver;
  int mBluetoothConnectionActive;
  int mHighPerfMode;
  int mSetScanActive;
  int mScanResultIsPending;
  int mIsScanMode;
  int mReconnectCount;
  int mRssiPollToken;
  int mEnableBackgroundScan;
  int mEnableRssiPolling;
  int mLastNetworkId;
  int mLastBssid;
  int mLastSignalLevel;
  int mTetherInterfaceName;
  int mInterfaceName;
  int mBackgroundScanSupported;
  int mScanResultCache;
  int SCAN_RESULT_CACHE_SIZE;
  int scanResultPattern;
  int mScanResults;
  int mPrimaryDeviceType;
  int mP2pSupported;
  int mCm;
  int mNwService;
  int mWifiConfigStore;
  int mWifiNative;
  int mWifiMonitor;
  int SOFTAP_IFACE;
  int DBG;
  int NETWORKTYPE;
  int TAG;
}
class WifiNative {
  int mInterface;
  int BLUETOOTH_COEXISTENCE_MODE_SENSE;
  int BLUETOOTH_COEXISTENCE_MODE_DISABLED;
  int BLUETOOTH_COEXISTENCE_MODE_ENABLED;
  int DEFAULT_GROUP_OWNER_INTENT;
  int mTAG;
  int DBG;
}
class WifiMonitor {
  class MonitorThread {
  }
  int MAX_RECV_ERRORS;
  int mRecvErrors;
  int WPA_RECV_ERROR_STR;
  int MONITOR_SOCKET_CLOSED_STR;
  int AP_STA_CONNECTED_EVENT;
  int AP_STA_DISCONNECTED_EVENT;
  int P2P_SERV_DISC_RESP_EVENT;
  int P2P_FIND_STOPPED_EVENT;
  int P2P_PROV_DISC_SHOW_PIN_EVENT;
  int P2P_PROV_DISC_ENTER_PIN_EVENT;
  int P2P_PROV_DISC_PBC_RSP_EVENT;
  int P2P_PROV_DISC_PBC_REQ_EVENT;
  int P2P_INVITATION_RESULT_EVENT;
  int P2P_INVITATION_RECEIVED_EVENT;
  int P2P_GROUP_REMOVED_EVENT;
  int P2P_GROUP_STARTED_EVENT;
  int P2P_GROUP_FORMATION_FAILURE_EVENT;
  int P2P_GROUP_FORMATION_SUCCESS_EVENT;
  int P2P_GO_NEGOTIATION_FAILURE_EVENT;
  int P2P_GO_NEGOTIATION_SUCCESS_EVENT;
  int P2P_GO_NEGOTIATION_REQUEST_EVENT;
  int P2P_DEVICE_LOST_EVENT;
  int P2P_DEVICE_FOUND_EVENT;
  int DRIVER_HUNG_EVENT;
  int WPS_TIMEOUT_EVENT;
  int WPS_OVERLAP_EVENT;
  int WPS_FAIL_EVENT;
  int WPS_SUCCESS_EVENT;
  int AUTHENTICATION_FAILURE_EVENT;
  int SUPPLICANT_STATE_CHANGE_EVENT;
  int SCAN_RESULTS_EVENT;
  int NETWORK_DISCONNECTION_EVENT;
  int NETWORK_CONNECTION_EVENT;
  int SUP_DISCONNECTION_EVENT;
  int SUP_CONNECTION_EVENT;
  int BASE;
  int mWifiNative;
  int mStateMachine;
  int AP_STA_DISCONNECTED_STR;
  int AP_STA_CONNECTED_STR;
  int HOST_AP_EVENT_PREFIX_STR;
  int P2P_SERV_DISC_RESP_STR;
  int P2P_PROV_DISC_SHOW_PIN_STR;
  int P2P_PROV_DISC_ENTER_PIN_STR;
  int P2P_PROV_DISC_PBC_RSP_STR;
  int P2P_PROV_DISC_PBC_REQ_STR;
  int P2P_INVITATION_RESULT_STR;
  int P2P_INVITATION_RECEIVED_STR;
  int P2P_GROUP_REMOVED_STR;
  int P2P_GROUP_STARTED_STR;
  int P2P_GROUP_FORMATION_FAILURE_STR;
  int P2P_GROUP_FORMATION_SUCCESS_STR;
  int P2P_GO_NEG_FAILURE_STR;
  int P2P_GO_NEG_SUCCESS_STR;
  int P2P_GO_NEG_REQUEST_STR;
  int P2P_FIND_STOPPED_STR;
  int P2P_DEVICE_LOST_STR;
  int P2P_DEVICE_FOUND_STR;
  int P2P_EVENT_PREFIX_STR;
  int mConnectedEventPattern;
  int EAP_AUTH_FAILURE_STR;
  int EAP_FAILURE_STR;
  int DRIVER_STATE_STR;
  int TERMINATING_STR;
  int LINK_SPEED_STR;
  int SCAN_RESULTS_STR;
  int STATE_CHANGE_STR;
  int DISCONNECTED_STR;
  int CONNECTED_STR;
  int WPS_TIMEOUT_STR;
  int WPS_OVERLAP_STR;
  int REASON_WEP_PROHIBITED;
  int REASON_TKIP_ONLY_PROHIBITED;
  int CONFIG_AUTH_FAILURE;
  int CONFIG_MULTIPLE_PBC_DETECTED;
  int WPS_FAIL_PATTERN;
  int WPS_FAIL_STR;
  int WPS_SUCCESS_STR;
  int PASSWORD_MAY_BE_INCORRECT_STR;
  int WPA_EVENT_PREFIX_STR;
  int EVENT_PREFIX_LEN_STR;
  int EVENT_PREFIX_STR;
  int UNKNOWN;
  int EAP_FAILURE;
  int DRIVER_STATE;
  int TERMINATING;
  int LINK_SPEED;
  int SCAN_RESULTS;
  int STATE_CHANGE;
  int DISCONNECTED;
  int CONNECTED;
  int TAG;
}
class WifiManager {
  class MulticastLock {
    int mHeld;
    int mRefCounted;
    int mRefCount;
    int mBinder;
    int mTag;
  }
  class WifiLock {
    int mWorkSource;
    int mHeld;
    int mRefCounted;
    int mLockType;
    int mRefCount;
    int mBinder;
    int mTag;
  }
  class Channel {
    class WifiHandler {
    }
    int mHandler;
    int mAsyncChannel;
    int INVALID_KEY;
    int mListenerKey;
    int mListenerMapLock;
    int mListenerMap;
    int mChannelListener;
  }
  class WpsListener {
  }
  class ActionListener {
  }
  class ChannelListener {
  }
  int WPS_TIMED_OUT;
  int WPS_AUTH_FAILURE;
  int WPS_TKIP_ONLY_PROHIBITED;
  int WPS_WEP_PROHIBITED;
  int WPS_OVERLAP_ERROR;
  int BUSY;
  int IN_PROGRESS;
  int ERROR;
  int TRAFFIC_STATS_POLL;
  int ENABLE_TRAFFIC_STATS_POLL;
  int DISABLE_NETWORK_SUCCEEDED;
  int DISABLE_NETWORK_FAILED;
  int DISABLE_NETWORK;
  int CANCEL_WPS_SUCCEDED;
  int CANCEL_WPS_FAILED;
  int CANCEL_WPS;
  int WPS_COMPLETED;
  int WPS_FAILED;
  int START_WPS_SUCCEEDED;
  int START_WPS;
  int SAVE_NETWORK_SUCCEEDED;
  int SAVE_NETWORK_FAILED;
  int SAVE_NETWORK;
  int FORGET_NETWORK_SUCCEEDED;
  int FORGET_NETWORK_FAILED;
  int FORGET_NETWORK;
  int CONNECT_NETWORK_SUCCEEDED;
  int CONNECT_NETWORK_FAILED;
  int CONNECT_NETWORK;
  int BASE;
  int mActiveLockCount;
  int MAX_ACTIVE_LOCKS;
  int mHandler;
  int mService;
  int DATA_ACTIVITY_INOUT;
  int DATA_ACTIVITY_OUT;
  int DATA_ACTIVITY_IN;
  int DATA_ACTIVITY_NONE;
  int DATA_ACTIVITY_NOTIFICATION;
  int WIFI_FREQUENCY_BAND_2GHZ;
  int WIFI_FREQUENCY_BAND_5GHZ;
  int WIFI_FREQUENCY_BAND_AUTO;
  int RSSI_LEVELS;
  int MAX_RSSI;
  int MIN_RSSI;
  int WIFI_MODE_FULL_HIGH_PERF;
  int WIFI_MODE_SCAN_ONLY;
  int WIFI_MODE_FULL;
  int ACTION_PICK_WIFI_NETWORK;
  int NETWORK_IDS_CHANGED_ACTION;
  int EXTRA_LINK_CAPABILITIES;
  int EXTRA_LINK_PROPERTIES;
  int LINK_CONFIGURATION_CHANGED_ACTION;
  int EXTRA_NEW_RSSI;
  int RSSI_CHANGED_ACTION;
  int SCAN_RESULTS_AVAILABLE_ACTION;
  int CHANGE_REASON_CONFIG_CHANGE;
  int CHANGE_REASON_REMOVED;
  int CHANGE_REASON_ADDED;
  int EXTRA_CHANGE_REASON;
  int EXTRA_MULTIPLE_NETWORKS_CHANGED;
  int EXTRA_WIFI_CONFIGURATION;
  int CONFIGURED_NETWORKS_CHANGED_ACTION;
  int EXTRA_SUPPLICANT_ERROR;
  int EXTRA_NEW_STATE;
  int SUPPLICANT_STATE_CHANGED_ACTION;
  int EXTRA_WIFI_INFO;
  int EXTRA_BSSID;
  int EXTRA_NETWORK_INFO;
  int NETWORK_STATE_CHANGED_ACTION;
  int EXTRA_SUPPLICANT_CONNECTED;
  int SUPPLICANT_CONNECTION_CHANGE_ACTION;
  int WIFI_AP_STATE_FAILED;
  int WIFI_AP_STATE_ENABLED;
  int WIFI_AP_STATE_ENABLING;
  int WIFI_AP_STATE_DISABLED;
  int WIFI_AP_STATE_DISABLING;
  int EXTRA_PREVIOUS_WIFI_AP_STATE;
  int EXTRA_WIFI_AP_STATE;
  int WIFI_AP_STATE_CHANGED_ACTION;
  int WIFI_STATE_UNKNOWN;
  int WIFI_STATE_ENABLED;
  int WIFI_STATE_ENABLING;
  int WIFI_STATE_DISABLED;
  int WIFI_STATE_DISABLING;
  int EXTRA_PREVIOUS_WIFI_STATE;
  int EXTRA_WIFI_STATE;
  int WIFI_STATE_CHANGED_ACTION;
  int ERROR_AUTHENTICATING;
}
class WifiInfo {
  int CREATOR;
  int mMeteredHint;
  int mMacAddress;
  int mIpAddress;
  int mLinkSpeed;
  int LINK_SPEED_UNITS;
  int mRssi;
  int mHiddenSSID;
  int mNetworkId;
  int mSSID;
  int mBSSID;
  int mSupplicantState;
  int stateMap;
}
class WifiConfiguration {
  int CREATOR;
  int linkProperties;
  int proxySettings;
  class ProxySettings {
    int UNASSIGNED;
    int STATIC;
    int NONE;
  }
  int ipAssignment;
  class IpAssignment {
    int UNASSIGNED;
    int DHCP;
    int STATIC;
  }
  int allowedGroupCiphers;
  int allowedPairwiseCiphers;
  int allowedAuthAlgorithms;
  int allowedProtocols;
  int allowedKeyManagement;
  int hiddenSSID;
  int priority;
  int wepTxKeyIndex;
  int wepKeys;
  int preSharedKey;
  int BSSID;
  int SSID;
  int disableReason;
  int status;
  int networkId;
  int DISABLED_AUTH_FAILURE;
  int DISABLED_DHCP_FAILURE;
  int DISABLED_DNS_FAILURE;
  int DISABLED_UNKNOWN_REASON;
  class Status {
    int strings;
    int ENABLED;
    int DISABLED;
    int CURRENT;
  }
  class GroupCipher {
    int strings;
    int varName;
    int CCMP;
    int TKIP;
    int WEP104;
    int WEP40;
  }
  class PairwiseCipher {
    int strings;
    int varName;
    int CCMP;
    int TKIP;
    int NONE;
  }
  class AuthAlgorithm {
    int strings;
    int varName;
    int LEAP;
    int SHARED;
    int OPEN;
  }
  class Protocol {
    int strings;
    int varName;
    int RSN;
    int WPA;
  }
  class KeyMgmt {
    int strings;
    int varName;
    int WPA2_PSK;
    int IEEE8021X;
    int WPA_EAP;
    int WPA_PSK;
    int NONE;
  }
  int enterpriseFields;
  int ca_cert;
  int key_id;
  int engine_id;
  int engine;
  int client_cert;
  int password;
  int anonymous_identity;
  int identity;
  int phase2;
  int eap;
  class EnterpriseField {
    int value;
    int varName;
  }
  int INVALID_NETWORK_ID;
  int hiddenSSIDVarName;
  int priorityVarName;
  int wepTxKeyIdxVarName;
  int wepKeyVarNames;
  int pskVarName;
  int bssidVarName;
  int ssidVarName;
  int ENGINE_DISABLE;
  int ENGINE_ENABLE;
  int KEYSTORE_URI;
  int KEYSTORE_ENGINE_ID;
  int OLD_PRIVATE_KEY_NAME;
}
class WifiConfigStore {
  class DelayedDiskWrite {
    int TAG;
    int sWriteSequence;
    int sDiskWriteHandler;
    int sDiskWriteHandlerThread;
  }
  int mWifiNative;
  int EOS;
  int EXCLUSION_LIST_KEY;
  int PROXY_PORT_KEY;
  int PROXY_HOST_KEY;
  int PROXY_SETTINGS_KEY;
  int DNS_KEY;
  int GATEWAY_KEY;
  int LINK_ADDRESS_KEY;
  int IP_ASSIGNMENT_KEY;
  int ID_KEY;
  int IPCONFIG_FILE_VERSION;
  int ipConfigFile;
  int mLastPriority;
  int mNetworkIds;
  int mConfiguredNetworks;
  int DBG;
  int TAG;
  int mContext;
}
class WifiApConfigStore {
  class ActiveState {
  }
  class InactiveState {
  }
  class DefaultState {
  }
  int mReplyChannel;
  int mWifiApConfig;
  int mActiveState;
  int mInactiveState;
  int mDefaultState;
  int AP_CONFIG_FILE_VERSION;
  int AP_CONFIG_FILE;
  int TAG;
  int mContext;
}
class SupplicantStateTracker {
  class DormantState {
  }
  class CompletedState {
  }
  class HandshakeState {
    int mLoopDetectCount;
    int mLoopDetectIndex;
    int MAX_SUPPLICANT_LOOP_ITERATIONS;
  }
  class ScanState {
  }
  class DisconnectedState {
  }
  class InactiveState {
  }
  class UninitializedState {
  }
  class DefaultState {
  }
  int mDormantState;
  int mCompletedState;
  int mHandshakeState;
  int mScanState;
  int mDisconnectState;
  int mInactiveState;
  int mDefaultState;
  int mUninitializedState;
  int mContext;
  int mNetworksDisabledDuringConnect;
  int MAX_RETRIES_ON_AUTHENTICATION_FAILURE;
  int mAuthFailureInSupplicantBroadcast;
  int mAuthenticationFailuresCount;
  int mWifiConfigStore;
  int mWifiStateMachine;
  int DBG;
  int TAG;
}
class SupplicantState {
  int INVALID;
  int UNINITIALIZED;
  int DORMANT;
  int COMPLETED;
  int GROUP_HANDSHAKE;
  int FOUR_WAY_HANDSHAKE;
  int ASSOCIATED;
  int ASSOCIATING;
  int AUTHENTICATING;
  int SCANNING;
  int INACTIVE;
  int INTERFACE_DISABLED;
  int DISCONNECTED;
  int CREATOR;
}
class StateChangeResult {
  int state;
  int BSSID;
  int SSID;
  int networkId;
}
class ScanResult {
  int CREATOR;
  int frequency;
  int level;
  int capabilities;
  int BSSID;
  int SSID;
}
class NetworkUpdateResult {
  int isNewNetwork;
  int proxyChanged;
  int ipChanged;
  int netId;
}
