package android.net.wifi.p2p;
class WifiP2pService {
  class ClientInfo {
    int mServList;
    int mReqList;
    int mMessenger;
  }
  class P2pStateMachine {
    class UserAuthorizingJoinState {
    }
    class GroupCreatedState {
    }
    class GroupNegotiationState {
    }
    class ProvisionDiscoveryState {
    }
    class UserAuthorizingInvitationState {
    }
    class GroupCreatingState {
    }
    class InactiveState {
    }
    class P2pEnabledState {
    }
    class P2pEnablingState {
    }
    class P2pDisabledState {
    }
    class P2pDisablingState {
    }
    class P2pNotSupportedState {
    }
    class DefaultState {
    }
    int mSavedProvDiscDevice;
    int mSavedP2pGroup;
    int mSavedPeerConfig;
    int mGroup;
    int mWifiP2pInfo;
    int mPeers;
    int mWifiMonitor;
    int mWifiNative;
    int mUserAuthorizingJoinState;
    int mGroupCreatedState;
    int mGroupNegotiationState;
    int mProvisionDiscoveryState;
    int mUserAuthorizingInvitationState;
    int mGroupCreatingState;
    int mInactiveState;
    int mP2pEnabledState;
    int mP2pEnablingState;
    int mP2pDisabledState;
    int mP2pDisablingState;
    int mP2pNotSupportedState;
    int mDefaultState;
  }
  int SERVER_ADDRESS;
  int DHCP_RANGE;
  int mForegroundAppPkgName;
  int mForegroundAppMessenger;
  int mClientInfoList;
  int mServiceDiscReqId;
  int mServiceTransactionId;
  int mNetworkInfo;
  int mDiscoveryStarted;
  int mJoinExistingGroup;
  int mAutonomousGroup;
  int mThisDevice;
  int mP2pSupported;
  int PEER_CONNECTION_USER_REJECT;
  int PEER_CONNECTION_USER_ACCEPT;
  int GROUP_CREATING_TIMED_OUT;
  int BASE;
  int mP2pRestartCount;
  int P2P_RESTART_TRIES;
  int P2P_RESTART_INTERVAL_MSECS;
  int GROUP_IDLE_TIME_S;
  int DISCOVER_TIMEOUT_S;
  int mGroupCreatingTimeoutIndex;
  int GROUP_CREATING_WAIT_TIME_MS;
  int FORM_GROUP;
  int JOIN_GROUP;
  int mWifiChannel;
  int mReplyChannel;
  int mP2pStateMachine;
  int mActivityMgr;
  int mDhcpStateMachine;
  int mNwService;
  int mNotification;
  int mInterface;
  int mContext;
  int NETWORKTYPE;
  int DBG;
  int TAG;
}
class WifiP2pProvDiscEvent {
  int pin;
  int device;
  int event;
  int SHOW_PIN;
  int ENTER_PIN;
  int PBC_RSP;
  int PBC_REQ;
  int TAG;
}
class WifiP2pManager {
  class Channel {
    class P2pHandler {
    }
    int mContext;
    int mHandler;
    int mAsyncChannel;
    int mDialogListener;
    int mListenerKey;
    int mListenerMapLock;
    int mListenerMap;
    int mUpnpServRspListener;
    int mDnsSdTxtListener;
    int mDnsSdServRspListener;
    int mServRspListener;
    int mChannelListener;
    int INVALID_LISTENER_KEY;
  }
  class DialogListener {
  }
  class UpnpServiceResponseListener {
  }
  class DnsSdTxtRecordListener {
  }
  class DnsSdServiceResponseListener {
  }
  class ServiceResponseListener {
  }
  class GroupInfoListener {
  }
  class ConnectionInfoListener {
  }
  class PeerListListener {
  }
  class ActionListener {
  }
  class ChannelListener {
  }
  int NOT_IN_FOREGROUND;
  int NO_SERVICE_REQUESTS;
  int BUSY;
  int P2P_UNSUPPORTED;
  int ERROR;
  int SHOW_PIN_REQUESTED;
  int CONNECTION_REQUESTED;
  int DIALOG_LISTENER_ATTACHED;
  int DIALOG_LISTENER_DETACHED;
  int SET_DIALOG_LISTENER;
  int SET_DEVICE_NAME_SUCCEEDED;
  int SET_DEVICE_NAME_FAILED;
  int SET_DEVICE_NAME;
  int RESPONSE_SERVICE;
  int PING;
  int DISCOVER_SERVICES_SUCCEEDED;
  int DISCOVER_SERVICES_FAILED;
  int DISCOVER_SERVICES;
  int CLEAR_SERVICE_REQUESTS_SUCCEEDED;
  int CLEAR_SERVICE_REQUESTS_FAILED;
  int CLEAR_SERVICE_REQUESTS;
  int REMOVE_SERVICE_REQUEST_SUCCEEDED;
  int REMOVE_SERVICE_REQUEST_FAILED;
  int REMOVE_SERVICE_REQUEST;
  int ADD_SERVICE_REQUEST_SUCCEEDED;
  int ADD_SERVICE_REQUEST_FAILED;
  int ADD_SERVICE_REQUEST;
  int CLEAR_LOCAL_SERVICES_SUCCEEDED;
  int CLEAR_LOCAL_SERVICES_FAILED;
  int CLEAR_LOCAL_SERVICES;
  int REMOVE_LOCAL_SERVICE_SUCCEEDED;
  int REMOVE_LOCAL_SERVICE_FAILED;
  int REMOVE_LOCAL_SERVICE;
  int ADD_LOCAL_SERVICE_SUCCEEDED;
  int ADD_LOCAL_SERVICE_FAILED;
  int ADD_LOCAL_SERVICE;
  int RESPONSE_GROUP_INFO;
  int REQUEST_GROUP_INFO;
  int RESPONSE_CONNECTION_INFO;
  int REQUEST_CONNECTION_INFO;
  int RESPONSE_PEERS;
  int REQUEST_PEERS;
  int REMOVE_GROUP_SUCCEEDED;
  int REMOVE_GROUP_FAILED;
  int REMOVE_GROUP;
  int CREATE_GROUP_SUCCEEDED;
  int CREATE_GROUP_FAILED;
  int CREATE_GROUP;
  int CANCEL_CONNECT_SUCCEEDED;
  int CANCEL_CONNECT_FAILED;
  int CANCEL_CONNECT;
  int CONNECT_SUCCEEDED;
  int CONNECT_FAILED;
  int CONNECT;
  int STOP_DISCOVERY_SUCCEEDED;
  int STOP_DISCOVERY_FAILED;
  int STOP_DISCOVERY;
  int DISCOVER_PEERS_SUCCEEDED;
  int DISCOVER_PEERS_FAILED;
  int DISCOVER_PEERS;
  int BASE;
  int mService;
  int P2P_CONFIG_BUNDLE_KEY;
  int P2P_DEV_BUNDLE_KEY;
  int WPS_PIN_BUNDLE_KEY;
  int RESET_DIALOG_LISTENER_BUNDLE_KEY;
  int APP_PKG_BUNDLE_KEY;
  int EXTRA_WIFI_P2P_DEVICE;
  int WIFI_P2P_THIS_DEVICE_CHANGED_ACTION;
  int WIFI_P2P_DISCOVERY_STARTED;
  int WIFI_P2P_DISCOVERY_STOPPED;
  int EXTRA_DISCOVERY_STATE;
  int WIFI_P2P_DISCOVERY_CHANGED_ACTION;
  int WIFI_P2P_PEERS_CHANGED_ACTION;
  int EXTRA_LINK_CAPABILITIES;
  int EXTRA_LINK_PROPERTIES;
  int EXTRA_NETWORK_INFO;
  int EXTRA_WIFI_P2P_INFO;
  int WIFI_P2P_CONNECTION_CHANGED_ACTION;
  int WIFI_P2P_STATE_ENABLED;
  int WIFI_P2P_STATE_DISABLED;
  int EXTRA_WIFI_STATE;
  int WIFI_P2P_STATE_CHANGED_ACTION;
  int TAG;
}
class WifiP2pInfo {
  int CREATOR;
  int groupOwnerAddress;
  int isGroupOwner;
  int groupFormed;
}
class WifiP2pGroup {
  int CREATOR;
  int groupStartedPattern;
  int mInterface;
  int mPassphrase;
  int mClients;
  int mIsGroupOwner;
  int mOwner;
  int mNetworkName;
}
class WifiP2pDeviceList {
  int CREATOR;
  int mDevices;
}
class WifiP2pDevice {
  int CREATOR;
  int threeTokenPattern;
  int twoTokenPattern;
  int detailedDevicePattern;
  int status;
  int UNAVAILABLE;
  int AVAILABLE;
  int FAILED;
  int INVITED;
  int CONNECTED;
  int groupCapability;
  int deviceCapability;
  int wpsConfigMethodsSupported;
  int GROUP_CAPAB_GROUP_FORMATION;
  int GROUP_CAPAB_PERSISTENT_RECONN;
  int GROUP_CAPAB_CROSS_CONN;
  int GROUP_CAPAB_INTRA_BSS_DIST;
  int GROUP_CAPAB_GROUP_LIMIT;
  int GROUP_CAPAB_PERSISTENT_GROUP;
  int GROUP_CAPAB_GROUP_OWNER;
  int DEVICE_CAPAB_INVITATION_PROCEDURE;
  int DEVICE_CAPAB_DEVICE_LIMIT;
  int DEVICE_CAPAB_INFRA_MANAGED;
  int DEVICE_CAPAB_CONCURRENT_OPER;
  int DEVICE_CAPAB_CLIENT_DISCOVERABILITY;
  int DEVICE_CAPAB_SERVICE_DISCOVERY;
  int WPS_CONFIG_KEYPAD;
  int WPS_CONFIG_PUSHBUTTON;
  int WPS_CONFIG_DISPLAY;
  int secondaryDeviceType;
  int primaryDeviceType;
  int deviceAddress;
  int deviceName;
  int TAG;
}
class WifiP2pConfig {
  int CREATOR;
  int persist;
  class Persist {
    int NO;
    int YES;
    int SYSTEM_DEFAULT;
  }
  int groupOwnerIntent;
  int wps;
  int deviceAddress;
}
