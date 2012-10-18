package com.android.server.connectivity;
class Vpn {
  class LegacyVpnRunner {
    int mTimer;
    int mInfo;
    int mOuterInterface;
    int mSockets;
    int mArguments;
    int mDaemons;
    int mConfig;
    int TAG;
  }
  class Connection {
    int mService;
  }
  int mLegacyVpnRunner;
  int mConnection;
  int mInterface;
  int mPackage;
  int mCallback;
  int mContext;
  int BIND_VPN_SERVICE;
  int TAG;
}
class Tethering {
  class TetherMasterSM {
    class SetDnsForwardersErrorState {
    }
    class StopTetheringErrorState {
    }
    class StartTetheringErrorState {
    }
    class SetIpForwardingDisabledErrorState {
    }
    class SetIpForwardingEnabledErrorState {
    }
    class ErrorState {
      int mErrorNotification;
    }
    class TetherModeAliveState {
      int mTryCell;
    }
    class InitialState {
    }
    class TetherMasterUtilState {
      int WAIT_FOR_NETWORK_TO_SETTLE;
      int TRY_TO_SETUP_MOBILE_CONNECTION;
    }
    int CELL_CONNECTION_RENEW_MS;
    int UPSTREAM_SETTLE_TIME_MS;
    int mUpstreamIfaceName;
    int mMobileApnReserved;
    int mCurrentConnectionSequence;
    int mNotifyList;
    int mSetDnsForwardersErrorState;
    int mStopTetheringErrorState;
    int mStartTetheringErrorState;
    int mSetIpForwardingDisabledErrorState;
    int mSetIpForwardingEnabledErrorState;
    int mTetherModeAliveState;
    int mInitialState;
    int mSequenceNumber;
    int CMD_RETRY_UPSTREAM;
    int CMD_CELL_CONNECTION_RENEW;
    int CMD_UPSTREAM_CHANGED;
    int CMD_TETHER_MODE_UNREQUESTED;
    int CMD_TETHER_MODE_REQUESTED;
  }
  class TetherInterfaceSM {
    class UnavailableState {
    }
    class TetheredState {
    }
    class StartingState {
    }
    class InitialState {
    }
    int mUsb;
    int mMyUpstreamIfaceName;
    int mIfaceName;
    int mLastError;
    int mTethered;
    int mAvailable;
    int mUnavailableState;
    int mTetheredState;
    int mStartingState;
    int mInitialState;
    int mDefaultState;
    int CMD_TETHER_CONNECTION_CHANGED;
    int CMD_SET_DNS_FORWARDERS_ERROR;
    int CMD_STOP_TETHERING_ERROR;
    int CMD_START_TETHERING_ERROR;
    int CMD_IP_FORWARDING_DISABLE_ERROR;
    int CMD_IP_FORWARDING_ENABLE_ERROR;
    int CMD_CELL_DUN_ERROR;
    int CMD_INTERFACE_UP;
    int CMD_INTERFACE_DOWN;
    int CMD_TETHER_UNREQUESTED;
    int CMD_TETHER_REQUESTED;
    int CMD_TETHER_MODE_DEAD;
  }
  class StateReceiver {
  }
  int mUsbTetherRequested;
  int mRndisEnabled;
  int mTetheredNotification;
  int mTetherMasterSM;
  int DNS_DEFAULT_SERVER2;
  int DNS_DEFAULT_SERVER1;
  int mDefaultDnsServers;
  int DHCP_DEFAULT_RANGE;
  int mDhcpRange;
  int USB_PREFIX_LENGTH;
  int USB_NEAR_IFACE_ADDR;
  int mStateReceiver;
  int mIfaces;
  int mThread;
  int mLooper;
  int mConnService;
  int mStatsService;
  int mNMService;
  int mPreferredUpstreamMobileApn;
  int DUN_TYPE;
  int HIPRI_TYPE;
  int MOBILE_TYPE;
  int mPublicSync;
  int mUpstreamIfaceTypes;
  int mTetherableBluetoothRegexs;
  int mTetherableWifiRegexs;
  int mTetherableUsbRegexs;
  int VDBG;
  int DBG;
  int TAG;
  int mContext;
}
