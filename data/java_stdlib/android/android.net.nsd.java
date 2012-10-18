package android.net.nsd;
class NsdServiceInfo {
  int CREATOR;
  int mPort;
  int mHost;
  int mTxtRecord;
  int mServiceType;
  int mServiceName;
}
class NsdManager {
  class ServiceHandler {
  }
  class ResolveListener {
  }
  class RegistrationListener {
  }
  class DiscoveryListener {
  }
  int FAILURE_MAX_LIMIT;
  int FAILURE_ALREADY_ACTIVE;
  int FAILURE_INTERNAL_ERROR;
  int mConnected;
  int mHandler;
  int mAsyncChannel;
  int mMapLock;
  int mServiceMap;
  int mListenerMap;
  int mListenerKey;
  int INVALID_LISTENER_KEY;
  int mContext;
  int PROTOCOL_DNS_SD;
  int NATIVE_DAEMON_EVENT;
  int DISABLE;
  int ENABLE;
  int RESOLVE_SERVICE_SUCCEEDED;
  int RESOLVE_SERVICE_FAILED;
  int RESOLVE_SERVICE;
  int UNREGISTER_SERVICE_SUCCEEDED;
  int UNREGISTER_SERVICE_FAILED;
  int UNREGISTER_SERVICE;
  int REGISTER_SERVICE_SUCCEEDED;
  int REGISTER_SERVICE_FAILED;
  int REGISTER_SERVICE;
  int STOP_DISCOVERY_SUCCEEDED;
  int STOP_DISCOVERY_FAILED;
  int STOP_DISCOVERY;
  int SERVICE_LOST;
  int SERVICE_FOUND;
  int DISCOVER_SERVICES_FAILED;
  int DISCOVER_SERVICES_STARTED;
  int DISCOVER_SERVICES;
  int BASE;
  int NSD_STATE_ENABLED;
  int NSD_STATE_DISABLED;
  int EXTRA_NSD_STATE;
  int ACTION_NSD_STATE_CHANGED;
  int mService;
  int TAG;
}
class DnsSdTxtRecord {
  int CREATOR;
  int mData;
  int mSeperator;
}
