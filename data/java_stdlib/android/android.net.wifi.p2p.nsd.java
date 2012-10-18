package android.net.wifi.p2p.nsd;
class WifiP2pUpnpServiceResponse {
  int mUniqueServiceNames;
  int mVersion;
}
class WifiP2pUpnpServiceRequest {
}
class WifiP2pUpnpServiceInfo {
  int VERSION_1_0;
}
class WifiP2pServiceResponse {
  int CREATOR;
  class Status {
    int BAD_REQUEST;
    int REQUESTED_INFORMATION_NOT_AVAILABLE;
    int SERVICE_PROTOCOL_NOT_AVAILABLE;
    int SUCCESS;
  }
  int mData;
  int mDevice;
  int mTransId;
  int mStatus;
  int mServiceType;
  int MAX_BUF_SIZE;
}
class WifiP2pServiceRequest {
  int CREATOR;
  int mQuery;
  int mTransId;
  int mLength;
  int mProtocolType;
}
class WifiP2pServiceInfo {
  int CREATOR;
  int mQueryList;
  int SERVICE_TYPE_VENDOR_SPECIFIC;
  int SERVICE_TYPE_WS_DISCOVERY;
  int SERVICE_TYPE_UPNP;
  int SERVICE_TYPE_BONJOUR;
  int SERVICE_TYPE_ALL;
}
class WifiP2pDnsSdServiceResponse {
  int sVmpack;
  int mTxtRecord;
  int mVersion;
  int mDnsType;
  int mInstanceName;
  int mDnsQueryName;
}
class WifiP2pDnsSdServiceRequest {
}
class WifiP2pDnsSdServiceInfo {
  int sVmPacket;
  int DNS_TYPE_TXT;
  int DNS_TYPE_PTR;
  int VERSION_1;
}
