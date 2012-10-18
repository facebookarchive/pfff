package android.net.dhcp;
class DhcpStateMachine {
}
class DhcpRequestPacket {
}
class DhcpPacket {
  int mClientMac;
  int mBroadcast;
  int mRelayIp;
  int mNextIp;
  int mYourIp;
  int mClientIp;
  int mTransId;
  int DHCP_CLIENT_IDENTIFIER;
  int DHCP_VENDOR_CLASS_ID;
  int DHCP_RENEWAL_TIME;
  int mMessage;
  int DHCP_MESSAGE;
  int mRequestedParams;
  int DHCP_PARAMETER_LIST;
  int mServerIdentifier;
  int DHCP_SERVER_IDENTIFIER;
  int DHCP_MESSAGE_TYPE_INFORM;
  int DHCP_MESSAGE_TYPE_NAK;
  int DHCP_MESSAGE_TYPE_ACK;
  int DHCP_MESSAGE_TYPE_DECLINE;
  int DHCP_MESSAGE_TYPE_REQUEST;
  int DHCP_MESSAGE_TYPE_OFFER;
  int DHCP_MESSAGE_TYPE_DISCOVER;
  int DHCP_MESSAGE_TYPE;
  int mLeaseTime;
  int DHCP_LEASE_TIME;
  int mRequestedIp;
  int DHCP_REQUESTED_IP;
  int mBroadcastAddress;
  int DHCP_BROADCAST_ADDRESS;
  int mDomainName;
  int DHCP_DOMAIN_NAME;
  int mHostName;
  int DHCP_HOST_NAME;
  int mDnsServers;
  int DHCP_DNS_SERVER;
  int mGateway;
  int DHCP_ROUTER;
  int mSubnetMask;
  int DHCP_SUBNET_MASK;
  int MAX_LENGTH;
  int CLIENT_ID_ETHER;
  int DHCP_BOOTREPLY;
  int DHCP_BOOTREQUEST;
  int DHCP_SERVER;
  int DHCP_CLIENT;
  int IP_TTL;
  int IP_TOS_LOWDELAY;
  int IP_FLAGS_OFFSET;
  int IP_VERSION_HEADER_LEN;
  int IP_TYPE_UDP;
  int ENCAP_BOOTP;
  int ENCAP_L3;
  int ENCAP_L2;
  int TAG;
}
class DhcpOfferPacket {
  int mSrcIp;
}
class DhcpNakPacket {
}
class DhcpInformPacket {
}
class DhcpDiscoverPacket {
}
class DhcpDeclinePacket {
}
class DhcpAckPacket {
  int mSrcIp;
}
