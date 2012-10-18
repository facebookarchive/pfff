package android.net;
class WebAddressTest {
}
class WebAddress {
  int sAddressPattern;
  int MATCH_GROUP_PATH;
  int MATCH_GROUP_PORT;
  int MATCH_GROUP_HOST;
  int MATCH_GROUP_AUTHORITY;
  int MATCH_GROUP_SCHEME;
  int mAuthInfo;
  int mPath;
  int mPort;
  int mHost;
  int mScheme;
}
class VpnService {
  class Builder {
    int mRoutes;
    int mAddresses;
    int mConfig;
  }
  class Callback {
  }
  int SERVICE_INTERFACE;
}
class UrlQuerySanitizer {
  int sAllButNulAndAngleBracketsLegal;
  int sSpaceLegal;
  int sAmpAndSpaceLegal;
  int sAmpLegal;
  int sUrlAndSpaceLegal;
  int sURLLegal;
  int sAllButWhitespaceLegal;
  int sAllButNulLegal;
  int sAllIllegal;
  class IllegalCharacterValueSanitizer {
    int MIN_SCRIPT_PREFIX_LENGTH;
    int VBSCRIPT_PREFIX;
    int JAVASCRIPT_PREFIX;
    int ALL_BUT_NUL_AND_ANGLE_BRACKETS_LEGAL;
    int SPACE_LEGAL;
    int AMP_AND_SPACE_LEGAL;
    int AMP_LEGAL;
    int URL_AND_SPACE_LEGAL;
    int URL_LEGAL;
    int ALL_BUT_WHITESPACE_LEGAL;
    int ALL_BUT_NUL_LEGAL;
    int ALL_ILLEGAL;
    int ALL_WHITESPACE_OK;
    int ALL_OK;
    int SCRIPT_URL_OK;
    int NUL_OK;
    int PCT_OK;
    int AMP_OK;
    int GT_OK;
    int LT_OK;
    int SQUOTE_OK;
    int DQUOTE_OK;
    int NON_7_BIT_ASCII_OK;
    int OTHER_WHITESPACE_OK;
    int SPACE_OK;
    int mFlags;
  }
  class ValueSanitizer {
  }
  int mUnregisteredParameterValueSanitizer;
  int mPreferFirstRepeatedParameter;
  int mAllowUnregisteredParamaters;
  int mEntriesList;
  int mEntries;
  int mSanitizers;
  class ParameterValuePair {
    int mValue;
    int mParameter;
  }
}
class UriTest {
}
class UriMatcherTest {
  int mURLMatcher;
  int FILTERRECENT;
  int CALLERID_TEXT;
  int CALLERID;
  int CALLS_ID;
  int CALLS;
  int PEOPLE_CONTACTMETH_ID;
  int PEOPLE_CONTACTMETH;
  int PEOPLE_ADDRESSES_ID;
  int PEOPLE_ADDRESSES;
  int PEOPLE_PHONES_ID;
  int PEOPLE_PHONES;
  int PEOPLE_ID;
  int PEOPLE;
  int ROOT;
}
class Uri {
  class PathPart {
    int pathSegments;
    int EMPTY;
    int NULL;
  }
  class Part {
    class EmptyPart {
    }
    int EMPTY;
    int NULL;
  }
  class AbstractPart {
    int decoded;
    int encoded;
    class Representation {
      int DECODED;
      int ENCODED;
      int BOTH;
    }
  }
  int HEX_DIGITS;
  int CREATOR;
  int NULL_TYPE_ID;
  class Builder {
    int fragment;
    int query;
    int path;
    int authority;
    int opaquePart;
    int scheme;
  }
  class HierarchicalUri {
    int uriString;
    int ssp;
    int fragment;
    int query;
    int path;
    int authority;
    int scheme;
    int TYPE_ID;
  }
  class AbstractHierarchicalUri {
    int port;
    int host;
    int userInfo;
  }
  class PathSegmentsBuilder {
    int size;
    int segments;
  }
  class PathSegments {
    int size;
    int segments;
    int EMPTY;
  }
  class OpaqueUri {
    int cachedString;
    int fragment;
    int ssp;
    int scheme;
    int TYPE_ID;
  }
  class StringUri {
    int fragment;
    int query;
    int path;
    int authority;
    int ssp;
    int scheme;
    int cachedFsi;
    int cachedSsi;
    int uriString;
    int TYPE_ID;
  }
  int DEFAULT_ENCODING;
  int NOT_HIERARCHICAL;
  int NOT_CALCULATED;
  int NOT_FOUND;
  int EMPTY;
  int NOT_CACHED;
  int LOG;
}
class TrafficStats {
  int TYPE_TX_PACKETS;
  int TYPE_TX_BYTES;
  int TYPE_RX_PACKETS;
  int TYPE_RX_BYTES;
  int sProfilingLock;
  int sActiveProfilingStart;
  int sStatsService;
  int TAG_SYSTEM_BACKUP;
  int TAG_SYSTEM_MEDIA;
  int TAG_SYSTEM_DOWNLOAD;
  int UID_TETHERING;
  int UID_REMOVED;
  int GB_IN_BYTES;
  int MB_IN_BYTES;
  int KB_IN_BYTES;
  int UNSUPPORTED;
}
class ThrottleManager {
  int mService;
  int PERIOD_SECOND;
  int PERIOD_60SEC;
  int PERIOD_MINUTE;
  int PERIOD_60MIN;
  int PERIOD_HOUR;
  int PERIOD_24HOUR;
  int PERIOD_DAY;
  int PERIOD_7DAY;
  int PERIOD_WEEK;
  int PERIOD_MONTH;
  int PERIOD_YEAR;
  int PERIOD_CYCLE;
  int DIRECTION_RX;
  int DIRECTION_TX;
  int POLICY_CHANGED_ACTION;
  int EXTRA_THROTTLE_LEVEL;
  int THROTTLE_ACTION;
  int EXTRA_CYCLE_END;
  int EXTRA_CYCLE_START;
  int EXTRA_CYCLE_WRITE;
  int EXTRA_CYCLE_READ;
  int THROTTLE_POLL_ACTION;
}
class SntpClient {
  int mRoundTripTime;
  int mNtpTimeReference;
  int mNtpTime;
  int OFFSET_1900_TO_1970;
  int NTP_VERSION;
  int NTP_MODE_CLIENT;
  int NTP_PORT;
  int NTP_PACKET_SIZE;
  int TRANSMIT_TIME_OFFSET;
  int RECEIVE_TIME_OFFSET;
  int ORIGINATE_TIME_OFFSET;
  int REFERENCE_TIME_OFFSET;
  int TAG;
}
class SSLTest {
}
class SSLSessionCache {
  int mSessionCache;
  int TAG;
}
class SSLCertificateSocketFactory {
  int mSecure;
  int mSessionCache;
  int mHandshakeTimeoutMillis;
  int mNpnProtocols;
  int mKeyManagers;
  int mTrustManagers;
  int mSecureFactory;
  int mInsecureFactory;
  int HOSTNAME_VERIFIER;
  int INSECURE_TRUST_MANAGER;
  int TAG;
}
class RouteInfo {
  int CREATOR;
  int mIsHost;
  int mIsDefault;
  int mGateway;
  int mDestination;
}
class ProxyProperties {
  int CREATOR;
  int mParsedExclusionList;
  int mExclusionList;
  int mPort;
  int mHost;
}
class Proxy {
  class AndroidProxySelectorRoutePlanner {
    int mContext;
  }
  int EXCLLIST_PATTERN;
  int EXCLLIST_REGEXP;
  int HOSTNAME_PATTERN;
  int HOSTNAME_REGEXP;
  int NAME_IP_REGEX;
  int sConnectivityManager;
  int EXTRA_PROXY_INFO;
  int PROXY_CHANGE_ACTION;
  int TAG;
  int DEBUG;
}
class ParseException {
  int response;
}
class NetworkUtils {
  int RESET_ALL_ADDRESSES;
  int RESET_IPV6_ADDRESSES;
  int RESET_IPV4_ADDRESSES;
  int TAG;
}
class NetworkTemplate {
  int CREATOR;
  int mNetworkId;
  int mSubscriberId;
  int mMatchRule;
  int sForceAllNetworkTypes;
  int DATA_USAGE_NETWORK_TYPES;
  int MATCH_WIFI_WILDCARD;
  int MATCH_MOBILE_WILDCARD;
  int MATCH_ETHERNET;
  int MATCH_WIFI;
  int MATCH_MOBILE_4G;
  int MATCH_MOBILE_3G_LOWER;
  int MATCH_MOBILE_ALL;
}
class NetworkStatsTest {
  int TEST_START;
  int TEST_UID;
  int TEST_IFACE2;
  int TEST_IFACE;
}
class NetworkStatsHistoryTest {
  int stats;
  int TEST_START;
  int TAG;
}
class NetworkStatsHistory {
  class ParcelUtils {
  }
  class DataStreamUtils {
  }
  int CREATOR;
  class Entry {
    int operations;
    int txPackets;
    int txBytes;
    int rxPackets;
    int rxBytes;
    int activeTime;
    int bucketStart;
    int bucketDuration;
    int UNKNOWN;
  }
  int totalBytes;
  int operations;
  int txPackets;
  int txBytes;
  int rxPackets;
  int rxBytes;
  int activeTime;
  int bucketStart;
  int bucketCount;
  int bucketDuration;
  int FIELD_ALL;
  int FIELD_OPERATIONS;
  int FIELD_TX_PACKETS;
  int FIELD_TX_BYTES;
  int FIELD_RX_PACKETS;
  int FIELD_RX_BYTES;
  int FIELD_ACTIVE_TIME;
  int VERSION_ADD_ACTIVE;
  int VERSION_ADD_PACKETS;
  int VERSION_INIT;
}
class NetworkStats {
  class NonMonotonicObserver {
  }
  int CREATOR;
  class Entry {
    int operations;
    int txPackets;
    int txBytes;
    int rxPackets;
    int rxBytes;
    int tag;
    int set;
    int uid;
    int iface;
  }
  int operations;
  int txPackets;
  int txBytes;
  int rxPackets;
  int rxBytes;
  int tag;
  int set;
  int uid;
  int iface;
  int size;
  int elapsedRealtime;
  int TAG_NONE;
  int SET_FOREGROUND;
  int SET_DEFAULT;
  int SET_ALL;
  int UID_ALL;
  int IFACE_ALL;
}
class NetworkStateTracker {
  int EVENT_RESTORE_DEFAULT_NETWORK;
  int EVENT_CONFIGURATION_CHANGED;
  int EVENT_STATE_CHANGED;
  int MAX_NETWORK_STATE_TRACKER_EVENT;
  int MIN_NETWORK_STATE_TRACKER_EVENT;
}
class NetworkState {
  int CREATOR;
  int networkId;
  int subscriberId;
  int linkCapabilities;
  int linkProperties;
  int networkInfo;
}
class NetworkQuotaInfo {
  int CREATOR;
  int NO_LIMIT;
  int mHardLimitBytes;
  int mSoftLimitBytes;
  int mEstimatedBytes;
}
class NetworkPolicyManager {
  int mService;
  int EXTRA_NETWORK_TEMPLATE;
  int ALLOW_PLATFORM_APP_POLICY;
  int RULE_REJECT_METERED;
  int RULE_ALLOW_ALL;
  int POLICY_REJECT_METERED_BACKGROUND;
  int POLICY_NONE;
}
class NetworkPolicy {
  int CREATOR;
  int DEFAULT_MTU;
  int inferred;
  int metered;
  int lastLimitSnooze;
  int lastWarningSnooze;
  int limitBytes;
  int warningBytes;
  int cycleTimezone;
  int cycleDay;
  int template;
  int SNOOZE_NEVER;
  int LIMIT_DISABLED;
  int WARNING_DISABLED;
  int CYCLE_NONE;
}
class NetworkInfo {
  int CREATOR;
  int mIsAvailable;
  int mIsRoaming;
  int mIsFailover;
  int mExtraInfo;
  int mReason;
  int mDetailedState;
  int mState;
  int mSubtypeName;
  int mTypeName;
  int mSubtype;
  int mNetworkType;
  int stateMap;
  class DetailedState {
    int VERIFYING_POOR_LINK;
    int BLOCKED;
    int FAILED;
    int DISCONNECTED;
    int DISCONNECTING;
    int SUSPENDED;
    int CONNECTED;
    int OBTAINING_IPADDR;
    int AUTHENTICATING;
    int CONNECTING;
    int SCANNING;
    int IDLE;
  }
  class State {
    int UNKNOWN;
    int DISCONNECTED;
    int DISCONNECTING;
    int SUSPENDED;
    int CONNECTED;
    int CONNECTING;
  }
}
class NetworkIdentity {
  int mRoaming;
  int mNetworkId;
  int mSubscriberId;
  int mSubType;
  int mType;
  int SUBTYPE_COMBINED;
  int COMBINE_SUBTYPE_ENABLED;
}
class NetworkConfig {
  int restoreTime;
  int dependencyMet;
  int priority;
  int radio;
  int type;
  int name;
}
class MobileDataStateTracker {
  class MobileDataStateReceiver {
  }
  class MdstHandler {
    int mMdst;
  }
  int mMessenger;
  int mDataConnectionTrackerAc;
  int mHandler;
  int mPolicyDataEnabled;
  int mUserDataEnabled;
  int mDefaultRouteSet;
  int mPrivateDnsRouteSet;
  int mLinkCapabilities;
  int mLinkProperties;
  int mContext;
  int mTarget;
  int mTeardownRequested;
  int mNetworkInfo;
  int mApnType;
  int mPhoneService;
  int mMobileDataState;
  int VDBG;
  int DBG;
  int TAG;
}
class MailTo {
  int SUBJECT;
  int CC;
  int BODY;
  int TO;
  int mHeaders;
  int MAILTO_SCHEME;
}
class LocalSocketTest {
}
class LocalSocketImpl {
  class SocketOutputStream {
  }
  class SocketInputStream {
  }
  int outboundFileDescriptors;
  int inboundFileDescriptors;
  int fd;
  int writeMonitor;
  int readMonitor;
  int fos;
  int fis;
}
class LocalSocketAddress {
  int namespace;
  int name;
  class Namespace {
    int FILESYSTEM;
    int RESERVED;
    int ABSTRACT;
    int id;
  }
}
class LocalSocket {
  int isConnected;
  int isBound;
  int localAddress;
  int implCreated;
  int impl;
}
class LocalServerSocket {
  int LISTEN_BACKLOG;
  int localAddress;
  int impl;
}
class LinkSocketTest {
}
class LinkSocketNotifier {
}
class LinkSocket {
  class LinkRequestReason {
    int LINK_PROBLEM_UNKNOWN;
    int LINK_PROBLEM_NONE;
  }
  int DBG;
  int TAG;
}
class LinkPropertiesTest {
  int NAME;
  int GATEWAY2;
  int GATEWAY1;
  int DNS2;
  int DNS1;
  int ADDRV6;
  int ADDRV4;
}
class LinkProperties {
  int CREATOR;
  class CompareResult {
    int added;
    int removed;
  }
  int mHttpProxy;
  int mRoutes;
  int mDnses;
  int mLinkAddresses;
  int mIfaceName;
}
class LinkCapabilities {
  int CREATOR;
  class Role {
    int VIDEO_CHAT_480P;
    int VIDEO_CHAT_360P;
    int VIDEO_STREAMING_720I;
    int VIDEO_STREAMING_480P;
    int VOIP_32KBPS;
    int VOIP_24KBPS;
    int BULK_UPLOAD;
    int BULK_DOWNLOAD;
    int DEFAULT;
  }
  class Key {
    int RO_PHYSICAL_INTERFACE;
    int RO_BOUND_INTERFACE;
    int RW_MAX_ALLOWED_LATENCY;
    int RO_AVAILABLE_REV_BW;
    int RW_REQUIRED_REV_BW;
    int RW_DESIRED_REV_BW;
    int RO_AVAILABLE_FWD_BW;
    int RW_REQUIRED_FWD_BW;
    int RW_DESIRED_FWD_BW;
    int RO_NETWORK_TYPE;
  }
  int mCapabilities;
  int DBG;
  int TAG;
}
class LinkAddress {
  int CREATOR;
  int prefixLength;
  int address;
}
class InterfaceConfiguration {
  int CREATOR;
  int FLAG_DOWN;
  int FLAG_UP;
  int mFlags;
  int mAddr;
  int mHwAddr;
}
class EthernetDataTracker {
  class InterfaceObserver {
    int mTracker;
  }
  int mNMService;
  int mIface;
  int sIfaceMatch;
  int sInstance;
  int mContext;
  int mCsHandler;
  int mHwAddr;
  int mInterfaceObserver;
  int mNetworkInfo;
  int mLinkCapabilities;
  int mLinkProperties;
  int mLinkUp;
  int mDefaultRouteSet;
  int mDefaultGatewayAddr;
  int mPrivateDnsRouteSet;
  int mTeardownRequested;
  int TAG;
  int NETWORKTYPE;
}
class DummyDataStateTracker {
  int mIsDefaultOrHipri;
  int mDefaultRouteSet;
  int mPrivateDnsRouteSet;
  int mLinkCapabilities;
  int mLinkProperties;
  int mContext;
  int mTarget;
  int mTeardownRequested;
  int mNetworkInfo;
  int VDBG;
  int DBG;
  int TAG;
}
class DownloadManagerHostTests {
  int mExtraParams;
  int externalDownloadUriValue;
  int EXTERNAL_DOWNLOAD_URI_KEY;
  int DOWNLOAD_TEST_RUNNER_NAME;
  int FILE_DOWNLOAD_CLASS;
  int FILE_DOWNLOAD_PKG;
  int FILE_DOWNLOAD_APK;
  int LOG_TAG;
  int mPMUtils;
}
class DnsPinger {
  int mDnsQuery;
  class DnsArg {
    int seq;
    int dns;
  }
  class ActivePing {
    int start;
    int result;
    int timeout;
    int packetId;
    int internalId;
    int socket;
  }
  int mEventCounter;
  int mActivePings;
  int ACTION_CANCEL_ALL_PINGS;
  int ACTION_LISTEN_FOR_RESPONSE;
  int ACTION_PING_DNS;
  int SOCKET_EXCEPTION;
  int TIMEOUT;
  int DNS_PING_RESULT;
  int BASE;
  int mCurrentToken;
  int TAG;
  int mDefaultDns;
  int mTarget;
  int mConnectionType;
  int mContext;
  int mConnectivityManager;
  int sCounter;
  int sRandom;
  int SOCKET_TIMEOUT_MS;
  int DNS_PORT;
  int RECEIVE_POLL_INTERVAL_MS;
  int DBG;
}
class DhcpStateMachine {
  class WaitBeforeRenewalState {
  }
  class RunningState {
  }
  class WaitBeforeStartState {
  }
  class StoppedState {
  }
  class DefaultState {
  }
  int mWaitBeforeRenewalState;
  int mRunningState;
  int mWaitBeforeStartState;
  int mStoppedState;
  int mDefaultState;
  int DHCP_FAILURE;
  int DHCP_SUCCESS;
  int CMD_PRE_DHCP_ACTION_COMPLETE;
  int CMD_POST_DHCP_ACTION;
  int CMD_PRE_DHCP_ACTION;
  int CMD_RENEW_DHCP;
  int CMD_STOP_DHCP;
  int CMD_START_DHCP;
  int BASE;
  int mRegisteredForPreDhcpNotification;
  int mInterfaceName;
  class DhcpAction {
    int RENEW;
    int START;
  }
  int MIN_RENEWAL_TIME_SECS;
  int ACTION_DHCP_RENEW;
  int DHCP_RENEW;
  int mDhcpInfo;
  int WAKELOCK_TAG;
  int mDhcpRenewWakeLock;
  int mDhcpRenewalIntent;
  int mAlarmManager;
  int mBroadcastReceiver;
  int mContext;
  int mController;
  int DBG;
  int TAG;
}
class DhcpInfoInternal {
  int mRoutes;
  int vendorInfo;
  int leaseDuration;
  int serverAddress;
  int dns2;
  int dns1;
  int prefixLength;
  int ipAddress;
  int TAG;
}
class DhcpInfo {
  int CREATOR;
  int leaseDuration;
  int serverAddress;
  int dns2;
  int dns1;
  int netmask;
  int gateway;
  int ipAddress;
}
class Credentials {
  int gid;
  int uid;
  int pid;
}
class ConnectivityManager {
  int TETHER_ERROR_IFACE_CFG_ERROR;
  int TETHER_ERROR_DISABLE_NAT_ERROR;
  int TETHER_ERROR_ENABLE_NAT_ERROR;
  int TETHER_ERROR_UNTETHER_IFACE_ERROR;
  int TETHER_ERROR_TETHER_IFACE_ERROR;
  int TETHER_ERROR_MASTER_ERROR;
  int TETHER_ERROR_UNAVAIL_IFACE;
  int TETHER_ERROR_UNSUPPORTED;
  int TETHER_ERROR_SERVICE_UNAVAIL;
  int TETHER_ERROR_UNKNOWN_IFACE;
  int TETHER_ERROR_NO_ERROR;
  int mService;
  int DEFAULT_NETWORK_PREFERENCE;
  int MAX_NETWORK_TYPE;
  int MAX_RADIO_TYPE;
  int TYPE_WIFI_P2P;
  int TYPE_MOBILE_CBS;
  int TYPE_MOBILE_IMS;
  int TYPE_MOBILE_FOTA;
  int TYPE_ETHERNET;
  int TYPE_DUMMY;
  int TYPE_BLUETOOTH;
  int TYPE_WIMAX;
  int TYPE_MOBILE_HIPRI;
  int TYPE_MOBILE_DUN;
  int TYPE_MOBILE_SUPL;
  int TYPE_MOBILE_MMS;
  int TYPE_WIFI;
  int TYPE_MOBILE;
  int TYPE_NONE;
  int EXTRA_ERRORED_TETHER;
  int EXTRA_ACTIVE_TETHER;
  int EXTRA_AVAILABLE_TETHER;
  int ACTION_TETHER_STATE_CHANGED;
  int INET_CONDITION_ACTION;
  int ACTION_BACKGROUND_DATA_SETTING_CHANGED;
  int EXTRA_INET_CONDITION;
  int EXTRA_EXTRA_INFO;
  int EXTRA_REASON;
  int EXTRA_NO_CONNECTIVITY;
  int EXTRA_OTHER_NETWORK_INFO;
  int EXTRA_IS_FAILOVER;
  int EXTRA_NETWORK_INFO;
  int CONNECTIVITY_ACTION_IMMEDIATE;
  int CONNECTIVITY_ACTION;
  int TAG;
}
