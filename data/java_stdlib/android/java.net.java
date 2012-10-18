package java.net;
class UnknownServiceException {
  int serialVersionUID;
}
class UnknownHostException {
  int serialVersionUID;
}
class URLStreamHandlerFactory {
}
class URLStreamHandler {
}
class URLEncoder {
  int ENCODER;
}
class URLDecoder {
}
class URLConnection {
  class DefaultContentHandler {
  }
  int fileNameMap;
  int contentHandlers;
  int connectTimeout;
  int readTimeout;
  int contentHandlerFactory;
  int allowUserInteraction;
  int doInput;
  int doOutput;
  int connected;
  int useCaches;
  int ifModifiedSince;
  int lastModified;
  int defaultHandler;
  int defaultUseCaches;
  int defaultAllowUserInteraction;
  int contentType;
  int url;
}
class URLClassLoader {
  class URLFileHandler {
    int prefix;
  }
  class URLJarHandler {
    int subHandlers;
    int index;
    int prefixName;
    int jf;
  }
  class URLHandler {
    int codeSourceUrl;
    int url;
  }
  class IndexFile {
    int map;
  }
  int factory;
  int handlerMap;
  int handlerList;
  int searchList;
  int originalUrls;
}
class URL {
  int hashCode;
  int streamHandler;
  int query;
  int path;
  int userInfo;
  int ref;
  int file;
  int port;
  int host;
  int authority;
  int protocol;
  int streamHandlers;
  int streamHandlerFactory;
  int serialVersionUID;
}
class URISyntaxException {
  int index;
  int input;
  int serialVersionUID;
}
class URI {
  int hash;
  int serverAuthority;
  int absolute;
  int opaque;
  int fragment;
  int query;
  int path;
  int port;
  int host;
  int userInfo;
  int authority;
  int schemeSpecificPart;
  int scheme;
  int string;
  class PartEncoder {
    int extraLegalCharacters;
  }
  int ASCII_ONLY;
  int ALL_LEGAL_ENCODER;
  int FILE_AND_QUERY_ENCODER;
  int AUTHORITY_ENCODER;
  int PATH_ENCODER;
  int USER_INFO_ENCODER;
  int PUNCTUATION;
  int UNRESERVED;
  int serialVersionUID;
}
class Socks4Message {
  int buffer;
  int MAX_USER_ID_LENGTH;
  int BUFFER_LENGTH;
  int INDEX_USER_ID;
  int INDEX_IP;
  int INDEX_PORT;
  int INDEX_COMMAND;
  int SOCKS_VERSION;
  int INDEX_VERSION;
  int REPLY_LENGTH;
  int RETURN_DIFFERENT_USER_IDS;
  int RETURN_CANNOT_CONNECT_TO_IDENTD;
  int RETURN_FAILURE;
  int RETURN_SUCCESS;
  int COMMAND_BIND;
  int COMMAND_CONNECT;
}
class SocketUtils {
}
class SocketTimeoutException {
  int serialVersionUID;
}
class SocketPermission {
}
class SocketOptions {
  int IP_MULTICAST_IF2;
  int SO_OOBINLINE;
  int SO_BROADCAST;
  int IP_MULTICAST_LOOP;
  int IP_TOS;
  int SO_KEEPALIVE;
  int SO_RCVBUF;
  int SO_SNDBUF;
  int SO_REUSEADDR;
  int SO_BINDADDR;
  int IP_MULTICAST_IF;
  int TCP_NODELAY;
  int SO_TIMEOUT;
  int SO_LINGER;
}
class SocketImplFactory {
}
class SocketImpl {
  int localport;
  int fd;
  int port;
  int address;
}
class SocketException {
  int serialVersionUID;
}
class SocketAddress {
  int serialVersionUID;
}
class Socket {
  int connectLock;
  int localAddress;
  int isOutputShutdown;
  int isInputShutdown;
  int isClosed;
  int isConnected;
  int isBound;
  int isCreated;
  int proxy;
  int impl;
  int factory;
}
class ServerSocket {
  int isClosed;
  int isBound;
  int factory;
  int impl;
  int DEFAULT_BACKLOG;
}
class SecureCacheResponse {
}
class ResponseSource {
  int NETWORK;
  int CONDITIONAL_CACHE;
  int CACHE;
}
class ResponseCache {
  int defaultResponseCache;
}
class ProxySelectorImpl {
}
class ProxySelector {
  int defaultSelector;
}
class Proxy {
  class Type {
    int SOCKS;
    int HTTP;
    int DIRECT;
  }
  int address;
  int type;
  int NO_PROXY;
}
class ProtocolException {
  int serialVersionUID;
}
class PortUnreachableException {
  int serialVersionUID;
}
class PlainSocketImpl {
  class PlainSocketOutputStream {
    int socketImpl;
  }
  class PlainSocketInputStream {
    int socketImpl;
  }
  int guard;
  int proxy;
  int shutdownInput;
  int streaming;
  int lastConnectedPort;
  int lastConnectedAddress;
}
class PlainServerSocketImpl {
}
class PlainDatagramSocketImpl {
  int connectedPort;
  int connectedAddress;
  int guard;
  int isNativeConnected;
}
class PasswordAuthentication {
  int password;
  int userName;
}
class NoRouteToHostException {
  int serialVersionUID;
}
class NetworkInterface {
  int parent;
  int children;
  int addresses;
  int interfaceAddresses;
  int interfaceIndex;
  int name;
}
class NetPermission {
}
class MulticastSocket {
  int setAddress;
}
class MalformedURLException {
  int serialVersionUID;
}
class JarURLConnection {
  int file;
  int fileURL;
  int entryName;
  int jarFileURLConnection;
}
class InterfaceAddress {
  int prefixLength;
  int broadcastAddress;
  int address;
}
class InetSocketAddress {
  int port;
  int hostname;
  int addr;
  int serialVersionUID;
}
class InetAddress {
  int serialPersistentFields;
  int UNSPECIFIED;
  int hostName;
  int ipaddress;
  int family;
  int serialVersionUID;
  int addressCache;
}
class Inet6Address {
  int serialPersistentFields;
  int ifname;
  int scope_ifname_set;
  int scope_id;
  int scope_id_set;
  int LOOPBACK;
  int ANY;
  int serialVersionUID;
}
class Inet4Address {
  int LOOPBACK;
  int ALL;
  int ANY;
  int serialVersionUID;
}
class IDN {
  int USE_STD3_ASCII_RULES;
  int ALLOW_UNASSIGNED;
}
class HttpURLConnection {
  int HTTP_VERSION;
  int HTTP_UNAVAILABLE;
  int HTTP_UNSUPPORTED_TYPE;
  int HTTP_UNAUTHORIZED;
  int HTTP_USE_PROXY;
  int HTTP_SERVER_ERROR;
  int HTTP_SEE_OTHER;
  int HTTP_RESET;
  int HTTP_REQ_TOO_LONG;
  int HTTP_PROXY_AUTH;
  int HTTP_PRECON_FAILED;
  int HTTP_PAYMENT_REQUIRED;
  int HTTP_PARTIAL;
  int HTTP_OK;
  int HTTP_NOT_MODIFIED;
  int HTTP_NOT_IMPLEMENTED;
  int HTTP_NOT_FOUND;
  int HTTP_NOT_AUTHORITATIVE;
  int HTTP_NOT_ACCEPTABLE;
  int HTTP_NO_CONTENT;
  int HTTP_MULT_CHOICE;
  int HTTP_MOVED_TEMP;
  int HTTP_MOVED_PERM;
  int HTTP_LENGTH_REQUIRED;
  int HTTP_INTERNAL_ERROR;
  int HTTP_GONE;
  int HTTP_GATEWAY_TIMEOUT;
  int HTTP_FORBIDDEN;
  int HTTP_ENTITY_TOO_LARGE;
  int HTTP_CREATED;
  int HTTP_CONFLICT;
  int HTTP_CLIENT_TIMEOUT;
  int HTTP_BAD_REQUEST;
  int HTTP_BAD_METHOD;
  int HTTP_BAD_GATEWAY;
  int HTTP_ACCEPTED;
  int fixedContentLength;
  int chunkLength;
  int followRedirects;
  int instanceFollowRedirects;
  int responseMessage;
  int responseCode;
  int method;
  int PERMITTED_USER_METHODS;
}
class HttpRetryException {
  int location;
  int responseCode;
  int serialVersionUID;
}
class HttpCookie {
  int version;
  int value;
  int secure;
  int portList;
  int path;
  int name;
  int maxAge;
  int domain;
  int discard;
  int commentURL;
  int comment;
  class CookieParser {
    int hasVersion;
    int hasMaxAge;
    int hasExpires;
    int pos;
    int inputLowerCase;
    int input;
    int WHITESPACE;
    int ATTRIBUTE_NAME_TERMINATORS;
  }
  int RESERVED_NAMES;
}
class FileNameMap {
}
class ExtendedResponseCache {
}
class DefaultFileNameMap {
}
class DatagramSocketImplFactory {
}
class DatagramSocketImpl {
  int localPort;
  int fd;
}
class DatagramSocket {
  int lock;
  int isClosed;
  int pendingConnectException;
  int isConnected;
  int isBound;
  int factory;
  int port;
  int address;
  int impl;
}
class DatagramPacket {
  int offset;
  int port;
  int address;
  int userSuppliedLength;
  int length;
  int data;
}
class CookieStoreImpl {
  int map;
}
class CookieStore {
}
class CookiePolicy {
  int ACCEPT_ORIGINAL_SERVER;
  int ACCEPT_NONE;
  int ACCEPT_ALL;
}
class CookieManager {
  int VERSION_ONE_HEADER;
  int VERSION_ZERO_HEADER;
  int policy;
  int store;
}
class CookieHandler {
  int systemWideCookieHandler;
}
class ContentHandlerFactory {
}
class ContentHandler {
}
class ConnectException {
  int serialVersionUID;
}
class CacheResponse {
}
class CacheRequest {
}
class BindException {
  int serialVersionUID;
}
class Authenticator {
  class RequestorType {
    int SERVER;
    int PROXY;
  }
  int rt;
  int url;
  int scheme;
  int prompt;
  int protocol;
  int port;
  int addr;
  int host;
  int thisAuthenticator;
}
class AddressCache {
  class AddressCacheEntry {
    int expiryNanos;
    int value;
  }
  int cache;
  int TTL_NANOS;
  int MAX_ENTRIES;
}
