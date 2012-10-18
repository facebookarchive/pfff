package java.net;
class VMURLConnection {
  int LENGTH;
}
class VMNetworkInterface {
  int addresses;
  int name;
}
class VMInetAddress {
}
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
  int hex;
}
class URLDecoder {
}
class URLConnection {
  int position;
  int readTimeout;
  int connectTimeout;
  int dateformats_initialized;
  int dateFormats;
  int url;
  int ifModifiedSince;
  int useCaches;
  int doOutput;
  int doInput;
  int connected;
  int allowUserInteraction;
  int defaultFactory;
  int defaultUseCaches;
  int defaultAllowUserInteraction;
  int factory;
  int fileNameMap;
}
class URLClassLoader {
  int thisString;
  int securityContext;
  int factory;
  int urlinfos;
  int urls;
  int URL_LOADER_PREFIX;
  int factoryCache;
}
class URL {
  int cache_handlers;
  int ph_cache;
  int serialVersionUID;
  int factory;
  int ph;
  int hashCode;
  int ref;
  int file;
  int port;
  int userInfo;
  int host;
  int authority;
  int protocol;
  int systemClassLoader;
  int DEFAULT_SEARCH_PATH;
}
class URISyntaxException {
  int index;
  int input;
  int serialVersionUID;
}
class URI {
  int string;
  int fragment;
  int rawFragment;
  int query;
  int rawQuery;
  int path;
  int rawPath;
  int port;
  int host;
  int rawHost;
  int userInfo;
  int rawUserInfo;
  int authority;
  int rawAuthority;
  int schemeSpecificPart;
  int rawSchemeSpecificPart;
  int scheme;
  int HEX;
  int AUTHORITY_PATTERN;
  int URI_PATTERN;
  int AUTHORITY_PORT_GROUP;
  int AUTHORITY_HOST_GROUP;
  int AUTHORITY_USERINFO_GROUP;
  int FRAGMENT_GROUP;
  int QUERY_GROUP;
  int PATH_GROUP;
  int AUTHORITY_GROUP;
  int SCHEME_SPEC_PART_GROUP;
  int SCHEME_GROUP;
  int RFC3986_USERINFO;
  int RFC3986_HOST;
  int RFC3986_SSP;
  int RFC3986_PATH_SEGMENTS;
  int RFC3986_SEGMENT;
  int RFC3986_PCHAR;
  int RFC3986_REG_NAME;
  int RFC3986_SUBDELIMS;
  int RFC3986_UNRESERVED;
  int RFC2396_ALPHANUM;
  int RFC2396_ALPHA;
  int RFC2396_UPALPHA;
  int RFC2396_LOWALPHA;
  int RFC2396_DIGIT;
  int AUTHORITY_REGEXP;
  int URI_REGEXP;
  int serialVersionUID;
}
class SocketTimeoutException {
  int serialVersionUID;
}
class SocketPermission {
  int ACTIONS;
  int actionmask;
  int actions;
  int MAX_PORT;
  int MIN_PORT;
  int maxport;
  int minport;
  int address;
  int hostname;
  int serialVersionUID;
}
class SocketOptions {
  int IP_TOS;
  int IP_MULTICAST_LOOP;
  int IP_MULTICAST_IF2;
  int IP_MULTICAST_IF;
  int TCP_NODELAY;
  int SO_OOBINLINE;
  int SO_BROADCAST;
  int SO_REUSEADDR;
  int SO_RCVBUF;
  int SO_SNDBUF;
  int SO_BINDADDR;
  int SO_TIMEOUT;
  int SO_LINGER;
  int SO_KEEPALIVE;
}
class SocketImplFactory {
}
class SocketImpl {
  int port;
  int localport;
  int fd;
  int address;
}
class SocketException {
  int serialVersionUID;
}
class SocketAddress {
  int serialVersionUID;
}
class Socket {
  int outputShutdown;
  int inputShutdown;
  int bound;
  int implCreated;
  int impl;
  int factory;
}
class ServerSocket {
  int port;
  int local;
  int impl;
  int factory;
}
class ResolverCache {
  class Entry {
    int expires;
    int value;
    int key;
  }
  int killqueue;
  int cache;
  int NEGATIVE_TTL;
  int POSITIVE_TTL;
}
class ProxySelector {
  int defaultSelector;
}
class Proxy {
  int address;
  int type;
  int NO_PROXY;
  class Type {
    int SOCKS;
    int HTTP;
    int DIRECT;
    int serialVersionUID;
  }
}
class ProtocolException {
  int serialVersionUID;
}
class PortUnreachableException {
  int serialVersionUID;
}
class PasswordAuthentication {
  int password;
  int username;
}
class NoRouteToHostException {
  int serialVersionUID;
}
class NetworkInterface {
  int netif;
}
class NetPermission {
  int serialVersionUID;
}
class MulticastSocket {
}
class MimeTypeMapper {
  int mime_types;
  int mime_strings;
}
class MalformedURLException {
  int serialVersionUID;
}
class JarURLConnection {
  int entryName;
  int jarFileURLConnection;
  int jarFileURL;
}
class InetSocketAddress {
  int port;
  int addr;
  int hostname;
  int serialVersionUID;
}
class InetAddress {
  int family;
  int hostName;
  int addr;
  int address;
  int LOCALHOST;
  int ANY_IF;
  int serialVersionUID;
}
class Inet6Address {
  int AF_INET6;
  int nif;
  int ifname;
  int scope_ifname_set;
  int scope_id_set;
  int scope_id;
  int ipaddress;
  int serialVersionUID;
}
class Inet4Address {
  int AF_INET;
  int serialVersionUID;
}
class HttpURLConnection {
  int gotResponseVals;
  int instanceFollowRedirects;
  int responseMessage;
  int responseCode;
  int method;
  int valid_methods;
  int followRedirects;
  int HTTP_VERSION;
  int HTTP_GATEWAY_TIMEOUT;
  int HTTP_UNAVAILABLE;
  int HTTP_BAD_GATEWAY;
  int HTTP_NOT_IMPLEMENTED;
  int HTTP_INTERNAL_ERROR;
  int HTTP_SERVER_ERROR;
  int HTTP_UNSUPPORTED_TYPE;
  int HTTP_REQ_TOO_LONG;
  int HTTP_ENTITY_TOO_LARGE;
  int HTTP_PRECON_FAILED;
  int HTTP_LENGTH_REQUIRED;
  int HTTP_GONE;
  int HTTP_CONFLICT;
  int HTTP_CLIENT_TIMEOUT;
  int HTTP_PROXY_AUTH;
  int HTTP_NOT_ACCEPTABLE;
  int HTTP_BAD_METHOD;
  int HTTP_NOT_FOUND;
  int HTTP_FORBIDDEN;
  int HTTP_PAYMENT_REQUIRED;
  int HTTP_UNAUTHORIZED;
  int HTTP_BAD_REQUEST;
  int HTTP_USE_PROXY;
  int HTTP_NOT_MODIFIED;
  int HTTP_SEE_OTHER;
  int HTTP_MOVED_TEMP;
  int HTTP_MOVED_PERM;
  int HTTP_MULT_CHOICE;
  int HTTP_PARTIAL;
  int HTTP_RESET;
  int HTTP_NO_CONTENT;
  int HTTP_NOT_AUTHORITATIVE;
  int HTTP_ACCEPTED;
  int HTTP_CREATED;
  int HTTP_OK;
  int HTTP_CONTINUE;
}
class FileNameMap {
}
class DatagramSocketImplFactory {
}
class DatagramSocketImpl {
  int fd;
  int localPort;
}
class DatagramSocket {
  int bound;
  int remotePort;
  int remoteAddress;
  int implCreated;
  int impl;
  int factory;
}
class DatagramPacket {
  int port;
  int address;
  int maxlen;
  int length;
  int offset;
  int buffer;
}
class ContentHandlerFactory {
}
class ContentHandler {
}
class ConnectException {
  int serialVersionUID;
}
class BindException {
  int serialVersionUID;
}
class Authenticator {
  int scheme;
  int prompt;
  int protocol;
  int port;
  int addr;
  int host;
  int defaultAuthenticator;
}
