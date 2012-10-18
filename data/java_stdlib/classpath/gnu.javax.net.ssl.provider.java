package gnu.javax.net.ssl.provider;
class X509TrustManagerFactory {
  class Manager {
    int anchors;
  }
  int current;
  int CA_CERTS;
  int JSSE_CERTS;
  int sep;
}
class X509KeyManagerFactory {
  class Manager {
    int certChains;
    int privateKeys;
  }
  int current;
}
class X500PrincipalList {
  class Iterator {
    int count;
    int index;
    int modCount;
  }
  int modCount;
  int buffer;
}
class Util {
  class WrappedBuffer {
    int prefix;
    int buffer;
  }
  int HEX;
}
class UnresolvedExtensionValue {
  int buffer;
}
class TrustedAuthorities {
  class IdentifierType {
    int CERT_SHA1_HASH;
    int X509_NAME;
    int KEY_SHA1_HASH;
    int PRE_AGREED;
    int value;
  }
  class TrustedAuthority {
    int buffer;
  }
  class AuthoritiesIterator {
    int index;
  }
  int buffer;
}
class TruncatedHMAC {
}
class TLSRandom {
  int init;
  int idx;
  int buffer;
  int seed;
  int md5_a;
  int sha_a;
  int hmac_md5;
  int hmac_sha;
  int SEED;
  int SECRET;
}
class TLSHMac {
  int OPAD_BYTE;
  int IPAD_BYTE;
}
class SimpleSessionContext {
  int storeLimit;
  int store;
  int DEFAULT_TIMEOUT;
}
class SignatureAlgorithm {
  int DSA;
  int RSA;
  int ANONYMOUS;
}
class Signature {
  int alg;
  int buffer;
}
class SessionImpl {
  class PrivateData {
    int masterSecret;
    int serialVersionUID;
  }
  int privateData;
  int maxLength;
  int sealedPrivateData;
  int privateDataSalt;
  int version;
  int suite;
  int serialVersionUID;
}
class ServerRSA_PSKParameters {
}
class ServerRSAParams {
  int buffer;
}
class ServerPSKParameters {
  int buffer;
}
class ServerNameList {
  class NameType {
    int HOST_NAME;
    int value;
  }
  class ServerName {
    int buffer;
  }
  class Iterator {
    int index;
  }
  int buffer;
}
class ServerKeyExchangeParams {
}
class ServerKeyExchangeBuilder {
}
class ServerKeyExchange {
  int suite;
  int buffer;
}
class ServerHelloDone {
}
class ServerHelloBuilder {
}
class ServerHello {
  int disableExtensions;
  int buffer;
  int SESSID_OFFSET2;
  int SESSID_OFFSET;
  int RANDOM_OFFSET;
}
class ServerHandshake {
  class RSA_PSKExchange {
    int psKey;
    int encryptedPreMasterSecret;
  }
  class RSAKeyExchange {
    int encryptedPreMasterSecret;
  }
  class GenDH {
    int sigBuffer;
    int paramsBuffer;
  }
  class CertLoader {
  }
  int keyExchangeTask;
  int certLoader;
  int certVerifier;
  int genDH;
  int serverKey;
  int dhPair;
  int helloV2;
  int localCert;
  int clientCert;
  int keyAlias;
  int requestedNames;
  int continuedSession;
  int clientHadExtensions;
  int outBuffer;
  int state;
  class State {
    int DONE;
    int READ_FINISHED;
    int READ_CERTIFICATE_VERIFY;
    int READ_CLIENT_KEY_EXCHANGE;
    int READ_CERTIFICATE;
    int READ_CLIENT_HELLO;
    int WRITE_FINISHED;
    int WRITE_SERVER_HELLO_DONE;
    int WRITE_CERTIFICATE_REQUEST;
    int WRITE_SERVER_KEY_EXCHANGE;
    int WRITE_CERTIFICATE;
    int WRITE_SERVER_HELLO;
    int WRITE_HELLO_REQUEST;
    int isReadState;
    int isWriteState;
  }
}
class ServerDHParams {
  int buffer;
}
class ServerDHE_PSKParameters {
  int buffer;
}
class SSLv3HMacSHAImpl {
  int adaptee;
}
class SSLv3HMacMD5Impl {
  int adaptee;
}
class SSLSocketImpl {
  int autoClose;
  int initialHandshakeDone;
  int handshakeException;
  int isHandshaking;
  int underlyingSocket;
  int listeners;
  int engine;
  int logger;
  class SocketInputStream {
    int in;
    int appBuffer;
    int inBuffer;
  }
  class SocketOutputStream {
    int out;
    int buffer;
  }
}
class SSLSocketFactoryImpl {
  int contextImpl;
}
class SSLServerSocketImpl {
  int clientMode;
  int wantClientAuth;
  int needClientAuth;
  int enabledProtocols;
  int enabledCipherSuites;
  int enableSessionCreation;
  int contextImpl;
}
class SSLServerSocketFactoryImpl {
  int contextImpl;
}
class SSLRandom {
  int idx;
  int seed;
  int pad;
  int buffer;
  int secret;
  int sha;
  int md5;
  int SEED;
  int SECRET;
}
class SSLRSASignatureImpl {
  int initVerify;
  int initSign;
  int sha;
  int md5;
  int privkey;
  int pubkey;
  int logger;
}
class SSLHMac {
  int pad2;
  int pad1;
  int key;
  int md;
  int PAD2;
  int PAD1;
}
class SSLEngineImpl {
  class Mode {
    int CLIENT;
    int SERVER;
  }
  int mode;
  int alertBuffer;
  int enabledProtocols;
  int enabledSuites;
  int changeCipherSpec;
  int handshakeStatus;
  int lastAlert;
  int handshake;
  int initialHandshakeDone;
  int wantClientAuth;
  int needClientAuth;
  int createSessions;
  int outClosed;
  int inClosed;
  int outsec;
  int insec;
  int session;
  int logger;
  int handlers;
  int contextImpl;
}
class SSLContextImpl {
  int random;
  int srpTrustManager;
  int trustManager;
  int keyManager;
  int pskManager;
  int clientContext;
  int serverContext;
}
class SRPTrustManagerFactory {
  class Manager {
    int file;
  }
  int current;
}
class Record {
  int buffer;
}
class Random {
  int buffer;
  int RANDOM_LENGTH;
}
class ProtocolVersion {
  int minor;
  int major;
  int TLS_1_1;
  int TLS_1;
  int SSL_3;
}
class PreSharedKeyManagerFactoryImpl {
  class Manager {
  }
  int params;
}
class OutputSecurityParameters {
  int enableCBCProtection;
  int sequence;
  int suite;
  int session;
  int deflater;
  int mac;
  int cipher;
  int logger;
}
class MaxFragmentLength {
  int length;
  int value;
  int LEN_2_12;
  int LEN_2_11;
  int LEN_2_10;
  int LEN_2_9;
}
class MacException {
}
class MacAlgorithm {
  int SHA;
  int MD5;
  int NULL;
}
class KeyExchangeAlgorithm {
  int RSA_PSK;
  int DHE_PSK;
  int PSK;
  int DHE_RSA;
  int DHE_DSS;
  int DH_anon;
  int DH_RSA;
  int DH_DSS;
  int RSA;
  int NONE;
}
class Jessie {
  int VERSION_DOUBLE;
  int VERSION;
  int serialVersionUID;
}
class InputSecurityParameters {
  int sequence;
  int suite;
  int session;
  int inflater;
  int mac;
  int cipher;
  int logger;
}
class HelloRequest {
}
class Handshake {
  class Type {
    int CERTIFICATE_STATUS;
    int CERTIFICATE_URL;
    int FINISHED;
    int CLIENT_KEY_EXCHANGE;
    int CERTIFICATE_VERIFY;
    int SERVER_HELLO_DONE;
    int CERTIFICATE_REQUEST;
    int SERVER_KEY_EXCHANGE;
    int CERTIFICATE;
    int SERVER_HELLO;
    int CLIENT_HELLO;
    int HELLO_REQUEST;
    int value;
  }
  class Body {
  }
  int version;
  int suite;
  int buffer;
}
class Finished {
  int version;
  int buffer;
}
class ExtensionList {
  class ExtensionsIterator {
    int size;
    int index;
    int modCount;
  }
  int modCount;
  int buffer;
}
class Extension {
  class Value {
  }
  class Type {
    int CERT_TYPE;
    int SRP;
    int STATUS_REQUEST;
    int TRUNCATED_HMAC;
    int TRUSTED_CA_KEYS;
    int CLIENT_CERTIFICATE_URL;
    int MAX_FRAGMENT_LENGTH;
    int SERVER_NAME;
    int value;
  }
  int buffer;
}
class ExchangeKeys {
  int buffer;
}
class EncryptedPreMasterSecret {
  int version;
}
class EmptyExchangeKeys {
}
class DiffieHellman {
  int GROUP_18;
  int GROUP_17;
  int GROUP_16;
  int GROUP_15;
  int GROUP_14;
  int GROUP_5;
  int GROUP_2;
  int GROUP_1;
  int DH_G;
}
class DelegatedTask {
  int thrown;
  int hasRun;
  int logger;
}
class Debug {
  int DEBUG_DECRYPTION;
  int DEBUG_KEY_EXCHANGE;
  int DEBUG;
}
class ContentType {
  int APPLICATION_DATA;
  int HANDSHAKE;
  int ALERT;
  int CHANGE_CIPHER_SPEC;
  int CLIENT_HELLO_V2;
  int value;
}
class Constructed {
}
class CompressionMethodList {
  class Iterator {
    int modCount;
    int index;
  }
  int modCount;
  int buffer;
}
class CompressionMethod {
  int ZLIB;
  int NULL;
  int value;
}
class ClientRSA_PSKParameters {
}
class ClientPSKParameters {
}
class ClientKeyExchangeBuilder {
}
class ClientKeyExchange {
  int version;
  int suite;
  int buffer;
}
class ClientHelloV2 {
  int buffer;
}
class ClientHelloBuilder {
}
class ClientHello {
  int disableExtensions;
  int buffer;
  int SESSID_OFFSET2;
  int SESSID_OFFSET;
  int RANDOM_OFFSET;
}
class ClientHandshake {
  class GenCertVerify {
    int signed;
    int sha;
    int md5;
  }
  class RSAGen {
    int full;
    int encryptedPreMasterSecret;
  }
  class CertLoader {
    int issuers;
    int keyTypes;
  }
  class ClientDHGen {
    int full;
    int params;
    int serverKey;
  }
  class ParamsVerifier {
    int verified;
    int signature;
    int paramsBuffer;
  }
  int genCertVerify;
  int certLoader;
  int keyExchange;
  int paramsVerifier;
  int certVerifier;
  int sentVersion;
  int truncatedHMacSent;
  int maxFragmentLengthSent;
  int privateKey;
  int keyAlias;
  int dhPair;
  int continued;
  int continuedSession;
  int outBuffer;
  int state;
  class State {
    int DONE;
    int READ_FINISHED;
    int WRITE_FINISHED;
    int WRITE_CERTIFICATE_VERIFY;
    int WRITE_CLIENT_KEY_EXCHANGE;
    int WRITE_CERTIFICATE;
    int READ_SERVER_HELLO_DONE;
    int READ_CERTIFICATE_REQUEST;
    int READ_SERVER_KEY_EXCHANGE;
    int READ_CERTIFICATE;
    int READ_SERVER_HELLO;
    int WRITE_CLIENT_HELLO;
    int isReadState;
    int isWriteState;
  }
}
class ClientDiffieHellmanPublic {
}
class ClientDHE_PSKParameters {
}
class ClientCertificateTypeList {
  class Iterator {
    int modCount;
    int index;
  }
  int modCount;
  int buffer;
}
class CipherSuiteList {
  class Iterator {
    int index;
    int modCount;
  }
  int modCount;
  int version;
  int buffer;
}
class CipherSuite {
  int isResolved;
  int name;
  int id;
  int keyLength;
  int isCBCMode;
  int isStream;
  int exportable;
  int ephemeralDH;
  int macAlgorithm;
  int signatureAlgorithm;
  int keyExchangeAlgorithm;
  int cipherAlgorithm;
  int TLS_RSA_PSK_WITH_AES_256_CBC_SHA;
  int TLS_RSA_PSK_WITH_AES_128_CBC_SHA;
  int TLS_RSA_PSK_WITH_3DES_EDE_CBC_SHA;
  int TLS_RSA_PSK_WITH_RC4_128_SHA;
  int TLS_DHE_PSK_WITH_AES_256_CBC_SHA;
  int TLS_DHE_PSK_WITH_AES_128_CBC_SHA;
  int TLS_DHE_PSK_WITH_3DES_EDE_CBC_SHA;
  int TLS_DHE_PSK_WITH_RC4_128_SHA;
  int TLS_PSK_WITH_AES_256_CBC_SHA;
  int TLS_PSK_WITH_AES_128_CBC_SHA;
  int TLS_PSK_WITH_3DES_EDE_CBC_SHA;
  int TLS_PSK_WITH_RC4_128_SHA;
  int TLS_DHE_RSA_WITH_AES_256_CBC_SHA;
  int TLS_DHE_DSS_WITH_AES_256_CBC_SHA;
  int TLS_DH_RSA_WITH_AES_256_CBC_SHA;
  int TLS_DH_DSS_WITH_AES_256_CBC_SHA;
  int TLS_RSA_WITH_AES_256_CBC_SHA;
  int TLS_DHE_RSA_WITH_AES_128_CBC_SHA;
  int TLS_DHE_DSS_WITH_AES_128_CBC_SHA;
  int TLS_DH_RSA_WITH_AES_128_CBC_SHA;
  int TLS_DH_DSS_WITH_AES_128_CBC_SHA;
  int TLS_RSA_WITH_AES_128_CBC_SHA;
  int TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA;
  int TLS_DHE_RSA_WITH_DES_CBC_SHA;
  int TLS_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA;
  int TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA;
  int TLS_DHE_DSS_WITH_DES_CBC_SHA;
  int TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA;
  int TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA;
  int TLS_DH_RSA_WITH_DES_CBC_SHA;
  int TLS_DH_RSA_EXPORT_WITH_DES40_CBC_SHA;
  int TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA;
  int TLS_DH_DSS_WITH_DES_CBC_SHA;
  int TLS_DH_DSS_EXPORT_WITH_DES40_CBC_SHA;
  int TLS_RSA_WITH_3DES_EDE_CBC_SHA;
  int TLS_RSA_WITH_DES_CBC_SHA;
  int TLS_RSA_EXPORT_WITH_DES40_CBC_SHA;
  int TLS_RSA_WITH_RC4_128_SHA;
  int TLS_RSA_WITH_RC4_128_MD5;
  int TLS_RSA_EXPORT_WITH_RC4_40_MD5;
  int TLS_RSA_WITH_NULL_SHA;
  int TLS_RSA_WITH_NULL_MD5;
  int TLS_NULL_WITH_NULL_NULL;
  int namesToSuites;
  int tlsSuiteNames;
}
class CipherAlgorithm {
  int AES;
  int CAST5;
  int DESede;
  int DES;
  int RC4;
  int NULL;
}
class CertificateVerify {
}
class CertificateURL {
  class URLAndOptionalHash {
    int buffer;
  }
  class CertChainType {
    int PKIPATH;
    int INDIVIDUAL_CERTS;
    int value;
  }
  class Iterator {
    int index;
  }
  int buffer;
}
class CertificateType {
  int OPEN_PGP;
  int X509;
  int value;
}
class CertificateStatusType {
  int OCSP;
  int value;
}
class CertificateStatusRequest {
  class ResponderIdIterator {
    int index;
  }
  int buffer;
}
class CertificateRequestBuilder {
}
class CertificateRequest {
  class ClientCertificateType {
    int DSS_FIXED_DH;
    int RSA_FIXED_DH;
    int DSS_SIGN;
    int RSA_SIGN;
    int value;
  }
  int buffer;
}
class CertificateBuilder {
}
class Certificate {
  int type;
  int buffer;
}
class Builder {
}
class AlertException {
  int isLocal;
  int alert;
}
class Alert {
  class Description {
    int MISSING_SRP_USERNAME;
    int UNKNOWN_SRP_USERNAME;
    int BAD_CERTIFICATE_HASH_VALUE;
    int BAD_CERTIFICATE_STATUS_RESPONSE;
    int UNRECOGNIZED_NAME;
    int CERTIFICATE_UNOBTAINABLE;
    int UNSUPPORTED_EXTENSION;
    int NO_RENEGOTIATION;
    int USER_CANCELED;
    int INTERNAL_ERROR;
    int INSUFFICIENT_SECURITY;
    int PROTOCOL_VERSION;
    int EXPORT_RESTRICTION;
    int DECRYPT_ERROR;
    int DECODE_ERROR;
    int ACCESS_DENIED;
    int UNKNOWN_CA;
    int ILLEGAL_PARAMETER;
    int CERTIFICATE_UNKNOWN;
    int CERTIFICATE_EXPIRED;
    int CERTIFICATE_REVOKED;
    int UNSUPPORTED_CERTIFICATE;
    int BAD_CERTIFICATE;
    int NO_CERTIFICATE;
    int HANDSHAKE_FAILURE;
    int DECOMPRESSION_FAILURE;
    int RECORD_OVERFLOW;
    int DECRYPTION_FAILED;
    int BAD_RECORD_MAC;
    int UNEXPECTED_MESSAGE;
    int CLOSE_NOTIFY;
    int value;
  }
  class Level {
    int FATAL;
    int WARNING;
    int value;
  }
  int buffer;
}
class AbstractHandshake {
  class DHE_PSKGen {
    int isClient;
    int psKey;
    int dhKey;
  }
  class CertVerifier {
    int verified;
    int chain;
    int clientSide;
  }
  class DHPhase {
    int full;
    int key;
  }
  int compression;
  int clientRandom;
  int serverRandom;
  int tasks;
  int outParams;
  int inParams;
  int preMasterSecret;
  int keyAgreement;
  int engine;
  int md5;
  int sha;
  int handshakeOffset;
  int handshakeBuffer;
  int PAD2;
  int PAD1;
  int SENDER_SERVER;
  int SENDER_CLIENT;
  int IV_BLOCK;
  int SERVER_WRITE_KEY;
  int CLIENT_WRITE_KEY;
  int MASTER_SECRET;
  int KEY_EXPANSION;
  int CLIENT_FINISHED;
  int SERVER_FINISHED;
  int logger;
}
