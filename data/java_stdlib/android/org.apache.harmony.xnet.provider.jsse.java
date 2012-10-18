package org.apache.harmony.xnet.provider.jsse;
class TrustedCertificateStoreTest {
  int store;
  int ALIAS_USER_CA3_COLLISION;
  int ALIAS_USER_CA3;
  int ALIAS_SYSTEM_CA3_COLLISION;
  int ALIAS_SYSTEM_CA3;
  int ALIAS_USER_CHAIN2;
  int ALIAS_USER_CHAIN1;
  int ALIAS_USER_CHAIN0;
  int ALIAS_SYSTEM_CHAIN2;
  int ALIAS_SYSTEM_CHAIN1;
  int ALIAS_SYSTEM_CHAIN0;
  int ALIAS_USER_CA2;
  int ALIAS_USER_CA1;
  int ALIAS_SYSTEM_CA2;
  int ALIAS_SYSTEM_CA1;
  int CA3_WITH_CA1_SUBJECT;
  int CHAIN;
  int PRIVATE;
  int CA2;
  int CA1;
  int DIR_DELETED;
  int DIR_ADDED;
  int DIR_SYSTEM;
  int DIR_TEST;
  int DIR_TEMP;
}
class TrustedCertificateStore {
  class CertSelector {
  }
  int deletedDir;
  int addedDir;
  int systemDir;
  int CERT_FACTORY;
  int CA_CERTS_DIR_DELETED;
  int CA_CERTS_DIR_ADDED;
  int CA_CERTS_DIR_SYSTEM;
  int PREFIX_USER;
  int PREFIX_SYSTEM;
}
class TrustedCertificateKeyStoreSpi {
  int store;
}
class TrustedCertificateIndex {
  int subjectToTrustAnchors;
}
class TrustManagerImplTest {
}
class TrustManagerImpl {
  int factory;
  int err;
  int acceptedIssuers;
  int trustedCertificateIndex;
  int validator;
  int trustedCertificateStore;
  int rootKeyStore;
}
class TrustManagerFactoryImpl {
  int keyStore;
}
class ServerSessionContext {
  int persistentCache;
}
class ServerKeyExchange {
  int key;
  int hash;
  int bytes3;
  int par3;
  int bytes2;
  int par2;
  int bytes1;
  int par1;
}
class ServerHelloDone {
}
class ServerHello {
  int compression_method;
  int cipher_suite;
  int session_id;
  int random;
  int server_version;
}
class ServerHandshakeImpl {
  int privKey;
}
class SSLv3Constants {
  int SHApad2;
  int MD5pad2;
  int SHApad1;
  int MD5pad1;
  int server;
  int client;
}
class SSLStreamedInput {
  int in;
}
class SSLSocketWrapper {
  int autoClose;
  int socket;
}
class SSLSocketOutputStream {
  int owner;
}
class SSLSocketInputStream {
  class Adapter {
  }
  int dataPoint;
  int end_reached;
  int owner;
  int end;
  int pos;
  int buffer;
  int BUFFER_SIZE;
}
class SSLSocketImpl {
  int logger;
  int listeners;
  int output;
  int input;
  int sslParameters;
  int socket_was_closed;
  int session;
  int appDataOS;
  int appDataIS;
  int alertProtocol;
  int handshakeProtocol;
  int recordProtocol;
  int handshake_started;
}
class SSLSocketFactoryImpl {
  int instantiationException;
  int sslParameters;
}
class SSLSessionImpl {
  int isServer;
  int serverRandom;
  int clientRandom;
  int master_secret;
  int peerPort;
  int peerHost;
  int peerCertificates;
  int localCertificates;
  int context;
  int cipherSuite;
  int protocol;
  int lastAccessedTime;
  int id;
  int values;
  int isValid;
  int creationTime;
  int NULL_SESSION;
}
class SSLServerSocketImpl {
  int logger;
  int sslParameters;
}
class SSLServerSocketFactoryImpl {
  int instantiationException;
  int sslParameters;
}
class SSLServerSessionCache {
}
class SSLRecordProtocol {
  int change_cipher_spec_byte;
  int sessionWasChanged;
  int logger;
  int pendingConnectionState;
  int activeWriteState;
  int activeReadState;
  int appData;
  int alertProtocol;
  int handshakeProtocol;
  int in;
  int version;
  int session;
  int MAX_SSL_PACKET_SIZE;
  int MAX_CIPHERED_DATA_LENGTH;
  int MAX_COMPRESSED_DATA_LENGTH;
  int MAX_DATA_LENGTH;
}
class SSLParametersImpl {
  int enable_session_creation;
  int want_client_auth;
  int need_client_auth;
  int client_mode;
  int enabledProtocols;
  int enabledCipherSuiteNames;
  int enabledCipherSuites;
  int secureRandom;
  int trustManager;
  int keyManager;
  int serverSessionContext;
  int clientSessionContext;
  int defaultParameters;
  int defaultSecureRandom;
  int defaultTrustManager;
  int defaultKeyManager;
}
class SSLInputStream {
}
class SSLEngineImpl {
  int logger;
  int remaining_hsh_data;
  int remaining_wrapped_data;
  int sslParameters;
  int session;
  int dataStream;
  int appData;
  int alertProtocol;
  int handshakeProtocol;
  int recProtIS;
  int recordProtocol;
  int engine_was_shutteddown;
  int engine_was_closed;
  int close_notify_was_received;
  int close_notify_was_sent;
  int isOutboundDone;
  int isInboundDone;
  int handshake_started;
  int peer_mode_was_set;
}
class SSLEngineDataStream {
  int consumed;
  int available;
  int limit;
  int offset;
  int srcs;
}
class SSLEngineAppData {
  int buffer;
}
class SSLContextImpl {
  int sslParameters;
  int serverSessionContext;
  int clientSessionContext;
  int DEFAULT_SSL_CONTEXT_IMPL;
}
class SSLClientSessionCache {
}
class SSLBufferedInput {
  int consumed;
  int bytik;
  int in;
}
class ProtocolVersion {
  int version;
  int name;
  int TLSv1;
  int SSLv3;
  int protocolsByName;
  int supportedProtocols;
}
class PRF {
  int sha_mac_length;
  int md5_mac_length;
  int sha;
  int md5;
  int sha_mac;
  int md5_mac;
  int logger;
}
class OpenSSLSocketImplWrapper {
  int socket;
}
class OpenSSLSocketImpl {
  class SSLOutputStream {
  }
  class SSLInputStream {
  }
  int wrappedPort;
  int wrappedHost;
  int handshakeTimeoutMilliseconds;
  int timeoutMilliseconds;
  int listeners;
  int handshakeCompleted;
  int guard;
  int handshakeStarted;
  int autoClose;
  int socket;
  int sslSession;
  int hostname;
  int useSessionTickets;
  int enabledCompressionMethods;
  int enabledCipherSuites;
  int enabledProtocols;
  int npnProtocols;
  int sslParameters;
  int writeLock;
  int readLock;
  int handshakeLock;
  int os;
  int is;
  int sslNativePointer;
}
class OpenSSLSocketFactoryImpl {
  int instantiationException;
  int sslParameters;
}
class OpenSSLSignatureTest {
}
class OpenSSLSignature {
  class SHA1DSA {
  }
  class SHA512RSA {
  }
  class SHA384RSA {
  }
  class SHA256RSA {
  }
  class SHA1RSA {
  }
  class MD5RSA {
  }
  int singleByte;
  int evpAlgorithm;
  int engineType;
  int key;
  int ctx;
  class EngineType {
    int DSA;
    int RSA;
  }
}
class OpenSSLSessionImpl {
  int id;
  int sessionContext;
  int compressionMethod;
  int protocol;
  int cipherSuite;
  int peerPort;
  int peerHost;
  int sslSessionNativePointer;
  int peerCertificateChain;
  int values;
  int isValid;
  int peerCertificates;
  int localCertificates;
  int lastAccessedTime;
  int creationTime;
}
class OpenSSLServerSocketImpl {
  int enabledCompressionMethods;
  int enabledCipherSuites;
  int enabledProtocols;
  int sslParameters;
}
class OpenSSLServerSocketFactoryImpl {
  int instantiationException;
  int sslParameters;
}
class OpenSSLRSAPublicKey {
  int fetchedParams;
  int modulus;
  int publicExponent;
  int key;
  int serialVersionUID;
}
class OpenSSLRSAPrivateKey {
  int privateExponent;
  int modulus;
  int fetchedParams;
  int key;
  int serialVersionUID;
}
class OpenSSLRSAPrivateCrtKey {
  int crtCoefficient;
  int primeExponentQ;
  int primeExponentP;
  int primeQ;
  int primeP;
  int publicExponent;
}
class OpenSSLRSAKeyPairGenerator {
  int modulusBits;
  int publicExponent;
}
class OpenSSLRSAKeyFactory {
}
class OpenSSLProvider {
  int PROVIDER_NAME;
}
class OpenSSLMessageDigestJDK {
  class SHA512 {
    int SIZE;
    int EVP_MD;
  }
  class SHA384 {
    int SIZE;
    int EVP_MD;
  }
  class SHA256 {
    int SIZE;
    int EVP_MD;
  }
  class SHA1 {
    int SIZE;
    int EVP_MD;
  }
  class MD5 {
    int SIZE;
    int EVP_MD;
  }
  int singleByte;
  int size;
  int evp_md;
  int ctx;
}
class OpenSSLKey {
  int engine;
  int ctx;
}
class OpenSSLEngine {
  int ctx;
}
class OpenSSLDSAPublicKey {
  int params;
  int key;
  int serialVersionUID;
}
class OpenSSLDSAPrivateKey {
  int params;
  int key;
  int serialVersionUID;
}
class OpenSSLDSAParams {
  int x;
  int y;
  int q;
  int p;
  int g;
  int fetchedParams;
  int key;
}
class OpenSSLDSAKeyPairGenerator {
  int q;
  int p;
  int g;
  int random;
  int primeBits;
}
class OpenSSLDSAKeyFactory {
}
class OpenSSLContextImpl {
}
class NativeCryptoTest {
  int BYTES;
  class ServerHooks {
    int certificates;
    int privateKey;
  }
  class TestSSLHandshakeCallbacks {
    int handshakeCompletedCalled;
    int clientCertificateRequestedCalled;
    int asn1DerEncodedX500Principals;
    int keyTypes;
    int verifyCertificateChainCalled;
    int authMethod;
    int asn1DerEncodedCertificateChain;
    int hooks;
    int sslNativePointer;
  }
  class Hooks {
  }
  int DEBUG;
  int CA_PRINCIPALS;
  int CLIENT_CERTIFICATES;
  int CLIENT_PRIVATE_KEY;
  int SERVER_CERTIFICATES;
  int SERVER_PRIVATE_KEY;
  int TIMEOUT_SECONDS;
  int DUMMY_CB;
  int INVALID_FD;
  int NULL;
}
class NativeCrypto {
  class SSLHandshakeCallbacks {
  }
  int SSL_VERIFY_FAIL_IF_NO_PEER_CERT;
  int SSL_VERIFY_PEER;
  int SSL_VERIFY_NONE;
  int SUPPORTED_COMPRESSION_METHODS;
  int SUPPORTED_COMPRESSION_METHOD_NULL;
  int SUPPORTED_COMPRESSION_METHOD_ZLIB;
  int SSL_OP_NO_TLSv1_2;
  int SSL_OP_NO_TLSv1_1;
  int SSL_OP_NO_TLSv1;
  int SSL_OP_NO_SSLv3;
  int SSL_OP_NO_COMPRESSION;
  int SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION;
  int SSL_OP_NO_TICKET;
  int SSL_MODE_HANDSHAKE_CUTTHROUGH;
  int EVP_PKEY_EC;
  int EVP_PKEY_DH;
  int EVP_PKEY_DSA;
  int EVP_PKEY_RSA;
  int SUPPORTED_CIPHER_SUITES;
  int TLS_EMPTY_RENEGOTIATION_INFO_SCSV;
  int STANDARD_TO_OPENSSL_CIPHER_SUITES;
  int OPENSSL_TO_STANDARD_CIPHER_SUITES;
  int SUPPORTED_PROTOCOL_TLSV1_2;
  int SUPPORTED_PROTOCOL_TLSV1_1;
  int SUPPORTED_PROTOCOL_TLSV1;
  int SUPPORTED_PROTOCOL_SSLV3;
  int RAND_SEED_LENGTH_IN_BYTES;
}
class Message {
  int length;
}
class Logger {
  int names;
  class Stream {
    int indent;
    int prefix;
  }
}
class KeyManagerImpl {
  int hash;
}
class KeyManagerFactoryImpl {
  int pwd;
  int keyStore;
}
class JSSEProvider {
  int serialVersionUID;
}
class HelloRequest {
}
class HandshakeProtocol {
  int socketOwner;
  int engineOwner;
  int needSendHelloRequest;
  int needSendCCSpec;
  int master_secret_bytes;
  int verify_data;
  int delegatedTaskErr;
  int preMasterSecret;
  int isResuming;
  int changeCipherSpecReceived;
  int serverFinished;
  int clientFinished;
  int certificateVerify;
  int clientKeyExchange;
  int clientCert;
  int serverHelloDone;
  int certificateRequest;
  int serverKeyExchange;
  int serverCert;
  int serverHello;
  int clientHello;
  int session;
  int nonBlocking;
  int delegatedTasks;
  int parameters;
  int recordProtocol;
  int io_stream;
  int status;
  int NEED_TASK;
  int FINISHED;
  int NOT_HANDSHAKING;
  int NEED_UNWRAP;
}
class HandshakeIODataStream {
  int write_pos_beg;
  int write_pos;
  int read_pos_end;
  int marked_pos;
  int read_pos;
  int buffer;
  int inc_buff_size;
  int buff_size;
  int sha;
  int md5;
}
class Handshake {
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
}
class Finished {
  int data;
}
class FileClientSessionCacheTest {
}
class FileClientSessionCache {
  class CacheFile {
    int lastModified;
    int name;
  }
  int caches;
  class Impl {
    int initialFiles;
    int size;
    int accessOrder;
    int directory;
  }
  int MAX_SIZE;
}
class EndOfSourceException {
  int serialVersionUID;
}
class EndOfBufferException {
  int serialVersionUID;
}
class DigitalSignature {
  int sha_hash;
  int md5_hash;
  int cipher;
  int signature;
  int sha;
  int md5;
}
class DelegatedTask {
  int action;
  int handshaker;
}
class DefaultSSLContextImpl {
  int TRUST_MANAGERS;
  int KEY_MANAGERS;
}
class DataStream {
}
class DHParameters {
  int prime;
  int primeGroup2;
  int primeGroup1;
  int prime512;
}
class ContentType {
  int APPLICATION_DATA;
  int HANDSHAKE;
  int ALERT;
  int CHANGE_CIPHER_SPEC;
}
class ConnectionStateTLS {
  int mac_material_header;
  int decMac;
  int encMac;
  int IV_BLOCK_LABEL;
  int SERVER_WRITE_KEY_LABEL;
  int CLIENT_WRITE_KEY_LABEL;
  int KEY_EXPANSION_LABEL;
}
class ConnectionStateSSLv3 {
  int mac_material_part;
  int pad_2;
  int pad_1;
  int mac_read_secret;
  int mac_write_secret;
  int messageDigest;
}
class ConnectionState {
  int logger;
  int read_seq_num;
  int write_seq_num;
  int hash_size;
  int block_size;
  int decCipher;
  int encCipher;
}
class ClientSessionContextTest {
  class ValidSSLSession {
  }
}
class ClientSessionContext {
  class HostAndPort {
    int port;
    int host;
  }
  int persistentCache;
  int sessionsByHostAndPort;
}
class ClientKeyExchange {
  int isRSA;
  int isTLS;
  int exchange_keys;
}
class ClientHello {
  int compression_methods;
  int cipher_suites;
  int session_id;
  int random;
  int client_version;
}
class ClientHandshakeImpl {
}
class CipherSuiteTest {
}
class CipherSuite {
  int AUTH_TYPE_ECDHE_RSA;
  int AUTH_TYPE_ECDHE_ECDSA;
  int AUTH_TYPE_ECDH_RSA;
  int AUTH_TYPE_ECDH_ECDSA;
  int AUTH_TYPE_DH_RSA;
  int AUTH_TYPE_DH_DSS;
  int AUTH_TYPE_DHE_RSA;
  int AUTH_TYPE_DHE_DSS;
  int AUTH_TYPE_RSA_EXPORT;
  int AUTH_TYPE_RSA;
  int TLS_CT_ECDSA_FIXED_ECDH;
  int TLS_CT_RSA_FIXED_ECDH;
  int TLS_CT_ECDSA_SIGN;
  int TLS_CT_DSS_FIXED_DH;
  int TLS_CT_RSA_FIXED_DH;
  int TLS_CT_DSS_SIGN;
  int TLS_CT_RSA_SIGN;
  int KEY_TYPE_EC_RSA;
  int KEY_TYPE_EC_EC;
  int KEY_TYPE_EC;
  int KEY_TYPE_DH_DSA;
  int KEY_TYPE_DH_RSA;
  int KEY_TYPE_DSA;
  int KEY_TYPE_RSA;
  int DEFAULT_CIPHER_SUITES;
  int SUPPORTED_CIPHER_SUITE_NAMES;
  int SUPPORTED_CIPHER_SUITES;
  int SUITES_BY_NAME;
  int SUITES_BY_CODE_0xc0;
  int SUITES_BY_CODE_0x00;
  int TLS_ECDH_anon_WITH_AES_256_CBC_SHA;
  int TLS_ECDH_anon_WITH_AES_128_CBC_SHA;
  int TLS_ECDH_anon_WITH_3DES_EDE_CBC_SHA;
  int TLS_ECDH_anon_WITH_RC4_128_SHA;
  int TLS_ECDH_anon_WITH_NULL_SHA;
  int TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA;
  int TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA;
  int TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA;
  int TLS_ECDHE_RSA_WITH_RC4_128_SHA;
  int TLS_ECDHE_RSA_WITH_NULL_SHA;
  int TLS_ECDH_RSA_WITH_AES_256_CBC_SHA;
  int TLS_ECDH_RSA_WITH_AES_128_CBC_SHA;
  int TLS_ECDH_RSA_WITH_3DES_EDE_CBC_SHA;
  int TLS_ECDH_RSA_WITH_RC4_128_SHA;
  int TLS_ECDH_RSA_WITH_NULL_SHA;
  int TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA;
  int TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA;
  int TLS_ECDHE_ECDSA_WITH_3DES_EDE_CBC_SHA;
  int TLS_ECDHE_ECDSA_WITH_RC4_128_SHA;
  int TLS_ECDHE_ECDSA_WITH_NULL_SHA;
  int TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA;
  int TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA;
  int TLS_ECDH_ECDSA_WITH_3DES_EDE_CBC_SHA;
  int TLS_ECDH_ECDSA_WITH_RC4_128_SHA;
  int TLS_ECDH_ECDSA_WITH_NULL_SHA;
  int TLS_DH_anon_WITH_AES_256_CBC_SHA;
  int TLS_DHE_RSA_WITH_AES_256_CBC_SHA;
  int TLS_DHE_DSS_WITH_AES_256_CBC_SHA;
  int TLS_RSA_WITH_AES_256_CBC_SHA;
  int TLS_DH_anon_WITH_AES_128_CBC_SHA;
  int TLS_DHE_RSA_WITH_AES_128_CBC_SHA;
  int TLS_DHE_DSS_WITH_AES_128_CBC_SHA;
  int TLS_RSA_WITH_AES_128_CBC_SHA;
  int SSL_DH_anon_WITH_3DES_EDE_CBC_SHA;
  int SSL_DH_anon_WITH_DES_CBC_SHA;
  int SSL_DH_anon_EXPORT_WITH_DES40_CBC_SHA;
  int SSL_DH_anon_WITH_RC4_128_MD5;
  int SSL_DH_anon_EXPORT_WITH_RC4_40_MD5;
  int SSL_DHE_RSA_WITH_3DES_EDE_CBC_SHA;
  int SSL_DHE_RSA_WITH_DES_CBC_SHA;
  int SSL_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA;
  int SSL_DHE_DSS_WITH_3DES_EDE_CBC_SHA;
  int SSL_DHE_DSS_WITH_DES_CBC_SHA;
  int SSL_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA;
  int SSL_RSA_WITH_3DES_EDE_CBC_SHA;
  int SSL_RSA_WITH_DES_CBC_SHA;
  int SSL_RSA_EXPORT_WITH_DES40_CBC_SHA;
  int SSL_RSA_EXPORT_WITH_RC2_CBC_40_MD5;
  int SSL_RSA_WITH_RC4_128_SHA;
  int SSL_RSA_WITH_RC4_128_MD5;
  int SSL_RSA_EXPORT_WITH_RC4_40_MD5;
  int SSL_RSA_WITH_NULL_SHA;
  int SSL_RSA_WITH_NULL_MD5;
  int SSL_NULL_WITH_NULL_NULL;
  int CODE_TLS_ECDH_anon_WITH_AES_256_CBC_SHA;
  int CODE_TLS_ECDH_anon_WITH_AES_128_CBC_SHA;
  int CODE_TLS_ECDH_anon_WITH_3DES_EDE_CBC_SHA;
  int CODE_TLS_ECDH_anon_WITH_RC4_128_SHA;
  int CODE_TLS_ECDH_anon_WITH_NULL_SHA;
  int CODE_TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA;
  int CODE_TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA;
  int CODE_TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA;
  int CODE_TLS_ECDHE_RSA_WITH_RC4_128_SHA;
  int CODE_TLS_ECDHE_RSA_WITH_NULL_SHA;
  int CODE_TLS_ECDH_RSA_WITH_AES_256_CBC_SHA;
  int CODE_TLS_ECDH_RSA_WITH_AES_128_CBC_SHA;
  int CODE_TLS_ECDH_RSA_WITH_3DES_EDE_CBC_SHA;
  int CODE_TLS_ECDH_RSA_WITH_RC4_128_SHA;
  int CODE_TLS_ECDH_RSA_WITH_NULL_SHA;
  int CODE_TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA;
  int CODE_TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA;
  int CODE_TLS_ECDHE_ECDSA_WITH_3DES_EDE_CBC_SHA;
  int CODE_TLS_ECDHE_ECDSA_WITH_RC4_128_SHA;
  int CODE_TLS_ECDHE_ECDSA_WITH_NULL_SHA;
  int CODE_TLS_ECDH_ECDSA_WITH_AES_256_CBC_SHA;
  int CODE_TLS_ECDH_ECDSA_WITH_AES_128_CBC_SHA;
  int CODE_TLS_ECDH_ECDSA_WITH_3DES_EDE_CBC_SHA;
  int CODE_TLS_ECDH_ECDSA_WITH_RC4_128_SHA;
  int CODE_TLS_ECDH_ECDSA_WITH_NULL_SHA;
  int CODE_TLS_DH_anon_WITH_AES_256_CBC_SHA;
  int CODE_TLS_DHE_RSA_WITH_AES_256_CBC_SHA;
  int CODE_TLS_DHE_DSS_WITH_AES_256_CBC_SHA;
  int CODE_TLS_RSA_WITH_AES_256_CBC_SHA;
  int CODE_TLS_DH_anon_WITH_AES_128_CBC_SHA;
  int CODE_TLS_DHE_RSA_WITH_AES_128_CBC_SHA;
  int CODE_TLS_DHE_DSS_WITH_AES_128_CBC_SHA;
  int CODE_TLS_RSA_WITH_AES_128_CBC_SHA;
  int CODE_SSL_DH_anon_WITH_3DES_EDE_CBC_SHA;
  int CODE_SSL_DH_anon_WITH_DES_CBC_SHA;
  int CODE_SSL_DH_anon_EXPORT_WITH_DES40_CBC_SHA;
  int CODE_SSL_DH_anon_WITH_RC4_128_MD5;
  int CODE_SSL_DH_anon_EXPORT_WITH_RC4_40_MD5;
  int CODE_SSL_DHE_RSA_WITH_3DES_EDE_CBC_SHA;
  int CODE_SSL_DHE_RSA_WITH_DES_CBC_SHA;
  int CODE_SSL_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA;
  int CODE_SSL_DHE_DSS_WITH_3DES_EDE_CBC_SHA;
  int CODE_SSL_DHE_DSS_WITH_DES_CBC_SHA;
  int CODE_SSL_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA;
  int CODE_SSL_RSA_WITH_3DES_EDE_CBC_SHA;
  int CODE_SSL_RSA_WITH_DES_CBC_SHA;
  int CODE_SSL_RSA_EXPORT_WITH_DES40_CBC_SHA;
  int CODE_SSL_RSA_EXPORT_WITH_RC2_CBC_40_MD5;
  int CODE_SSL_RSA_WITH_RC4_128_SHA;
  int CODE_SSL_RSA_WITH_RC4_128_MD5;
  int CODE_SSL_RSA_EXPORT_WITH_RC4_40_MD5;
  int CODE_SSL_RSA_WITH_NULL_SHA;
  int CODE_SSL_RSA_WITH_NULL_MD5;
  int CODE_SSL_NULL_WITH_NULL_NULL;
  int KEY_EXCHANGE_ECDH_anon;
  int KEY_EXCHANGE_ECDHE_RSA;
  int KEY_EXCHANGE_ECDH_RSA;
  int KEY_EXCHANGE_ECDHE_ECDSA;
  int KEY_EXCHANGE_ECDH_ECDSA;
  int KEY_EXCHANGE_DH_anon_EXPORT;
  int KEY_EXCHANGE_DH_anon;
  int KEY_EXCHANGE_DHE_RSA_EXPORT;
  int KEY_EXCHANGE_DHE_RSA;
  int KEY_EXCHANGE_DHE_DSS_EXPORT;
  int KEY_EXCHANGE_DHE_DSS;
  int KEY_EXCHANGE_RSA_EXPORT;
  int KEY_EXCHANGE_RSA;
  int hashSize;
  int hmacName;
  int hashName;
  int isExportable;
  int name;
  int cipherSuiteCode;
  int blockSize;
  int ivSize;
  int effectiveKeyBytes;
  int expandedKeyMaterial;
  int keyMaterial;
  int cipherName;
  int authType;
  int keyExchange;
  int supported;
}
class CertificateVerify {
  int signedHash;
}
class CertificateRequest {
  int encoded_principals;
  int types;
  int certificate_authorities;
  int certificate_types;
}
class CertificateMessage {
  int encoded_certs;
  int certs;
}
class ByteArray {
  int hashCode;
  int bytes;
}
class Appendable {
}
class AlertProtocol {
  int logger;
  int recordProtocol;
  int alert;
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
  int HANDSHAKE_FAILURE;
  int DECOMPRESSION_FAILURE;
  int RECORD_OVERFLOW;
  int DECRYPTION_FAILED;
  int BAD_RECORD_MAC;
  int UNEXPECTED_MESSAGE;
  int CLOSE_NOTIFY;
  int FATAL;
  int WARNING;
}
class AlertException {
  int description;
  int reason;
  int serialVersionUID;
}
class AbstractSessionContext {
  int sessions;
  int OPEN_SSL;
  int sslCtxNativePointer;
  int timeout;
  int maximumSize;
}
