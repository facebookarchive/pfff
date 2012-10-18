package gnu.javax.net.ssl;
class StaticTrustAnchors {
  int CA_CERTS;
  int certs;
}
class SessionStoreException {
}
class Session {
  class ID {
    int id;
    int serialVersionUID;
  }
  int context;
  int random;
  int truncatedMac;
  int valid;
  int values;
  int peerVerified;
  int peerPort;
  int peerHost;
  int peerCertChain;
  int peerCerts;
  int localCerts;
  int sessionId;
  int applicationBufferSize;
  int lastAccessedTime;
  int creationTime;
}
class SSLRecordHandler {
  int contentType;
}
class SSLProtocolVersion {
  int TLSv1;
  int SSLv3;
  int minor;
  int major;
}
class SSLCipherSuite {
  int provider;
  int version;
  int id;
  int algorithm;
  int SERVICE;
}
class SRPTrustManager {
}
class SRPManagerParameters {
  int file;
}
class PrivateCredentials {
  int certChains;
  int privateKeys;
  int END_RSA;
  int BEGIN_RSA;
  int END_DSA;
  int BEGIN_DSA;
}
class PreSharedKeyManagerParameters {
  int keys;
}
class PreSharedKeyManager {
}
class NullManagerParameters {
}
class EntropySource {
}
class AbstractSessionContext {
  int implClass;
  int timeout;
}
