package javax.net.ssl;
class X509TrustManager {
}
class X509KeyManager {
}
class X509ExtendedKeyManager {
}
class TrustManagerFactorySpi {
}
class TrustManagerFactory {
  int algorithm;
  int spiImpl;
  int provider;
  int PROPERTYNAME;
  int ENGINE;
  int SERVICE;
}
class TrustManager {
}
class SSLSocketFactory {
  int defaultName;
  int defaultSocketFactory;
}
class SSLSocket {
}
class SSLSessionContext {
}
class SSLSessionBindingListener {
}
class SSLSessionBindingEvent {
  int name;
}
class SSLSession {
}
class SSLServerSocketFactory {
  int defaultName;
  int defaultServerSocketFactory;
}
class SSLServerSocket {
}
class SSLProtocolException {
  int serialVersionUID;
}
class SSLPermission {
}
class SSLPeerUnverifiedException {
  int serialVersionUID;
}
class SSLParameters {
  int wantClientAuth;
  int needClientAuth;
  int protocols;
  int cipherSuites;
}
class SSLKeyException {
  int serialVersionUID;
}
class SSLHandshakeException {
  int serialVersionUID;
}
class SSLException {
  int serialVersionUID;
}
class SSLEngineResult {
  int bytesProduced;
  int bytesConsumed;
  int handshakeStatus;
  int status;
  class Status {
    int OK;
    int CLOSED;
    int BUFFER_UNDERFLOW;
    int BUFFER_OVERFLOW;
  }
  class HandshakeStatus {
    int NEED_UNWRAP;
    int NEED_WRAP;
    int NEED_TASK;
    int FINISHED;
    int NOT_HANDSHAKING;
  }
}
class SSLEngine {
  int peerPort;
  int peerHost;
}
class SSLContextSpi {
}
class SSLContext {
  int protocol;
  int spiImpl;
  int provider;
  int DEFAULT;
  int ENGINE;
  int SERVICE;
}
class ManagerFactoryParameters {
}
class KeyStoreBuilderParameters {
  int ksbuilders;
}
class KeyManagerFactorySpi {
}
class KeyManagerFactory {
  int algorithm;
  int spiImpl;
  int provider;
  int PROPERTY_NAME;
  int ENGINE;
  int SERVICE;
}
class KeyManager {
}
class HttpsURLConnection {
  int sslSocketFactory;
  int hostnameVerifier;
  int defaultSSLSocketFactory;
  int defaultHostnameVerifier;
}
class HostnameVerifier {
}
class HandshakeCompletedListener {
}
class HandshakeCompletedEvent {
  int session;
}
class DistinguishedNameParser {
  int chars;
  int cur;
  int end;
  int beg;
  int pos;
  int length;
  int dn;
}
class DefaultSSLSocketFactory {
  int errMessage;
}
class DefaultSSLServerSocketFactory {
  int errMessage;
}
class DefaultHostnameVerifier {
  int ALT_IPA_NAME;
  int ALT_DNS_NAME;
}
class CertPathTrustManagerParameters {
  int param;
}
