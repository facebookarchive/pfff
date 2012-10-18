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
  int provider;
  int tmfSpi;
  int DEFAULT_ALGORITHM;
  int TRUST_MANAGER_FACTORY;
}
class TrustManager {
}
class TrivialHostnameVerifier {
}
class SSLSocketFactory {
  class ErrorSocketFactory {
    int x;
  }
  int context;
}
class SSLSocket {
}
class SSLSessionContext {
}
class SSLSessionBindingListener {
}
class SSLSessionBindingEvent {
  int name;
  int serialVersionUID;
}
class SSLSession {
}
class SSLServerSocketFactory {
  class ErrorServerSocketFactory {
    int x;
  }
  int context;
}
class SSLServerSocket {
}
class SSLProtocolException {
}
class SSLPermission {
  int serialVersionUID;
}
class SSLPeerUnverifiedException {
}
class SSLKeyException {
}
class SSLHandshakeException {
}
class SSLException {
  int serialVersionUID;
}
class SSLEngineResult {
  class HandshakeStatus {
    int NEED_UNWRAP;
    int NEED_WRAP;
    int NEED_TASK;
    int FINISHED;
    int NOT_HANDSHAKING;
  }
  class Status {
    int CLOSED;
    int OK;
    int BUFFER_OVERFLOW;
    int BUFFER_UNDERFLOW;
  }
  int bytesProduced;
  int bytesConsumed;
  int status;
  int handshakeStatus;
}
class SSLEngine {
  int peerPort;
  int peerHost;
}
class SSLContextSpi {
}
class SSLContext {
  int protocol;
  int provider;
  int ctxSpi;
  int SSL_CONTEXT;
}
class ManagerFactoryParameters {
}
class KeyStoreBuilderParameters {
}
class KeyManagerFactorySpi {
}
class KeyManagerFactory {
  int algorithm;
  int provider;
  int kmfSpi;
  int DEFAULT_ALGORITHM;
  int KEY_MANAGER_FACTORY;
}
class KeyManager {
}
class HttpsURLConnection {
  int factory;
  int hostnameVerifier;
  int defaultFactory;
  int defaultVerifier;
}
class HostnameVerifier {
}
class HandshakeCompletedListener {
}
class HandshakeCompletedEvent {
  int session;
  int serialVersionUID;
}
class CertPathTrustManagerParameters {
  int params;
}
