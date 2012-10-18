package java.security;
class UnresolvedPermission {
}
class UnrecoverableKeyException {
  int serialVersionUID;
}
class UnrecoverableEntryException {
  int serialVersionUID;
}
class Timestamp {
  int hash;
  int signerCertPath;
  int timestamp;
  int serialVersionUID;
}
class Signer {
  int privateKey;
  int serialVersionUID;
}
class SignedObject {
  int thealgorithm;
  int signature;
  int content;
  int serialVersionUID;
}
class SignatureSpi {
  int appRandom;
}
class SignatureException {
  int serialVersionUID;
}
class Signature {
  class SignatureImpl {
    int spiImpl;
  }
  int state;
  int VERIFY;
  int SIGN;
  int UNINITIALIZED;
  int algorithm;
  int provider;
  int ENGINE;
  int SERVICE;
}
class SecurityPermission {
}
class Security {
  class SecurityDoor {
  }
  int secprops;
}
class SecureRandomSpi {
  int serialVersionUID;
}
class SecureRandom {
  int internalSecureRandom;
  int algorithm;
  int secureRandomSpi;
  int provider;
  int ENGINE;
  int SERVICE;
  int serialVersionUID;
}
class SecureClassLoader {
  int pds;
}
class PublicKey {
  int serialVersionUID;
}
class ProviderException {
  int serialVersionUID;
}
class Provider {
  class Service {
    int lastClassName;
    int implementation;
    int attributes;
    int aliases;
    int className;
    int algorithm;
    int type;
    int provider;
  }
  int lastServicesByType;
  int lastType;
  int lastServicesSet;
  int lastServiceName;
  int lastAlgorithm;
  int returnedService;
  int changedProperties;
  int propertyAliasTable;
  int propertyServiceTable;
  int aliasTable;
  int serviceTable;
  int providerNumber;
  int info;
  int versionString;
  int version;
  int name;
  int serialVersionUID;
}
class ProtectionDomain {
}
class PrivilegedExceptionAction {
}
class PrivilegedActionException {
  int serialVersionUID;
}
class PrivilegedAction {
}
class PrivateKey {
  int serialVersionUID;
}
class Principal {
}
class PolicySpi {
}
class Policy {
  int UNSUPPORTED_EMPTY_COLLECTION;
  class Parameters {
  }
}
class PermissionsHash {
  int perms;
  int serialVersionUID;
}
class Permissions {
}
class PermissionCollection {
}
class Permission {
}
class NoSuchProviderException {
  int serialVersionUID;
}
class NoSuchAlgorithmException {
  int serialVersionUID;
}
class MessageDigestSpi {
}
class MessageDigest {
  class MessageDigestImpl {
    int spiImpl;
  }
  int algorithm;
  int provider;
  int ENGINE;
}
class KeyStoreSpi {
}
class KeyStoreException {
  int serialVersionUID;
}
class KeyStore {
  class TrustedCertificateEntry {
    int trustCertificate;
  }
  class SecretKeyEntry {
    int secretKey;
  }
  class PrivateKeyEntry {
    int privateKey;
    int chain;
  }
  class ProtectionParameter {
  }
  class PasswordProtection {
    int isDestroyed;
    int password;
  }
  class LoadStoreParameter {
  }
  class Entry {
  }
  class CallbackHandlerProtection {
    int callbackHandler;
  }
  class Builder {
    class TmpLSParameter {
      int protPar;
    }
    class BuilderImpl {
      int lastException;
      int isGetKeyStore;
      int fileForLoad;
      int providerForKeyStore;
      int typeForKeyStore;
      int protParameter;
      int keyStore;
    }
  }
  int type;
  int provider;
  int implSpi;
  int isInit;
  int DEFAULT_KEYSTORE_TYPE;
  int PROPERTYNAME;
  int ENGINE;
  int SERVICE;
}
class KeyRep {
  class Type {
    int PRIVATE;
    int PUBLIC;
    int SECRET;
  }
  int encoded;
  int format;
  int algorithm;
  int type;
  int serialVersionUID;
}
class KeyPairGeneratorSpi {
}
class KeyPairGenerator {
  class KeyPairGeneratorImpl {
    int spiImpl;
  }
  int algorithm;
  int provider;
  int RANDOM;
  int ENGINE;
  int SERVICE;
}
class KeyPair {
  int publicKey;
  int privateKey;
  int serialVersionUID;
}
class KeyManagementException {
  int serialVersionUID;
}
class KeyFactorySpi {
}
class KeyFactory {
  int algorithm;
  int spiImpl;
  int provider;
  int ENGINE;
  int SERVICE;
}
class KeyException {
  int serialVersionUID;
}
class Key {
  int serialVersionUID;
}
class InvalidParameterException {
  int serialVersionUID;
}
class InvalidKeyException {
  int serialVersionUID;
}
class InvalidAlgorithmParameterException {
  int serialVersionUID;
}
class IdentityScope {
  int systemScope;
  int serialVersionUID;
}
class Identity {
  int certificates;
  int scope;
  int info;
  int publicKey;
  int name;
  int serialVersionUID;
}
class GuardedObject {
  int guard;
  int object;
  int serialVersionUID;
}
class Guard {
}
class GeneralSecurityException {
  int serialVersionUID;
}
class DomainCombiner {
}
class DigestOutputStream {
  int isOn;
  int digest;
}
class DigestInputStream {
  int isOn;
  int digest;
}
class DigestException {
  int serialVersionUID;
}
class CodeSource {
}
class CodeSigner {
  int hash;
  int timestamp;
  int signerCertPath;
  int serialVersionUID;
}
class Certificate {
}
class BasicPermission {
}
class AuthProvider {
}
class AllPermissionCollection {
}
class AllPermission {
}
class AlgorithmParametersSpi {
}
class AlgorithmParameters {
  int initialized;
  int algorithm;
  int spiImpl;
  int provider;
  int ENGINE;
  int SEVICE;
}
class AlgorithmParameterGeneratorSpi {
}
class AlgorithmParameterGenerator {
  int algorithm;
  int spiImpl;
  int provider;
  int RANDOM;
  int ENGINE;
  int SERVICE;
}
class AccessController {
}
class AccessControlException {
  int perm;
  int serialVersionUID;
}
class AccessControlContext {
}
