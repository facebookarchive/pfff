package java.security;
class VMSecureRandom {
  class Spinner {
    int running;
    int value;
  }
}
class VMAccessController {
  int DEBUG;
  int DEFAULT_CONTEXT;
  int inGetContext;
  int contexts;
}
class UnresolvedPermissionCollection {
  int permissions;
  int serialVersionUID;
}
class UnresolvedPermission {
  int name;
  int type;
  int certs;
  int actions;
  int serialVersionUID;
}
class UnrecoverableKeyException {
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
  int provider;
  int algorithm;
  int state;
  int VERIFY;
  int SIGN;
  int UNINITIALIZED;
  int SIGNATURE;
}
class SecurityPermission {
  int serialVersionUID;
}
class Security {
  int secprops;
  int providers;
  int ALG_ALIAS;
}
class SecureRandomSpi {
  int serialVersionUID;
}
class SecureRandom {
  int isSeeded;
  int algorithm;
  int state;
  int secureRandomSpi;
  int randomBytesUsed;
  int randomBytes;
  int provider;
  int counter;
  int serialVersionUID;
  int SECURE_RANDOM;
}
class SecureClassLoader {
  int protectionDomainCache;
}
class PublicKey {
  int serialVersionUID;
}
class ProviderException {
  int serialVersionUID;
}
class Provider {
  int version;
  int name;
  int info;
  int serialVersionUID;
}
class ProtectionDomain {
  int hasAllPermissions;
  int staticBinding;
  int principals;
  int classloader;
  int perms;
  int code_source;
}
class PrivilegedExceptionAction {
}
class PrivilegedActionException {
  int exception;
  int serialVersionUID;
}
class PrivilegedAction {
}
class PrivateKey {
  int serialVersionUID;
}
class Principal {
}
class Policy {
  int pd2pc;
  int currentPolicy;
}
class Permissions {
  class PermissionsHash {
    int perms;
    int serialVersionUID;
  }
  int perms;
  int allPermission;
  int serialVersionUID;
}
class PermissionCollection {
  int readOnly;
  int serialVersionUID;
}
class Permission {
  int name;
  int serialVersionUID;
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
  int lastDigest;
  int provider;
  int algorithm;
  int MESSAGE_DIGEST;
}
class KeyStoreSpi {
}
class KeyStoreException {
  int serialVersionUID;
}
class KeyStore {
  int type;
  int provider;
  int keyStoreSpi;
  int KEY_STORE;
}
class KeyPairGeneratorSpi {
}
class KeyPairGenerator {
  int algorithm;
  int provider;
  int KEY_PAIR_GENERATOR;
}
class KeyPair {
  int privateKey;
  int publicKey;
  int serialVersionUID;
}
class KeyManagementException {
  int serialVersionUID;
}
class KeyFactorySpi {
}
class KeyFactory {
  int algorithm;
  int provider;
  int keyFacSpi;
  int KEY_FACTORY;
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
class IntersectingDomainCombiner {
  int SINGLETON;
}
class IdentityScope {
  int systemScope;
  int serialVersionUID;
}
class Identity {
  int certificates;
  int info;
  int publicKey;
  int scope;
  int name;
  int serialVersionUID;
}
class GuardedObject {
  int object;
  int guard;
  int serialVersionUID;
}
class Guard {
}
class GeneralSecurityException {
  int serialVersionUID;
}
class DummySignature {
  int sigSpi;
}
class DummyMessageDigest {
  int mdSpi;
}
class DummyKeyPairGenerator {
  int kpgSpi;
}
class DomainCombiner {
}
class DigestOutputStream {
  int state;
  int digest;
}
class DigestInputStream {
  int state;
  int digest;
}
class DigestException {
  int serialVersionUID;
}
class CodeSource {
  int certs;
  int location;
  int serialVersionUID;
}
class Certificate {
}
class BasicPermission {
  class BasicPermissionCollection {
    int permClass;
    int all_allowed;
    int permissions;
    int serialVersionUID;
  }
  int serialVersionUID;
}
class AllPermission {
  class AllPermissionCollection {
    int all_allowed;
    int serialVersionUID;
  }
  int serialVersionUID;
}
class AlgorithmParametersSpi {
}
class AlgorithmParameters {
  int algorithm;
  int provider;
  int paramSpi;
  int ALGORITHM_PARAMETERS;
}
class AlgorithmParameterGeneratorSpi {
}
class AlgorithmParameterGenerator {
  int algorithm;
  int provider;
  int paramGenSpi;
  int ALGORITHM_PARAMETER_GENERATOR;
}
class AccessController {
}
class AccessControlException {
  int perm;
  int serialVersionUID;
}
class AccessControlContext {
  int combiner;
  int protectionDomains;
}
