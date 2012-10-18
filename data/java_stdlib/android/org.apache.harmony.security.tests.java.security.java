package org.apache.harmony.security.tests.java.security;
class UnrecoverableKeyExceptionTest {
  int tCause;
  int msgs;
}
class UnrecoverableEntryExceptionTest {
  int errNotExc;
  int msgs;
}
class TimestampTest {
  int cpath;
  int encoding;
  int now;
}
class SignerTest {
  class MySecurityManager {
    int denied;
  }
}
class SignedObjectTest {
}
class SignatureTest {
  class MyProvider {
  }
  class MySignature {
  }
  class MyCertificate {
  }
  class MyPrivateKey {
  }
  class MyPublicKey {
  }
  class MyKey {
  }
}
class SignatureSpiTest {
  class MySignature {
    int calledMethods;
  }
  class MySignatureSpi2 {
  }
  class MySignatureSpi1 {
  }
  int engineGetParametersExceptionOcurred;
  int engineGetParametersCalled;
}
class SignatureExceptionTest {
  int tCause;
  int msgs;
}
class Signature2Test {
  class MySignature {
  }
  class MyProvider {
    int services;
  }
  int RSA_KEYS;
  int DSA_KEYS;
  int MESSAGE;
}
class SecurityTest {
  class MyProvider {
  }
}
class Security2Test {
}
class SecureRandomSpiTest {
  class MySecureRandomSpi {
  }
}
class SecureRandom2Test {
  class MyProvider {
  }
  class MySecureRandomSpi {
    int serialVersionUID;
  }
  class MySecureRandom {
    int serialVersionUID;
  }
  int SEED_VALUE;
  int SEED_SIZE;
  int SEED_BYTES;
}
class PublicKeyTest {
  class checkPublicKey {
  }
}
class ProviderTest {
  class TestSecurityManager {
    int permissionName;
    int called;
  }
  class MyProvider {
  }
  int p;
  int storedProviders;
}
class ProviderServiceTest {
  class MyService {
  }
  class MyProvider {
  }
}
class ProviderExceptionTest {
  int tCause;
  int msgs;
}
class Provider2Test {
  class MockProvider {
    int serialVersionUID;
  }
  int provTest;
  class MyEntry {
  }
  class TestProvider {
  }
}
class PrivilegedActionTest {
  class MyPrivilegedAction2 {
    int called;
  }
  class MyPrivilegedAction {
    int called;
  }
}
class PrivateKeyTest {
  class checkPrivateKey {
  }
}
class NoSuchProviderExceptionTest {
  int tCause;
  int msgs;
}
class NoSuchAlgorithmExceptionTest {
  int tCause;
  int msgs;
}
class MessageDigestSpiTest {
  class MyMessageDigestCloneable {
  }
  class MyMessageDigest {
  }
}
class MessageDigest2Test {
  class TestProvider {
  }
  class MessageDigestStub {
  }
  int SHA_DATA_1;
  int SHA_DATA_2;
  int MESSAGE_DIGEST_65_As;
  int MESSAGE_DIGEST_64_As;
  int MESSAGE_DIGEST_63_As;
  int MESSAGE_DIGEST;
  int MESSAGE;
  int AR2;
  int AR1;
  int digestAlgs;
  int MESSAGEDIGEST_ID;
}
class MessageDigest1Test {
  class TestMessageDigestSpi {
  }
  class TestProvider {
  }
}
class KeyTest {
  class checkKey {
  }
}
class KeyStoreTest {
  int mProv;
  int NotSupportMsg;
  int defaultProvider;
  int defaultProviderName;
  int KSSupported;
  int defaultType;
  int KeyStoreProviderClass;
}
class MyCertificate {
  int encoding;
}
class tmpProtection {
}
class tmpEntry {
}
class KeyStoreSpiTest {
}
class KeyStorePrivateKeyEntryTest {
}
class KeyStoreLoadStoreParameterTest {
  class MyLoadStoreParameter {
  }
}
class KeyStoreExceptionTest {
  int tCause;
  int msgs;
}
class KeyStoreBuilderTest {
  class KeyStoreBuilder {
  }
  class MyProtectionParameter {
  }
  int defaultProvider;
  int defaultType;
  int validValues;
  int myProtParam;
  int callbackHand;
  int tmpCall;
  int protPass;
  int pass;
}
class KeyStore4Test {
  int KEY_STORE_TYPE;
  int failing;
  int uninitialized;
  int keyStore;
  int provider;
}
class KeyStore3Test {
  class MyProvider {
  }
  class MyKeyStoreSpi {
  }
  class MyKeyStore {
  }
  int certificate;
  int keyPair;
  int mockKeyStore;
}
class KeyStore2Test {
  class MyCertificate {
    int encoding;
  }
  int certArray3;
  int certificate3;
  int certArray2;
  int certificate2;
  int certArray;
  int certificate;
  int support_TestProvider;
  int testEncoding;
  int pssWord;
  int PRIVATE_KEY;
}
class KeyRepTypeTest {
}
class KeyRepTest {
  class KeyRepChild {
  }
  int keyFactoryAlgorithm;
}
class KeyPairTest {
  class TestKeyPair {
  }
}
class KeyPairGeneratorSpiTest {
  class MyAlgorithmParameterSpec {
  }
}
class KeyPairGenerator4Test {
}
class KeyPairGenerator3Test {
  class MykeyPGen {
  }
  int NotSupportMsg;
  int DSASupported;
  int validProvider;
  int validProviderName;
}
class KeyPairGenerator2Test {
  int resAlg;
  int mProv;
  int post;
  int validValues;
  int invalidValues;
  int defaultAlg;
  int KeyPairGeneratorProviderClass4;
  int KeyPairGeneratorProviderClass3;
  int KeyPairGeneratorProviderClass2;
  int KeyPairGeneratorProviderClass1;
  int KeyPairGeneratorProviderClass;
}
class KeyPairGenerator1Test {
  int NotSupportMsg;
  int DSASupported;
  int validProvider;
  int validProviderName;
  int validAlgName;
  int algs;
  int srvKeyPairGenerator;
  int invalidValues;
}
class KeyManagementExceptionTest {
  int tCause;
  int msgs;
}
class KeyFactoryTest {
  class AnotherKey {
  }
  class TestPublicKey {
    int encoded;
  }
  class TestPrivateKey {
    int encoded;
  }
  class TestPublicKeySpec {
    int encoded;
  }
  class TestPrivateKeySpec {
    int encoded;
  }
  class TestKeyFactorySpi {
  }
  class TestKeyFactoryProvider {
  }
  int TEST_KEYFACTORY_NAME;
  int TEST_PROVIDER_NAME;
  int existingProvider;
  int exceptionThrown;
  int provider;
}
class KeyFactorySpiTest {
  class MyKeySpec {
  }
  class MyKeyFactorySpi {
  }
}
class KeyFactory2Test {
  class KeyFactorySpiStub {
  }
  class KeyFactoryStub {
  }
  class KeepAlive {
    int iterations;
    int sleepTime;
  }
  int providerName;
  int keyfactAlgs;
  int KEYFACTORY_ID;
}
class KeyExceptionTest {
  int tCause;
  int msgs;
}
class KSTrustedCertificateEntryTest {
}
class tmpSecretKey {
}
class KSSecretKeyEntryTest {
}
class KSPrivateKeyEntryTest {
  class tmpPrivateKey {
    int alg;
  }
  int testChain;
  int testPrivateKey;
}
class KSPasswordProtectionTest {
}
class KSCallbackHandlerProtectionTest {
}
class InvalidParameterExceptionTest {
  int tCause;
  int msgs;
}
class InvalidKeyExceptionTest {
  int tCause;
  int msgs;
}
class InvalidAlgorithmParameterExceptionTest {
  int tCause;
  int msgs;
}
class IdentityScopeTest {
  int is;
  class MySecurityManager {
    int denied;
  }
}
class IdentityScope2Test {
  class IdentityScopeSubclass {
    int identities;
    int serialVersionUID;
  }
  int PUB_KEY;
}
class Identity2Test {
  class IdentitySubclass {
    int serialVersionUID;
  }
  int certArray2;
  int certificate2;
  int certArray;
  int certificate;
  class CertificateImpl {
    int cert;
  }
  int PUB_KEY;
}
class GuardedObjectTest {
}
class GeneralSecurityExceptionTest {
  int tCause;
  int msgs;
}
class DigestOutputStreamTest {
  class MyDigestOutputStream {
  }
  class MyOutputStream {
  }
  int MY_MESSAGE_LEN;
  int myMessage;
  int CHUNK_SIZE;
  int algorithmName;
}
class DigestInputStreamTest {
  int MY_MESSAGE_LEN;
  int myMessage;
  int CHUNK_SIZE;
  int algorithmName;
}
class DigestInputStream2Test {
  int digest;
  int inStream1;
  int inStream;
}
class DigestExceptionTest {
  int tCause;
  int msgs;
}
class CodeSignerTest {
  int ts;
  int now;
  int cpath;
}
class AlgorithmParametersTest {
  class myAlgP {
  }
  class MyAlgorithmParameters {
    int runEngineToString;
    int runEngineInitB$String;
    int runEngineInitB$;
    int runEngineInit_AlgParamSpec;
  }
  class DummyAlgorithmParameters {
  }
  class MyAlgorithmParameterSpec {
  }
  class MyProvider {
  }
  int p;
}
class AlgorithmParametersSpiTest {
  class MyAlgorithmParameterSpec {
  }
  class MyAlgorithmParametersSpi {
  }
}
class AlgorithmParameterGenerator2Test {
  class tmpAlgorithmParameterSpec {
    int type;
  }
  int mProv;
  int validValues;
  int invalidValues;
  int defaultAlg;
  int AlgorithmParameterGeneratorProviderClass;
}
class myAlgPG {
}
class AlgorithmParameterGenerator1Test {
  int DSASupported;
  int validProvider;
  int validProviderName;
  int srvAlgorithmParameterGenerator;
  int algs;
  int validAlgName;
  int invalidValues;
}
