package java.security.cert;
class X509Extension {
}
class X509Certificate {
  int serialVersionUID;
}
class X509CertSelector {
  int pathToNames;
  int policy;
  int nameConstraints;
  int matchAllNames;
  int altNames;
  int keyPurposeSet;
  int subjectKeySpec;
  int subjectKey;
  int sigId;
  int certValid;
  int keyUsage;
  int authKeyId;
  int subjectKeyId;
  int subject;
  int issuer;
  int serialNo;
  int cert;
  int basicConstraints;
  int NAME_CONSTRAINTS_ID;
  int SUBJECT_KEY_ID;
  int AUTH_KEY_ID;
}
class X509CRLSelector {
  int cert;
  int date;
  int minCrlNumber;
  int maxCrlNumber;
  int issuerNames;
  int CRL_NUMBER_ID;
}
class X509CRLEntry {
}
class X509CRL {
}
class TrustAnchor {
  int nameConstraints;
  int trustedCert;
  int caKey;
  int caName;
}
class PolicyQualifierInfo {
  int qualifier;
  int encoded;
  int oid;
}
class PolicyNode {
}
class PKIXParameters {
  int targetConstraints;
  int sigProvider;
  int date;
  int policyQualRejected;
  int anyPolicyInhibited;
  int policyMappingInhibited;
  int exPolicyRequired;
  int revocationEnabled;
  int pathCheckers;
  int certStores;
  int initPolicies;
  int trustAnchors;
}
class PKIXCertPathValidatorResult {
  int subjectPublicKey;
  int policyTree;
  int trustAnchor;
}
class PKIXCertPathChecker {
}
class PKIXCertPathBuilderResult {
  int certPath;
}
class PKIXBuilderParameters {
  int maxPathLength;
}
class LDAPCertStoreParameters {
  int port;
  int serverName;
  int LDAP_PORT;
}
class CollectionCertStoreParameters {
  int collection;
}
class CertificateParsingException {
  int serialVersionUID;
}
class CertificateNotYetValidException {
  int serialVersionUID;
}
class CertificateFactorySpi {
}
class CertificateFactory {
  int type;
  int provider;
  int certFacSpi;
  int CERTIFICATE_FACTORY;
}
class CertificateExpiredException {
  int serialVersionUID;
}
class CertificateException {
  int serialVersionUID;
}
class CertificateEncodingException {
  int serialVersionUID;
}
class Certificate {
  class CertificateRep {
    int data;
    int type;
    int serialVersionUID;
  }
  int type;
  int serialVersionUID;
}
class CertStoreSpi {
}
class CertStoreParameters {
}
class CertStoreException {
  int serialVersionUID;
}
class CertStore {
  int params;
  int type;
  int provider;
  int storeSpi;
  int CERT_STORE;
}
class CertSelector {
}
class CertPathValidatorSpi {
}
class CertPathValidatorResult {
}
class CertPathValidatorException {
  int certPath;
  int index;
  int serialVersionUID;
}
class CertPathValidator {
  int algorithm;
  int provider;
  int validatorSpi;
  int CERT_PATH_VALIDATOR;
}
class CertPathParameters {
}
class CertPathBuilderSpi {
}
class CertPathBuilderResult {
}
class CertPathBuilderException {
  int serialVersionUID;
}
class CertPathBuilder {
  int algorithm;
  int provider;
  int cpbSpi;
  int CERT_PATH_BUILDER;
}
class CertPath {
  int type;
  int serialVersionUID;
  class CertPathRep {
    int data;
    int type;
    int serialVersionUID;
  }
}
class CRLSelector {
}
class CRLException {
  int serialVersionUID;
}
class CRL {
  int type;
}
