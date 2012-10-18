package java.security.cert;
class X509Extension {
}
class X509Certificate {
  int serialVersionUID;
}
class X509CertSelector {
  int issuerBytes;
  int issuerName;
  int subjectPublicKeyImpl;
  int pathToNames;
  int policies;
  int nameConstraints;
  int subjectAltNames;
  int pathLen;
  int matchAllNames;
  int extendedKeyUsage;
  int keyUsage;
  int subjectPublicKey;
  int privateKeyValid;
  int subjectPublicKeyAlgID;
  int certificateValid;
  int authorityKeyIdentifier;
  int subjectKeyIdentifier;
  int subject;
  int issuer;
  int serialNumber;
  int certificateEquals;
}
class X509CRLSelector {
  int certificateChecking;
  int dateAndTime;
  int maxCRL;
  int minCRL;
  int issuerPrincipals;
  int issuerNames;
}
class X509CRLEntry {
}
class X509CRL {
}
class TrustAnchor {
  int nameConstraints;
  int trustedCert;
  int caPublicKey;
  int caName;
  int caPrincipal;
}
class PolicyQualifierInfo {
  int policyQualifier;
  int policyQualifierId;
  int encoded;
}
class PolicyNode {
}
class PKIXParameters {
  int policyQualifiersRejected;
  int anyPolicyInhibited;
  int policyMappingInhibited;
  int explicitPolicyRequired;
  int revocationEnabled;
  int targetCertConstraints;
  int sigProvider;
  int certPathCheckers;
  int date;
  int certStores;
  int initialPolicies;
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
  int DEFAULT_LDAP_PORT;
  int DEFAULT_LDAP_SERVER_NAME;
}
class CollectionCertStoreParameters {
  int collection;
  int defaultCollection;
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
  int spiImpl;
  int provider;
  int ENGINE;
  int SERVICE;
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
    int serialPersistentFields;
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
  int certStoreParams;
  int type;
  int spiImpl;
  int provider;
  int DEFAULTPROPERTY;
  int PROPERTYNAME;
  int ENGINE;
  int SERVICE;
}
class CertSelector {
}
class CertPathValidatorSpi {
}
class CertPathValidatorResult {
}
class CertPathValidatorException {
  int index;
  int certPath;
  int serialVersionUID;
}
class CertPathValidator {
  int algorithm;
  int spiImpl;
  int provider;
  int DEFAULTPROPERTY;
  int PROPERTYNAME;
  int ENGINE;
  int SERVICE;
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
  int spiImpl;
  int provider;
  int DEFAULTPROPERTY;
  int PROPERTYNAME;
  int ENGINE;
  int SERVICE;
}
class CertPath {
  class CertPathRep {
    int serialPersistentFields;
    int data;
    int type;
    int serialVersionUID;
  }
  int type;
  int serialVersionUID;
}
class CRLSelector {
}
class CRLException {
  int serialVersionUID;
}
class CRL {
  int type;
}
