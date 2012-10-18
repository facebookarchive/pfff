package gnu.java.security.x509;
class X509Certificate {
  int signature;
  int sigAlgVal;
  int sigAlgId;
  int extensions;
  int subjectUniqueId;
  int issuerUniqueId;
  int subjectKey;
  int subject;
  int notAfter;
  int notBefore;
  int issuer;
  int algVal;
  int algId;
  int serialNo;
  int version;
  int tbsCertBytes;
  int encoded;
  int ID_ECDSA_WITH_SHA1;
  int ID_RSA_WITH_SHA1;
  int ID_RSA_WITH_MD5;
  int ID_RSA_WITH_MD2;
  int ID_RSA;
  int ID_DSA_WITH_SHA1;
  int ID_DSA;
  int logger;
  int serialVersionUID;
}
class X509CertSelectorImpl {
  int subjectNames;
  int issuerNames;
}
class X509CertPath {
  int pki_encoded;
  int pkcs_encoded;
  int path;
  int PKCS7_DATA;
  int PKCS7_SIGNED_DATA;
  int ENCODINGS;
}
class X509CRLSelectorImpl {
  int issuerNames;
}
class X509CRLEntry {
  int extensions;
  int revocationDate;
  int serialNo;
  int encoded;
  int log;
}
class X509CRL {
  int signature;
  int rawSig;
  int sigAlgParams;
  int sigAlg;
  int extensions;
  int revokedCerts;
  int issuerDN;
  int nextUpdate;
  int thisUpdate;
  int algParams;
  int algId;
  int version;
  int tbsCRLBytes;
  int encoded;
  int ID_RSA_WITH_SHA1;
  int ID_RSA_WITH_MD5;
  int ID_RSA_WITH_MD2;
  int ID_RSA;
  int ID_DSA_WITH_SHA1;
  int ID_DSA;
  int log;
}
class X500DistinguishedName {
  int sep;
  int encoded;
  int stringRep;
  int fixed;
  int currentRdn;
  int components;
  int UID;
  int DC;
  int EMAIL;
  int GENERATION;
  int INITIALS;
  int GIVENNAME;
  int NAME;
  int DNQ;
  int T;
  int OU;
  int O;
  int STREET;
  int ST;
  int L;
  int C;
  int CN;
}
class Util {
  int HEX;
}
class PolicyNodeImpl {
  int readOnly;
  int critical;
  int depth;
  int parent;
  int children;
  int qualifiers;
  int expectedPolicies;
  int policy;
}
class GnuPKIExtension {
}
