package gnu.java.security.x509.ext;
class SubjectKeyIdentifier {
  int keyIdentifier;
  int ID;
}
class SubjectAlternativeNames {
  int names;
  int ID;
}
class ReasonCode {
  int reason;
  int ID;
}
class PrivateKeyUsagePeriod {
  int notAfter;
  int notBefore;
  int ID;
}
class PolicyMappings {
  int mappings;
  int ID;
}
class PolicyConstraint {
  int inhibitPolicyMapping;
  int requireExplicitPolicy;
  int ID;
}
class NameConstraints {
  int excludedSubtrees;
  int permittedSubtrees;
  int ID;
}
class KeyUsage {
  int keyUsage;
  int DECIPHER_ONLY;
  int ENCIPHER_ONLY;
  int CRL_SIGN;
  int KEY_CERT_SIGN;
  int KEY_AGREEMENT;
  int DATA_ENCIPHERMENT;
  int KEY_ENCIPHERMENT;
  int NON_REPUDIATION;
  int DIGITAL_SIGNATURE;
  int ID;
}
class IssuerAlternativeNames {
  int names;
  int ID;
}
class GeneralSubtree {
  int maximum;
  int minimum;
  int base;
}
class GeneralNames {
  int names;
}
class GeneralName {
  int encoded;
  int name;
  int kind;
  class Kind {
    int registeredId;
    int iPAddress;
    int uniformResourceIdentifier;
    int ediPartyName;
    int directoryName;
    int x400Address;
    int dNSName;
    int rfc822Name;
    int otherName;
    int tag;
  }
}
class Extension {
  class Value {
    int encoded;
  }
  int encoded;
  int value;
  int isSupported;
  int critical;
  int oid;
  int log;
}
class ExtendedKeyUsage {
  int purposeIds;
  int ID;
}
class CertificatePolicies {
  int policyQualifierInfos;
  int policies;
  int ID;
}
class CRLNumber {
  int number;
  int ID;
}
class BasicConstraints {
  int pathLenConstraint;
  int ca;
  int ID;
}
class AuthorityKeyIdentifier {
  int authorityCertSerialNumber;
  int authorityCertIssuer;
  int keyIdentifier;
  int ID;
}
