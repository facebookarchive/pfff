package org.apache.harmony.security.x509;
class X509PublicKey {
  int keyBytes;
  int encoded;
  int algorithm;
}
class Validity {
  int ASN1;
  int encoding;
  int notAfter;
  int notBefore;
}
class Time {
  int ASN1;
  int JAN_01_2050;
}
class TBSCertificate {
  int ASN1;
  int encoding;
  int extensions;
  int subjectUniqueID;
  int issuerUniqueID;
  int subjectPublicKeyInfo;
  int subject;
  int validity;
  int issuer;
  int signature;
  int serialNumber;
  int version;
}
class TBSCertList {
  int ASN1;
  class RevokedCertificate {
    int ASN1;
    int encoding;
    int issuer;
    int issuerRetrieved;
    int crlEntryExtensions;
    int revocationDate;
    int userCertificate;
  }
  int encoding;
  int crlExtensions;
  int revokedCertificates;
  int nextUpdate;
  int thisUpdate;
  int issuer;
  int signature;
  int version;
}
class SubjectPublicKeyInfo {
  int ASN1;
  int encoding;
  int unusedBits;
  int publicKey;
  int subjectPublicKey;
  int algorithmID;
}
class SubjectKeyIdentifier {
  int keyIdentifier;
}
class ReasonFlags {
  int ASN1;
  int flags;
  int REASONS;
}
class ReasonCode {
  int ASN1;
  int code;
  int AA_COMPROMISE;
  int PRIVILEGE_WITHDRAWN;
  int REMOVE_FROM_CRL;
  int CERTIFICATE_HOLD;
  int CESSATION_OF_OPERATION;
  int SUPERSEDED;
  int AFFILIATION_CHANGED;
  int CA_COMPROMISE;
  int KEY_COMPROMISE;
  int UNSPECIFIED;
}
class PrivateKeyUsagePeriod {
  int ASN1;
  int encoding;
  int notAfterDate;
  int notBeforeDate;
}
class PolicyQualifierInfo {
  int ASN1;
}
class PolicyInformation {
  int ASN1;
  int encoding;
  int policyIdentifier;
}
class PolicyConstraints {
  int ASN1;
  int encoding;
  int inhibitPolicyMapping;
  int requireExplicitPolicy;
}
class OtherName {
  int ASN1;
  int encoding;
  int value;
  int typeID;
}
class ORAddress {
  int ASN1;
  int encoding;
}
class NameConstraints {
  int ASN1;
  int excluded_names;
  int permitted_names;
  int encoding;
  int excludedSubtrees;
  int permittedSubtrees;
}
class KeyUsage {
  int ASN1;
  int keyUsage;
  int USAGES;
}
class IssuingDistributionPoint {
  int ASN1;
  int onlyContainsAttributeCerts;
  int indirectCRL;
  int onlySomeReasons;
  int onlyContainsCACerts;
  int onlyContainsUserCerts;
  int distributionPoint;
}
class InvalidityDate {
  int ASN1;
  int date;
}
class InhibitAnyPolicy {
  int skipCerts;
}
class InfoAccessSyntax {
  int ASN1;
  int accessDescriptions;
}
class GeneralSubtrees {
  int ASN1;
  int encoding;
  int generalSubtrees;
}
class GeneralSubtree {
  int ASN1;
  int encoding;
  int maximum;
  int minimum;
  int base;
}
class GeneralNames {
  int ASN1;
  int encoding;
  int generalNames;
}
class GeneralName {
  int ASN1;
  int name_encoding;
  int encoding;
  int name;
  int tag;
  int nameASN1;
  int REG_ID;
  int IP_ADDR;
  int UR_ID;
  int EDIP_NAME;
  int DIR_NAME;
  int X400_ADDR;
  int DNS_NAME;
  int RFC822_NAME;
  int OTHER_NAME;
}
class Extensions {
  int ASN1;
  int encoding;
  int oidMap;
  int hasUnsupported;
  int noncritical;
  int critical;
  int extensions;
  int SUPPORTED_CRITICAL;
}
class ExtensionValue {
  int encoding;
}
class Extension {
  int ASN1;
  int valueDecoded;
  int extnValueObject;
  int rawExtnValue;
  int encoding;
  int extnValue;
  int critical;
  int extnID_str;
  int extnID;
  int ISSUING_DISTR_POINTS;
  int REASON_CODE;
  int INVALIDITY_DATE;
  int CERTIFICATE_ISSUER;
  int CRL_NUMBER;
  int ISSUING_DISTR_POINT;
  int SUBJECT_INFO_ACCESS;
  int AUTHORITY_INFO_ACCESS;
  int INHIBIT_ANY_POLICY;
  int FRESHEST_CRL;
  int EXTENDED_KEY_USAGE;
  int POLICY_CONSTRAINTS;
  int AUTH_KEY_ID;
  int POLICY_MAPPINGS;
  int CERTIFICATE_POLICIES;
  int CRL_DISTR_POINTS;
  int NAME_CONSTRAINTS;
  int BASIC_CONSTRAINTS;
  int ISSUER_ALTERNATIVE_NAME;
  int SUBJECT_ALT_NAME;
  int PRIVATE_KEY_USAGE_PERIOD;
  int KEY_USAGE;
  int SUBJ_KEY_ID;
  int SUBJ_DIRECTORY_ATTRS;
  int NON_CRITICAL;
  int CRITICAL;
}
class ExtendedKeyUsage {
  int ASN1;
  int keys;
}
class EDIPartyName {
  int ASN1;
  int encoding;
  int partyName;
  int nameAssigner;
}
class DistributionPointName {
  int ASN1;
  int nameRelativeToCRLIssuer;
  int fullName;
}
class DistributionPoint {
  int ASN1;
  int cRLIssuer;
  int reasons;
  int distributionPoint;
}
class DNParser {
  int encoded;
  int hasQE;
  int chars;
  int end;
  int beg;
  int pos;
}
class CertificatePolicies {
  int ASN1;
  int encoding;
  int policyInformations;
}
class CertificateList {
  int ASN1;
  int encoding;
  int signatureValue;
  int signatureAlgorithm;
  int tbsCertList;
}
class CertificateIssuer {
  int ASN1;
  int issuer;
}
class Certificate {
  int ASN1;
  int encoding;
  int signatureValue;
  int signatureAlgorithm;
  int tbsCertificate;
}
class CRLNumber {
  int ASN1;
  int number;
}
class CRLDistributionPoints {
  int ASN1;
  int encoding;
  int distributionPoints;
}
class BasicConstraints {
  int ASN1;
  int pathLenConstraint;
  int ca;
}
class AuthorityKeyIdentifier {
  int ASN1;
  int authorityCertSerialNumber;
  int authorityCertIssuer;
  int keyIdentifier;
}
class AlternativeName {
  int alternativeNames;
  int which;
  int SUBJECT;
  int ISSUER;
}
class AlgorithmIdentifier {
  int ASN1;
  int encoding;
  int parameters;
  int algorithmName;
  int algorithm;
}
class AccessDescription {
  int ASN1;
  int encoding;
  int accessLocation;
  int accessMethod;
}
