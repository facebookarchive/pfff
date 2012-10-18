package org.apache.harmony.security.pkcs7;
class SignerInfo {
  int ASN1;
  int ISSUER_AND_SERIAL_NUMBER;
  int unauthenticatedAttributes;
  int encryptedDigest;
  int digestEncryptionAlgorithm;
  int authenticatedAttributes;
  int digestAlgorithm;
  int serialNumber;
  int issuer;
  int version;
}
class SignedData {
  int ASN1;
  int signerInfos;
  int crls;
  int certificates;
  int contentInfo;
  int digestAlgorithms;
  int version;
}
class ContentInfo {
  int ASN1;
  int encoding;
  int content;
  int oid;
  int ENCRYPTED_DATA;
  int DIGESTED_DATA;
  int SIGNED_AND_ENVELOPED_DATA;
  int ENVELOPED_DATA;
  int SIGNED_DATA;
  int DATA;
}
class AuthenticatedAttributes {
  int ASN1;
  int authenticatedAttributes;
  int encoding;
}
