package gnu.java.security.pkcs;
class SignerInfo {
  int unauthenticatedAttributes;
  int encryptedDigest;
  int digestEncryptionAlgorithmParams;
  int digestEncryptionAlgorithmId;
  int authenticatedAttributes;
  int digestAlgorithmParams;
  int digestAlgorithmId;
  int issuer;
  int serialNumber;
  int version;
  int log;
}
class PKCS7SignedData {
  int signerInfos;
  int crls;
  int certificates;
  int content;
  int contentType;
  int digestAlgorithms;
  int version;
  int PKCS7_SIGNED_DATA;
  int log;
}
class PKCS7Data {
  int content;
  int PKCS7_DATA;
}
