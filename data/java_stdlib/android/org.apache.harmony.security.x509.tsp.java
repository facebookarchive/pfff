package org.apache.harmony.security.x509.tsp;
class TimeStampResp {
  int ASN1;
  int timeStampToken;
  int status;
}
class TimeStampReq {
  int ASN1;
  int encoding;
  int extensions;
  int certReq;
  int nonce;
  int reqPolicy;
  int messageImprint;
  int version;
}
class TSTInfo {
  int ASN1;
  int ACCURACY;
  int extensions;
  int tsa;
  int nonce;
  int ordering;
  int accuracy;
  int genTime;
  int serialNumber;
  int messageImprint;
  int policy;
  int version;
}
class PKIStatusInfo {
  int ASN1;
  int failInfo;
  int statusString;
  int status;
}
class PKIStatus {
  int REVOCATION_NOTIFICATION;
  int REVOCATION_WARNING;
  int WAITING;
  int REJECTION;
  int GRANTED_WITH_MODS;
  int GRANTED;
  int status;
}
class PKIFailureInfo {
  int SYSTEM_FAILURE;
  int ADD_INFO_NOT_AVAILABLE;
  int UNACCEPTED_EXTENSION;
  int UNACCEPTED_POLICY;
  int TIME_NOT_AVAILABLE;
  int BAD_DATA_FORMAT;
  int BAD_REQUEST;
  int BAD_ALG;
  int maxValue;
  int value;
}
class MessageImprint {
  int ASN1;
  int hashedMessage;
  int algId;
}
