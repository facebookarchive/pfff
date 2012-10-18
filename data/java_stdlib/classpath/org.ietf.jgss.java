package org.ietf.jgss;
class Oid {
  int relative;
  int strOid;
  int derOid;
  int components;
  int RELATIVE_OID;
  int OBJECT_IDENTIFIER;
}
class MessageProp {
  int minorString;
  int minorStatus;
  int gap;
  int unseq;
  int old;
  int duplicate;
  int privState;
  int qopVal;
}
class GSSName {
  int NT_USER_NAME;
  int NT_STRING_UID_NAME;
  int NT_MACHINE_UID_NAME;
  int NT_HOSTBASED_SERVICE;
  int NT_EXPORT_NAME;
  int NT_ANONYMOUS;
}
class GSSManager {
}
class GSSException {
  int messages;
  int minorString;
  int minor;
  int major;
  int GAP_TOKEN;
  int UNSEQ_TOKEN;
  int OLD_TOKEN;
  int DUPLICATE_TOKEN;
  int NAME_NOT_MN;
  int DUPLICATE_ELEMENT;
  int UNAVAILABLE;
  int UNAUTHORIZED;
  int BAD_QOP;
  int NO_CRED;
  int NO_CONTEXT;
  int FAILURE;
  int DEFECTIVE_TOKEN;
  int DEFECTIVE_CREDENTIAL;
  int CREDENTIALS_EXPIRED;
  int CONTEXT_EXPIRED;
  int BAD_MIC;
  int BAD_STATUS;
  int BAD_NAMETYPE;
  int BAD_NAME;
  int BAD_MECH;
  int BAD_BINDINGS;
  int serialVersionUID;
}
class GSSCredential {
  int INDEFINITE_LIFETIME;
  int DEFAULT_LIFETIME;
  int ACCEPT_ONLY;
  int INITIATE_ONLY;
  int INITIATE_AND_ACCEPT;
}
class GSSContext {
  int INDEFINITE_LIFETIME;
  int DEFAULT_LIFETIME;
}
class ChannelBinding {
  int acceptAddr;
  int initAddr;
  int appData;
}
