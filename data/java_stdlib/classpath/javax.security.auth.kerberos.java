package javax.security.auth.kerberos;
class ServicePermission {
  int flags;
  int ACCEPT;
  int INITIATE;
}
class KeyImpl {
  int key;
  int type;
  int algorithm;
}
class KerberosTicket {
  int clientAddresses;
  int server;
  int client;
  int renewTill;
  int endTime;
  int startTime;
  int authTime;
  int flags;
  int sessionKey;
  int asn1Encoding;
  int NUM_FLAGS;
  int INITIAL;
  int RENEWABLE;
  int POSTDATED;
  int PROXY;
  int PROXIABLE;
  int FORWARDED;
  int FORWARDABLE;
  int serialVersionUID;
}
class KerberosPrincipal {
  int realm;
  int type;
  int name;
  int KRB_NT_UNKNOWN;
  int KRB_NT_UID;
  int KRB_NT_SRV_XHST;
  int KRB_NT_SRV_INST;
  int KRB_NT_SRV_HST;
  int KRB_NT_PRINCIPAL;
}
class KerberosKey {
  int key;
  int versionNum;
  int principal;
  int serialVersionUID;
}
class DelegationPermission {
}
