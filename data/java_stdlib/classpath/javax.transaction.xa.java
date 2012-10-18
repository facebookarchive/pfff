package javax.transaction.xa;
class Xid {
  int MAXBQUALSIZE;
  int MAXGTRIDSIZE;
}
class XAResource {
  int XA_OK;
  int XA_RDONLY;
  int TMSUSPEND;
  int TMSUCCESS;
  int TMSTARTRSCAN;
  int TMRESUME;
  int TMONEPHASE;
  int TMNOFLAGS;
  int TMJOIN;
  int TMFAIL;
  int TMENDRSCAN;
}
class XAException {
  int XAER_OUTSIDE;
  int XAER_DUPID;
  int XAER_RMFAIL;
  int XAER_PROTO;
  int XAER_INVAL;
  int XAER_NOTA;
  int XAER_RMERR;
  int XAER_ASYNC;
  int XA_RDONLY;
  int XA_RETRY;
  int XA_HEURMIX;
  int XA_HEURRB;
  int XA_HEURCOM;
  int XA_HEURHAZ;
  int XA_NOMIGRATE;
  int XA_RBEND;
  int XA_RBTRANSIENT;
  int XA_RBTIMEOUT;
  int XA_RBPROTO;
  int XA_RBOTHER;
  int XA_RBINTEGRITY;
  int XA_RBDEADLOCK;
  int XA_RBCOMMFAIL;
  int XA_RBROLLBACK;
  int XA_RBBASE;
  int errorCode;
}
