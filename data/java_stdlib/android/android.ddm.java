package android.ddm;
class DdmRegister {
}
class DdmHandleThread {
  int mInstance;
  int CHUNK_STKL;
  int CHUNK_THST;
  int CHUNK_THDE;
  int CHUNK_THCR;
  int CHUNK_THEN;
}
class DdmHandleProfiling {
  int mInstance;
  int CHUNK_MPRQ;
  int CHUNK_MPSE;
  int CHUNK_MPSS;
  int CHUNK_MPRE;
  int CHUNK_MPRS;
}
class DdmHandleNativeHeap {
  int mInstance;
  int CHUNK_NHGT;
}
class DdmHandleHello {
  int mInstance;
  int CHUNK_FEAT;
  int CHUNK_WAIT;
  int CHUNK_HELO;
}
class DdmHandleHeap {
  int mInstance;
  int CHUNK_REAL;
  int CHUNK_REAQ;
  int CHUNK_REAE;
  int CHUNK_HPGC;
  int CHUNK_NHSG;
  int CHUNK_HPDS;
  int CHUNK_HPDU;
  int CHUNK_HPSG;
  int CHUNK_HPIF;
}
class DdmHandleExit {
  int mInstance;
  int CHUNK_EXIT;
}
class DdmHandleAppName {
  int mInstance;
  int mAppName;
  int CHUNK_APNM;
}
