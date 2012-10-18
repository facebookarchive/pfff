package org.apache.harmony.dalvik.ddmc;
class DdmVmInternal {
}
class DdmServer {
  int mRegistrationTimedOut;
  int mRegistrationComplete;
  int DISCONNECTED;
  int CONNECTED;
  int mHandlerMap;
  int CLIENT_PROTOCOL_VERSION;
}
class ChunkHandler {
  int CHUNK_FAIL;
  int CHUNK_ORDER;
}
class Chunk {
  int length;
  int offset;
  int data;
  int type;
}
