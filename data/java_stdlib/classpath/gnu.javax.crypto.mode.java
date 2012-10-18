package gnu.javax.crypto.mode;
class OFB {
  int outputBlock;
}
class ModeFactory {
  int names;
}
class IMode {
  int DECRYPTION;
  int ENCRYPTION;
  int IV;
  int MODE_BLOCK_SIZE;
  int STATE;
}
class ICM {
  int blockNdx;
  int C0;
  int counterRange;
  int maxBlocksPerSegment;
  int TWO_FIFTY_SIX;
}
class IAuthenticatedMode {
}
class ECB {
}
class EAX {
  int valid;
  int t_n;
  int cipher;
  int cipherBlockSize;
  int init;
  int state;
  int ctr;
  int msgOmac;
  int headerOmac;
  int nonceOmac;
  int tagSize;
}
class CTR {
  int enc;
  int counter;
  int off;
}
class CFB {
  int scratch;
  int shiftRegister;
}
class CBC {
  int scratch;
  int lastBlock;
}
class BaseMode {
  int lock;
  int iv;
  int modeBlockSize;
  int cipherBlockSize;
  int cipher;
  int state;
  int name;
}
