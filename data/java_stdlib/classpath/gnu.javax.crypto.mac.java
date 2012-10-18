package gnu.javax.crypto.mac;
class UMac32 {
  int K;
  int nonceReuseCount;
  int uhash32;
  int nonce;
  int valid;
  int KEY_LEN;
  int L1_KEY_LEN;
  int OUTPUT_LEN;
  int MAX_NONCE_ITERATIONS;
  int TV1;
  int NONCE_MATERIAL;
}
class UHash32 {
  class L3Hash32 {
    int k;
    int PRIME_36;
  }
  class L2Hash32 {
    int buffer;
    int bytesSoFar;
    int highBound;
    int y;
    int k128;
    int k64;
  }
  class L1Hash32 {
    int l3hash;
    int l2hash;
    int totalCount;
    int Y;
    int count;
    int buffer;
    int key;
  }
  int l1hash;
  int streams;
  int ALL_ZEROES;
  int UPPER_RANGE;
  int LOWER_RANGE;
  int BOUNDARY;
  int TWO;
  int PRIME_128;
  int PRIME_64;
  int PRIME_36;
  int PRIME_32;
  int PRIME_19;
}
class TMMH16 {
  int Mi;
  int Ki;
  int K0;
  int context;
  int msgWords;
  int msgLength;
  int keyWords;
  int prefix;
  int keystream;
  int tagWords;
  int valid;
  int P;
  int PREFIX;
  int KEYSTREAM;
  int TAG_LENGTH;
}
class OMAC {
  int index;
  int init;
  int Y;
  int M;
  int Lu2;
  int Lu;
  int outputSize;
  int blockSize;
  int mode;
  int name;
  int cipher;
  int valid;
  int DIGEST0;
  int KEY0;
  int C2;
  int C1;
  int log;
}
class MacOutputStream {
  int mac;
  int digesting;
}
class MacInputStream {
  int mac;
  int digesting;
}
class MacFactory {
  int names;
}
class IMac {
  int TRUNCATED_SIZE;
  int MAC_KEY_MATERIAL;
}
class HMacFactory {
}
class HMac {
  int ipad;
  int opadHash;
  int ipadHash;
  int blockSize;
  int macSize;
  int valid;
  int OPAD_BYTE;
  int IPAD_BYTE;
  int USE_WITH_PKCS5_V2;
}
class BaseMac {
  int truncatedSize;
  int underlyingHash;
  int name;
}
