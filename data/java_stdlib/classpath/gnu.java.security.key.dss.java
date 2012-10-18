package gnu.java.security.key.dss;
class FIPS186 {
  int prng;
  int rnd;
  int L;
  int sha;
  int TWO_POW_160;
  int TWO;
  int DSA_PARAMS_G;
  int DSA_PARAMS_E;
  int DSA_PARAMS_P;
  int DSA_PARAMS_Q;
  int DSA_PARAMS_COUNTER;
  int DSA_PARAMS_SEED;
}
class DSSPublicKey {
  int str;
  int y;
}
class DSSPrivateKey {
  int str;
  int x;
}
class DSSKeyPairX509Codec {
  int DSA_ALG_OID;
}
class DSSKeyPairRawCodec {
}
class DSSKeyPairPKCS8Codec {
  int DSA_ALG_OID;
  int log;
}
class DSSKeyPairGenerator {
  int preferredFormat;
  int prng;
  int XKEY;
  int g;
  int e;
  int q;
  int p;
  int counter;
  int seed;
  int rnd;
  int L;
  int TWO_POW_160;
  int KEY_PARAMS_1024;
  int KEY_PARAMS_768;
  int KEY_PARAMS_512;
  int T_SHS;
  int DEFAULT_ENCODING_FORMAT;
  int DEFAULT_MODULUS_LENGTH;
  int PREFERRED_ENCODING_FORMAT;
  int DSS_PARAMETERS;
  int SOURCE_OF_RANDOMNESS;
  int STRICT_DEFAULTS;
  int USE_DEFAULTS;
  int MODULUS_LENGTH;
  int TWO;
  int log;
}
class DSSKey {
  int str;
  int defaultFormat;
  int g;
  int q;
  int p;
}
