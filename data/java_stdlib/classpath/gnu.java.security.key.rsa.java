package gnu.java.security.key.rsa;
class RSAKeyPairX509Codec {
  int RSA_ALG_OID;
  int log;
}
class RSAKeyPairRawCodec {
}
class RSAKeyPairPKCS8Codec {
  int RSA_ALG_OID;
  int log;
}
class RSAKeyPairGenerator {
  int preferredFormat;
  int prng;
  int rnd;
  int e;
  int L;
  int DEFAULT_ENCODING_FORMAT;
  int DEFAULT_MODULUS_LENGTH;
  int PREFERRED_ENCODING_FORMAT;
  int RSA_PARAMETERS;
  int SOURCE_OF_RANDOMNESS;
  int MODULUS_LENGTH;
  int TWO;
  int ONE;
  int log;
}
class GnuRSAPublicKey {
  int str;
}
class GnuRSAPrivateKey {
  int str;
  int qInv;
  int dQ;
  int dP;
  int d;
  int q;
  int p;
}
class GnuRSAKey {
  int str;
  int defaultFormat;
  int e;
  int n;
}
