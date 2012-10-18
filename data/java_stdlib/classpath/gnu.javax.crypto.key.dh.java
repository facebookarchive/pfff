package gnu.javax.crypto.key.dh;
class RFC2631 {
  int prng;
  int rnd;
  int L;
  int m;
  int sha;
  int TWO;
  int DH_PARAMS_G;
  int DH_PARAMS_J;
  int DH_PARAMS_P;
  int DH_PARAMS_Q;
  int DH_PARAMS_COUNTER;
  int DH_PARAMS_SEED;
}
class GnuDHPublicKey {
  int str;
  int y;
}
class GnuDHPrivateKey {
  int str;
  int x;
}
class GnuDHKeyPairGenerator {
  int preferredFormat;
  int prng;
  int g;
  int j;
  int p;
  int q;
  int counter;
  int seed;
  int m;
  int l;
  int rnd;
  int DEFAULT_ENCODING_FORMAT;
  int DEFAULT_EXPONENT_SIZE;
  int DEFAULT_PRIME_SIZE;
  int PREFERRED_ENCODING_FORMAT;
  int EXPONENT_SIZE;
  int PRIME_SIZE;
  int DH_PARAMETERS;
  int SOURCE_OF_RANDOMNESS;
  int log;
}
class GnuDHKey {
  int str;
  int defaultFormat;
  int g;
  int p;
  int q;
}
class ElGamalSender {
  int B;
}
class ElGamalReceiver {
  int B;
}
class ElGamalKeyAgreement {
  int ZZ;
  int KA_ELGAMAL_RECIPIENT_PUBLIC_KEY;
  int KA_ELGAMAL_RECIPIENT_PRIVATE_KEY;
  int SOURCE_OF_RANDOMNESS;
}
class DiffieHellmanSender {
  int x;
}
class DiffieHellmanReceiver {
  int y;
}
class DiffieHellmanKeyAgreement {
  int ZZ;
  int ownerKey;
  int KA_DIFFIE_HELLMAN_OWNER_PRIVATE_KEY;
  int SOURCE_OF_RANDOMNESS;
}
class DHKeyPairX509Codec {
  int DH_ALG_OID;
}
class DHKeyPairRawCodec {
}
class DHKeyPairPKCS8Codec {
  int DH_ALG_OID;
}
