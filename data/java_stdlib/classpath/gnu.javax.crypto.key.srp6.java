package gnu.javax.crypto.key.srp6;
class SRPPublicKey {
  int Y;
}
class SRPPrivateKey {
  int v;
  int X;
}
class SRPKeyPairRawCodec {
}
class SRPKeyPairGenerator {
  int prng;
  int v;
  int g;
  int N;
  int l;
  int rnd;
  int DEFAULT_MODULUS_LENGTH;
  int SOURCE_OF_RANDOMNESS;
  int USER_VERIFIER;
  int GENERATOR;
  int SHARED_MODULUS;
  int USE_DEFAULTS;
  int MODULUS_LENGTH;
  int THREE;
  int TWO;
  int ONE;
  int ZERO;
  int log;
}
class SRPKey {
  int g;
  int N;
}
class SRPAlgorithm {
  int TWO;
  int ONE;
  int ZERO;
  int N_264;
  int N_384;
  int N_512;
  int N_640;
  int N_768;
  int N_1024;
  int N_1280;
  int N_1536;
  int N_2048;
}
class SRP6User {
  int userKeyPair;
  int p;
  int I;
}
class SRP6TLSServer {
  int passwordDB;
  int hostKeyPair;
}
class SRP6TLSClient {
  int userKeyPair;
  int p;
  int I;
}
class SRP6SaslServer {
}
class SRP6SaslClient {
}
class SRP6KeyAgreement {
  int K;
  int g;
  int N;
  int srp;
  int THREE;
  int HOST_PASSWORD_DB;
  int USER_PASSWORD;
  int USER_IDENTITY;
  int HASH_FUNCTION;
  int GENERATOR;
  int SHARED_MODULUS;
  int SOURCE_OF_RANDOMNESS;
}
class SRP6Host {
  int passwordDB;
  int hostKeyPair;
}
