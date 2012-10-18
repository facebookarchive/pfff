package gnu.java.security.sig;
class SignatureFactory {
  int names;
}
class SignatureCodecFactory {
  int names;
}
class ISignatureCodec {
  int RAW_FORMAT;
}
class ISignature {
  int SOURCE_OF_RANDOMNESS;
  int SIGNER_KEY;
  int VERIFIER_KEY;
}
class BaseSignature {
  int prng;
  int irnd;
  int rnd;
  int privateKey;
  int publicKey;
  int md;
  int schemeName;
}
