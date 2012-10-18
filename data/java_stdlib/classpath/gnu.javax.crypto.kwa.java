package gnu.javax.crypto.kwa;
class TripleDESKeyWrap {
  int rnd;
  int sha;
  int modeAttributes;
  int asmAttributes;
  int asm;
  int DEFAULT_IV;
}
class KeyWrappingAlgorithmFactory {
  int names;
}
class KeyUnwrappingException {
}
class IKeyWrappingAlgorithm {
  int SOURCE_OF_RANDOMNESS;
  int INITIAL_VALUE;
  int KEY_ENCRYPTION_KEY_MATERIAL;
}
class BaseKeyWrappingAlgorithm {
  int prng;
  int name;
}
class AESKeyWrap {
  int iv;
  int aes;
  int DEFAULT_IV;
}
