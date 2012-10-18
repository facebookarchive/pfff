package javax.crypto.spec;
class SecretKeySpec {
  int algorithm;
  int key;
  int serialVersionUID;
}
class RC5ParameterSpec {
  int iv;
  int wordSize;
  int rounds;
  int version;
}
class RC2ParameterSpec {
  int iv;
  int effectiveKeyBits;
}
class PSource {
  class PSpecified {
    int DEFAULT;
    int p;
  }
  int pSrcName;
}
class PBEParameterSpec {
  int iterationCount;
  int salt;
}
class PBEKeySpec {
  int keyLength;
  int iterationCount;
  int salt;
  int password;
}
class OAEPParameterSpec {
  int DEFAULT;
  int pSrc;
  int mgfSpec;
  int mgfName;
  int mdName;
}
class IvParameterSpec {
  int iv;
}
class DHPublicKeySpec {
  int g;
  int p;
  int y;
}
class DHPrivateKeySpec {
  int g;
  int p;
  int x;
}
class DHParameterSpec {
  int l;
  int g;
  int p;
}
class DHGenParameterSpec {
  int exponentSize;
  int primeSize;
}
class DESedeKeySpec {
  int key;
  int DES_EDE_KEY_LEN;
}
class DESKeySpec {
  int SEMIWEAKS;
  int key;
  int DES_KEY_LEN;
}
