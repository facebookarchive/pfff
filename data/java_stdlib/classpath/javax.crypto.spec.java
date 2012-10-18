package javax.crypto.spec;
class SecretKeySpec {
  int algorithm;
  int key;
  int serialVersionUID;
}
class RC5ParameterSpec {
  int wordSize;
  int version;
  int rounds;
  int iv;
}
class RC2ParameterSpec {
  int iv;
  int effectiveKeyBits;
  int RC2_IV_LENGTH;
}
class PBEParameterSpec {
  int salt;
  int iterationCount;
}
class PBEKeySpec {
  int passwordValid;
  int salt;
  int password;
  int keyLength;
  int iterationCount;
}
class IvParameterSpec {
  int iv;
}
class DHPublicKeySpec {
  int y;
  int p;
  int g;
}
class DHPrivateKeySpec {
  int x;
  int p;
  int g;
}
class DHParameterSpec {
  int l;
  int p;
  int g;
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
  int WEAK_KEYS;
  int key;
  int DES_KEY_LEN;
}
