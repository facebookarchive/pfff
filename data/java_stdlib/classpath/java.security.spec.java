package java.security.spec;
class X509EncodedKeySpec {
}
class RSAPublicKeySpec {
  int publicExponent;
  int modulus;
}
class RSAPrivateKeySpec {
  int privateExponent;
  int modulus;
}
class RSAPrivateCrtKeySpec {
  int crtCoefficient;
  int primeExponentQ;
  int primeExponentP;
  int primeQ;
  int primeP;
  int publicExponent;
}
class RSAOtherPrimeInfo {
  int crtCoefficient;
  int primeExponent;
  int prime;
}
class RSAMultiPrimePrivateCrtKeySpec {
  int otherPrimeInfo;
  int crtCoefficient;
  int primeExponentQ;
  int primeExponentP;
  int primeQ;
  int primeP;
  int publicExponent;
}
class RSAKeyGenParameterSpec {
  int F4;
  int F0;
  int publicExponent;
  int keysize;
}
class PSSParameterSpec {
  int saltLen;
}
class PKCS8EncodedKeySpec {
}
class KeySpec {
}
class InvalidParameterSpecException {
  int serialVersionUID;
}
class InvalidKeySpecException {
  int serialVersionUID;
}
class EncodedKeySpec {
  int encodedKey;
}
class DSAPublicKeySpec {
  int g;
  int q;
  int p;
  int y;
}
class DSAPrivateKeySpec {
  int g;
  int q;
  int p;
  int x;
}
class DSAParameterSpec {
  int g;
  int q;
  int p;
}
class AlgorithmParameterSpec {
}
