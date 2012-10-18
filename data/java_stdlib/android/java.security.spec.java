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
  int publicExponent;
  int keysize;
  int F4;
  int F0;
}
class PSSParameterSpec {
  int saltLen;
  int trailerField;
  int mgfSpec;
  int mgfName;
  int mdName;
  int DEFAULT;
}
class PKCS8EncodedKeySpec {
}
class MGF1ParameterSpec {
  int mdName;
  int SHA512;
  int SHA384;
  int SHA256;
  int SHA1;
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
class EllipticCurve {
  int hash;
  int seed;
  int b;
  int a;
  int field;
}
class ECPublicKeySpec {
  int params;
  int w;
}
class ECPrivateKeySpec {
  int params;
  int s;
}
class ECPoint {
  int affineY;
  int affineX;
  int POINT_INFINITY;
}
class ECParameterSpec {
  int cofactor;
  int order;
  int generator;
  int curve;
}
class ECGenParameterSpec {
  int name;
}
class ECFieldFp {
  int p;
}
class ECFieldF2m {
  int ks;
  int rp;
  int m;
  int PPB_LEN;
  int TPB_LEN;
  int PPB_MID_LEN;
  int TPB_MID_LEN;
}
class ECField {
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
