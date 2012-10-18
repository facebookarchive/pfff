package gnu.java.security.hash;
class Whirlpool {
  int w7;
  int w6;
  int w5;
  int w4;
  int w3;
  int w2;
  int w1;
  int w0;
  int nn7;
  int nn6;
  int nn5;
  int nn4;
  int nn3;
  int nn2;
  int nn1;
  int nn0;
  int n7;
  int n6;
  int n5;
  int n4;
  int n3;
  int n2;
  int n1;
  int n0;
  int Kr7;
  int Kr6;
  int Kr5;
  int Kr4;
  int Kr3;
  int Kr2;
  int Kr1;
  int Kr0;
  int k07;
  int k06;
  int k05;
  int k04;
  int k03;
  int k02;
  int k01;
  int k00;
  int H7;
  int H6;
  int H5;
  int H4;
  int H3;
  int H2;
  int H1;
  int H0;
  int valid;
  int rc;
  int T7;
  int T6;
  int T5;
  int T4;
  int T3;
  int T2;
  int T1;
  int T0;
  int S_box;
  int R;
  int DIGEST0;
  int BLOCK_SIZE;
  int log;
}
class Tiger {
  int c;
  int b;
  int a;
  int valid;
  int T4;
  int T3;
  int T2;
  int T1;
  int C;
  int B;
  int A;
  int DIGEST0;
  int BLOCK_SIZE;
  int HASH_SIZE;
}
class Sha512 {
  int h7;
  int h6;
  int h5;
  int h4;
  int h3;
  int h2;
  int h1;
  int h0;
  int valid;
  int w;
  int DIGEST0;
  int BLOCK_SIZE;
  int k;
}
class Sha384 {
  int h7;
  int h6;
  int h5;
  int h4;
  int h3;
  int h2;
  int h1;
  int h0;
  int valid;
  int w;
  int DIGEST0;
  int BLOCK_SIZE;
  int k;
}
class Sha256 {
  int h7;
  int h6;
  int h5;
  int h4;
  int h3;
  int h2;
  int h1;
  int h0;
  int valid;
  int w;
  int DIGEST0;
  int BLOCK_SIZE;
  int k;
}
class Sha160 {
  int h4;
  int h3;
  int h2;
  int h1;
  int h0;
  int valid;
  int w;
  int DIGEST0;
  int BLOCK_SIZE;
}
class RipeMD160 {
  int X;
  int h4;
  int h3;
  int h2;
  int h1;
  int h0;
  int valid;
  int Sp;
  int S;
  int Rp;
  int R;
  int DIGEST0;
  int BLOCK_SIZE;
}
class RipeMD128 {
  int X;
  int h3;
  int h2;
  int h1;
  int h0;
  int valid;
  int Sp;
  int S;
  int Rp;
  int R;
  int DIGEST0;
  int BLOCK_SIZE;
}
class MD5 {
  int h3;
  int h2;
  int h1;
  int h0;
  int valid;
  int DIGEST0;
  int BLOCK_SIZE;
}
class MD4 {
  int d;
  int c;
  int b;
  int a;
  int valid;
  int DIGEST0;
  int D;
  int C;
  int B;
  int A;
  int BLOCK_LENGTH;
  int DIGEST_LENGTH;
}
class MD2 {
  int work;
  int checksum;
  int valid;
  int DIGEST0;
  int PI;
  int BLOCK_LENGTH;
  int DIGEST_LENGTH;
}
class IMessageDigest {
}
class Haval {
  int h7;
  int h6;
  int h5;
  int h4;
  int h3;
  int h2;
  int h1;
  int h0;
  int rounds;
  int valid;
  int DIGEST0;
  int BLOCK_SIZE;
  int HAVAL_5_ROUND;
  int HAVAL_4_ROUND;
  int HAVAL_3_ROUND;
  int HAVAL_256_BIT;
  int HAVAL_224_BIT;
  int HAVAL_192_BIT;
  int HAVAL_160_BIT;
  int HAVAL_128_BIT;
  int HAVAL_VERSION;
}
class HashFactory {
}
class BaseHash {
  int buffer;
  int count;
  int blockSize;
  int hashSize;
  int name;
}
