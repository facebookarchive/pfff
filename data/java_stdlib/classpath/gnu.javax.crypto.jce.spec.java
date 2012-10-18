package gnu.javax.crypto.jce.spec;
class UMac32ParameterSpec {
  int nonce;
}
class TMMHParameterSpec {
  int prefix;
  int tagLength;
  int keystream;
}
class BlockCipherParameterSpec {
  int keySize;
  int blockSize;
  int iv;
}
