package gnu.javax.crypto.assembly;
class TransformerException {
  int _exception;
}
class Transformer {
  int outBuffer;
  int inBuffer;
  int tail;
  int mode;
  int wired;
  int DIRECTION;
}
class Stage {
  int wired;
  int forward;
  int DIRECTION;
}
class PaddingTransformer {
  int outputBlockSize;
  int delegate;
}
class Operation {
  int value;
  int POST_PROCESSING;
  int PRE_PROCESSING;
}
class ModeStage {
  int cachedBlockSizes;
  int delegate;
}
class LoopbackTransformer {
}
class Direction {
  int value;
  int REVERSED;
  int FORWARD;
}
class DeflateTransformer {
  int zlibBuffer;
  int outputBlockSize;
  int decompressor;
  int compressor;
}
class CascadeTransformer {
  int blockSize;
  int delegate;
}
class CascadeStage {
  int delegate;
}
class Cascade {
  int blockSize;
  int wired;
  int stageKeys;
  int stages;
  int DIRECTION;
}
class Assembly {
  int head;
  int wired;
  int DIRECTION;
}
