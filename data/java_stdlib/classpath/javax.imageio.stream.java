package javax.imageio.stream;
class MemoryCacheImageOutputStream {
  int stream;
}
class MemoryCacheImageInputStream {
  int READLIMIT;
  int buffer;
  int stream;
}
class ImageOutputStreamImpl {
}
class ImageOutputStream {
}
class ImageInputStreamImpl {
  int streamPos;
  int flushedPos;
  int byteOrder;
  int bitOffset;
  int buffer;
  int markStack;
  int closed;
}
class ImageInputStream {
}
class IIOByteBuffer {
  int length;
  int offset;
  int data;
}
class FileImageOutputStream {
  int file;
}
class FileImageInputStream {
  int file;
}
class FileCacheImageOutputStream {
  int maxPos;
  int cache;
  int cacheFile;
  int stream;
}
class FileCacheImageInputStream {
  int cacheDir;
  int stream;
}
