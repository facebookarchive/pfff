package java.nio;
class SocketChannelImpl {
  class SocketChannelInputStream {
    int channel;
  }
  class SocketChannelOutputStream {
    int channel;
  }
  class SocketAdapter {
    int socketImpl;
    int channel;
  }
  int writeLock;
  int readLock;
  int isBound;
  int status;
  int localPort;
  int localAddress;
  int connectAddress;
  int socket;
  int fd;
  int SOCKET_STATUS_CLOSED;
  int SOCKET_STATUS_CONNECTED;
  int SOCKET_STATUS_PENDING;
  int SOCKET_STATUS_UNCONNECTED;
  int SOCKET_STATUS_UNINITIALIZED;
}
class ShortToByteBufferAdapter {
  int byteBuffer;
}
class ShortBuffer {
}
class ShortArrayBuffer {
  int offset;
  int backingArray;
}
class ServerSocketChannelImpl {
  class ServerSocketAdapter {
    int channelImpl;
  }
  int acceptLock;
  int isBound;
  int impl;
  int socket;
}
class SelectorProviderImpl {
}
class SelectorImpl {
  class UnaddableSet {
    int set;
  }
  int pollFds;
  int wakeupOut;
  int wakeupIn;
  int selectedKeys;
  int mutableSelectedKeys;
  int unmodifiableKeys;
  int mutableKeys;
  int keysLock;
}
class SelectionKeyImpl {
  int selector;
  int readyOps;
  int interestOps;
  int channel;
}
class ReadWriteShortArrayBuffer {
}
class ReadWriteLongArrayBuffer {
}
class ReadWriteIntArrayBuffer {
}
class ReadWriteHeapByteBuffer {
}
class ReadWriteFloatArrayBuffer {
}
class ReadWriteDoubleArrayBuffer {
}
class ReadWriteDirectByteBuffer {
}
class ReadWriteCharArrayBuffer {
}
class ReadOnlyShortArrayBuffer {
}
class ReadOnlyLongArrayBuffer {
}
class ReadOnlyIntArrayBuffer {
}
class ReadOnlyHeapByteBuffer {
}
class ReadOnlyFloatArrayBuffer {
}
class ReadOnlyDoubleArrayBuffer {
}
class ReadOnlyDirectByteBuffer {
}
class ReadOnlyCharArrayBuffer {
}
class ReadOnlyBufferException {
  int serialVersionUID;
}
class PipeImpl {
  class PipeSinkChannel {
    int channel;
    int fd;
  }
  class PipeSourceChannel {
    int channel;
    int fd;
  }
  class FdCloser {
    int fd;
  }
  int source;
  int sink;
}
class NioUtils {
}
class NIOAccess {
}
class MemoryBlock {
  int size;
  int address;
  class UnmanagedBlock {
  }
  class NonMovableHeapBlock {
    int array;
  }
  class MemoryMappedBlock {
  }
}
class MappedByteBufferAdapter {
}
class MappedByteBuffer {
  int mapMode;
  int wrapped;
}
class LongToByteBufferAdapter {
  int byteBuffer;
}
class LongBuffer {
}
class LongArrayBuffer {
  int offset;
  int backingArray;
}
class IoVec {
  int direction;
  int byteCounts;
  int offsets;
  int ioBuffers;
  int bufferCount;
  int offset;
  int byteBuffers;
  class Direction {
    int WRITEV;
    int READV;
  }
}
class InvalidMarkException {
  int serialVersionUID;
}
class IntToByteBufferAdapter {
  int byteBuffer;
}
class IntBuffer {
}
class IntArrayBuffer {
  int offset;
  int backingArray;
}
class HeapByteBuffer {
  int offset;
  int backingArray;
}
class FloatToByteBufferAdapter {
  int byteBuffer;
}
class FloatBuffer {
}
class FloatArrayBuffer {
  int offset;
  int backingArray;
}
class FileDescriptorChannel {
}
class FileChannelImpl {
  class FileLockImpl {
    int isReleased;
  }
  int locks;
  int mode;
  int fd;
  int stream;
  int LOCK_COMPARATOR;
}
class DoubleToByteBufferAdapter {
  int byteBuffer;
}
class DoubleBuffer {
}
class DoubleArrayBuffer {
  int offset;
  int backingArray;
}
class DirectByteBuffer {
  int offset;
}
class DatagramChannelImpl {
  class DatagramSocketAdapter {
    int channelImpl;
  }
  int writeLock;
  int readLock;
  int isBound;
  int connected;
  int localPort;
  int connectAddress;
  int socket;
  int fd;
}
class CharToByteBufferAdapter {
  int byteBuffer;
}
class CharSequenceAdapter {
  int sequence;
}
class CharBuffer {
}
class CharArrayBuffer {
  int offset;
  int backingArray;
}
class ByteOrder {
  int needsSwap;
  int name;
  int LITTLE_ENDIAN;
  int BIG_ENDIAN;
  int NATIVE_ORDER;
}
class ByteBuffer {
  int order;
}
class BufferUnderflowException {
  int serialVersionUID;
}
class BufferOverflowException {
  int serialVersionUID;
}
class Buffer {
  int block;
  int effectiveDirectAddress;
  int _elementSizeShift;
  int position;
  int mark;
  int limit;
  int capacity;
  int UNSET_MARK;
}
class BaseByteBuffer {
}
