package java.io;
class VMObjectStreamClass {
}
class VMObjectInputStream {
}
class VMFile {
  int IS_DOS_8_3;
  int IS_CASE_SENSITIVE;
}
class VMConsole {
}
class Writer {
  int lock;
}
class WriteAbortedException {
  int detail;
  int serialVersionUID;
}
class UnsupportedEncodingException {
  int serialVersionUID;
}
class UTFDataFormatException {
  int serialVersionUID;
}
class SyncFailedException {
  int serialVersionUID;
}
class StringWriter {
  int buffer;
  int DEFAULT_BUFFER_SIZE;
}
class StringReader {
  int count;
  int markedPos;
  int pos;
  int buf;
}
class StringBufferInputStream {
  int count;
  int pos;
  int buffer;
}
class StreamTokenizer {
  int lineNumber;
  int pushedBack;
  int in;
  int comment;
  int quote;
  int numeric;
  int alphabetic;
  int whitespace;
  int slashStar;
  int slashSlash;
  int lowerCase;
  int eolSignificant;
  int nval;
  int sval;
  int ttype;
  int TT_NONE;
  int TT_WORD;
  int TT_NUMBER;
  int TT_EOL;
  int TT_EOF;
}
class StreamCorruptedException {
  int serialVersionUID;
}
class SerializablePermission {
  int legal_names;
  int serialVersionUID;
}
class Serializable {
}
class SequenceInputStream {
  int e;
  int in2;
  int in;
}
class Reader {
  int lock;
}
class RandomAccessFile {
  int in;
  int out;
  int fd;
  int ch;
}
class PushbackReader {
  int pos;
  int buf;
  int DEFAULT_BUFFER_SIZE;
}
class PushbackInputStream {
  int pos;
  int buf;
  int DEFAULT_BUFFER_SIZE;
}
class PrintWriter {
  int line_separator;
  int out;
  int closed;
  int error;
  int autoflush;
}
class PrintStream {
  int auto_flush;
  int error_occurred;
  int encoding;
  int line_separator;
}
class PipedWriter {
  int read_buf;
  int closed;
  int sink;
}
class PipedReader {
  int read_buf;
  int out;
  int in;
  int buffer;
  int PIPE_SIZE;
  int closed;
  int source;
}
class PipedOutputStream {
  int closed;
  int sink;
}
class PipedInputStream {
  int read_buf;
  int out;
  int in;
  int buffer;
  int PIPE_SIZE;
  int closed;
  int source;
}
class OutputStreamWriter {
  int BUFFER_SIZE;
  int outputBuffer;
  int encodingName;
  int encoder;
  int out;
}
class OutputStream {
}
class OptionalDataException {
  int length;
  int eof;
  int serialVersionUID;
}
class ObjectStreamField {
  int field;
  int toset;
  int persistent;
  int unshared;
  int offset;
  int typename;
  int type;
  int name;
}
class ObjectStreamException {
  int serialVersionUID;
}
class ObjectStreamConstants {
  int SUBCLASS_IMPLEMENTATION_PERMISSION;
  int SUBSTITUTION_PERMISSION;
  int SC_ENUM;
  int SC_BLOCK_DATA;
  int SC_EXTERNALIZABLE;
  int SC_SERIALIZABLE;
  int SC_WRITE_METHOD;
  int baseWireHandle;
  int TC_MAX;
  int TC_BASE;
  int TC_ENUM;
  int TC_PROXYCLASSDESC;
  int TC_LONGSTRING;
  int TC_EXCEPTION;
  int TC_BLOCKDATALONG;
  int TC_RESET;
  int TC_ENDBLOCKDATA;
  int TC_BLOCKDATA;
  int TC_CLASS;
  int TC_ARRAY;
  int TC_STRING;
  int TC_OBJECT;
  int TC_CLASSDESC;
  int TC_REFERENCE;
  int TC_NULL;
  int STREAM_VERSION;
  int STREAM_MAGIC;
  int PROTOCOL_VERSION_2;
  int PROTOCOL_VERSION_1;
}
class ObjectStreamClass {
  class MemberComparator {
  }
  class InterfaceComparator {
  }
  int serialVersionUID;
  int isProxyClass;
  int constructor;
  int firstNonSerializableParentConstructor;
  int fieldMapping;
  int realClassIsExternalizable;
  int realClassIsSerializable;
  int writeObjectMethod;
  int writeReplaceMethod;
  int readResolveMethod;
  int readObjectMethod;
  int objectFieldCount;
  int primFieldSize;
  int fields;
  int flags;
  int uid;
  int name;
  int clazz;
  int superClass;
  int writeMethodArgTypes;
  int memberComparator;
  int interfaceComparator;
  int nullOutputStream;
  int classLookupTable;
  int NO_FIELDS;
  int uidCache;
  int writeObjectSignature;
  int readObjectSignature;
  int methodCache;
  int noArgs;
  int hierarchy;
  int INVALID_FIELDS;
}
class ObjectOutputStream {
  int DEBUG;
  int dump;
  int depth;
  int setAccessible;
  int useSubclassMethod;
  int protocolVersion;
  int OIDLookupTable;
  int nextOID;
  int isSerializing;
  int replacementEnabled;
  int fieldsAlreadyWritten;
  int currentPutField;
  int currentObjectStreamClass;
  int currentObject;
  int blockDataCount;
  int blockData;
  int blockDataOutput;
  int realOutput;
  int writeDataAsBlocks;
  int dataOutput;
  int defaultProtocolVersion;
  int BUFFER_SIZE;
  class PutField {
  }
}
class ObjectOutput {
}
class ObjectInputValidation {
}
class ObjectInputStream {
  class ValidatorAndPriority {
    int validator;
    int priority;
  }
  int DEBUG;
  int depth;
  int dump;
  int prereadFields;
  int classLookupTable;
  int fieldsAlreadyRead;
  int readDataFromBlock;
  int currentObjectValidators;
  int currentObjectStreamClass;
  int currentObject;
  int handles;
  int resolveEnabled;
  int nextOID;
  int useSubclassMethod;
  int blockData;
  int blockDataBytes;
  int blockDataPosition;
  int blockDataInput;
  int dataInputStream;
  int realInputStream;
  int BUFFER_SIZE;
  class GetField {
  }
}
class ObjectInput {
}
class NotSerializableException {
  int serialVersionUID;
}
class NotActiveException {
  int serialVersionUID;
}
class LineNumberReader {
  int savedLineNumber;
  int matchedNewLine;
  int lineNumber;
}
class LineNumberInputStream {
  int justReadReturnChar;
  int markLineNumber;
  int lineNumber;
}
class InvalidObjectException {
  int serialVersionUID;
}
class InvalidClassException {
  int classname;
  int serialVersionUID;
}
class InterruptedIOException {
  int bytesTransferred;
  int serialVersionUID;
}
class InputStreamReader {
  int cacheLock;
  int bytesCache;
  int hasSavedSurrogate;
  int savedSurrogate;
  int encoding;
  int byteBuffer;
  int maxBytesPerChar;
  int isDone;
  int decoder;
  int in;
}
class InputStream {
}
class IOException {
  int serialVersionUID;
}
class IOError {
  int serialVersionUID;
}
class Flushable {
}
class FilterWriter {
  int out;
}
class FilterReader {
  int in;
}
class FilterOutputStream {
  int out;
}
class FilterInputStream {
  int in;
}
class FilenameFilter {
}
class FileWriter {
}
class FileReader {
}
class FilePermission {
  int actionsString;
  int deletePerm;
  int executePerm;
  int writePerm;
  int readPerm;
  int ALL_FILES;
  int serialVersionUID;
}
class FileOutputStream {
  int ch;
  int fd;
}
class FileNotFoundException {
  int serialVersionUID;
}
class FileInputStream {
  int ch;
  int fd;
}
class FileFilter {
}
class FileDescriptor {
  int channel;
  int err;
  int out;
  int in;
}
class File {
  int n_created;
  int last_tmp;
  int path;
  int pathSeparatorChar;
  int pathSeparator;
  int separatorChar;
  int dupSeparator;
  int separator;
  int serialVersionUID;
}
class Externalizable {
}
class EOFException {
  int serialVersionUID;
}
class DeleteFileHelper {
  int filesToDelete;
}
class DataOutputStream {
  int buf;
  int written;
}
class DataOutput {
}
class DataInputStream {
  int buf;
}
class DataInput {
}
class Console {
  int console;
}
class Closeable {
}
class CharConversionException {
  int serialVersionUID;
}
class CharArrayWriter {
  int count;
  int buf;
  int DEFAULT_INITIAL_BUFFER_SIZE;
}
class CharArrayReader {
  int count;
  int markedPos;
  int pos;
  int buf;
}
class ByteArrayOutputStream {
  int initial_buffer_size;
  int DEFAULT_INITIAL_BUFFER_SIZE;
  int count;
  int buf;
}
class ByteArrayInputStream {
  int count;
  int mark;
  int pos;
  int buf;
}
class BufferedWriter {
  int count;
  int buffer;
  int out;
  int DEFAULT_BUFFER_SIZE;
}
class BufferedReader {
  int DEFAULT_BUFFER_SIZE;
  int markPos;
  int limit;
  int pos;
  int buffer;
  int in;
}
class BufferedOutputStream {
  int count;
  int buf;
  int DEFAULT_BUFFER_SIZE;
}
class BufferedInputStream {
  int bufferSize;
  int marklimit;
  int markpos;
  int pos;
  int count;
  int buf;
  int DEFAULT_BUFFER_SIZE;
}
