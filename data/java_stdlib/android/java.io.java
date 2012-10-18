package java.io;
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
  int buf;
}
class StringReader {
  int count;
  int pos;
  int markpos;
  int str;
}
class StringBufferInputStream {
  int pos;
  int count;
  int buffer;
}
class StreamTokenizer {
  int peekChar;
  int inReader;
  int inStream;
  int lastCr;
  int pushBackToken;
  int slashSlashComments;
  int slashStarComments;
  int isEOLSignificant;
  int forceLowercase;
  int lineNumber;
  int TOKEN_DIGIT;
  int TOKEN_WORD;
  int TOKEN_WHITE;
  int TOKEN_QUOTE;
  int TOKEN_COMMENT;
  int tokenTypes;
  int ttype;
  int TT_UNKNOWN;
  int TT_WORD;
  int TT_NUMBER;
  int TT_EOL;
  int TT_EOF;
  int sval;
  int nval;
}
class StreamCorruptedException {
  int serialVersionUID;
}
class SerializationHandleMap {
  int threshold;
  int size;
  int values;
  int keys;
  int LOAD_FACTOR;
}
class SerializablePermission {
}
class Serializable {
}
class SequenceInputStream {
  int in;
  int e;
}
class Reader {
  int lock;
}
class RandomAccessFile {
  int scratch;
  int guard;
  int mode;
  int channel;
  int syncMetadata;
  int fd;
}
class PushbackReader {
  int pos;
  int buf;
}
class PushbackInputStream {
  int pos;
  int buf;
}
class PrintWriter {
  int autoFlush;
  int ioError;
  int out;
}
class PrintStream {
  int encoding;
  int autoFlush;
  int ioError;
}
class PipedWriter {
  int isClosed;
  int destination;
}
class PipedReader {
  int isConnected;
  int PIPE_SIZE;
  int out;
  int in;
  int buffer;
  int isClosed;
  int lastWriter;
  int lastReader;
}
class PipedOutputStream {
  int target;
}
class PipedInputStream {
  int isConnected;
  int PIPE_SIZE;
  int out;
  int in;
  int buffer;
  int isClosed;
  int lastWriter;
  int lastReader;
}
class OutputStreamWriter {
  int bytes;
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
  int isDeserialized;
  int unshared;
  int typeString;
  int offset;
  int type;
  int name;
}
class ObjectStreamException {
  int serialVersionUID;
}
class ObjectStreamConstants {
  int SC_ENUM;
  int TC_ENUM;
  int SC_BLOCK_DATA;
  int SC_EXTERNALIZABLE;
  int SC_SERIALIZABLE;
  int SC_WRITE_METHOD;
  int SUBSTITUTION_PERMISSION;
  int SUBCLASS_IMPLEMENTATION_PERMISSION;
  int PROTOCOL_VERSION_2;
  int PROTOCOL_VERSION_1;
  int baseWireHandle;
  int TC_MAX;
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
  int TC_BASE;
  int STREAM_VERSION;
  int STREAM_MAGIC;
}
class ObjectStreamClass {
  int storage;
  int cachedHierarchy;
  int constructor;
  int reflectionFields;
  int loadFields;
  int fields;
  int superclass;
  int flags;
  int svUID;
  int resolvedConstructorMethodId;
  int resolvedConstructorClass;
  int resolvedClass;
  int className;
  int isEnum;
  int isProxy;
  int isExternalizable;
  int isSerializable;
  int arePropertiesResolved;
  int methodReadObjectNoData;
  int methodReadObject;
  int methodWriteObject;
  int methodReadResolve;
  int methodWriteReplace;
  int OBJECTSTREAMCLASSCLASS;
  int CLASSCLASS;
  int STRINGCLASS;
  int EXTERNALIZABLE;
  int SERIALIZABLE;
  int CLINIT_SIGNATURE;
  int CLINIT_MODIFIERS;
  int CLINIT_NAME;
  int ARRAY_OF_FIELDS;
  int NO_FIELDS;
  int WRITE_PARAM_TYPES;
  int READ_PARAM_TYPES;
  int METHOD_MODIFIERS_MASK;
  int FIELD_MODIFIERS_MASK;
  int CLASS_MODIFIERS_MASK;
  int CONSTRUCTOR_IS_NOT_RESOLVED;
  int UID_FIELD_NAME;
  int serialVersionUID;
}
class ObjectOutputStream {
  class PutField {
  }
  int proxyClassDesc;
  int subclassOverridingImplementation;
  int currentPutField;
  int nestedException;
  int protocolVersion;
  int currentClass;
  int currentObject;
  int currentHandle;
  int objectsWritten;
  int primitiveTypesBuffer;
  int primitiveTypes;
  int enableReplace;
  int output;
  int nestedLevels;
  int NOT_SC_BLOCK_DATA;
  int WRITE_UNSHARED_PARAM_TYPES;
}
class ObjectOutput {
}
class ObjectInputValidation {
}
class ObjectInputStream {
  int systemLoader;
  int bootstrapLoader;
  int cachedSuperclasses;
  class GetField {
  }
  class InputValidationDesc {
    int priority;
    int validator;
  }
  int PRIMITIVE_CLASSES;
  int descriptorHandle;
  int mustResolve;
  int callerClassLoader;
  int subclassOverridingImplementation;
  int validations;
  int currentClass;
  int currentObject;
  int objectsRead;
  int enableResolve;
  int primitiveData;
  int primitiveTypes;
  int input;
  int nextHandle;
  int nestedLevels;
  int pushbackTC;
  int hasPushbackTC;
  int UNSHARED_OBJ;
  int emptyStream;
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
  int markedLastWasCR;
  int lastWasCR;
  int markedLineNumber;
  int lineNumber;
}
class LineNumberInputStream {
  int markedLastChar;
  int lastChar;
  int markedLineNumber;
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
  int bytes;
  int decoder;
  int endOfInput;
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
class HistoricalCharsetNames {
  int historicalNames;
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
}
class FileOutputStream {
  int guard;
  int mode;
  int channel;
  int shouldClose;
  int fd;
}
class FileNotFoundException {
  int serialVersionUID;
}
class FileInputStream {
  int guard;
  int channel;
  int shouldClose;
  int fd;
}
class FileFilter {
}
class FileDescriptor {
  int descriptor;
  int err;
  int out;
  int in;
}
class File {
  int path;
  int pathSeparator;
  int pathSeparatorChar;
  int separator;
  int separatorChar;
  int tempFileRandom;
  int serialVersionUID;
}
class Externalizable {
}
class EmulatedFieldsForLoading {
  int emulatedFields;
  int streamClass;
}
class EmulatedFieldsForDumping {
  int emulatedFields;
  int oos;
}
class EmulatedFields {
  int declaredFields;
  int slotsToSerialize;
  class ObjectSlot {
    int defaulted;
    int fieldValue;
    int field;
  }
}
class EOFException {
  int serialVersionUID;
}
class DataOutputStream {
  int written;
  int scratch;
}
class DataOutput {
}
class DataInputStream {
  int scratch;
}
class DataInput {
}
class Console {
  class ConsoleWriter {
  }
  class ConsoleReader {
  }
  int writer;
  int reader;
  int console;
  int CONSOLE_LOCK;
}
class Closeable {
}
class CharConversionException {
  int serialVersionUID;
}
class CharArrayWriter {
  int count;
  int buf;
}
class CharArrayReader {
  int count;
  int markedPos;
  int pos;
  int buf;
}
class ByteArrayOutputStream {
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
  int pos;
  int buf;
  int out;
}
class BufferedReader {
  int markLimit;
  int mark;
  int end;
  int pos;
  int buf;
  int in;
}
class BufferedOutputStream {
  int count;
  int buf;
}
class BufferedInputStream {
  int pos;
  int markpos;
  int marklimit;
  int count;
  int buf;
}
