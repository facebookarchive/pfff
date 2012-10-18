package java.util.zip;
class ZipOutputStream {
  int nameBytes;
  int nameLength;
  int curOffset;
  int offset;
  int crc;
  int currentEntry;
  int cDir;
  int compressLevel;
  int compressMethod;
  int entries;
  int comment;
  int ZIPLocalHeaderVersionNeeded;
  int STORED;
  int DEFLATED;
}
class ZipInputStream {
  int charBuf;
  int nameBuf;
  int crc;
  int hdrBuf;
  int currentEntry;
  int lastRead;
  int inRead;
  int entryIn;
  int hasDD;
  int entriesEnd;
  int ZIPLocalHeaderVersionNeeded;
}
class ZipFile {
  class ZipInflaterInputStream {
    int bytesRead;
    int entry;
  }
  class RAFStream {
    int mLength;
    int mOffset;
    int mSharedRaf;
  }
  int guard;
  int mEntries;
  int mRaf;
  int fileToDeleteOnClose;
  int fileName;
  int OPEN_DELETE;
  int OPEN_READ;
  int GPBF_UTF8_FLAG;
  int GPBF_DATA_DESCRIPTOR_FLAG;
}
class ZipException {
  int serialVersionUID;
}
class ZipError {
  int serialVersionUID;
}
class ZipEntry {
  int STORED;
  int DEFLATED;
  int mLocalHeaderRelOffset;
  int nameLength;
  int extra;
  int modDate;
  int time;
  int compressionMethod;
  int size;
  int crc;
  int compressedSize;
  int comment;
  int name;
}
class ZipConstants {
  int ENDCOM;
  int ENDOFF;
  int ENDSIZ;
  int ENDTOT;
  int ENDSUB;
  int CENOFF;
  int CENATX;
  int CENATT;
  int CENDSK;
  int CENCOM;
  int CENEXT;
  int CENNAM;
  int CENLEN;
  int CENSIZ;
  int CENCRC;
  int CENTIM;
  int CENHOW;
  int CENFLG;
  int CENVER;
  int CENVEM;
  int EXTLEN;
  int EXTSIZ;
  int EXTCRC;
  int LOCEXT;
  int LOCNAM;
  int LOCLEN;
  int LOCSIZ;
  int LOCCRC;
  int LOCTIM;
  int LOCHOW;
  int LOCFLG;
  int LOCVER;
  int ENDHDR;
  int CENHDR;
  int EXTHDR;
  int LOCHDR;
  int ENDSIG;
  int CENSIG;
  int EXTSIG;
  int LOCSIG;
}
class InflaterOutputStream {
  int closed;
  int buf;
  int inf;
  int DEFAULT_BUFFER_SIZE;
}
class InflaterInputStream {
  int nativeEndBufSize;
  int BUF_SIZE;
  int eof;
  int closed;
  int len;
  int buf;
  int inf;
}
class Inflater {
  int guard;
  int streamHandle;
  int needsDictionary;
  int finished;
  int inRead;
  int inLength;
}
class GZIPOutputStream {
  int crc;
}
class GZIPInputStream {
  int eos;
  int crc;
  int GZIP_MAGIC;
  int FNAME;
  int FHCRC;
  int FEXTRA;
  int FCOMMENT;
}
class DeflaterOutputStream {
  int syncFlush;
  int done;
  int def;
  int buf;
  int BUF_SIZE;
}
class DeflaterInputStream {
  int available;
  int closed;
  int buf;
  int def;
  int DEFAULT_BUFFER_SIZE;
}
class Deflater {
  int guard;
  int inLength;
  int inRead;
  int inputBuffer;
  int streamHandle;
  int strategy;
  int compressLevel;
  int finished;
  int flushParm;
  int FINISH;
  int FULL_FLUSH;
  int SYNC_FLUSH;
  int NO_FLUSH;
  int NO_COMPRESSION;
  int HUFFMAN_ONLY;
  int FILTERED;
  int DEFLATED;
  int DEFAULT_STRATEGY;
  int DEFAULT_COMPRESSION;
  int BEST_SPEED;
  int BEST_COMPRESSION;
}
class DataFormatException {
  int serialVersionUID;
}
class Checksum {
}
class CheckedOutputStream {
  int check;
}
class CheckedInputStream {
  int check;
}
class CRC32 {
  int tbytes;
  int crc;
}
class Adler32 {
  int adler;
}
