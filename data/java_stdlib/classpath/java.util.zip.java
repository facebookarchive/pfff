package java.util.zip;
class ZipOutputStream {
  int DEFLATED;
  int STORED;
  int ZIP_DEFLATED_VERSION;
  int ZIP_STORED_VERSION;
  int defaultMethod;
  int zipComment;
  int offset;
  int size;
  int curMethod;
  int curEntry;
  int crc;
  int entries;
}
class ZipInputStream {
  int entryAtEOF;
  int avail;
  int flags;
  int method;
  int size;
  int csize;
  int entry;
  int crc;
}
class ZipFile {
  class PartialInputStream {
    int dummyByteCount;
    int end;
    int pos;
    int bufferOffset;
    int buffer;
    int raf;
    int utf8Decoder;
    int UTF8CHARSET;
  }
  class ZipEntryEnumeration {
    int elements;
  }
  int closed;
  int entries;
  int raf;
  int name;
  int ENDNRD;
  int OPEN_DELETE;
  int OPEN_READ;
}
class ZipException {
  int serialVersionUID;
}
class ZipEntry {
  int DEFLATED;
  int STORED;
  int offset;
  int flags;
  int extra;
  int time;
  int dostime;
  int known;
  int method;
  int comment;
  int crc;
  int compressedSize;
  int size;
  int name;
  int KNOWN_EXTRA;
  int KNOWN_DOSTIME;
  int KNOWN_TIME;
  int KNOWN_CRC;
  int KNOWN_CSIZE;
  int KNOWN_SIZE;
}
class ZipConstants {
  int ENDCOM;
  int ENDOFF;
  int ENDSIZ;
  int ENDTOT;
  int ENDSUB;
  int ENDHDR;
  int ENDSIG;
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
  int CENHDR;
  int CENSIG;
  int EXTLEN;
  int EXTSIZ;
  int EXTCRC;
  int EXTHDR;
  int EXTSIG;
  int LOCEXT;
  int LOCNAM;
  int LOCLEN;
  int LOCSIZ;
  int LOCCRC;
  int LOCTIM;
  int LOCHOW;
  int LOCFLG;
  int LOCVER;
  int LOCSIG;
  int LOCHDR;
}
class StreamManipulator {
  int bits_in_buffer;
  int buffer;
  int window_end;
  int window_start;
  int window;
}
class PendingBuffer {
  int bitCount;
  int bits;
  int end;
  int start;
  int buf;
}
class OutputWindow {
  int window_filled;
  int window_end;
  int window;
  int WINDOW_MASK;
  int WINDOW_SIZE;
}
class InflaterInputStream {
  int onebytebuffer;
  int len;
  int buf;
  int inf;
}
class InflaterHuffmanTree {
  int defDistTree;
  int defLitLenTree;
  int tree;
  int MAX_BITLEN;
}
class InflaterDynHeader {
  int BL_ORDER;
  int ptr;
  int lastLen;
  int repSymbol;
  int num;
  int blnum;
  int dnum;
  int lnum;
  int mode;
  int blTree;
  int litdistLens;
  int blLens;
  int repBits;
  int repMin;
  int REPS;
  int LENS;
  int BLLENS;
  int BLNUM;
  int DNUM;
  int LNUM;
}
class Inflater {
  int adler;
  int distTree;
  int litlenTree;
  int dynHeader;
  int outputWindow;
  int input;
  int nowrap;
  int totalIn;
  int totalOut;
  int isLastBlock;
  int uncomprLen;
  int repDist;
  int repLength;
  int neededBits;
  int readAdler;
  int mode;
  int FINISHED;
  int DECODE_CHKSUM;
  int DECODE_HUFFMAN_DISTBITS;
  int DECODE_HUFFMAN_DIST;
  int DECODE_HUFFMAN_LENBITS;
  int DECODE_HUFFMAN;
  int DECODE_DYN_HEADER;
  int DECODE_STORED;
  int DECODE_STORED_LEN2;
  int DECODE_STORED_LEN1;
  int DECODE_BLOCKS;
  int DECODE_DICT;
  int DECODE_HEADER;
  int CPDEXT;
  int CPDIST;
  int CPLEXT;
  int CPLENS;
}
class GZIPOutputStream {
  int crc;
}
class GZIPInputStream {
  int readGZIPHeader;
  int eos;
  int crc;
  int FCOMMENT;
  int FNAME;
  int FEXTRA;
  int FHCRC;
  int FTEXT;
  int GZIP_MAGIC;
}
class DeflaterPending {
}
class DeflaterOutputStream {
  int def;
  int buf;
}
class DeflaterHuffman {
  int staticDLength;
  int staticDCodes;
  int staticLLength;
  int staticLCodes;
  int extra_bits;
  int last_lit;
  int l_buf;
  int d_buf;
  int blTree;
  int distTree;
  int literalTree;
  int pending;
  class Tree {
    int maxLength;
    int numCodes;
    int minNumCodes;
    int bl_counts;
    int length;
    int codes;
    int freqs;
  }
  int bit4Reverse;
  int BL_ORDER;
  int EOF_SYMBOL;
  int REP_11_138;
  int REP_3_10;
  int REP_3_6;
  int BITLEN_NUM;
  int DIST_NUM;
  int LITERAL_NUM;
  int BUFSIZE;
}
class DeflaterEngine {
  int adler;
  int huffman;
  int pending;
  int inputEnd;
  int inputOff;
  int totalIn;
  int inputBuf;
  int comprFunc;
  int goodLength;
  int niceLength;
  int max_lazy;
  int max_chain;
  int strategy;
  int window;
  int lookahead;
  int strstart;
  int blockStart;
  int prevAvailable;
  int matchLen;
  int matchStart;
  int prev;
  int head;
  int ins_h;
  int TOO_FAR;
}
class DeflaterConstants {
  int COMPR_FUNC;
  int MAX_CHAIN;
  int NICE_LENGTH;
  int MAX_LAZY;
  int GOOD_LENGTH;
  int DEFLATE_SLOW;
  int DEFLATE_FAST;
  int DEFLATE_STORED;
  int MAX_BLOCK_SIZE;
  int PENDING_BUF_SIZE;
  int MAX_DIST;
  int MIN_LOOKAHEAD;
  int HASH_SHIFT;
  int HASH_MASK;
  int HASH_SIZE;
  int HASH_BITS;
  int WMASK;
  int WSIZE;
  int MAX_WBITS;
  int MIN_MATCH;
  int MAX_MATCH;
  int DEFAULT_MEM_LEVEL;
  int PRESET_DICT;
  int DYN_TREES;
  int STATIC_TREES;
  int STORED_BLOCK;
  int DEBUGGING;
}
class Deflater {
  int engine;
  int pending;
  int totalOut;
  int state;
  int noHeader;
  int level;
  int CLOSED_STATE;
  int FINISHED_STATE;
  int FINISHING_STATE;
  int FLUSHING_STATE;
  int BUSY_STATE;
  int SETDICT_FINISHING_STATE;
  int INIT_FINISHING_STATE;
  int SETDICT_STATE;
  int INIT_STATE;
  int IS_FINISHING;
  int IS_FLUSHING;
  int IS_SETDICT;
  int DEFLATED;
  int HUFFMAN_ONLY;
  int FILTERED;
  int DEFAULT_STRATEGY;
  int NO_COMPRESSION;
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
  int sum;
}
class CheckedInputStream {
  int sum;
}
class CRC32 {
  int crc_table;
  int crc;
}
class Adler32 {
  int checksum;
  int BASE;
}
