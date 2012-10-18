package gnu.java.net.protocol.ftp;
class StreamOutputStream {
}
class StreamInputStream {
}
class PassiveModeDTP {
  int transferMode;
  int inProgress;
  int completed;
  int out;
  int in;
  int socket;
  int port;
  int address;
}
class Handler {
}
class FTPURLConnection {
  class ClosingOutputStream {
  }
  class ClosingInputStream {
  }
  int transferMode;
  int fileStructure;
  int representationType;
  int passive;
  int connection;
}
class FTPResponse {
  int data;
  int message;
  int code;
}
class FTPException {
  int response;
}
class FTPConnection {
  int passive;
  int transferMode;
  int fileStructure;
  int representationType;
  int dtp;
  int debug;
  int timeout;
  int connectionTimeout;
  int out;
  int in;
  int socket;
  int US_ASCII;
  int MODE_COMPRESSED;
  int MODE_BLOCK;
  int MODE_STREAM;
  int STRUCTURE_PAGE;
  int STRUCTURE_RECORD;
  int STRUCTURE_FILE;
  int TYPE_BINARY;
  int TYPE_EBCDIC;
  int TYPE_ASCII;
  int TLS;
  int CCC;
  int PROT;
  int PBSZ;
  int AUTH;
  int NOOP;
  int HELP;
  int STAT;
  int SYST;
  int SITE;
  int NLST;
  int LIST;
  int PWD;
  int MKD;
  int RMD;
  int DELE;
  int ABOR;
  int RNTO;
  int RNFR;
  int REST;
  int ALLO;
  int APPE;
  int STOU;
  int STOR;
  int RETR;
  int MODE;
  int STRU;
  int TYPE;
  int PASV;
  int PORT;
  int QUIT;
  int REIN;
  int SMNT;
  int CDUP;
  int CWD;
  int ACCT;
  int PASS;
  int USER;
  int FTP_DATA_PORT;
  int FTP_PORT;
}
class DTPOutputStream {
  int transferComplete;
  int dtp;
}
class DTPInputStream {
  int transferComplete;
  int dtp;
}
class DTP {
}
class CompressedOutputStream {
  int EOF;
  int RECORD;
}
class CompressedInputStream {
  int n;
  int rep;
  int state;
  int count;
  int max;
  int descriptor;
  int FILLER;
  int COMPRESSED;
  int RAW;
  int EOF;
}
class BlockOutputStream {
  int EOF;
  int RECORD;
}
class BlockInputStream {
  int count;
  int max;
  int descriptor;
  int EOF;
}
class ActiveModeDTP {
  int connectionTimeout;
  int acceptThread;
  int exception;
  int transferMode;
  int inProgress;
  int completed;
  int out;
  int in;
  int socket;
  int server;
}
