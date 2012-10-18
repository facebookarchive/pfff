package gnu.javax.swing.text.html.parser.support.low;
class pattern {
  int nodes;
}
class node {
  int kind;
  int optional;
}
class Token {
  int charImage;
  int stringImage;
  int kind;
  int category;
  int where;
}
class ReaderTokenizer {
  int readerPosition;
  int stringTokens;
  int charTokens;
  int reader;
  int queue;
  int backup;
  int buffer;
  int backupMode;
  int advanced;
}
class Queue {
  int b;
  int a;
  int m;
}
class ParseException {
}
class Location {
  int startPosition;
  int endPosition;
  int endLine;
  int beginLine;
}
class Constants {
  int ENTITY_NUMERIC;
  int ENTITY_NAMED;
  int bNAME;
  int bQUOTING;
  int bWHITESPACE;
  int bLINEBREAK;
  int bDIGIT;
  int bLETTER;
  int bSPECIAL;
  int bSINGLE_CHAR_TOKEN;
  int EOF;
  int ETX;
  int OTHER;
  int TAG_CLOSE;
  int TAG;
  int STYLE_CLOSE;
  int SCRIPT_CLOSE;
  int SGML;
  int SCRIPT_OPEN;
  int STYLE_OPEN;
  int COMMENT_TRIPLEDASH_END;
  int COMMENT_END;
  int COMMENT_OPEN;
  int NUMTOKEN;
  int ENTITY;
  int WS;
  int SCRIPT;
  int STYLE;
  int DOUBLE_DASH;
  int QUOT;
  int AP;
  int EQ;
  int SLASH;
  int EXCLAMATION;
  int END;
  int BEGIN;
}
class Buffer {
  int length;
  int current_line;
  int position;
  int line;
  int chr;
  int r_seen;
  int n_seen;
  int INITIAL_SIZE;
}
