package javax.swing.text.rtf;
class Token {
  int type;
  int TEXT;
  int CONTROL_WORD;
  int RCURLY;
  int LCURLY;
  int EOF;
}
class TextToken {
  int text;
}
class RTFScanner {
  int lastToken;
  int buffer;
  int in;
}
class RTFParser {
  int pos;
  int doc;
  int scanner;
}
class RTFParseException {
}
class RTFEditorKit {
}
class ControlWordToken {
  int param;
  int name;
}
