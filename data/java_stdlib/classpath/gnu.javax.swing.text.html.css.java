package gnu.javax.swing.text.html.css;
class Selector {
  int implicit;
  int specificity;
  int classes;
  int ids;
  int elements;
  int selector;
}
class Length {
  int exBase;
  int emBase;
  int isFontEXRelative;
  int isFontEMRelative;
  int isPercentage;
  int floatValue;
  int value;
}
class FontWeight {
  int value;
}
class FontStyle {
  int value;
}
class FontSize {
  int SCALE;
  int DEFAULT_FONT_SIZE;
  int isRelative;
  int sizeIndex;
  int size;
  int value;
}
class CSSScanner {
  int lookahead;
  int tokenEnd;
  int parseBuffer;
  int in;
  int EOF;
  int DELIM;
  int DASHMATCH;
  int INCLUDES;
  int FUNCTION;
  int COMMENT;
  int S;
  int BRACE_RIGHT;
  int BRACE_LEFT;
  int PAREN_RIGHT;
  int PAREN_LEFT;
  int CURLY_RIGHT;
  int CURLY_LEFT;
  int SEMICOLON;
  int CDC;
  int CDO;
  int UNICODE_RANGE;
  int URI;
  int DIMENSION;
  int PERCENTAGE;
  int NUMBER;
  int HASH;
  int INVALID;
  int STRING;
  int ATKEYWORD;
  int IDENT;
}
class CSSParserException {
}
class CSSParserCallback {
}
class CSSParser {
  int error;
  int lookahead;
  int callback;
  int scanner;
}
class CSSLexicalException {
}
class CSSColor {
  int color;
  int value;
  int COLOR_MAP;
}
class BorderWidth {
}
class BorderStyle {
}
