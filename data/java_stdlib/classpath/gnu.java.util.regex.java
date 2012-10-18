package gnu.java.util.regex;
class UncheckedRE {
}
class RETokenWordBoundary {
  int END;
  int BEGIN;
  int where;
  int negated;
}
class RETokenStart {
  int check_java_line_terminators;
  int newline;
}
class RETokenRange {
  int insens;
  int hi;
  int lo;
}
class RETokenPOSIX {
  int s_nameTable;
  int XDIGIT;
  int UPPER;
  int SPACE;
  int PUNCT;
  int PRINT;
  int LOWER;
  int GRAPH;
  int DIGIT;
  int CNTRL;
  int BLANK;
  int ALPHA;
  int ALNUM;
  int negated;
  int insens;
  int type;
}
class RETokenNamedProperty {
  class JavaCategoryHandler {
    int method;
  }
  class UnicodeBlockHandler {
    int block;
  }
  class UnicodeCategoriesHandler {
    int categories;
  }
  class UnicodeCategoryHandler {
    int category;
  }
  class POSIXHandler {
    int retoken;
  }
  class Handler {
  }
  int OTHER;
  int PUNCTUATION;
  int NUMBER;
  int SYMBOL;
  int SEPARATOR;
  int MARK;
  int LETTER;
  int handler;
  int negate;
  int insens;
  int name;
}
class RETokenLookBehind {
  class RETokenMatchHereOnly {
    int index;
  }
  int negative;
  int re;
}
class RETokenLookAhead {
  int negative;
  int re;
}
class RETokenIndependent {
  int re;
}
class RETokenEndSub {
}
class RETokenEndOfPreviousMatch {
}
class RETokenEnd {
  int fake;
  int check_java_line_terminators;
  int newline;
}
class RETokenChar {
  int matchedLength;
  int insens;
  int ch;
}
class RETokenBackRef {
  int insens;
  int num;
}
class RETokenAny {
  int matchNull;
  int newline;
}
class REToken {
  int unicodeAware;
  int subIndex;
  int uncle;
  int next;
}
class RESyntax {
  int RE_SYNTAX_JAVA_1_4;
  int RE_SYNTAX_PERL5_S;
  int RE_SYNTAX_PERL5;
  int RE_SYNTAX_PERL4_S;
  int RE_SYNTAX_PERL4;
  int RE_SYNTAX_SED;
  int RE_SYNTAX_POSIX_MINIMAL_EXTENDED;
  int RE_SYNTAX_POSIX_MINIMAL_BASIC;
  int RE_SYNTAX_POSIX_EXTENDED;
  int RE_SYNTAX_POSIX_EGREP;
  int RE_SYNTAX_POSIX_BASIC;
  int RE_SYNTAX_POSIX_AWK;
  int RE_SYNTAX_GREP;
  int RE_SYNTAX_EMACS;
  int RE_SYNTAX_EGREP;
  int RE_SYNTAX_ED;
  int RE_SYNTAX_AWK;
  int BIT_TOTAL;
  int RE_NESTED_CHARCLASS;
  int RE_NAMED_PROPERTY;
  int RE_UNICODE_CHAR;
  int RE_HEX_CHAR;
  int RE_OCTAL_CHAR;
  int RE_EMBEDDED_FLAGS;
  int RE_POSSESSIVE_OPS;
  int RE_CHAR_CLASS_ESC_IN_LISTS;
  int RE_COMMENTS;
  int RE_STRING_ANCHORS;
  int RE_LOOKAHEAD;
  int RE_PURE_GROUPING;
  int RE_CHAR_CLASS_ESCAPES;
  int RE_STINGY_OPS;
  int RE_HAT_LISTS_NOT_NEWLINE;
  int RE_UNMATCHED_RIGHT_PAREN_ORD;
  int RE_NO_EMPTY_RANGES;
  int RE_NO_BK_VBAR;
  int RE_NO_BK_REFS;
  int RE_NO_BK_PARENS;
  int RE_NO_BK_BRACES;
  int RE_NEWLINE_ALT;
  int RE_LIMITED_OPS;
  int RE_INTERVALS;
  int RE_DOT_NOT_NULL;
  int RE_DOT_NEWLINE;
  int RE_CONTEXT_INVALID_OPS;
  int RE_CONTEXT_INDEP_OPS;
  int RE_CONTEXT_INDEP_ANCHORS;
  int RE_CHAR_CLASSES;
  int RE_BK_PLUS_QM;
  int RE_BACKSLASH_ESCAPE_IN_LISTS;
  int lineSeparator;
  int isFinal;
  int bits;
  int DEFAULT_LINE_SEPARATOR;
}
class REMatch {
  int backtrackStack;
  int empty;
  int end;
  int start1;
  int start;
  int index;
  int anchor;
  int offset;
  int eflags;
  int matchedCharIndexed;
  int matchedText;
}
class REFilterInputStream {
  int stream;
  int offset;
  int bufpos;
  int buffer;
  int replace;
  int expr;
}
class REException {
  int REG_ESPACE;
  int REG_ESIZE;
  int REG_BADPAT;
  int REG_ESCAPE;
  int REG_EEND;
  int REG_ESUBREG;
  int REG_EPAREN;
  int REG_ECTYPE;
  int REG_ERANGE;
  int REG_EBRACK;
  int REG_EBRACE;
  int REG_BADBR;
  int REG_BADRPT;
  int pos;
  int type;
}
class CharIndexedStringBuffer {
}
class CharIndexedString {
}
class CharIndexedInputStream {
  int lookBehind;
  int cached;
  int end;
  int bufsize;
  int index;
  int br;
  int UNKNOWN;
  int BUFFER_INCREMENT;
}
class CharIndexedCharSequence {
  int rightmostTriedPosition;
  int lastMatch;
  int len;
  int anchor;
  int s;
}
class CharIndexedCharArray {
}
class CharIndexed {
  int OUT_OF_BOUNDS;
}
class BacktrackStack {
  int CAPACITY_INCREMENT;
  int INITIAL_CAPACITY;
  int capacity;
  int size;
  int stack;
  class Backtrack {
    int param;
    int match;
    int input;
    int token;
  }
}
