package gnu.xml.xpath;
class XPathTokenizer {
  int lastToken;
  int token;
  int in;
  int keywords;
  class XPathToken {
    int val;
    int type;
  }
}
class XPathParser {
  class YyNameClass {
    int yyName;
  }
  class YyCheckClass {
    int yyCheck;
  }
  class YyTableClass {
    int yyTable;
  }
  class YyGindexClass {
    int yyGindex;
  }
  class YyRindexClass {
    int yyRindex;
  }
  class YySindexClass {
    int yySindex;
  }
  class YyDgotoClass {
    int yyDgoto;
  }
  class YyDefRedClass {
    int yyDefRed;
  }
  class YyLenClass {
    int yyLen;
  }
  class YyLhsClass {
    int yyLhs;
  }
  int yyMax;
  int yyFinal;
  class yyInput {
  }
  class yyException {
  }
  int yyErrorCode;
  int UNARY;
  int NODE;
  int TEXT;
  int PROCESSING_INSTRUCTION;
  int COMMENT;
  int AND;
  int OR;
  int MOD;
  int DIV;
  int SELF;
  int PRECEDING_SIBLING;
  int PRECEDING;
  int PARENT;
  int NAMESPACE;
  int FOLLOWING_SIBLING;
  int FOLLOWING;
  int DESCENDANT_OR_SELF;
  int DESCENDANT;
  int CHILD;
  int ATTRIBUTE;
  int ANCESTOR_OR_SELF;
  int ANCESTOR;
  int DOUBLE_DOT;
  int DOT;
  int DOUBLE_COLON;
  int COLON;
  int DOLLAR;
  int STAR;
  int AT;
  int MINUS;
  int PLUS;
  int LTE;
  int GTE;
  int LT;
  int GT;
  int NE;
  int EQ;
  int DOUBLE_SLASH;
  int SLASH;
  int PIPE;
  int COMMA;
  int RB;
  int LB;
  int RP;
  int LP;
  int NAME;
  int DIGITS;
  int LITERAL;
  int functionResolver;
  int variableResolver;
  int namespaceContext;
}
class XPathImpl {
  int functionResolver;
  int variableResolver;
  int namespaceContext;
  int parser;
}
class XPathFactoryImpl {
  int functionResolver;
  int variableResolver;
}
class VariableReference {
  int name;
  int resolver;
}
class UnionExpr {
  int rhs;
  int lhs;
}
class TrueFunction {
}
class TranslateFunction {
  int arg3;
  int arg2;
  int arg1;
}
class Test {
}
class SumFunction {
  int arg;
}
class SubstringFunction {
  int arg3;
  int arg2;
  int arg1;
}
class SubstringBeforeFunction {
  int arg2;
  int arg1;
}
class SubstringAfterFunction {
  int arg2;
  int arg1;
}
class StringLengthFunction {
  int arg;
}
class StringFunction {
  int arg;
}
class Steps {
  int path;
}
class StartsWithFunction {
  int arg2;
  int arg1;
}
class Selector {
  int tests;
  int axis;
  int SELF;
  int PRECEDING_SIBLING;
  int PRECEDING;
  int PARENT;
  int NAMESPACE;
  int FOLLOWING_SIBLING;
  int FOLLOWING;
  int DESCENDANT_OR_SELF;
  int DESCENDANT;
  int CHILD;
  int ATTRIBUTE;
  int ANCESTOR_OR_SELF;
  int ANCESTOR;
}
class RoundFunction {
  int arg;
}
class Root {
}
class RelationalExpr {
  int eq;
  int lt;
  int rhs;
  int lhs;
}
class Predicate {
  int expr;
}
class PositionFunction {
}
class Pattern {
}
class Path {
}
class ParenthesizedExpr {
  int expr;
}
class OrExpr {
  int rhs;
  int lhs;
}
class NumberFunction {
  int arg;
}
class NotFunction {
  int arg;
}
class NormalizeSpaceFunction {
  int arg;
}
class NodeTypeTest {
  int data;
  int type;
}
class NegativeExpr {
  int expr;
}
class NamespaceUriFunction {
  int arg;
}
class NamespaceTest {
  int any;
  int anyLocalName;
  int qName;
}
class NameTest {
  int any;
  int anyLocalName;
  int qName;
}
class NameFunction {
  int arg;
}
class LocalNameFunction {
  int arg;
}
class LastFunction {
}
class LangFunction {
  int arg;
}
class IdFunction {
  int arg;
}
class FunctionCall {
  int args;
  int name;
  int resolver;
}
class Function {
}
class FloorFunction {
  int arg;
}
class FalseFunction {
}
class Expr {
  class ExprNodeSet {
    int list;
  }
  int decimalFormat;
  int documentOrderComparator;
}
class EqualityExpr {
  int invert;
  int rhs;
  int lhs;
}
class DocumentOrderComparator {
}
class CountFunction {
  int arg;
}
class ContainsFunction {
  int arg2;
  int arg1;
}
class Constant {
  int value;
}
class ConcatFunction {
  int args;
}
class CeilingFunction {
  int arg;
}
class BooleanFunction {
  int arg;
}
class ArithmeticExpr {
  int op;
  int rhs;
  int lhs;
  int MODULO;
  int DIVIDE;
  int MULTIPLY;
  int SUBTRACT;
  int ADD;
}
class AndExpr {
  int rhs;
  int lhs;
}
