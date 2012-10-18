package gnu.classpath.tools.gjdoc.expr;
class UnknownIdentifierException {
}
class UnaryExpression {
  int expr;
}
class TypeCastExpression {
  int type;
}
class Type {
  int clazz;
  int NULL;
  int STRING;
  int VOID;
  int SHORT;
  int BYTE;
  int CHAR;
  int FLOAT;
  int DOUBLE;
  int BOOLEAN;
  int INTEGER;
  int LONG;
}
class SubtractionExpression {
}
class ShiftRightExpression {
}
class ShiftLeftExpression {
}
class NotExpression {
}
class NotEqualExpression {
}
class NegateExpression {
}
class MultiplicationExpression {
}
class ModuloExpression {
}
class LogicalOrExpression {
}
class LogicalNotExpression {
}
class LogicalAndExpression {
}
class LessThanOrEqualExpression {
}
class LessThanExpression {
}
class InclusiveOrExpression {
}
class IllegalExpressionException {
}
class IdentifierExpression {
  int identifier;
}
class GreaterThanOrEqualExpression {
}
class GreaterThanExpression {
}
class Expression {
}
class ExclusiveOrExpression {
}
class EvaluatorEnvironment {
}
class Evaluator {
}
class EqualExpression {
}
class DivisionExpression {
}
class Context {
  int visitedFields;
  int evaluatorEnvironment;
}
class ConstantString {
  int value;
}
class ConstantShort {
  int value;
}
class ConstantNull {
}
class ConstantLong {
  int value;
}
class ConstantInteger {
  int value;
}
class ConstantFloat {
  int value;
}
class ConstantExpression {
}
class ConstantDouble {
  int value;
}
class ConstantChar {
  int value;
}
class ConstantByte {
  int value;
}
class ConstantBoolean {
  int value;
}
class ConditionalExpression {
  int ifFalse;
  int ifTrue;
  int condition;
}
class CircularExpressionException {
}
class BitShiftRightExpression {
}
class BinaryShiftExpression {
}
class BinaryRelationExpression {
}
class BinaryLogicalExpression {
}
class BinaryExpression {
  int right;
  int left;
}
class BinaryEqualityExpression {
}
class BinaryComputationExpression {
}
class BinaryBitwiseExpression {
}
class AndExpression {
}
class AdditionExpression {
}
