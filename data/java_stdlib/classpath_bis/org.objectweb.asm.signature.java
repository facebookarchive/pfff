package org.objectweb.asm.signature;
class SignatureWriter {
  int argumentStack;
  int hasParameters;
  int hasFormals;
  int buf;
}
class SignatureVisitor {
  int INSTANCEOF;
  int SUPER;
  int EXTENDS;
}
class SignatureReader {
  int signature;
}
