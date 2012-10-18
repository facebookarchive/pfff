package java.nio.charset;
class UnsupportedCharsetException {
  int charsetName;
  int serialVersionUID;
}
class UnmappableCharacterException {
  int inputLength;
  int serialVersionUID;
}
class ModifiedUtf8 {
}
class MalformedInputException {
  int inputLength;
  int serialVersionUID;
}
class IllegalCharsetNameException {
  int charsetName;
  int serialVersionUID;
}
class CodingErrorAction {
  int action;
  int REPORT;
  int REPLACE;
  int IGNORE;
}
class CoderResult {
  int length;
  int type;
  int _unmappableErrors;
  int _malformedErrors;
  int OVERFLOW;
  int UNDERFLOW;
  int TYPE_UNMAPPABLE_CHAR;
  int TYPE_MALFORMED_INPUT;
  int TYPE_OVERFLOW;
  int TYPE_UNDERFLOW;
}
class CoderMalfunctionError {
  int serialVersionUID;
}
class Charsets {
  int UTF_8;
  int US_ASCII;
  int ISO_8859_1;
}
class CharsetICU {
  int icuCanonicalName;
}
class CharsetEncoderICU {
  int ec;
  int outEnd;
  int inEnd;
  int allocatedOutput;
  int allocatedInput;
  int output;
  int input;
  int converterHandle;
  int data;
  int INVALID_CHARS;
  int OUTPUT_OFFSET;
  int INPUT_OFFSET;
  int DEFAULT_REPLACEMENTS;
}
class CharsetEncoder {
  int decoder;
  int unmappableCharacterAction;
  int malformedInputAction;
  int finished;
  int status;
  int replacementBytes;
  int maxBytesPerChar;
  int averageBytesPerChar;
  int cs;
  int INIT;
  int FLUSH;
  int END;
  int ONGOING;
  int READY;
}
class CharsetDecoderICU {
  int ec;
  int outEnd;
  int inEnd;
  int allocatedOutput;
  int allocatedInput;
  int output;
  int input;
  int converterHandle;
  int data;
  int INVALID_BYTES;
  int OUTPUT_OFFSET;
  int INPUT_OFFSET;
  int MAX_CHARS_PER_BYTE;
}
class CharsetDecoder {
  int status;
  int replacementChars;
  int unmappableCharacterAction;
  int malformedInputAction;
  int cs;
  int maxCharsPerByte;
  int averageCharsPerByte;
  int FLUSH;
  int END;
  int ONGOING;
  int INIT;
}
class Charset {
  int aliasesSet;
  int canonicalName;
  int DEFAULT_CHARSET;
  int CACHED_CHARSETS;
}
class CharacterCodingException {
  int serialVersionUID;
}
