package java.nio.charset;
class UnsupportedCharsetException {
  int charsetName;
  int serialVersionUID;
}
class UnmappableCharacterException {
  int inputLength;
  int serialVersionUID;
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
  int name;
  int REPORT;
  int REPLACE;
  int IGNORE;
}
class CoderResult {
  class Cache {
    int cache;
  }
  int length;
  int type;
  int unmappableCache;
  int malformedCache;
  int names;
  int UNDERFLOW;
  int OVERFLOW;
  int TYPE_UNMAPPABLE;
  int TYPE_UNDERFLOW;
  int TYPE_OVERFLOW;
  int TYPE_MALFORMED;
}
class CoderMalfunctionError {
  int serialVersionUID;
}
class CharsetEncoder {
  int unmappableCharacterAction;
  int malformedInputAction;
  int state;
  int replacement;
  int maxBytesPerChar;
  int averageBytesPerChar;
  int charset;
  int DEFAULT_REPLACEMENT;
  int STATE_FLUSHED;
  int STATE_END;
  int STATE_CODING;
  int STATE_RESET;
}
class CharsetDecoder {
  int unmappableCharacterAction;
  int malformedInputAction;
  int state;
  int replacement;
  int maxCharsPerByte;
  int averageCharsPerByte;
  int charset;
  int DEFAULT_REPLACEMENT;
  int STATE_FLUSHED;
  int STATE_END;
  int STATE_CODING;
  int STATE_RESET;
}
class Charset {
  int aliases;
  int canonicalName;
  int providers;
  int cachedDecoder;
  int cachedEncoder;
}
class CharacterCodingException {
  int serialVersionUID;
}
