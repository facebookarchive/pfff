package com.android.internal.http.multipart;
class StringPart {
  int value;
  int content;
  int DEFAULT_TRANSFER_ENCODING;
  int DEFAULT_CHARSET;
  int DEFAULT_CONTENT_TYPE;
  int LOG;
}
class PartSource {
}
class PartBase {
  int transferEncoding;
  int charSet;
  int contentType;
  int name;
}
class Part {
  int boundaryBytes;
  int CONTENT_TRANSFER_ENCODING_BYTES;
  int CONTENT_TRANSFER_ENCODING;
  int CHARSET_BYTES;
  int CHARSET;
  int CONTENT_TYPE_BYTES;
  int CONTENT_TYPE;
  int CONTENT_DISPOSITION_BYTES;
  int CONTENT_DISPOSITION;
  int EXTRA_BYTES;
  int EXTRA;
  int QUOTE_BYTES;
  int QUOTE;
  int CRLF_BYTES;
  int CRLF;
  int DEFAULT_BOUNDARY_BYTES;
  int BOUNDARY_BYTES;
  int BOUNDARY;
  int LOG;
}
class MultipartTest {
}
class MultipartEntity {
  int contentConsumed;
  int params;
  int multipartBoundary;
  int parts;
  int MULTIPART_CHARS;
  int MULTIPART_BOUNDARY;
  int MULTIPART_FORM_CONTENT_TYPE;
  int log;
}
class FilePartSource {
  int fileName;
  int file;
}
class FilePart {
  int source;
  int FILE_NAME_BYTES;
  int FILE_NAME;
  int LOG;
  int DEFAULT_TRANSFER_ENCODING;
  int DEFAULT_CHARSET;
  int DEFAULT_CONTENT_TYPE;
}
class ByteArrayPartSource {
  int bytes;
  int fileName;
}
