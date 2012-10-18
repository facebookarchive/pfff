package gnu.javax.print.ipp.attribute.supported;
class UriSecuritySupported {
  int enumValueTable;
  int stringTable;
  int TLS;
  int SSL3;
  int NONE;
}
class UriAuthenticationSupported {
  int enumValueTable;
  int stringTable;
  int CERTIFICATE;
  int DIGEST;
  int BASIC;
  int REQUESTING_USER_NAME;
  int NONE;
}
class SidesSupported {
  int enumValueTable;
  int stringTable;
  int TUMBLE;
  int DUPLEX;
  int TWO_SIDED_SHORT_EDGE;
  int TWO_SIDED_LONG_EDGE;
  int ONE_SIDED;
}
class PrinterUriSupported {
}
class PrinterResolutionSupported {
}
class PrintQualitySupported {
  int enumValueTable;
  int stringTable;
  int HIGH;
  int NORMAL;
  int DRAFT;
}
class PageRangesSupported {
  int enumValueTable;
  int stringTable;
  int SUPPORTED;
  int NOT_SUPPORTED;
}
class OrientationRequestedSupported {
  int enumValueTable;
  int stringTable;
  int REVERSE_PORTRAIT;
  int REVERSE_LANDSCAPE;
  int LANDSCAPE;
  int PORTRAIT;
}
class OperationsSupported {
  int enumValueTable;
  int stringTable;
  int PURGE_JOBS;
  int RESUME_PRINTER;
  int RESERVED;
  int RESTART_JOB;
  int RELEASE_JOB;
  int HOLD_JOB;
  int GET_PRINTER_ATTRIBUTES;
  int GET_JOBS;
  int PAUSE_PRINTER;
  int GET_JOB_ATTRIBUTES;
  int CANCEL_JOB;
  int SEND_URI;
  int SEND_DOCUMENT;
  int CREATE_JOB;
  int VALIDATE_JOB;
  int PRINT_URI;
  int PRINT_JOB;
}
class MultipleDocumentJobsSupported {
  int enumValueTable;
  int stringTable;
  int SUPPORTED;
  int NOT_SUPPORTED;
}
class MultipleDocumentHandlingSupported {
  int enumValueTable;
  int stringTable;
  int SINGLE_DOCUMENT_NEW_SHEET;
  int SEPARATE_DOCUMENTS_COLLATED_COPIES;
  int SEPARATE_DOCUMENTS_UNCOLLATED_COPIES;
  int SINGLE_DOCUMENT;
}
class MediaSupported {
}
class JobSheetsSupported {
  int STANDARD;
  int NONE;
}
class JobHoldUntilSupported {
  int THIRD_SHIFT;
  int SECOND_SHIFT;
  int WEEKEND;
  int NIGHT;
  int EVENING;
  int DAY_TIME;
  int INDEFINITE;
  int NO_HOLD;
}
class IppVersionsSupported {
  int enumValueTable;
  int stringTable;
  int V_1_1;
  int V_1_0;
}
class GeneratedNaturalLanguageSupported {
}
class FinishingsSupported {
  int enumValueTable;
  int stringTable;
  int STAPLE_DUAL_BOTTOM;
  int STAPLE_DUAL_RIGHT;
  int STAPLE_DUAL_TOP;
  int STAPLE_DUAL_LEFT;
  int EDGE_STITCH_BOTTOM;
  int EDGE_STITCH_RIGHT;
  int EDGE_STITCH_TOP;
  int EDGE_STITCH_LEFT;
  int STAPLE_BOTTOM_RIGHT;
  int STAPLE_TOP_RIGHT;
  int STAPLE_BOTTOM_LEFT;
  int STAPLE_TOP_LEFT;
  int EDGE_STITCH;
  int SADDLE_STITCH;
  int BIND;
  int COVER;
  int STAPLE;
  int NONE;
}
class DocumentFormatSupported {
}
class CompressionSupported {
  int enumValueTable;
  int stringTable;
  int COMPRESS;
  int GZIP;
  int DEFLATE;
  int NONE;
}
class CharsetSupported {
}
