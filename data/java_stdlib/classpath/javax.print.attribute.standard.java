package javax.print.attribute.standard;
class Sides {
  int enumValueTable;
  int stringTable;
  int TUMBLE;
  int DUPLEX;
  int TWO_SIDED_SHORT_EDGE;
  int TWO_SIDED_LONG_EDGE;
  int ONE_SIDED;
  int serialVersionUID;
}
class SheetCollate {
  int enumValueTable;
  int stringTable;
  int COLLATED;
  int UNCOLLATED;
  int serialVersionUID;
}
class Severity {
  int enumValueTable;
  int stringTable;
  int ERROR;
  int WARNING;
  int REPORT;
  int serialVersionUID;
}
class RequestingUserName {
  int serialVersionUID;
}
class ReferenceUriSchemesSupported {
  int enumValueTable;
  int stringTable;
  int FILE;
  int WAIS;
  int NNTP;
  int NEWS;
  int GOPHER;
  int HTTPS;
  int HTTP;
  int FTP;
  int serialVersionUID;
}
class QueuedJobCount {
  int serialVersionUID;
}
class PrinterURI {
  int serialVersionUID;
}
class PrinterStateReasons {
  int serialVersionUID;
}
class PrinterStateReason {
  int enumValueTable;
  int stringTable;
  int INTERPRETER_RESOURCE_UNAVAILABLE;
  int DEVELOPER_EMPTY;
  int DEVELOPER_LOW;
  int OPC_LIFE_OVER;
  int OPC_NEAR_EOL;
  int FUSER_UNDER_TEMP;
  int FUSER_OVER_TEMP;
  int MARKER_WASTE_FULL;
  int MARKER_WASTE_ALMOST_FULL;
  int MARKER_SUPPLY_EMPTY;
  int MARKER_SUPPLY_LOW;
  int OUTPUT_AREA_FULL;
  int OUTPUT_AREA_ALMOST_FULL;
  int OUTPUT_TRAY_MISSING;
  int MEDIA_EMPTY;
  int MEDIA_LOW;
  int INPUT_TRAY_MISSING;
  int DOOR_OPEN;
  int INTERLOCK_OPEN;
  int COVER_OPEN;
  int SPOOL_AREA_FULL;
  int TONER_EMPTY;
  int TONER_LOW;
  int STOPPED_PARTLY;
  int STOPPING;
  int TIMED_OUT;
  int CONNECTING_TO_DEVICE;
  int SHUTDOWN;
  int PAUSED;
  int MOVING_TO_PAUSED;
  int MEDIA_JAM;
  int MEDIA_NEEDED;
  int OTHER;
  int serialVersionUID;
}
class PrinterState {
  int enumValueTable;
  int stringTable;
  int STOPPED;
  int PROCESSING;
  int IDLE;
  int UNKNOWN;
  int serialVersionUID;
}
class PrinterResolution {
  int serialVersionUID;
}
class PrinterName {
  int serialVersionUID;
}
class PrinterMoreInfoManufacturer {
  int serialVersionUID;
}
class PrinterMoreInfo {
  int serialVersionUID;
}
class PrinterMessageFromOperator {
  int serialVersionUID;
}
class PrinterMakeAndModel {
  int serialVersionUID;
}
class PrinterLocation {
  int serialVersionUID;
}
class PrinterIsAcceptingJobs {
  int enumValueTable;
  int stringTable;
  int ACCEPTING_JOBS;
  int NOT_ACCEPTING_JOBS;
  int serialVersionUID;
}
class PrinterInfo {
  int serialVersionUID;
}
class PrintQuality {
  int enumValueTable;
  int stringTable;
  int HIGH;
  int NORMAL;
  int DRAFT;
  int serialVersionUID;
}
class PresentationDirection {
  int enumValueTable;
  int stringTable;
  int TOLEFT_TOTOP;
  int TOLEFT_TOBOTTOM;
  int TORIGHT_TOTOP;
  int TORIGHT_TOBOTTOM;
  int TOTOP_TOLEFT;
  int TOTOP_TORIGHT;
  int TOBOTTOM_TOLEFT;
  int TOBOTTOM_TORIGHT;
  int serialVersionUID;
}
class PagesPerMinuteColor {
  int serialVersionUID;
}
class PagesPerMinute {
  int serialVersionUID;
}
class PageRanges {
  int serialVersionUID;
}
class PDLOverrideSupported {
  int enumValueTable;
  int stringTable;
  int ATTEMPTED;
  int NOT_ATTEMPTED;
  int serialVersionUID;
}
class OutputDeviceAssigned {
  int serialVersionUID;
}
class OrientationRequested {
  int enumValueTable;
  int stringTable;
  int REVERSE_PORTRAIT;
  int REVERSE_LANDSCAPE;
  int LANDSCAPE;
  int PORTRAIT;
  int serialVersionUID;
}
class NumberUpSupported {
  int serialVersionUID;
}
class NumberUp {
  int serialVersionUID;
}
class NumberOfInterveningJobs {
  int serialVersionUID;
}
class NumberOfDocuments {
  int serialVersionUID;
}
class MultipleDocumentHandling {
  int enumValueTable;
  int stringTable;
  int SINGLE_DOCUMENT_NEW_SHEET;
  int SEPARATE_DOCUMENTS_COLLATED_COPIES;
  int SEPARATE_DOCUMENTS_UNCOLLATED_COPIES;
  int SINGLE_DOCUMENT;
  int serialVersionUID;
}
class MediaTray {
  int enumValueTable;
  int stringTable;
  int SIDE;
  int MAIN;
  int LARGE_CAPACITY;
  int MANUAL;
  int ENVELOPE;
  int BOTTOM;
  int MIDDLE;
  int TOP;
  int serialVersionUID;
}
class MediaSizeName {
  int enumValueTable;
  int stringTable;
  int NA_8X10;
  int NA_5X7;
  int NA_10X15_ENVELOPE;
  int NA_10X14_ENVELOPE;
  int NA_10X13_ENVELOPE;
  int NA_9X12_ENVELOPE;
  int NA_9X11_ENVELOPE;
  int NA_7X9_ENVELOPE;
  int NA_6X9_ENVELOPE;
  int NA_NUMBER_14_ENVELOPE;
  int NA_NUMBER_12_ENVELOPE;
  int NA_NUMBER_11_ENVELOPE;
  int NA_NUMBER_10_ENVELOPE;
  int NA_NUMBER_9_ENVELOPE;
  int PERSONAL_ENVELOPE;
  int MONARCH_ENVELOPE;
  int ITALY_ENVELOPE;
  int ISO_DESIGNATED_LONG;
  int E;
  int D;
  int C;
  int B;
  int A;
  int JAPANESE_DOUBLE_POSTCARD;
  int JAPANESE_POSTCARD;
  int QUARTO;
  int FOLIO;
  int INVOICE;
  int TABLOID;
  int LEDGER;
  int EXECUTIVE;
  int NA_LEGAL;
  int NA_LETTER;
  int ISO_C6;
  int ISO_C5;
  int ISO_C4;
  int ISO_C3;
  int ISO_C2;
  int ISO_C1;
  int ISO_C0;
  int JIS_B10;
  int JIS_B9;
  int JIS_B8;
  int JIS_B7;
  int JIS_B6;
  int JIS_B5;
  int JIS_B4;
  int JIS_B3;
  int JIS_B2;
  int JIS_B1;
  int JIS_B0;
  int ISO_B10;
  int ISO_B9;
  int ISO_B8;
  int ISO_B7;
  int ISO_B6;
  int ISO_B5;
  int ISO_B4;
  int ISO_B3;
  int ISO_B2;
  int ISO_B1;
  int ISO_B0;
  int ISO_A10;
  int ISO_A9;
  int ISO_A8;
  int ISO_A7;
  int ISO_A6;
  int ISO_A5;
  int ISO_A4;
  int ISO_A3;
  int ISO_A2;
  int ISO_A1;
  int ISO_A0;
  int serialVersionUID;
}
class MediaSize {
  class Other {
    int TABLOID;
    int JAPANESE_DOUBLE_POSTCARD;
    int JAPANESE_POSTCARD;
    int ITALY_ENVELOPE;
    int PERSONAL_ENVELOPE;
    int MONARCH_ENVELOPE;
    int LEDGER;
    int INVOICE;
    int QUARTO;
    int FOLIO;
    int EXECUTIVE;
  }
  class JIS {
    int YOU_7;
    int YOU_6;
    int YOU_5;
    int YOU_4;
    int YOU_3;
    int YOU_2;
    int YOU_1;
    int KAKU_A4;
    int KAKU_8;
    int KAKU_7;
    int KAKU_6;
    int KAKU_5;
    int KAKU_4;
    int KAKU_3;
    int KAKU_20;
    int KAKU_2;
    int KAKU_1;
    int KAKU_0;
    int CHOU_40;
    int CHOU_30;
    int CHOU_4;
    int CHOU_3;
    int CHOU_2;
    int CHOU_1;
    int B10;
    int B9;
    int B8;
    int B7;
    int B6;
    int B5;
    int B4;
    int B3;
    int B2;
    int B1;
    int B0;
  }
  class Engineering {
    int E;
    int D;
    int C;
    int B;
    int A;
  }
  class NA {
    int NA_NUMBER_14_ENVELOPE;
    int NA_NUMBER_12_ENVELOPE;
    int NA_NUMBER_11_ENVELOPE;
    int NA_NUMBER_10_ENVELOPE;
    int NA_NUMBER_9_ENVELOPE;
    int NA_10X15_ENVELOPE;
    int NA_10x14_ENVELOPE;
    int NA_10x13_ENVELOPE;
    int NA_9x12_ENVELOPE;
    int NA_9x11_ENVELOPE;
    int NA_7X9_ENVELOPE;
    int NA_6X9_ENVELOPE;
    int NA_8X10;
    int NA_5X7;
    int LETTER;
    int LEGAL;
  }
  class ISO {
    int DESIGNATED_LONG;
    int C6;
    int C5;
    int C4;
    int C3;
    int B10;
    int B9;
    int B8;
    int B7;
    int B6;
    int B5;
    int B4;
    int B3;
    int B2;
    int B1;
    int B0;
    int A10;
    int A9;
    int A8;
    int A7;
    int A6;
    int A5;
    int A4;
    int A3;
    int A2;
    int A1;
    int A0;
  }
  int mediaName;
  int mediaCache;
  int serialVersionUID;
}
class MediaPrintableArea {
  int h;
  int w;
  int y;
  int x;
  int MM;
  int INCH;
  int serialVersionUID;
}
class MediaName {
  int enumValueTable;
  int stringTable;
  int ISO_A4_TRANSPARENT;
  int ISO_A4_WHITE;
  int NA_LETTER_TRANSPARENT;
  int NA_LETTER_WHITE;
  int serialVersionUID;
}
class Media {
  int serialVersionUID;
}
class JobStateReasons {
  int serialVersionUID;
}
class JobStateReason {
  int enumValueTable;
  int stringTable;
  int QUEUED_IN_DEVICE;
  int JOB_RESTARTABLE;
  int JOB_COMPLETED_WITH_ERRORS;
  int JOB_COMPLETED_WITH_WARNINGS;
  int JOB_COMPLETED_SUCCESSFULLY;
  int SERVICE_OFF_LINE;
  int PROCESSING_TO_STOP_POINT;
  int DOCUMENT_FORMAT_ERROR;
  int UNSUPPORTED_DOCUMENT_FORMAT;
  int COMPRESSION_ERROR;
  int UNSUPPORTED_COMPRESSION;
  int ABORTED_BY_SYSTEM;
  int JOB_CANCELED_AT_DEVICE;
  int JOB_CANCELED_BY_OPERATOR;
  int JOB_CANCELED_BY_USER;
  int JOB_PRINTING;
  int JOB_QUEUED_FOR_MARKER;
  int JOB_TRANSFORMING;
  int JOB_QUEUED;
  int JOB_INTERPRETING;
  int PRINTER_STOPPED;
  int PRINTER_STOPPED_PARTLY;
  int RESOURCES_ARE_NOT_READY;
  int JOB_HOLD_UNTIL_SPECIFIED;
  int JOB_OUTGOING;
  int SUBMISSION_INTERRUPTED;
  int DOCUMENT_ACCESS_ERROR;
  int JOB_DATA_INSUFFICIENT;
  int JOB_INCOMING;
  int serialVersionUID;
}
class JobState {
  int enumValueTable;
  int stringTable;
  int COMPLETED;
  int ABORTED;
  int CANCELED;
  int PROCESSING_STOPPED;
  int PROCESSING;
  int PENDING_HELD;
  int PENDING;
  int UNKNOWN;
  int serialVersionUID;
}
class JobSheets {
  int enumValueTable;
  int stringTable;
  int STANDARD;
  int NONE;
  int serialVersionUID;
}
class JobPrioritySupported {
  int serialVersionUID;
}
class JobPriority {
  int serialVersionUID;
}
class JobOriginatingUserName {
  int serialVersionUID;
}
class JobName {
  int serialVersionUID;
}
class JobMessageFromOperator {
  int serialVersionUID;
}
class JobMediaSheetsSupported {
  int serialVersionUID;
}
class JobMediaSheetsCompleted {
  int serialVersionUID;
}
class JobMediaSheets {
  int serialVersionUID;
}
class JobKOctetsSupported {
  int serialVersionUID;
}
class JobKOctetsProcessed {
  int serialVersionUID;
}
class JobKOctets {
  int serialVersionUID;
}
class JobImpressionsSupported {
  int serialVersionUID;
}
class JobImpressionsCompleted {
  int serialVersionUID;
}
class JobImpressions {
  int serialVersionUID;
}
class JobHoldUntil {
  int serialVersionUID;
}
class Finishings {
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
  int serialVersionUID;
}
class Fidelity {
  int enumValueTable;
  int stringTable;
  int FIDELITY_FALSE;
  int FIDELITY_TRUE;
  int serialVersionUID;
}
class DocumentName {
  int serialVersionUID;
}
class Destination {
  int serialVersionUID;
}
class DateTimeAtProcessing {
  int serialVersionUID;
}
class DateTimeAtCreation {
  int serialVersionUID;
}
class DateTimeAtCompleted {
  int serialVersionUID;
}
class CopiesSupported {
  int serialVersionUID;
}
class Copies {
  int serialVersionUID;
}
class Compression {
  int enumValueTable;
  int stringTable;
  int COMPRESS;
  int GZIP;
  int DEFLATE;
  int NONE;
  int serialVersionUID;
}
class ColorSupported {
  int enumValueTable;
  int stringTable;
  int SUPPORTED;
  int NOT_SUPPORTED;
  int serialVersionUID;
}
class Chromaticity {
  int enumValueTable;
  int stringTable;
  int COLOR;
  int MONOCHROME;
  int serialVersionUID;
}
