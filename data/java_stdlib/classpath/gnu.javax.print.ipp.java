package gnu.javax.print.ipp;
class MultiDocPrintJobImpl {
}
class IppValueTag {
  int MIME_MEDIA_TYPE;
  int NATURAL_LANGUAGE;
  int CHARSET;
  int URI_SCHEME;
  int URI;
  int KEYWORD;
  int NAME_WITHOUT_LANGUAGE;
  int TEXT_WITHOUT_LANGUAGE;
  int NAME_WITH_LANGUAGE;
  int TEXT_WITH_LANGUAGE;
  int RANGEOFINTEGER;
  int RESOLUTION;
  int DATETIME;
  int OCTECTSTRING_UNSPECIFIED;
  int ENUM;
  int BOOLEAN;
  int INTEGER;
  int NO_VALUE;
  int UNKNOWN;
  int UNSUPPORTED;
}
class IppUtilities {
  int instanceByClass;
  int classesByName;
  int TEXT_CLASS_ARRAY;
  int TEXT_ATT_VALUE;
  int INTEGER_CLASS_ARRAY;
  int INTEGER_ATT_VALUE;
}
class IppStatusCode {
  int SERVER_ERROR_MULTIPLE_DOCUMENT_JOBS_NOT_SUPPORTED;
  int SERVER_ERROR_JOB_CANCELED;
  int SERVER_ERROR_BUSY;
  int SERVER_ERROR_NOT_ACCEPTING_JOBS;
  int SERVER_ERROR_TEMPORARY_ERROR;
  int SERVER_ERROR_DEVICE_ERROR;
  int SERVER_ERROR_VERSION_NOT_SUPPORTED;
  int SERVER_ERROR_SERVICE_UNAVAILABLE;
  int SERVER_ERROR_OPERATION_NOT_SUPPORTED;
  int SERVER_ERROR_INTERNAL_ERROR;
  int CLIENT_ERROR_DOCUMENT_ACCESS_ERROR;
  int CLIENT_ERROR_DOCUMENT_FORMAT_ERROR;
  int CLIENT_ERROR_COMPRESSION_ERROR;
  int CLIENT_ERROR_COMPRESSION_NOT_SUPPORTED;
  int CLIENT_ERROR_CONFLICTING_ATTRIBUTES;
  int CLIENT_ERROR_CHARSET_NOT_SUPPORTED;
  int CLIENT_ERROR_URI_SCHEME_NOT_SUPPORTED;
  int CLIENT_ERROR_ATTRIBUTES_OR_VALUES_NOT_SUPPORTED;
  int CLIENT_ERROR_DOCUMENT_FORMAT_NOT_SUPPORTED;
  int CLIENT_ERROR_REQUEST_VALUE_TOO_LONG;
  int CLIENT_ERROR_REQUEST_ENTITY_TOO_LONG;
  int CLIENT_ERROR_GONE;
  int CLIENT_ERROR_NOT_FOUND;
  int CLIENT_ERROR_TIMEOUT;
  int CLIENT_ERROR_NOT_POSSIBLE;
  int CLIENT_ERROR_NOT_AUTHORIZED;
  int CLIENT_ERROR_NOT_AUTHENTICATED;
  int CLIENT_ERROR_FORBIDDEN;
  int CLIENT_ERROR_BAD_REQUEST;
  int SUCCESSFUL_OK_CONFLICTING_ATTRIBUTES;
  int SUCCESSFUL_OK_IGNORED_OR_SUBSTITUED_ATTRIBUTES;
  int SUCCESSFUL_OK;
}
class IppResponse {
  int data;
  int unsupportedAttributes;
  int jobAttributes;
  int printerAttributes;
  int operationAttributes;
  int request_id;
  int status_code;
  int operation_id;
  int uri;
  int logger;
  class ResponseReader {
    int VERSION;
  }
}
class IppRequest {
  int connection;
  int requestUri;
  int data;
  int jobAttributes;
  int printerAttributes;
  int operationAttributes;
  int request_id;
  int operation_id;
  int alreadySent;
  int VERSION;
  int requestIdCounter;
  int logger;
  class RequestWriter {
    int out;
  }
  int timeout;
}
class IppPrintService {
  int JOB_NAME;
  int REQUESTING_USER_NAME;
  int logger;
  int printerUris;
  int printerUri;
  int flavors;
  int name;
  int passwd;
  int user;
  int printServiceAttributeListener;
  int printerAttr;
}
class IppMultiDocPrintService {
  int passwd;
  int user;
}
class IppException {
}
class IppDelimiterTag {
  int UNSUPPORTED_ATTRIBUTES_TAG;
  int PRINTER_ATTRIBUTES_TAG;
  int END_OF_ATTRIBUTES_TAG;
  int JOB_ATTRIBUTES_TAG;
  int OPERATION_ATTRIBUTES_TAG;
}
class DocPrintJobImpl {
  int printing;
  int currentSet;
  int oldSet;
  int requestingUser;
  int jobId;
  int jobUri;
  int password;
  int username;
  int attributesListenerAttributes;
  int attributesListener;
  int printJobListener;
  int service;
}
