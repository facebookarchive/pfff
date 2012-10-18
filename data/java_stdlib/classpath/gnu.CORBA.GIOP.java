package gnu.CORBA.GIOP;
class ServiceContext {
  int context_data;
  int context_id;
  int ActivityService;
  int SecurityAttributeService;
  int ExceptionDetailMessage;
  int FT_REQUEST;
  int FT_GROUP_VERSION;
  int RTCorbaPriorityRange;
  int RTCorbaPriority;
  int UnknownExceptionInfo;
  int FORWARDED_IDENTITY;
  int INVOCATION_POLICIES;
  int SendingContextRunTime;
  int BI_DIR_IIOP;
  int LogicalThreadId;
  int ChainBypassInfo;
  int ChainBypassCheck;
  int CodeSets;
  int TransactionService;
  int serialVersionUID;
}
class RequestHeader {
  int response_expected;
  int request_id;
  int requesting_principal;
  int object_key;
  int operation;
  int freeId;
}
class ReplyHeader {
  int request_id;
  int reply_status;
  int NEEDS_ADDRESSING_MODE;
  int LOCATION_FORWARD_PERM;
  int LOCATION_FORWARD;
  int SYSTEM_EXCEPTION;
  int USER_EXCEPTION;
  int NO_EXCEPTION;
}
class MessageHeader {
  int message_size;
  int message_type;
  int flags;
  int version;
  int types;
  int MAGIC;
  int FRAGMENT;
  int MESSAGE_ERROR;
  int CLOSE_CONNECTION;
  int LOCATE_REPLY;
  int LOCATE_REQUEST;
  int CANCEL_REQUEST;
  int REPLY;
  int REQUEST;
  int serialVersionUID;
}
class ErrorMessage {
  int serialVersionUID;
}
class ContextHandler {
  int service_context;
  int NO_CONTEXT;
}
class CodeSetServiceContext {
  int wide_char_data;
  int char_data;
  int STANDARD;
  int ID;
}
class CloseMessage {
  int Singleton;
  int serialVersionUID;
}
class CharSets_OSF {
  int string_to_code;
  int code_to_string;
  int NATIVE_WIDE_CHARACTER;
  int NATIVE_CHARACTER;
  int EUC_TW;
  int EUC_KR;
  int EUC_JP;
  int Cp970;
  int Cp964;
  int Cp950;
  int Cp949C;
  int Cp949;
  int Cp943C;
  int Cp943;
  int Cp930;
  int Cp33722C;
  int Cp33722;
  int Cp1386;
  int Cp1383;
  int Cp1381;
  int Cp1363C;
  int Cp1363;
  int Cp1257;
  int Cp1256;
  int Cp1255;
  int Cp1254;
  int Cp1253;
  int Cp1252;
  int Cp1251;
  int Cp1250;
  int Cp1047;
  int UCS2;
  int UTF16;
  int UTF8;
  int ISO8859_15_FDIS;
  int ISO8859_9;
  int ISO8859_8;
  int ISO8859_7;
  int ISO8859_6;
  int ISO8859_5;
  int ISO8859_4;
  int ISO8859_3;
  int ISO8859_2;
  int ISO8859_1;
  int ASCII;
}
class CancelHeader {
  int request_id;
}
