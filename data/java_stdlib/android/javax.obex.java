package javax.obex;
class SessionNotifier {
}
class ServerSession {
  int mClosed;
  int mMaxPacketLength;
  int mProcessThread;
  int mListener;
  int mOutput;
  int mInput;
  int mTransport;
  int TAG;
}
class ServerRequestHandler {
  int mConnectionId;
}
class ServerOperation {
  int mHasBody;
  int mRequestFinished;
  int mListener;
  int mExceptionString;
  int mPrivateOutputOpen;
  int mPrivateOutput;
  int mPrivateInput;
  int mGetOperation;
  int mClosed;
  int mResponseSize;
  int mMaxPacketLength;
  int mParent;
  int mInput;
  int finalBitSet;
  int replyHeader;
  int requestHeader;
  int isAborted;
}
class ResponseCodes {
  int OBEX_DATABASE_LOCKED;
  int OBEX_DATABASE_FULL;
  int OBEX_HTTP_VERSION;
  int OBEX_HTTP_GATEWAY_TIMEOUT;
  int OBEX_HTTP_UNAVAILABLE;
  int OBEX_HTTP_BAD_GATEWAY;
  int OBEX_HTTP_NOT_IMPLEMENTED;
  int OBEX_HTTP_INTERNAL_ERROR;
  int OBEX_HTTP_UNSUPPORTED_TYPE;
  int OBEX_HTTP_REQ_TOO_LARGE;
  int OBEX_HTTP_ENTITY_TOO_LARGE;
  int OBEX_HTTP_PRECON_FAILED;
  int OBEX_HTTP_LENGTH_REQUIRED;
  int OBEX_HTTP_GONE;
  int OBEX_HTTP_CONFLICT;
  int OBEX_HTTP_TIMEOUT;
  int OBEX_HTTP_PROXY_AUTH;
  int OBEX_HTTP_NOT_ACCEPTABLE;
  int OBEX_HTTP_BAD_METHOD;
  int OBEX_HTTP_NOT_FOUND;
  int OBEX_HTTP_FORBIDDEN;
  int OBEX_HTTP_PAYMENT_REQUIRED;
  int OBEX_HTTP_UNAUTHORIZED;
  int OBEX_HTTP_BAD_REQUEST;
  int OBEX_HTTP_USE_PROXY;
  int OBEX_HTTP_NOT_MODIFIED;
  int OBEX_HTTP_SEE_OTHER;
  int OBEX_HTTP_MOVED_TEMP;
  int OBEX_HTTP_MOVED_PERM;
  int OBEX_HTTP_MULT_CHOICE;
  int OBEX_HTTP_PARTIAL;
  int OBEX_HTTP_RESET;
  int OBEX_HTTP_NO_CONTENT;
  int OBEX_HTTP_NOT_AUTHORITATIVE;
  int OBEX_HTTP_ACCEPTED;
  int OBEX_HTTP_CREATED;
  int OBEX_HTTP_OK;
  int OBEX_HTTP_CONTINUE;
}
class PrivateOutputStream {
  int mMaxPacketSize;
  int mOpen;
  int mArray;
  int mParent;
}
class PrivateInputStream {
  int mOpen;
  int mIndex;
  int mData;
  int mParent;
}
class PasswordAuthentication {
  int mPassword;
  int mUserName;
}
class Operation {
}
class ObexTransport {
}
class ObexSession {
  int mChallengeDigest;
  int mAuthenticator;
}
class ObexHelper {
  int OBEX_AUTH_REALM_CHARSET_UNICODE;
  int OBEX_AUTH_REALM_CHARSET_ISO_8859_9;
  int OBEX_AUTH_REALM_CHARSET_ISO_8859_8;
  int OBEX_AUTH_REALM_CHARSET_ISO_8859_7;
  int OBEX_AUTH_REALM_CHARSET_ISO_8859_6;
  int OBEX_AUTH_REALM_CHARSET_ISO_8859_5;
  int OBEX_AUTH_REALM_CHARSET_ISO_8859_4;
  int OBEX_AUTH_REALM_CHARSET_ISO_8859_3;
  int OBEX_AUTH_REALM_CHARSET_ISO_8859_2;
  int OBEX_AUTH_REALM_CHARSET_ISO_8859_1;
  int OBEX_AUTH_REALM_CHARSET_ASCII;
  int OBEX_OPCODE_ABORT;
  int OBEX_OPCODE_SETPATH;
  int OBEX_OPCODE_RESERVED_FINAL;
  int OBEX_OPCODE_RESERVED;
  int OBEX_OPCODE_GET_FINAL;
  int OBEX_OPCODE_GET;
  int OBEX_OPCODE_PUT_FINAL;
  int OBEX_OPCODE_PUT;
  int OBEX_OPCODE_DISCONNECT;
  int OBEX_OPCODE_CONNECT;
  int MAX_CLIENT_PACKET_SIZE;
  int MAX_PACKET_SIZE_INT;
  int BASE_PACKET_LENGTH;
}
class HeaderSet {
  int responseCode;
  int mConnectionID;
  int mAuthResp;
  int mAuthChall;
  int nonce;
  int mRandom;
  int mIntegerUserDefined;
  int mByteUserDefined;
  int mSequenceUserDefined;
  int mUnicodeUserDefined;
  int mObjectClass;
  int mAppParam;
  int mWho;
  int mHttpHeader;
  int mTarget;
  int mDescription;
  int mByteTime;
  int mIsoTime;
  int mLength;
  int mType;
  int mName;
  int mCount;
  int OBJECT_CLASS;
  int AUTH_RESPONSE;
  int AUTH_CHALLENGE;
  int APPLICATION_PARAMETER;
  int CONNECTION_ID;
  int WHO;
  int END_OF_BODY;
  int BODY;
  int HTTP;
  int TARGET;
  int DESCRIPTION;
  int TIME_4_BYTE;
  int TIME_ISO_8601;
  int LENGTH;
  int TYPE;
  int NAME;
  int COUNT;
}
class ClientSession {
  int mOutput;
  int mInput;
  int mRequestActive;
  int maxPacketSize;
  int mConnectionId;
  int mObexConnected;
  int mOpen;
}
class ClientOperation {
  int mEndOfBodySent;
  int mReplyHeader;
  int mRequestHeader;
  int mGetOperation;
  int mOperationDone;
  int mMaxPacketSize;
  int mExceptionMessage;
  int mPrivateOutputOpen;
  int mPrivateOutput;
  int mPrivateInputOpen;
  int mPrivateInput;
  int mInputOpen;
  int mParent;
}
class BaseStream {
}
class Authenticator {
}
class ApplicationParameter {
  class TRIPLET_LENGTH {
    int NEWMISSEDCALLS_LENGTH;
    int PHONEBOOKSIZE_LENGTH;
    int FORMAT_LENGTH;
    int FILTER_LENGTH;
    int LISTSTARTOFFSET_LENGTH;
    int MAXLISTCOUNT_LENGTH;
    int SEARCH_ATTRIBUTE_LENGTH;
    int ORDER_LENGTH;
  }
  class TRIPLET_VALUE {
    class FORMAT {
      int VCARD_VERSION_30;
      int VCARD_VERSION_21;
    }
    class SEARCHATTRIBUTE {
      int SEARCH_BY_SOUND;
      int SEARCH_BY_NUMBER;
      int SEARCH_BY_NAME;
    }
    class ORDER {
      int ORDER_BY_PHONETIC;
      int ORDER_BY_ALPHANUMERIC;
      int ORDER_BY_INDEX;
    }
  }
  class TRIPLET_TAGID {
    int NEWMISSEDCALLS_TAGID;
    int PHONEBOOKSIZE_TAGID;
    int FORMAT_TAGID;
    int FILTER_TAGID;
    int LISTSTARTOFFSET_TAGID;
    int MAXLISTCOUNT_TAGID;
    int SEARCH_ATTRIBUTE_TAGID;
    int SEARCH_VALUE_TAGID;
    int ORDER_TAGID;
  }
  int mMaxLength;
  int mLength;
  int mArray;
}
