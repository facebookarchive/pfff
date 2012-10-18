package android.telephony.gsm;
class SmsMessage {
  class SubmitPdu {
    int encodedMessage;
    int encodedScAddress;
  }
  int mWrappedSmsMessage;
  int MAX_USER_DATA_SEPTETS_WITH_HEADER;
  int MAX_USER_DATA_SEPTETS;
  int MAX_USER_DATA_BYTES_WITH_HEADER;
  int MAX_USER_DATA_BYTES;
  int ENCODING_16BIT;
  int ENCODING_8BIT;
  int ENCODING_7BIT;
  int ENCODING_UNKNOWN;
  class MessageClass {
    int CLASS_3;
    int CLASS_2;
    int CLASS_1;
    int CLASS_0;
    int UNKNOWN;
  }
  int LOG_TAG;
  int LOCAL_DEBUG;
}
class SmsManager {
  int RESULT_ERROR_NO_SERVICE;
  int RESULT_ERROR_NULL_PDU;
  int RESULT_ERROR_RADIO_OFF;
  int RESULT_ERROR_GENERIC_FAILURE;
  int STATUS_ON_SIM_UNSENT;
  int STATUS_ON_SIM_SENT;
  int STATUS_ON_SIM_UNREAD;
  int STATUS_ON_SIM_READ;
  int STATUS_ON_SIM_FREE;
  int mSmsMgrProxy;
  int sInstance;
}
class GsmCellLocation {
  int mPsc;
  int mCid;
  int mLac;
}
