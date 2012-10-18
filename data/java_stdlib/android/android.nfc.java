package android.nfc;
class TransceiveResult {
  int CREATOR;
  int mResponseData;
  int mResult;
  int RESULT_EXCEEDED_LENGTH;
  int RESULT_TAGLOST;
  int RESULT_FAILURE;
  int RESULT_SUCCESS;
}
class TechListParcel {
  int CREATOR;
  int mTechLists;
}
class TagLostException {
}
class Tag {
  int CREATOR;
  int mConnectedTechnology;
  int mTagService;
  int mServiceHandle;
  int mTechExtras;
  int mTechStringList;
  int mTechList;
  int mId;
}
class NfcManager {
  int mAdapter;
}
class NfcEvent {
  int nfcAdapter;
}
class NfcAdapter {
  int mForegroundDispatchListener;
  class CreateBeamUrisCallback {
  }
  class CreateNdefMessageCallback {
  }
  class OnNdefPushCompleteCallback {
  }
  int mContext;
  int mNfcActivityManager;
  int sNullContextNfcAdapter;
  int sNfcAdapters;
  int sTagService;
  int sService;
  int sIsInitialized;
  int EXTRA_HANDOVER_TRANSFER_URI;
  int HANDOVER_TRANSFER_STATUS_FAILURE;
  int HANDOVER_TRANSFER_STATUS_SUCCESS;
  int EXTRA_HANDOVER_TRANSFER_STATUS;
  int ACTION_HANDOVER_TRANSFER_DONE;
  int ACTION_HANDOVER_TRANSFER_STARTED;
  int STATE_TURNING_OFF;
  int STATE_ON;
  int STATE_TURNING_ON;
  int STATE_OFF;
  int EXTRA_ADAPTER_STATE;
  int ACTION_ADAPTER_STATE_CHANGED;
  int EXTRA_ID;
  int EXTRA_NDEF_MESSAGES;
  int EXTRA_TAG;
  int ACTION_TAG_LEFT_FIELD;
  int ACTION_TAG_DISCOVERED;
  int ACTION_TECH_DISCOVERED;
  int ACTION_NDEF_DISCOVERED;
  int TAG;
}
class NfcActivityManager {
  class NfcActivityState {
    int uris;
    int uriCallback;
    int onNdefPushCompleteCallback;
    int ndefMessageCallback;
    int ndefMessage;
    int activity;
    int resumed;
  }
  class NfcApplicationState {
    int app;
    int refCount;
  }
  int mActivities;
  int mApps;
  int mDefaultEvent;
  int mAdapter;
  int DBG;
  int TAG;
}
class NdefRecord {
  int CREATOR;
  int mPayload;
  int mId;
  int mType;
  int mTnf;
  int EMPTY_BYTE_ARRAY;
  int MAX_PAYLOAD_SIZE;
  int URI_PREFIX_MAP;
  int FLAG_IL;
  int FLAG_SR;
  int FLAG_CF;
  int FLAG_ME;
  int FLAG_MB;
  int RTD_ANDROID_APP;
  int RTD_HANDOVER_SELECT;
  int RTD_HANDOVER_REQUEST;
  int RTD_HANDOVER_CARRIER;
  int RTD_ALTERNATIVE_CARRIER;
  int RTD_SMART_POSTER;
  int RTD_URI;
  int RTD_TEXT;
  int TNF_RESERVED;
  int TNF_UNCHANGED;
  int TNF_UNKNOWN;
  int TNF_EXTERNAL_TYPE;
  int TNF_ABSOLUTE_URI;
  int TNF_MIME_MEDIA;
  int TNF_WELL_KNOWN;
  int TNF_EMPTY;
}
class NdefMessage {
  int CREATOR;
  int mRecords;
}
class FormatException {
}
class ErrorCodes {
  int ERROR_NOT_SUPPORTED;
  int ERROR_NO_SE_CONNECTED;
  int ERROR_SE_CONNECTED;
  int ERROR_SE_ALREADY_SELECTED;
  int ERROR_NOT_INITIALIZED;
  int ERROR_NFC_ON;
  int ERROR_SOCKET_OPTIONS;
  int ERROR_SERVICE_NAME_USED;
  int ERROR_SAP_USED;
  int ERROR_BUFFER_TO_SMALL;
  int ERROR_SOCKET_NOT_CONNECTED;
  int ERROR_SOCKET_CREATION;
  int ERROR_INSUFFICIENT_RESOURCES;
  int ERROR_INVALID_PARAM;
  int ERROR_WRITE;
  int ERROR_READ;
  int ERROR_DISCONNECT;
  int ERROR_CONNECT;
  int ERROR_BUSY;
  int ERROR_TIMEOUT;
  int ERROR_CANCELLED;
  int ERROR_IO;
  int SUCCESS;
}
class ApduList {
  int CREATOR;
  int commands;
}
