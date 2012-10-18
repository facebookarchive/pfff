package com.android.internal.telephony;
class WspTypeDecoder {
  int contentParameters;
  int stringValue;
  int unsigned32bit;
  int dataLength;
  int wspData;
  int CONTENT_TYPE_B_PUSH_SYNCML_NOTI;
  int CONTENT_TYPE_B_MMS;
  int CONTENT_TYPE_B_PUSH_CO;
  int Q_VALUE;
  int PARAMETER_ID_X_WAP_APPLICATION_ID;
  int WELL_KNOWN_PARAMETERS;
  int WELL_KNOWN_MIME_TYPES;
  int PDU_TYPE_CONFIRMED_PUSH;
  int PDU_TYPE_PUSH;
  int WAP_PDU_LENGTH_QUOTE;
  int WAP_PDU_SHORT_LENGTH_MAX;
}
class WapPushOverSms {
  class WapPushConnection {
    int mOwner;
    int mWapPushMan;
  }
  int mWapConn;
  int BIND_RETRY_INTERVAL;
  int WAKE_LOCK_TIMEOUT;
  int mSmsDispatcher;
  int pduDecoder;
  int mContext;
  int LOG_TAG;
}
class WapPushManagerParams {
  int FURTHER_PROCESSING;
  int EXCEPTION_CAUGHT;
  int INVALID_RECEIVER_NAME;
  int SIGNATURE_NO_MATCH;
  int APP_QUERY_FAILED;
  int MESSAGE_HANDLED;
  int APP_TYPE_SERVICE;
  int APP_TYPE_ACTIVITY;
}
class Wap230WspContentTypeTest {
  int TYPED_PARAM_MAC;
  int TYPED_PARAM_SEC;
  int PARAM_NO_VALUE;
  int PARAM_UNASSIGNED;
  int TYPED_PARAM_DOMAIN;
  int TYPED_PARAM_Q;
  int STRING_MIME_TYPE_ROLLOVER_CERTIFICATE;
  int SHORT_MIME_TYPE_UNASSIGNED;
  int SHORT_MIME_TYPE_ROLLOVER_CERTIFICATE;
  int LONG_MIME_TYPE_UNASSIGNED;
  int LONG_MIME_TYPE_OMA_DIRECTORY_XML;
  int WSP_QUOTE;
  int WSP_LENGTH_QUOTE;
  int WSP_SHORT_INTEGER_MASK;
  int WSP_STRING_TERMINATOR;
  int WSP_DEFINED_LONG_MIME_TYPE_COUNT;
  int WSP_DEFINED_SHORT_MIME_TYPE_COUNT;
  int WELL_KNOWN_PARAMETERS;
  int WELL_KNOWN_LONG_MIME_TYPES;
  int WELL_KNOWN_SHORT_MIME_TYPES;
}
class UUSInfo {
  int uusData;
  int uusDcs;
  int uusType;
  int UUS_DCS_IA5c;
  int UUS_DCS_RMCF;
  int UUS_DCS_X244;
  int UUS_DCS_OSIHLP;
  int UUS_DCS_USP;
  int UUS_TYPE3_NOT_REQUIRED;
  int UUS_TYPE3_REQUIRED;
  int UUS_TYPE2_NOT_REQUIRED;
  int UUS_TYPE2_REQUIRED;
  int UUS_TYPE1_NOT_REQUIRED;
  int UUS_TYPE1_REQUIRED;
  int UUS_TYPE1_IMPLICIT;
}
class TestPhoneNotifier {
}
class TelephonyUtilsTest {
}
class TelephonyProperties {
  int PROPERTY_IGNORE_NITZ;
  int PROPERTY_TEST_CSIM;
  int PROPERTY_SMS_SEND;
  int PROPERTY_SMS_RECEIVE;
  int PROPERTY_RESET_ON_RADIO_TECH_CHANGE;
  int PROPERTY_WAKE_LOCK_TIMEOUT;
  int PROPERTY_CDMA_MSG_ID;
  int PROPERTY_CALL_RING_DELAY;
  int PROPERTY_RIL_SENDS_MULTIPLE_CALL_RING;
  int PROPERTY_DISABLE_CALL;
  int PROPERTY_OTASP_NUM_SCHEMA;
  int PROPERTY_IDP_STRING;
  int PROPERTY_ECM_EXIT_TIMER;
  int PROPERTY_INECM_MODE;
  int PROPERTY_DATA_NETWORK_TYPE;
  int PROPERTY_ICC_OPERATOR_ISO_COUNTRY;
  int PROPERTY_ICC_OPERATOR_ALPHA;
  int PROPERTY_ICC_OPERATOR_NUMERIC;
  int PROPERTY_SIM_STATE;
  int CURRENT_ACTIVE_PHONE;
  int PROPERTY_LTE_ON_CDMA_DEVICE;
  int PROPERTY_LTE_ON_CDMA_PRODUCT_TYPE;
  int PROPERTY_OPERATOR_ISO_COUNTRY;
  int PROPERTY_OPERATOR_ISROAMING;
  int PROPERTY_OPERATOR_ISMANUAL;
  int PROPERTY_OPERATOR_NUMERIC;
  int PROPERTY_OPERATOR_ALPHA;
  int PROPERTY_RIL_IMPL;
  int PROPERTY_BASEBAND_VERSION;
}
class TelephonyIntents {
  int ACTION_SHOW_NOTICE_ECM_BLOCK_OTHERS;
  int ACTION_NETWORK_SET_TIMEZONE;
  int ACTION_NETWORK_SET_TIME;
  int ACTION_SIM_STATE_CHANGED;
  int ACTION_DATA_CONNECTION_FAILED;
  int ACTION_ANY_DATA_CONNECTION_STATE_CHANGED;
  int ACTION_SIGNAL_STRENGTH_CHANGED;
  int ACTION_EMERGENCY_CALLBACK_MODE_CHANGED;
  int ACTION_RADIO_TECHNOLOGY_CHANGED;
  int ACTION_SERVICE_STATE_CHANGED;
}
class TelephonyCapabilities {
  int LOG_TAG;
}
class SmsUsageMonitor {
  int mSmsStamp;
  int mMaxAllowed;
  int mCheckPeriod;
  int DEFAULT_SMS_MAX_COUNT;
  int DEFAULT_SMS_CHECK_PERIOD;
  int VDBG;
  int DBG;
  int TAG;
}
class SmsStorageMonitor {
  int mResultReceiver;
  int WAKE_LOCK_TIMEOUT;
  int mStorageAvailable;
  int mCm;
  int mReportMemoryStatusPending;
  int mWakeLock;
  int mContext;
  int EVENT_RADIO_ON;
  int EVENT_REPORT_MEMORY_STATUS_DONE;
  int EVENT_ICC_FULL;
  int TAG;
}
class SmsResponse {
  int errorCode;
  int ackPdu;
  int messageRef;
}
class SmsRawData {
  int CREATOR;
  int data;
}
class SmsMessageBodyTest {
  class CounterHelper {
    int mUnicodeCounter;
    int mStatsCounters;
    int mCounters;
  }
  class LanguagePair {
    int missingChars7bit;
    int length;
    int langShiftTableIndex;
    int langTableIndex;
  }
  int UDH_SEPTET_COST_CONCATENATED_MESSAGE;
  int UDH_SEPTET_COST_TWO_SHIFT_TABLES;
  int UDH_SEPTET_COST_ONE_SHIFT_TABLE;
  int UDH_SEPTET_COST_LENGTH;
  int sLanguagePairIndexesByEnabledIndex;
  int sEnabledLockingShiftTables;
  int sEnabledSingleShiftTables;
  int sUnicodeUnitsRemaining;
  int sSeptetUnitsRemaining;
  int sTestMsgCounts;
  int sUnicodeTestLengths;
  int sSeptetTestLengths;
  int sTestLengthCount;
  int sCharClassPresenceInTables;
  int sNumCharacterClasses;
  int sCharacterClasses;
  int sGreekLettersInPortugueseShiftTable;
  int sGreekLettersNotInPortugueseTables;
  int sPortugueseLockingShiftChars;
  int sPortugueseChars;
  int sNationalLanguageTablesOnly;
  int sPortugueseAndSpanishChars;
  int sTurkishChars;
  int sUnicodeChars;
  int sGsmExtendedEuroSymbol;
  int sGsmExtendedPortugueseLocking;
  int sGsmExtendedAsciiChars;
  int sGsmDefaultTableOnly;
  int sGsmDefaultAndTurkishTables;
  int sGsmDefaultChars;
  int sAsciiChars;
  int TAG;
}
class SmsMessageBase {
  class SubmitPduBase {
    int encodedMessage;
    int encodedScAddress;
  }
  class TextEncodingDetails {
    int languageShiftTable;
    int languageTable;
    int codeUnitSize;
    int codeUnitsRemaining;
    int codeUnitCount;
    int msgCount;
  }
  int messageRef;
  int indexOnIcc;
  int statusOnIcc;
  int mwiDontStore;
  int mwiSense;
  int isMwi;
  int userDataHeader;
  int userData;
  int mPdu;
  int scTimeMillis;
  int isEmail;
  int emailBody;
  int emailFrom;
  int pseudoSubject;
  int messageBody;
  int originatingAddress;
  int scAddress;
  int LOG_TAG;
}
class SmsHeader {
  int languageShiftTable;
  int languageTable;
  int miscEltList;
  int concatRef;
  int portAddrs;
  class MiscElt {
    int data;
    int id;
  }
  class ConcatRef {
    int isEightBits;
    int msgCount;
    int seqNumber;
    int refNumber;
  }
  class PortAddrs {
    int areEightBits;
    int origPort;
    int destPort;
  }
  int PORT_WAP_WSP;
  int PORT_WAP_PUSH;
  int ELT_ID_NATIONAL_LANGUAGE_LOCKING_SHIFT;
  int ELT_ID_NATIONAL_LANGUAGE_SINGLE_SHIFT;
  int ELT_ID_ENHANCED_VOICE_MAIL_INFORMATION;
  int ELT_ID_REPLY_ADDRESS_ELEMENT;
  int ELT_ID_HYPERLINK_FORMAT_ELEMENT;
  int ELT_ID_RFC_822_EMAIL_HEADER;
  int ELT_ID_EXTENDED_OBJECT_DATA_REQUEST_CMD;
  int ELT_ID_CHARACTER_SIZE_WVG_OBJECT;
  int ELT_ID_STANDARD_WVG_OBJECT;
  int ELT_ID_OBJECT_DISTR_INDICATOR;
  int ELT_ID_COMPRESSION_CONTROL;
  int ELT_ID_REUSED_EXTENDED_OBJECT;
  int ELT_ID_EXTENDED_OBJECT;
  int ELT_ID_USER_PROMPT_INDICATOR;
  int ELT_ID_VARIABLE_PICTURE;
  int ELT_ID_SMALL_PICTURE;
  int ELT_ID_LARGE_PICTURE;
  int ELT_ID_SMALL_ANIMATION;
  int ELT_ID_LARGE_ANIMATION;
  int ELT_ID_PREDEFINED_ANIMATION;
  int ELT_ID_USER_DEFINED_SOUND;
  int ELT_ID_PREDEFINED_SOUND;
  int ELT_ID_TEXT_FORMATTING;
  int ELT_ID_WIRELESS_CTRL_MSG_PROTOCOL;
  int ELT_ID_CONCATENATED_16_BIT_REFERENCE;
  int ELT_ID_UDH_SOURCE_INDICATION;
  int ELT_ID_SMSC_CONTROL_PARAMS;
  int ELT_ID_APPLICATION_PORT_ADDRESSING_16_BIT;
  int ELT_ID_APPLICATION_PORT_ADDRESSING_8_BIT;
  int ELT_ID_SPECIAL_SMS_MESSAGE_INDICATION;
  int ELT_ID_CONCATENATED_8_BIT_REFERENCE;
}
class SmsAddress {
  int origBytes;
  int address;
  int ton;
  int TON_ABBREVIATED;
  int TON_ALPHANUMERIC;
  int TON_SUBSCRIBER;
  int TON_NETWORK;
  int TON_NATIONAL;
  int TON_INTERNATIONAL;
  int TON_UNKNOWN;
}
class SimUtilsTest {
}
class SimSmsTest {
}
class SimPhoneBookTest {
}
class ServiceStateTracker {
  int mWantSingleLocationUpdate;
  int mWantContinuousLocationUpdates;
  int REGISTRATION_DENIED_AUTH;
  int REGISTRATION_DENIED_GEN;
  int GMT_COUNTRY_CODES;
  int TIMEZONE_PROPERTY;
  int EVENT_RADIO_ON;
  int EVENT_CDMA_PRL_VERSION_CHANGED;
  int EVENT_CDMA_SUBSCRIPTION_SOURCE_CHANGED;
  int EVENT_SET_RADIO_POWER_OFF;
  int EVENT_OTA_PROVISION_STATUS_CHANGE;
  int EVENT_ERI_FILE_LOADED;
  int EVENT_NV_READY;
  int EVENT_POLL_STATE_CDMA_SUBSCRIPTION;
  int EVENT_NV_LOADED;
  int EVENT_SIGNAL_STRENGTH_UPDATE_CDMA;
  int EVENT_GET_LOC_DONE_CDMA;
  int EVENT_NETWORK_STATE_CHANGED_CDMA;
  int EVENT_GET_SIGNAL_STRENGTH_CDMA;
  int EVENT_POLL_SIGNAL_STRENGTH_CDMA;
  int EVENT_RUIM_RECORDS_LOADED;
  int EVENT_RUIM_READY;
  int EVENT_POLL_STATE_OPERATOR_CDMA;
  int EVENT_POLL_STATE_REGISTRATION_CDMA;
  int EVENT_RESTRICTED_STATE_CHANGED;
  int EVENT_CHECK_REPORT_GPRS;
  int EVENT_RESET_PREFERRED_NETWORK_TYPE;
  int EVENT_SET_PREFERRED_NETWORK_TYPE;
  int EVENT_GET_PREFERRED_NETWORK_TYPE;
  int EVENT_LOCATION_UPDATES_ENABLED;
  int EVENT_SIM_READY;
  int EVENT_SIM_RECORDS_LOADED;
  int EVENT_GET_LOC_DONE;
  int EVENT_POLL_STATE_NETWORK_SELECTION_MODE;
  int EVENT_RADIO_AVAILABLE;
  int EVENT_SIGNAL_STRENGTH_UPDATE;
  int EVENT_NITZ_TIME;
  int EVENT_POLL_SIGNAL_STRENGTH;
  int EVENT_POLL_STATE_OPERATOR;
  int EVENT_POLL_STATE_GPRS;
  int EVENT_POLL_STATE_REGISTRATION;
  int EVENT_GET_SIGNAL_STRENGTH;
  int EVENT_NETWORK_STATE_CHANGED;
  int EVENT_RADIO_STATE_CHANGED;
  int DEFAULT_GPRS_CHECK_PERIOD_MILLIS;
  int POLL_PERIOD_MILLIS;
  int DBG;
  int mPendingRadioPowerOffAfterDataOffTag;
  int mPendingRadioPowerOffAfterDataOff;
  int mPsRestrictDisabledRegistrants;
  int mPsRestrictEnabledRegistrants;
  int mNetworkAttachedRegistrants;
  int mDetachedRegistrants;
  int mAttachedRegistrants;
  int mRoamingOffRegistrants;
  int mRoamingOnRegistrants;
  int dontPollSignalStrength;
  int mNewRilRadioTechnology;
  int mRilRadioTechnology;
  int mDesiredPowerState;
  int pollingContext;
  int OTASP_NOT_NEEDED;
  int OTASP_NEEDED;
  int OTASP_UNKNOWN;
  int OTASP_UNINITIALIZED;
  int mRestrictedState;
  int mSignalStrength;
  int newSS;
  int ss;
  int cm;
}
class SMSDispatcherTest {
}
class SMSDispatcher {
  int mResultReceiver;
  class ConfirmDialogListener {
    int mTracker;
  }
  class SmsTracker {
    int mDestAddress;
    int mAppPackage;
    int mDeliveryIntent;
    int mSentIntent;
    int mMessageRef;
    int mRetryCount;
    int mData;
  }
  int deliveryPendingList;
  int mRemainingMessages;
  int mSmsSendDisabled;
  int mSmsReceiveDisabled;
  int mSmsCapable;
  int WAKE_LOCK_TIMEOUT;
  int mWakeLock;
  int mPendingTrackerCount;
  int mUsageMonitor;
  int sConcatenatedRef;
  int MO_MSG_QUEUE_LIMIT;
  int SINGLE_PART_SMS;
  int SEND_RETRY_DELAY;
  int MAX_SEND_RETRIES;
  int mRawUri;
  int mWapPush;
  int mTelephonyManager;
  int mStorageMonitor;
  int mCm;
  int mResolver;
  int mContext;
  int mPhone;
  int EVENT_STOP_SENDING;
  int EVENT_SEND_CONFIRMED_SMS;
  int EVENT_SEND_LIMIT_REACHED_CONFIRMATION;
  int EVENT_SEND_RETRY;
  int EVENT_SEND_SMS_COMPLETE;
  int EVENT_NEW_SMS;
  int DESTINATION_PORT_COLUMN;
  int SEQUENCE_COLUMN;
  int PDU_COLUMN;
  int PDU_SEQUENCE_PORT_PROJECTION;
  int PDU_PROJECTION;
  int SEND_SMS_NO_CONFIRMATION_PERMISSION;
  int RECEIVE_EMERGENCY_BROADCAST_PERMISSION;
  int RECEIVE_SMS_PERMISSION;
  int SEND_NEXT_MSG_EXTRA;
  int TAG;
}
class RetryManager {
  int mConfig;
  int rng;
  int mRetryCount;
  int mMaxRetryCount;
  int mRetryForever;
  int mRetryArray;
  class RetryRec {
    int mRandomizationTime;
    int mDelayTime;
  }
  int VDBG;
  int DBG;
  int LOG_TAG;
}
class RestrictedState {
  int mCsEmergencyRestricted;
  int mCsNormalRestricted;
  int mPsRestricted;
}
class RILConstants {
  int RIL_UNSOL_VOICE_RADIO_TECH_CHANGED;
  int RIL_UNSOL_RIL_CONNECTED;
  int RIL_UNSOL_EXIT_EMERGENCY_CALLBACK_MODE;
  int RIL_UNSOl_CDMA_PRL_CHANGED;
  int RIL_UNSOL_CDMA_SUBSCRIPTION_SOURCE_CHANGED;
  int RIL_UNSOL_RESEND_INCALL_MUTE;
  int RIL_UNSOL_RINGBACK_TONE;
  int RIL_UNSOL_OEM_HOOK_RAW;
  int RIL_UNSOL_CDMA_INFO_REC;
  int RIL_UNSOL_CDMA_OTA_PROVISION_STATUS;
  int RIL_UNSOL_CDMA_CALL_WAITING;
  int RIL_UNSOL_ENTER_EMERGENCY_CALLBACK_MODE;
  int RIL_UNSOL_RESTRICTED_STATE_CHANGED;
  int RIL_UNSOL_CDMA_RUIM_SMS_STORAGE_FULL;
  int RIL_UNSOL_RESPONSE_NEW_BROADCAST_SMS;
  int RIL_UNSOL_RESPONSE_CDMA_NEW_SMS;
  int RIL_UNSOL_RESPONSE_SIM_STATUS_CHANGED;
  int RIL_UNSOL_CALL_RING;
  int RIL_UNSOL_SIM_REFRESH;
  int RIL_UNSOL_SIM_SMS_STORAGE_FULL;
  int RIL_UNSOL_STK_CALL_SETUP;
  int RIL_UNSOL_STK_EVENT_NOTIFY;
  int RIL_UNSOL_STK_PROACTIVE_COMMAND;
  int RIL_UNSOL_STK_SESSION_END;
  int RIL_UNSOL_SUPP_SVC_NOTIFICATION;
  int RIL_UNSOL_DATA_CALL_LIST_CHANGED;
  int RIL_UNSOL_SIGNAL_STRENGTH;
  int RIL_UNSOL_NITZ_TIME_RECEIVED;
  int RIL_UNSOL_ON_USSD_REQUEST;
  int RIL_UNSOL_ON_USSD;
  int RIL_UNSOL_RESPONSE_NEW_SMS_ON_SIM;
  int RIL_UNSOL_RESPONSE_NEW_SMS_STATUS_REPORT;
  int RIL_UNSOL_RESPONSE_NEW_SMS;
  int RIL_UNSOL_RESPONSE_VOICE_NETWORK_STATE_CHANGED;
  int RIL_UNSOL_RESPONSE_CALL_STATE_CHANGED;
  int RIL_UNSOL_RESPONSE_RADIO_STATE_CHANGED;
  int RIL_UNSOL_RESPONSE_BASE;
  int RIL_REQUEST_VOICE_RADIO_TECH;
  int RIL_REQUEST_STK_SEND_ENVELOPE_WITH_STATUS;
  int RIL_REQUEST_ACKNOWLEDGE_INCOMING_GSM_SMS_WITH_PDU;
  int RIL_REQUEST_ISIM_AUTHENTICATION;
  int RIL_REQUEST_CDMA_GET_SUBSCRIPTION_SOURCE;
  int RIL_REQUEST_REPORT_STK_SERVICE_IS_RUNNING;
  int RIL_REQUEST_REPORT_SMS_MEMORY_STATUS;
  int RIL_REQUEST_SET_SMSC_ADDRESS;
  int RIL_REQUEST_GET_SMSC_ADDRESS;
  int RIL_REQUEST_EXIT_EMERGENCY_CALLBACK_MODE;
  int RIL_REQUEST_DEVICE_IDENTITY;
  int RIL_REQUEST_CDMA_DELETE_SMS_ON_RUIM;
  int RIL_REQUEST_CDMA_WRITE_SMS_TO_RUIM;
  int RIL_REQUEST_CDMA_SUBSCRIPTION;
  int RIL_REQUEST_CDMA_BROADCAST_ACTIVATION;
  int RIL_REQUEST_CDMA_SET_BROADCAST_CONFIG;
  int RIL_REQUEST_CDMA_GET_BROADCAST_CONFIG;
  int RIL_REQUEST_GSM_BROADCAST_ACTIVATION;
  int RIL_REQUEST_GSM_SET_BROADCAST_CONFIG;
  int RIL_REQUEST_GSM_GET_BROADCAST_CONFIG;
  int RIL_REQUEST_CDMA_SMS_ACKNOWLEDGE;
  int RIL_REQUEST_CDMA_SEND_SMS;
  int RIL_REQUEST_CDMA_VALIDATE_AND_WRITE_AKEY;
  int RIL_REQUEST_CDMA_BURST_DTMF;
  int RIL_REQUEST_CDMA_FLASH;
  int RIL_REQUEST_CDMA_QUERY_PREFERRED_VOICE_PRIVACY_MODE;
  int RIL_REQUEST_CDMA_SET_PREFERRED_VOICE_PRIVACY_MODE;
  int RIL_REQUEST_QUERY_TTY_MODE;
  int RIL_REQUEST_SET_TTY_MODE;
  int RIL_REQUEST_CDMA_QUERY_ROAMING_PREFERENCE;
  int RIL_REQUEST_CDMA_SET_ROAMING_PREFERENCE;
  int RIL_REQUEST_CDMA_SET_SUBSCRIPTION_SOURCE;
  int RIL_REQUEST_SET_LOCATION_UPDATES;
  int RIL_REQUEST_GET_NEIGHBORING_CELL_IDS;
  int RIL_REQUEST_GET_PREFERRED_NETWORK_TYPE;
  int RIL_REQUEST_SET_PREFERRED_NETWORK_TYPE;
  int RIL_REQUEST_EXPLICIT_CALL_TRANSFER;
  int RIL_REQUEST_STK_HANDLE_CALL_SETUP_REQUESTED_FROM_SIM;
  int RIL_REQUEST_STK_SEND_TERMINAL_RESPONSE;
  int RIL_REQUEST_STK_SEND_ENVELOPE_COMMAND;
  int RIL_REQUEST_STK_SET_PROFILE;
  int RIL_REQUEST_STK_GET_PROFILE;
  int RIL_REQUEST_QUERY_AVAILABLE_BAND_MODE;
  int RIL_REQUEST_SET_BAND_MODE;
  int RIL_REQUEST_DELETE_SMS_ON_SIM;
  int RIL_REQUEST_WRITE_SMS_TO_SIM;
  int RIL_REQUEST_SET_SUPP_SVC_NOTIFICATION;
  int RIL_REQUEST_SCREEN_STATE;
  int RIL_REQUEST_OEM_HOOK_STRINGS;
  int RIL_REQUEST_OEM_HOOK_RAW;
  int RIL_REQUEST_RESET_RADIO;
  int RIL_REQUEST_DATA_CALL_LIST;
  int RIL_REQUEST_LAST_DATA_CALL_FAIL_CAUSE;
  int RIL_REQUEST_QUERY_CLIP;
  int RIL_REQUEST_GET_MUTE;
  int RIL_REQUEST_SET_MUTE;
  int RIL_REQUEST_SEPARATE_CONNECTION;
  int RIL_REQUEST_BASEBAND_VERSION;
  int RIL_REQUEST_DTMF_STOP;
  int RIL_REQUEST_DTMF_START;
  int RIL_REQUEST_QUERY_AVAILABLE_NETWORKS;
  int RIL_REQUEST_SET_NETWORK_SELECTION_MANUAL;
  int RIL_REQUEST_SET_NETWORK_SELECTION_AUTOMATIC;
  int RIL_REQUEST_QUERY_NETWORK_SELECTION_MODE;
  int RIL_REQUEST_CHANGE_BARRING_PASSWORD;
  int RIL_REQUEST_SET_FACILITY_LOCK;
  int RIL_REQUEST_QUERY_FACILITY_LOCK;
  int RIL_REQUEST_DEACTIVATE_DATA_CALL;
  int RIL_REQUEST_ANSWER;
  int RIL_REQUEST_GET_IMEISV;
  int RIL_REQUEST_GET_IMEI;
  int RIL_REQUEST_SMS_ACKNOWLEDGE;
  int RIL_REQUEST_SET_CALL_WAITING;
  int RIL_REQUEST_QUERY_CALL_WAITING;
  int RIL_REQUEST_SET_CALL_FORWARD;
  int RIL_REQUEST_QUERY_CALL_FORWARD_STATUS;
  int RIL_REQUEST_SET_CLIR;
  int RIL_REQUEST_GET_CLIR;
  int RIL_REQUEST_CANCEL_USSD;
  int RIL_REQUEST_SEND_USSD;
  int RIL_REQUEST_SIM_IO;
  int RIL_REQUEST_SETUP_DATA_CALL;
  int RIL_REQUEST_SEND_SMS_EXPECT_MORE;
  int RIL_REQUEST_SEND_SMS;
  int RIL_REQUEST_DTMF;
  int RIL_REQUEST_RADIO_POWER;
  int RIL_REQUEST_OPERATOR;
  int RIL_REQUEST_DATA_REGISTRATION_STATE;
  int RIL_REQUEST_VOICE_REGISTRATION_STATE;
  int RIL_REQUEST_SIGNAL_STRENGTH;
  int RIL_REQUEST_LAST_CALL_FAIL_CAUSE;
  int RIL_REQUEST_UDUB;
  int RIL_REQUEST_CONFERENCE;
  int RIL_REQUEST_SWITCH_WAITING_OR_HOLDING_AND_ACTIVE;
  int RIL_REQUEST_HANGUP_FOREGROUND_RESUME_BACKGROUND;
  int RIL_REQUEST_HANGUP_WAITING_OR_BACKGROUND;
  int RIL_REQUEST_HANGUP;
  int RIL_REQUEST_GET_IMSI;
  int RIL_REQUEST_DIAL;
  int RIL_REQUEST_GET_CURRENT_CALLS;
  int RIL_REQUEST_ENTER_NETWORK_DEPERSONALIZATION;
  int RIL_REQUEST_CHANGE_SIM_PIN2;
  int RIL_REQUEST_CHANGE_SIM_PIN;
  int RIL_REQUEST_ENTER_SIM_PUK2;
  int RIL_REQUEST_ENTER_SIM_PIN2;
  int RIL_REQUEST_ENTER_SIM_PUK;
  int RIL_REQUEST_ENTER_SIM_PIN;
  int RIL_REQUEST_GET_SIM_STATUS;
  int DATA_PROFILE_OEM_BASE;
  int DATA_PROFILE_CBS;
  int DATA_PROFILE_FOTA;
  int DATA_PROFILE_IMS;
  int DATA_PROFILE_TETHERED;
  int DATA_PROFILE_DEFAULT;
  int RIL_RESTRICTED_STATE_PS_ALL;
  int RIL_RESTRICTED_STATE_CS_ALL;
  int RIL_RESTRICTED_STATE_CS_NORMAL;
  int RIL_RESTRICTED_STATE_CS_EMERGENCY;
  int RIL_RESTRICTED_STATE_NONE;
  int DEACTIVATE_REASON_PDP_RESET;
  int DEACTIVATE_REASON_RADIO_OFF;
  int DEACTIVATE_REASON_NONE;
  int SETUP_DATA_PROTOCOL_IPV4V6;
  int SETUP_DATA_PROTOCOL_IPV6;
  int SETUP_DATA_PROTOCOL_IP;
  int SETUP_DATA_AUTH_PAP_CHAP;
  int SETUP_DATA_AUTH_CHAP;
  int SETUP_DATA_AUTH_PAP;
  int SETUP_DATA_AUTH_NONE;
  int SETUP_DATA_TECH_GSM;
  int SETUP_DATA_TECH_CDMA;
  int CDM_TTY_VCO_MODE;
  int CDM_TTY_HCO_MODE;
  int CDM_TTY_FULL_MODE;
  int CDM_TTY_MODE_ENABLED;
  int CDM_TTY_MODE_DISABLED;
  int LTE_ON_CDMA_TRUE;
  int LTE_ON_CDMA_FALSE;
  int LTE_ON_CDMA_UNKNOWN;
  int SIP_PHONE;
  int CDMA_PHONE;
  int GSM_PHONE;
  int NO_PHONE;
  int CDMA_CELL_BROADCAST_SMS_ENABLED;
  int CDMA_CELL_BROADCAST_SMS_DISABLED;
  int PREFERRED_NETWORK_MODE;
  int NETWORK_MODE_LTE_ONLY;
  int NETWORK_MODE_LTE_CMDA_EVDO_GSM_WCDMA;
  int NETWORK_MODE_LTE_GSM_WCDMA;
  int NETWORK_MODE_LTE_CDMA_EVDO;
  int NETWORK_MODE_GLOBAL;
  int NETWORK_MODE_EVDO_NO_CDMA;
  int NETWORK_MODE_CDMA_NO_EVDO;
  int NETWORK_MODE_CDMA;
  int NETWORK_MODE_GSM_UMTS;
  int NETWORK_MODE_WCDMA_ONLY;
  int NETWORK_MODE_GSM_ONLY;
  int NETWORK_MODE_WCDMA_PREF;
  int ILLEGAL_SIM_OR_ME;
  int FDN_CHECK_FAILURE;
  int MODE_NOT_SUPPORTED;
  int SUBSCRIPTION_NOT_AVAILABLE;
  int SIM_ABSENT;
  int SMS_SEND_FAIL_RETRY;
  int OP_NOT_ALLOWED_BEFORE_REG_NW;
  int OP_NOT_ALLOWED_DURING_VOICE_CALL;
  int REQUEST_CANCELLED;
  int REQUEST_NOT_SUPPORTED;
  int SIM_PUK2;
  int SIM_PIN2;
  int PASSWORD_INCORRECT;
  int GENERIC_FAILURE;
  int RADIO_NOT_AVAILABLE;
  int SUCCESS;
  int MAX_INT;
  int RIL_ERRNO_INVALID_RESPONSE;
}
class RIL {
  int mSetPreferredNetworkType;
  class RILReceiver {
    int buffer;
  }
  class RILSender {
    int dataLength;
  }
  int mIntentReceiver;
  int CDMA_BROADCAST_SMS_NO_OF_SERVICE_CATEGORIES;
  int CDMA_BSI_NO_OF_INTS_STRUCT;
  int SOCKET_OPEN_RETRY_MILLIS;
  int SOCKET_NAME_RIL;
  int RESPONSE_UNSOLICITED;
  int RESPONSE_SOLICITED;
  int RIL_MAX_COMMAND_BYTES;
  int EVENT_WAKE_LOCK_TIMEOUT;
  int EVENT_SEND;
  int mTestingEmergencyCall;
  int mLastNITZTimeInfo;
  int mRequestsList;
  int mRequestMessagesWaiting;
  int mRequestMessagesPending;
  int mWakeLockTimeout;
  int mWakeLock;
  int mReceiver;
  int mReceiverThread;
  int mSender;
  int mSenderThread;
  int mSocket;
  int DEFAULT_WAKE_LOCK_TIMEOUT;
  int RILJ_LOGV;
  int RILJ_LOGD;
  int LOG_TAG;
}
class RILRequest {
  int mNext;
  int mp;
  int mResult;
  int mRequest;
  int mSerial;
  int MAX_POOL_SIZE;
  int sPoolSize;
  int sPool;
  int sPoolSync;
  int sSerialMonitor;
  int sNextSerial;
  int LOG_TAG;
}
class PhoneSubInfoProxy {
  int mPhoneSubInfo;
}
class PhoneSubInfo {
  int READ_PRIVILEGED_PHONE_STATE;
  int CALL_PRIVILEGED;
  int READ_PHONE_STATE;
  int mContext;
  int mPhone;
  int LOG_TAG;
}
class PhoneStateIntentReceiver {
  int mAsuEventWhat;
  int mLocationEventWhat;
  int mServiceStateEventWhat;
  int mPhoneStateEventWhat;
  int mWants;
  int mFilter;
  int mTarget;
  int mContext;
  int mSignalStrength;
  int mServiceState;
  int mPhoneState;
  int NOTIF_MAX;
  int NOTIF_SIGNAL;
  int NOTIF_SERVICE;
  int NOTIF_PHONE;
  int DBG;
  int LOG_TAG;
}
class PhoneProxy {
  int LOG_TAG;
  int EVENT_RIL_CONNECTED;
  int EVENT_REQUEST_VOICE_RADIO_TECH_DONE;
  int EVENT_RADIO_ON;
  int EVENT_VOICE_RADIO_TECH_CHANGED;
  int mRilVersion;
  int mResetModemOnRadioTechnologyChange;
  int mPhoneSubInfoProxy;
  int mIccPhoneBookInterfaceManagerProxy;
  int mIccSmsInterfaceManagerProxy;
  int mCommandsInterface;
  int mActivePhone;
  int lockForRadioTechnologyChange;
}
class PhoneNumberWatcherTest {
}
class PhoneNumberUtilsTest {
}
class PhoneNotifier {
}
class PhoneFactory {
  int preferredCdmaSubscription;
  int sContext;
  int sLooper;
  int sPhoneNotifier;
  int sMadeDefaults;
  int sCommandsInterface;
  int sProxyPhone;
  int SOCKET_OPEN_MAX_RETRY;
  int SOCKET_OPEN_RETRY_MILLIS;
  int LOG_TAG;
}
class PhoneBase {
  int mUnitTestMode;
  int mSimulatedRadioControl;
  int mNotifier;
  int mContext;
  int mLooper;
  int mSuppServiceFailedRegistrants;
  int mUnknownConnectionRegistrants;
  int mMmiRegistrants;
  int mMmiCompleteRegistrants;
  int mServiceStateRegistrants;
  int mDisconnectRegistrants;
  int mIncomingRingRegistrants;
  int mNewRingingConnectionRegistrants;
  int mPreciseCallStateRegistrants;
  int mSMS;
  int mSmsUsageMonitor;
  int mSmsStorageMonitor;
  int mIccCard;
  int mIccRecords;
  int mIsVoiceCapable;
  int mIsTheCurrentActivePhone;
  int mCallRingDelay;
  int mCallRingContinueToken;
  int mDoesRilSendMultipleCallRing;
  int mDataConnectionTracker;
  int mDnsCheckDisabled;
  int mCM;
  int DNS_SERVER_CHECK_DISABLED_KEY;
  int CLIR_KEY;
  int EVENT_ICC_RECORD_EVENTS;
  int EVENT_NEW_ICC_SMS;
  int EVENT_SET_NETWORK_AUTOMATIC;
  int EVENT_CDMA_SUBSCRIPTION_SOURCE_CHANGED;
  int EVENT_EXIT_EMERGENCY_CALLBACK_RESPONSE;
  int EVENT_EMERGENCY_CALLBACK_MODE_ENTER;
  int EVENT_SET_ENHANCED_VP;
  int EVENT_NV_READY;
  int EVENT_RUIM_RECORDS_LOADED;
  int EVENT_GET_DEVICE_IDENTITY_DONE;
  int EVENT_SET_VM_NUMBER_DONE;
  int EVENT_REGISTERED_TO_NETWORK;
  int EVENT_SET_CLIR_COMPLETE;
  int EVENT_SET_NETWORK_AUTOMATIC_COMPLETE;
  int EVENT_SET_NETWORK_MANUAL_COMPLETE;
  int EVENT_CALL_RING_CONTINUE;
  int EVENT_CALL_RING;
  int EVENT_GET_CALL_FORWARD_DONE;
  int EVENT_SET_CALL_FORWARD_DONE;
  int EVENT_GET_SIM_STATUS_DONE;
  int EVENT_GET_IMEISV_DONE;
  int EVENT_GET_IMEI_DONE;
  int EVENT_RADIO_OFF_OR_NOT_AVAILABLE;
  int EVENT_USSD;
  int EVENT_GET_BASEBAND_VERSION_DONE;
  int EVENT_RADIO_ON;
  int EVENT_MMI_DONE;
  int EVENT_SIM_RECORDS_LOADED;
  int EVENT_SSN;
  int EVENT_RADIO_AVAILABLE;
  int DATA_DISABLED_ON_BOOT_KEY;
  int NETWORK_SELECTION_NAME_KEY;
  int NETWORK_SELECTION_KEY;
  int LOCAL_DEBUG;
  int LOG_TAG;
}
class Phone {
  int CDMA_OTA_PROVISION_STATUS_OTAPA_ABORTED;
  int CDMA_OTA_PROVISION_STATUS_OTAPA_STOPPED;
  int CDMA_OTA_PROVISION_STATUS_OTAPA_STARTED;
  int CDMA_OTA_PROVISION_STATUS_COMMITTED;
  int CDMA_OTA_PROVISION_STATUS_PRL_DOWNLOADED;
  int CDMA_OTA_PROVISION_STATUS_IMSI_DOWNLOADED;
  int CDMA_OTA_PROVISION_STATUS_MDN_DOWNLOADED;
  int CDMA_OTA_PROVISION_STATUS_NAM_DOWNLOADED;
  int CDMA_OTA_PROVISION_STATUS_SSD_UPDATED;
  int CDMA_OTA_PROVISION_STATUS_A_KEY_EXCHANGED;
  int CDMA_OTA_PROVISION_STATUS_SPC_RETRIES_EXCEEDED;
  int CDMA_OTA_PROVISION_STATUS_SPL_UNLOCKED;
  int TTY_MODE_VCO;
  int TTY_MODE_HCO;
  int TTY_MODE_FULL;
  int TTY_MODE_OFF;
  int PREFERRED_CDMA_SUBSCRIPTION;
  int CDMA_SUBSCRIPTION_NV;
  int CDMA_SUBSCRIPTION_RUIM_SIM;
  int CDMA_RM_ANY;
  int CDMA_RM_AFFILIATED;
  int CDMA_RM_HOME;
  int PREFERRED_NT_MODE;
  int NT_MODE_LTE_ONLY;
  int NT_MODE_GLOBAL;
  int NT_MODE_EVDO_NO_CDMA;
  int NT_MODE_CDMA_NO_EVDO;
  int NT_MODE_CDMA;
  int NT_MODE_GSM_UMTS;
  int NT_MODE_WCDMA_ONLY;
  int NT_MODE_GSM_ONLY;
  int NT_MODE_WCDMA_PREF;
  int LTE_ON_CDMA_TRUE;
  int LTE_ON_CDMA_FALSE;
  int LTE_ON_CDMA_UNKNOWN;
  int PHONE_TYPE_SIP;
  int PHONE_TYPE_CDMA;
  int PHONE_TYPE_GSM;
  int PHONE_TYPE_NONE;
  int BM_BOUNDARY;
  int BM_AUS2_BAND;
  int BM_AUS_BAND;
  int BM_JPN_BAND;
  int BM_US_BAND;
  int BM_EURO_BAND;
  int BM_UNSPECIFIED;
  int REASON_LINK_PROPERTIES_CHANGED;
  int REASON_DATA_DEPENDENCY_UNMET;
  int REASON_DATA_DEPENDENCY_MET;
  int REASON_NW_TYPE_CHANGED;
  int REASON_SIM_LOADED;
  int REASON_PS_RESTRICT_DISABLED;
  int REASON_PS_RESTRICT_ENABLED;
  int REASON_VOICE_CALL_STARTED;
  int REASON_VOICE_CALL_ENDED;
  int REASON_PDP_RESET;
  int REASON_RADIO_TURNED_OFF;
  int REASON_RESTORE_DEFAULT_APN;
  int REASON_APN_FAILED;
  int REASON_APN_SWITCHED;
  int REASON_APN_CHANGED;
  int REASON_CDMA_DATA_DETACHED;
  int REASON_CDMA_DATA_ATTACHED;
  int REASON_DATA_DETACHED;
  int REASON_DATA_ATTACHED;
  int REASON_DATA_ENABLED;
  int REASON_DATA_DISABLED;
  int REASON_ROAMING_OFF;
  int REASON_ROAMING_ON;
  int APN_ALREADY_INACTIVE;
  int APN_REQUEST_FAILED;
  int APN_TYPE_NOT_AVAILABLE;
  int APN_REQUEST_STARTED;
  int APN_ALREADY_ACTIVE;
  int FEATURE_ENABLE_CBS;
  int FEATURE_ENABLE_IMS;
  int FEATURE_ENABLE_FOTA;
  int FEATURE_ENABLE_DUN_ALWAYS;
  int FEATURE_ENABLE_HIPRI;
  int FEATURE_ENABLE_DUN;
  int FEATURE_ENABLE_SUPL;
  int FEATURE_ENABLE_MMS;
  int APN_TYPE_CBS;
  int APN_TYPE_IMS;
  int APN_TYPE_FOTA;
  int APN_TYPE_HIPRI;
  int APN_TYPE_DUN;
  int APN_TYPE_SUPL;
  int APN_TYPE_MMS;
  int APN_TYPE_DEFAULT;
  int APN_TYPE_ALL;
  int PHONE_IN_ECM_STATE;
  int DATA_NETWORK_ROAMING_KEY;
  int NETWORK_UNAVAILABLE_KEY;
  int DATA_IFACE_NAME_KEY;
  int DATA_LINK_CAPABILITIES_KEY;
  int DATA_LINK_PROPERTIES_KEY;
  int DATA_APN_KEY;
  int DATA_APN_TYPE_KEY;
  int STATE_CHANGE_REASON_KEY;
  int FAILURE_REASON_KEY;
  int PHONE_NAME_KEY;
  int STATE_KEY;
  class SuppService {
    int HANGUP;
    int REJECT;
    int CONFERENCE;
    int TRANSFER;
    int SEPARATE;
    int SWITCH;
    int UNKNOWN;
  }
  class DataActivityState {
    int DORMANT;
    int DATAINANDOUT;
    int DATAOUT;
    int DATAIN;
    int NONE;
  }
  class DataState {
    int SUSPENDED;
    int DISCONNECTED;
    int CONNECTING;
    int CONNECTED;
  }
  class State {
    int OFFHOOK;
    int RINGING;
    int IDLE;
  }
  int DEBUG_PHONE;
}
class OperatorInfo {
  int CREATOR;
  int state;
  int operatorNumeric;
  int operatorAlphaShort;
  int operatorAlphaLong;
  class State {
    int FORBIDDEN;
    int CURRENT;
    int AVAILABLE;
    int UNKNOWN;
  }
}
class NeighboringCellInfoTest {
}
class MmiCode {
  class State {
    int FAILED;
    int COMPLETE;
    int CANCELLED;
    int PENDING;
  }
}
class MccTableTest {
  int LOG_TAG;
}
class MccTable {
  class MccEntry {
    int language;
    int smallestDigitsMnc;
    int iso;
    int mcc;
  }
  int table;
  int LOG_TAG;
}
class IntRangeManagerTest {
  class TestIntRangeManager {
    int finishUpdateReturnValue;
    int flags;
    int mConfigList;
  }
  int ALL_FLAGS_SET;
  int FLAG_FINISH_UPDATE_CALLED;
  int FLAG_ADD_RANGE_CALLED;
  int FLAG_START_UPDATE_CALLED;
  int SMS_CB_CODE_SCHEME_MAX;
  int SMS_CB_CODE_SCHEME_MIN;
}
class IntRangeManager {
  int mRanges;
  class ClientRange {
    int client;
    int endId;
    int startId;
  }
  class IntRange {
    int clients;
    int endId;
    int startId;
  }
  int INITIAL_CLIENTS_ARRAY_SIZE;
}
class IccVmNotSupportedException {
}
class IccVmFixedException {
}
class IccUtils {
  int LOG_TAG;
}
class IccSmsInterfaceManagerProxy {
  int mIccSmsInterfaceManager;
}
class IccSmsInterfaceManager {
  int mDispatcher;
  int mContext;
  int mPhone;
}
class IccServiceTableTest {
  class TestIccServiceTable {
    class TestIccService {
      int SERVICE4;
      int SERVICE3;
      int SERVICE2;
      int SERVICE1;
    }
  }
}
class IccServiceTable {
  int mServiceTable;
}
class IccRefreshResponse {
  int aid;
  int efId;
  int refreshResult;
  int REFRESH_RESULT_RESET;
  int REFRESH_RESULT_INIT;
  int REFRESH_RESULT_FILE_UPDATE;
}
class IccRecords {
  class IccRecordLoaded {
  }
  int EVENT_GET_ICC_RECORD_DONE;
  int EVENT_SPN;
  int EVENT_CFI;
  int EVENT_MWI;
  int EVENT_SET_MSISDN_DONE;
  int SPN_RULE_SHOW_PLMN;
  int SPN_RULE_SHOW_SPN;
  int UNKNOWN;
  int UNINITIALIZED;
  int spn;
  int mailboxIndex;
  int mncLength;
  int countVoiceMessages;
  int isVoiceMailFixed;
  int newVoiceMailTag;
  int newVoiceMailNum;
  int voiceMailTag;
  int voiceMailNum;
  int msisdnTag;
  int msisdn;
  int iccid;
  int recordsRequested;
  int adnCache;
  int recordsToLoad;
  int mNetworkSelectionModeAutomaticRegistrants;
  int mNewSmsRegistrants;
  int mRecordsEventsRegistrants;
  int recordsLoadedRegistrants;
  int mParentCard;
  int mFh;
  int mCi;
  int mContext;
  int mDestroyed;
  int DBG;
}
class IccProvider {
  int URL_MATCHER;
  int STR_PIN2;
  int STR_EMAILS;
  int STR_NUMBER;
  int STR_TAG;
  int SDN;
  int FDN;
  int ADN;
  int ADDRESS_BOOK_COLUMN_NAMES;
  int DBG;
  int TAG;
}
class IccPhoneBookInterfaceManagerProxy {
  int mIccPhoneBookInterfaceManager;
}
class IccPhoneBookInterfaceManager {
  int mBaseHandler;
  int EVENT_UPDATE_DONE;
  int EVENT_LOAD_DONE;
  int EVENT_GET_SIZE_DONE;
  int ALLOW_SIM_OP_IN_UI_THREAD;
  int records;
  int success;
  int recordSize;
  int mLock;
  int adnCache;
  int phone;
  int DBG;
}
class IccIoResult {
  int payload;
  int sw2;
  int sw1;
}
class IccFileTypeMismatch {
}
class IccFileNotFound {
}
class IccFileHandler {
  class LoadLinearFixedContext {
    int results;
    int onLoaded;
    int loadAll;
    int countRecords;
    int recordSize;
    int recordNum;
    int efid;
  }
  int mAid;
  int mParentCard;
  int mCi;
  int EVENT_READ_ICON_DONE;
  int EVENT_READ_IMG_DONE;
  int EVENT_GET_EF_LINEAR_RECORD_SIZE_DONE;
  int EVENT_READ_RECORD_DONE;
  int EVENT_GET_RECORD_SIZE_DONE;
  int EVENT_READ_BINARY_DONE;
  int EVENT_GET_BINARY_SIZE_DONE;
  int RESPONSE_DATA_RECORD_LENGTH;
  int RESPONSE_DATA_STRUCTURE;
  int RESPONSE_DATA_LENGTH;
  int RESPONSE_DATA_FILE_STATUS;
  int RESPONSE_DATA_ACCESS_CONDITION_3;
  int RESPONSE_DATA_ACCESS_CONDITION_2;
  int RESPONSE_DATA_ACCESS_CONDITION_1;
  int RESPONSE_DATA_RFU_3;
  int RESPONSE_DATA_FILE_TYPE;
  int RESPONSE_DATA_FILE_ID_2;
  int RESPONSE_DATA_FILE_ID_1;
  int RESPONSE_DATA_FILE_SIZE_2;
  int RESPONSE_DATA_FILE_SIZE_1;
  int RESPONSE_DATA_RFU_2;
  int RESPONSE_DATA_RFU_1;
  int GET_RESPONSE_EF_IMG_SIZE_BYTES;
  int GET_RESPONSE_EF_SIZE_BYTES;
  int TYPE_EF;
  int TYPE_DF;
  int TYPE_MF;
  int TYPE_RFU;
  int EF_TYPE_CYCLIC;
  int EF_TYPE_LINEAR_FIXED;
  int EF_TYPE_TRANSPARENT;
  int READ_RECORD_MODE_ABSOLUTE;
  int COMMAND_GET_RESPONSE;
  int COMMAND_SEEK;
  int COMMAND_UPDATE_RECORD;
  int COMMAND_READ_RECORD;
  int COMMAND_UPDATE_BINARY;
  int COMMAND_READ_BINARY;
}
class IccException {
}
class IccConstants {
  int DF_ADFISIM;
  int DF_CDMA;
  int DF_GSM;
  int DF_GRAPHICS;
  int DF_PHONEBOOK;
  int DF_TELECOM;
  int MF_SIM;
  int SMS_RECORD_LENGTH;
  int EF_PCSCF;
  int EF_IST;
  int EF_DOMAIN;
  int EF_IMPI;
  int EF_IMPU;
  int EF_CSIM_EPRL;
  int EF_CSIM_CDMAHOME;
  int EF_CSIM_IMSIM;
  int EF_CSIM_MDN;
  int EF_CSIM_SPN;
  int EF_CSIM_LI;
  int EF_PL;
  int EF_RUIM_SPN;
  int EF_CST;
  int EF_CSP_CPHS;
  int EF_INFO_CPHS;
  int EF_SPN_SHORT_CPHS;
  int EF_SPN_CPHS;
  int EF_CFF_CPHS;
  int EF_VOICE_MAIL_INDICATOR_CPHS;
  int EF_MAILBOX_CPHS;
  int EF_PBR;
  int EF_IMG;
  int EF_CFIS;
  int EF_SST;
  int EF_SPDI;
  int EF_MSISDN;
  int EF_MBI;
  int EF_AD;
  int EF_ICCID;
  int EF_SMS;
  int EF_SPN;
  int EF_PNN;
  int EF_MBDN;
  int EF_MWIS;
  int EF_EXT6;
  int EF_EXT3;
  int EF_EXT2;
  int EF_EXT1;
  int EF_SDN;
  int EF_FDN;
  int EF_ADN;
}
class IccCardStatus {
  int mApplications;
  int mNumApplications;
  int mImsSubscriptionAppIndex;
  int mCdmaSubscriptionAppIndex;
  int mGsmUmtsSubscriptionAppIndex;
  int mUniversalPinState;
  int mCardState;
  class PinState {
    int PINSTATE_ENABLED_PERM_BLOCKED;
    int PINSTATE_ENABLED_BLOCKED;
    int PINSTATE_DISABLED;
    int PINSTATE_ENABLED_VERIFIED;
    int PINSTATE_ENABLED_NOT_VERIFIED;
    int PINSTATE_UNKNOWN;
  }
  class CardState {
    int CARDSTATE_ERROR;
    int CARDSTATE_PRESENT;
    int CARDSTATE_ABSENT;
  }
  int CARD_MAX_APPS;
}
class IccCardApplication {
  int pin2;
  int pin1;
  int pin1_replaced;
  int app_label;
  int aid;
  int perso_substate;
  int app_state;
  int app_type;
  class PersoSubState {
    int PERSOSUBSTATE_RUIM_RUIM_PUK;
    int PERSOSUBSTATE_RUIM_SERVICE_PROVIDER_PUK;
    int PERSOSUBSTATE_RUIM_CORPORATE_PUK;
    int PERSOSUBSTATE_RUIM_HRPD_PUK;
    int PERSOSUBSTATE_RUIM_NETWORK2_PUK;
    int PERSOSUBSTATE_RUIM_NETWORK1_PUK;
    int PERSOSUBSTATE_RUIM_RUIM;
    int PERSOSUBSTATE_RUIM_SERVICE_PROVIDER;
    int PERSOSUBSTATE_RUIM_CORPORATE;
    int PERSOSUBSTATE_RUIM_HRPD;
    int PERSOSUBSTATE_RUIM_NETWORK2;
    int PERSOSUBSTATE_RUIM_NETWORK1;
    int PERSOSUBSTATE_SIM_SIM_PUK;
    int PERSOSUBSTATE_SIM_SERVICE_PROVIDER_PUK;
    int PERSOSUBSTATE_SIM_CORPORATE_PUK;
    int PERSOSUBSTATE_SIM_NETWORK_SUBSET_PUK;
    int PERSOSUBSTATE_SIM_NETWORK_PUK;
    int PERSOSUBSTATE_SIM_SIM;
    int PERSOSUBSTATE_SIM_SERVICE_PROVIDER;
    int PERSOSUBSTATE_SIM_CORPORATE;
    int PERSOSUBSTATE_SIM_NETWORK_SUBSET;
    int PERSOSUBSTATE_SIM_NETWORK;
    int PERSOSUBSTATE_READY;
    int PERSOSUBSTATE_IN_PROGRESS;
    int PERSOSUBSTATE_UNKNOWN;
  }
  class AppState {
    int APPSTATE_READY;
    int APPSTATE_SUBSCRIPTION_PERSO;
    int APPSTATE_PUK;
    int APPSTATE_PIN;
    int APPSTATE_DETECTED;
    int APPSTATE_UNKNOWN;
  }
  class AppType {
    int APPTYPE_ISIM;
    int APPTYPE_CSIM;
    int APPTYPE_RUIM;
    int APPTYPE_USIM;
    int APPTYPE_SIM;
    int APPTYPE_UNKNOWN;
  }
}
class IccCard {
  int mHandler;
  class State {
    int PERM_DISABLED;
    int NOT_READY;
    int READY;
    int NETWORK_LOCKED;
    int PUK_REQUIRED;
    int PIN_REQUIRED;
    int ABSENT;
    int UNKNOWN;
  }
  int EVENT_RADIO_ON;
  int EVENT_CDMA_SUBSCRIPTION_SOURCE_CHANGED;
  int EVENT_CARD_ADDED;
  int EVENT_CARD_REMOVED;
  int EVENT_ICC_STATUS_CHANGED;
  int EVENT_CHANGE_FACILITY_FDN_DONE;
  int EVENT_QUERY_FACILITY_FDN_DONE;
  int EVENT_CHANGE_ICC_PASSWORD_DONE;
  int EVENT_CHANGE_FACILITY_LOCK_DONE;
  int EVENT_QUERY_FACILITY_LOCK_DONE;
  int EVENT_ICC_READY;
  int EVENT_REPOLL_STATUS_DONE;
  int EVENT_PINPUK_DONE;
  int EVENT_RADIO_OFF_OR_NOT_AVAILABLE;
  int EVENT_GET_ICC_STATUS_DONE;
  int EVENT_ICC_LOCKED;
  int INTENT_VALUE_ABSENT_ON_PERM_DISABLED;
  int INTENT_VALUE_LOCKED_NETWORK;
  int INTENT_VALUE_LOCKED_ON_PUK;
  int INTENT_VALUE_LOCKED_ON_PIN;
  int INTENT_KEY_LOCKED_REASON;
  int INTENT_VALUE_ICC_LOADED;
  int INTENT_VALUE_ICC_IMSI;
  int INTENT_VALUE_ICC_READY;
  int INTENT_VALUE_ICC_LOCKED;
  int INTENT_VALUE_ICC_ABSENT;
  int INTENT_VALUE_ICC_NOT_READY;
  int INTENT_KEY_ICC_STATE;
  int CARD_IS_NOT_3GPP;
  int CARD_IS_3GPP;
  int mIccFdnEnabled;
  int mIccPinLocked;
  int mDesiredFdnEnabled;
  int mDesiredPinLocked;
  int mRuimReadyRegistrants;
  int mReadyRegistrants;
  int mNetworkLockedRegistrants;
  int mPinLockedRegistrants;
  int mAbsentRegistrants;
  int mCatService;
  int mIccFileHandler;
  int mIccRecords;
  int mPhone;
  int mCdmaSSM;
  int isSubscriptionFromIccCard;
  int is3gpp;
  int mStateMonitor;
  int mState;
  int mIccCardStatus;
  int mDbg;
  int mLogTag;
}
class GsmSmsTest {
  int sExtendedTableIndexes;
  int sExtendedTables;
  int GSM_ESCAPE_CHARACTER;
  int sBasicTables;
}
class GsmAlphabetTest {
  int sGsmExtendedChars;
}
class GsmAlphabet {
  int sLanguageShiftTables;
  int sLanguageTables;
  class LanguagePairCount {
    int unencodableCounts;
    int septetCounts;
    int languageCode;
  }
  int sHighestEnabledSingleShiftCode;
  int sEnabledLockingShiftTables;
  int sEnabledSingleShiftTables;
  int sCharsToShiftTables;
  int sCharsToGsmTables;
  int UDH_SEPTET_COST_CONCATENATED_MESSAGE;
  int UDH_SEPTET_COST_TWO_SHIFT_TABLES;
  int UDH_SEPTET_COST_ONE_SHIFT_TABLE;
  int UDH_SEPTET_COST_LENGTH;
  int GSM_EXTENDED_ESCAPE;
  int TAG;
}
class EncodeException {
}
class DriverCall {
  int uusInfo;
  int namePresentation;
  int name;
  int numberPresentation;
  int als;
  int isVoicePrivacy;
  int isVoice;
  int TOA;
  int number;
  int isMpty;
  int state;
  int isMT;
  int index;
  class State {
    int WAITING;
    int INCOMING;
    int ALERTING;
    int DIALING;
    int HOLDING;
    int ACTIVE;
  }
  int LOG_TAG;
}
class DefaultPhoneNotifier {
  int mRegistry;
  int DBG;
  int LOG_TAG;
}
class DebugService {
  int TAG;
}
class DataConnectionTracker {
  class TxRxSum {
    int rxPkts;
    int txPkts;
  }
  class DataRoamingSettingObserver {
  }
  int mDataRoamingSettingObserver;
  int mIntentReceiver;
  int mIsDisposed;
  int mIsPsRestricted;
  int mPreferredApn;
  int mAllApns;
  int mActiveApn;
  int mApnContexts;
  int mApnToDataConnectionId;
  int mDataConnectionAsyncChannels;
  int mDataConnections;
  int mUniqueIdGenerator;
  int mIsScreenOn;
  int mAutoAttachOnCreation;
  int mCidActive;
  int mReconnectIntent;
  int mIsWifiConnected;
  int mNoRecvPollCount;
  int mSentSinceLastRecv;
  int mDataStallAlarmIntent;
  int mDataStallAlarmTag;
  int mDataStallTxRxSum;
  int mNetStatPollEnabled;
  int mNetStatPollPeriod;
  int mRxPkts;
  int mTxPkts;
  int mDataConnectionTracker;
  int mState;
  int mActivity;
  int mPhone;
  int DEFALUT_DATA_ON_BOOT_PROP;
  int mFailDataSetupFailCause;
  int FAIL_DATA_SETUP_FAIL_CAUSE;
  int mFailDataSetupCounter;
  int FAIL_DATA_SETUP_COUNTER;
  int INTENT_SET_FAIL_DATA_SETUP_COUNTER;
  int INTENT_RECONNECT_ALARM_EXTRA_REASON;
  int DATA_STALL_ALARM_TAG_EXTRA;
  int DATA_STALL_NO_RECV_POLL_LIMIT;
  int DATA_STALL_ALARM_AGGRESSIVE_DELAY_IN_MS_DEFAULT;
  int DATA_STALL_ALARM_NON_AGGRESSIVE_DELAY_IN_MS_DEFAULT;
  int NULL_IP;
  int APN_RESTORE_DELAY_PROP_NAME;
  int RESTORE_DEFAULT_APN_DELAY;
  int NUMBER_SENT_PACKETS_OF_HANG;
  int POLL_LONGEST_RTT;
  int POLL_NETSTAT_SCREEN_OFF_MILLIS;
  int POLL_NETSTAT_MILLIS;
  int NO_RECV_POLL_LIMIT;
  int DEFAULT_MAX_PDP_RESET_FAIL;
  int POLL_NETSTAT_SLOW_MILLIS;
  int SECONDARY_DATA_RETRY_CONFIG;
  int DEFAULT_DATA_RETRY_CONFIG;
  int mRequestedApnType;
  int enabledCount;
  int dataEnabled;
  int sPolicyDataEnabled;
  int mUserDataEnabled;
  int mInternalDataEnabled;
  int mDataEnabledLock;
  int APN_DELAY_MILLIS;
  int APN_TYPE_KEY;
  int ENABLED;
  int DISABLED;
  int APN_NUM_TYPES;
  int APN_CBS_ID;
  int APN_FOTA_ID;
  int APN_IMS_ID;
  int APN_HIPRI_ID;
  int APN_DUN_ID;
  int APN_SUPL_ID;
  int APN_MMS_ID;
  int APN_DEFAULT_ID;
  int APN_INVALID_ID;
  int CMD_SET_POLICY_DATA_ENABLE;
  int CMD_SET_DEPENDENCY_MET;
  int EVENT_CLEAN_UP_ALL_CONNECTIONS;
  int CMD_SET_USER_DATA_ENABLE;
  int EVENT_RESET_DONE;
  int EVENT_SET_INTERNAL_DATA_ENABLE;
  int EVENT_RESTART_RADIO;
  int EVENT_CDMA_OTA_PROVISION;
  int EVENT_CLEAN_UP_CONNECTION;
  int EVENT_PS_RESTRICT_DISABLED;
  int EVENT_PS_RESTRICT_ENABLED;
  int EVENT_CDMA_SUBSCRIPTION_SOURCE_CHANGED;
  int EVENT_CDMA_DATA_DETACHED;
  int EVENT_APN_CHANGED;
  int EVENT_DO_RECOVERY;
  int EVENT_DATA_STALL_ALARM;
  int EVENT_DATA_CONNECTION_ATTACHED;
  int EVENT_DISCONNECT_DONE;
  int EVENT_RESTORE_DEFAULT_APN;
  int EVENT_ENABLE_NEW_APN;
  int EVENT_ROAMING_OFF;
  int EVENT_ROAMING_ON;
  int EVENT_LINK_STATE_CHANGED;
  int EVENT_DATA_CONNECTION_DETACHED;
  int EVENT_VOICE_CALL_ENDED;
  int EVENT_VOICE_CALL_STARTED;
  int EVENT_RADIO_OFF_OR_NOT_AVAILABLE;
  int EVENT_POLL_PDP;
  int EVENT_DATA_STATE_CHANGED;
  int EVENT_TRY_SETUP_DATA;
  int EVENT_RECORDS_LOADED;
  int EVENT_RADIO_AVAILABLE;
  int EVENT_DATA_SETUP_COMPLETE;
  int BASE;
  int EXTRA_MESSENGER;
  int ACTION_DATA_CONNECTION_TRACKER_MESSENGER;
  class Activity {
    int DORMANT;
    int DATAINANDOUT;
    int DATAOUT;
    int DATAIN;
    int NONE;
  }
  class State {
    int FAILED;
    int DISCONNECTING;
    int CONNECTED;
    int SCANNING;
    int CONNECTING;
    int INITING;
    int IDLE;
  }
  int VDBG;
  int DBG;
}
class DataConnectionAc {
  class LinkPropertyChangeAction {
    int RESET;
    int CHANGED;
    int NONE;
  }
  int sCmdToString;
  int CMD_TO_STRING_COUNT;
  int RSP_GET_RECONNECT_INTENT;
  int REQ_GET_RECONNECT_INTENT;
  int RSP_SET_RECONNECT_INTENT;
  int REQ_SET_RECONNECT_INTENT;
  int RSP_GET_APNCONTEXT_LIST;
  int REQ_GET_APNCONTEXT_LIST;
  int RSP_REMOVE_APNCONTEXT;
  int REQ_REMOVE_APNCONTEXT;
  int RSP_ADD_APNCONTEXT;
  int REQ_ADD_APNCONTEXT;
  int RSP_GET_REFCOUNT;
  int REQ_GET_REFCOUNT;
  int RSP_RESET;
  int REQ_RESET;
  int RSP_UPDATE_LINK_PROPERTIES_DATA_CALL_STATE;
  int REQ_UPDATE_LINK_PROPERTIES_DATA_CALL_STATE;
  int RSP_GET_LINK_CAPABILITIES;
  int REQ_GET_LINK_CAPABILITIES;
  int RSP_SET_LINK_PROPERTIES_HTTP_PROXY;
  int REQ_SET_LINK_PROPERTIES_HTTP_PROXY;
  int RSP_GET_LINK_PROPERTIES;
  int REQ_GET_LINK_PROPERTIES;
  int RSP_GET_APNSETTING;
  int REQ_GET_APNSETTING;
  int RSP_GET_CID;
  int REQ_GET_CID;
  int RSP_IS_INACTIVE;
  int REQ_IS_INACTIVE;
  int BASE;
  int dataConnection;
  int mLogTag;
  int DBG;
}
class DataConnection {
  int mDisconnectingErrorCreatingConnection;
  class DcDisconnectionErrorCreatingConnection {
  }
  int mDisconnectingState;
  class DcDisconnectingState {
  }
  int mActiveState;
  class DcActiveState {
    int mFailCause;
    int mConnectionParams;
  }
  int mActivatingState;
  class DcActivatingState {
  }
  int mInactiveState;
  class DcInactiveState {
    int mDisconnectParams;
    int mFailCause;
    int mConnectionParams;
  }
  int mDefaultState;
  class DcDefaultState {
  }
  class UpdateLinkPropertyResult {
    int newLp;
    int oldLp;
    int setupResult;
  }
  int mRetryMgr;
  int mId;
  int userData;
  int mRefCount;
  int NULL_IP;
  int mRetryOverride;
  int lastFailCause;
  int lastFailTime;
  int createTime;
  int mCapabilities;
  int mLinkProperties;
  int cid;
  int mRilVersion;
  int phone;
  int mTag;
  int mApn;
  int EVENT_LOG_BAD_DNS_ADDRESS;
  int sCmdToString;
  int CMD_TO_STRING_COUNT;
  int EVENT_DISCONNECT_ALL;
  int EVENT_RIL_CONNECTED;
  int EVENT_DISCONNECT;
  int EVENT_DEACTIVATE_DONE;
  int EVENT_GET_LAST_FAIL_DONE;
  int EVENT_SETUP_DATA_CONNECTION_DONE;
  int EVENT_CONNECT;
  int BASE;
  class CallSetupException {
    int mRetryOverride;
  }
  class FailCause {
    int CONNECTION_TO_DATACONNECTIONAC_BROKEN;
    int UNACCEPTABLE_NETWORK_PARAMETER;
    int RADIO_NOT_AVAILABLE;
    int UNKNOWN;
    int ERROR_UNSPECIFIED;
    int TETHERED_CALL_ACTIVE;
    int RADIO_POWER_OFF;
    int PREF_RADIO_TECH_CHANGED;
    int SIGNAL_LOST;
    int GPRS_REGISTRATION_FAIL;
    int REGISTRATION_FAIL;
    int PROTOCOL_ERRORS;
    int ONLY_SINGLE_BEARER_ALLOWED;
    int ONLY_IPV6_ALLOWED;
    int ONLY_IPV4_ALLOWED;
    int NSAPI_IN_USE;
    int SERVICE_OPTION_OUT_OF_ORDER;
    int SERVICE_OPTION_NOT_SUBSCRIBED;
    int SERVICE_OPTION_NOT_SUPPORTED;
    int ACTIVATION_REJECT_UNSPECIFIED;
    int ACTIVATION_REJECT_GGSN;
    int USER_AUTHENTICATION;
    int UNKNOWN_PDP_ADDRESS_TYPE;
    int MISSING_UNKNOWN_APN;
    int INSUFFICIENT_RESOURCES;
    int OPERATOR_BARRED;
    int NONE;
    int sErrorCodeToFailCauseMap;
    int mErrorCode;
  }
  class DisconnectParams {
    int onCompletedMsg;
    int reason;
    int tag;
  }
  class ConnectionParams {
    int onCompletedMsg;
    int apn;
    int tag;
  }
  int mDataConnectionTracker;
  int mReconnectIntent;
  int mApnList;
  int mAc;
  int mCount;
  int mCountLock;
  int VDBG;
  int DBG;
}
class DataCallState {
  class SetupResult {
    int ERR_RilError;
    int ERR_Stale;
    int ERR_GetLastErrorFromRil;
    int ERR_UnacceptableParameter;
    int ERR_BadCommand;
    int SUCCESS;
    int mFailCause;
  }
  int suggestedRetryTime;
  int gateways;
  int dnses;
  int addresses;
  int ifname;
  int type;
  int active;
  int cid;
  int status;
  int version;
  int LOG_TAG;
  int DBG;
}
class Connection {
  class PostDialState {
    int PAUSE;
    int CANCELLED;
    int COMPLETE;
    int WILD;
    int WAIT;
    int STARTED;
    int NOT_STARTED;
  }
  int userData;
  class DisconnectCause {
    int ERROR_UNSPECIFIED;
    int CDMA_ACCESS_BLOCKED;
    int CDMA_NOT_EMERGENCY;
    int CDMA_PREEMPTED;
    int CDMA_ACCESS_FAILURE;
    int CDMA_RETRY_ORDER;
    int CDMA_SO_REJECT;
    int CDMA_REORDER;
    int CDMA_INTERCEPT;
    int CDMA_DROP;
    int CDMA_LOCKED_UNTIL_POWER_CYCLE;
    int UNOBTAINABLE_NUMBER;
    int CS_RESTRICTED_EMERGENCY;
    int CS_RESTRICTED_NORMAL;
    int CS_RESTRICTED;
    int FDN_BLOCKED;
    int CALL_BARRED;
    int ICC_ERROR;
    int OUT_OF_SERVICE;
    int POWER_OFF;
    int INCOMING_REJECTED;
    int LIMIT_EXCEEDED;
    int LOST_SIGNAL;
    int TIMED_OUT;
    int SERVER_ERROR;
    int OUT_OF_NETWORK;
    int INVALID_CREDENTIALS;
    int SERVER_UNREACHABLE;
    int NUMBER_UNREACHABLE;
    int INVALID_NUMBER;
    int MMI;
    int CONGESTION;
    int BUSY;
    int LOCAL;
    int NORMAL;
    int INCOMING_MISSED;
    int NOT_DISCONNECTED;
  }
  int LOG_TAG;
  int PRESENTATION_PAYPHONE;
  int PRESENTATION_UNKNOWN;
  int PRESENTATION_RESTRICTED;
  int PRESENTATION_ALLOWED;
}
class CommandsInterface {
  int CDMA_SMS_FAIL_CAUSE_ENCODING_PROBLEM;
  int CDMA_SMS_FAIL_CAUSE_OTHER_TERMINAL_PROBLEM;
  int CDMA_SMS_FAIL_CAUSE_RESOURCE_SHORTAGE;
  int CDMA_SMS_FAIL_CAUSE_INVALID_TELESERVICE_ID;
  int GSM_SMS_FAIL_CAUSE_UNSPECIFIED_ERROR;
  int GSM_SMS_FAIL_CAUSE_USIM_DATA_DOWNLOAD_ERROR;
  int GSM_SMS_FAIL_CAUSE_USIM_APP_TOOLKIT_BUSY;
  int GSM_SMS_FAIL_CAUSE_MEMORY_CAPACITY_EXCEEDED;
  int USSD_MODE_REQUEST;
  int USSD_MODE_NOTIFY;
  int SERVICE_CLASS_MAX;
  int SERVICE_CLASS_PAD;
  int SERVICE_CLASS_PACKET;
  int SERVICE_CLASS_DATA_ASYNC;
  int SERVICE_CLASS_DATA_SYNC;
  int SERVICE_CLASS_SMS;
  int SERVICE_CLASS_FAX;
  int SERVICE_CLASS_DATA;
  int SERVICE_CLASS_VOICE;
  int SERVICE_CLASS_NONE;
  int CB_FACILITY_BA_FD;
  int CB_FACILITY_BA_SIM;
  int CB_FACILITY_BA_MT;
  int CB_FACILITY_BA_MO;
  int CB_FACILITY_BA_ALL;
  int CB_FACILITY_BAICr;
  int CB_FACILITY_BAIC;
  int CB_FACILITY_BAOICxH;
  int CB_FACILITY_BAOIC;
  int CB_FACILITY_BAOC;
  int CF_REASON_ALL_CONDITIONAL;
  int CF_REASON_ALL;
  int CF_REASON_NOT_REACHABLE;
  int CF_REASON_NO_REPLY;
  int CF_REASON_BUSY;
  int CF_REASON_UNCONDITIONAL;
  int CF_ACTION_ERASURE;
  int CF_ACTION_REGISTRATION;
  int CF_ACTION_ENABLE;
  int CF_ACTION_DISABLE;
  int CLIR_SUPPRESSION;
  int CLIR_INVOCATION;
  int CLIR_DEFAULT;
  class RadioState {
    int RADIO_ON;
    int RADIO_UNAVAILABLE;
    int RADIO_OFF;
  }
}
class CommandException {
  class Error {
    int ILLEGAL_SIM_OR_ME;
    int FDN_CHECK_FAILURE;
    int MODE_NOT_SUPPORTED;
    int SUBSCRIPTION_NOT_AVAILABLE;
    int SIM_ABSENT;
    int SMS_FAIL_RETRY;
    int OP_NOT_ALLOWED_BEFORE_REG_NW;
    int OP_NOT_ALLOWED_DURING_VOICE_CALL;
    int REQUEST_NOT_SUPPORTED;
    int SIM_PUK2;
    int SIM_PIN2;
    int PASSWORD_INCORRECT;
    int GENERIC_FAILURE;
    int RADIO_NOT_AVAILABLE;
    int INVALID_RESPONSE;
  }
  int e;
}
class CallerInfoTest {
  class QueryRunner {
    int mAsyncCompleted;
    int mNumber;
    int mLooper;
  }
  class MockContext {
    int mResources;
    int mResolver;
  }
  class MockResources {
  }
  int TAG;
  int kToken;
  int kEmergencyNumber;
  int mContext;
  int mInfo;
}
class CallerInfoAsyncQuery {
  class CallerInfoAsyncQueryHandler {
    class CallerInfoWorkerHandler {
    }
    int mCallerInfo;
    int mQueryUri;
    int mQueryContext;
  }
  class QueryPoolException {
  }
  class CookieWrapper {
    int number;
    int event;
    int cookie;
    int listener;
  }
  class OnQueryCompleteListener {
  }
  int ENABLE_UNKNOWN_NUMBER_GEO_DESCRIPTION;
  int mHandler;
  int EVENT_VOICEMAIL_NUMBER;
  int EVENT_EMERGENCY_NUMBER;
  int EVENT_END_OF_QUEUE;
  int EVENT_ADD_LISTENER;
  int EVENT_NEW_QUERY;
  int LOG_TAG;
  int DBG;
}
class CallerInfo {
  int mIsVoiceMail;
  int mIsEmergency;
  int isCachedPhotoCurrent;
  int cachedPhotoIcon;
  int cachedPhoto;
  int shouldSendToVoicemail;
  int contactRingtoneUri;
  int contactRefUri;
  int needUpdate;
  int person_id;
  int photoResource;
  int numberLabel;
  int numberType;
  int phoneLabel;
  int contactExists;
  int namePresentation;
  int numberPresentation;
  int cnapName;
  int geoDescription;
  int normalizedNumber;
  int phoneNumber;
  int name;
  int PAYPHONE_NUMBER;
  int PRIVATE_NUMBER;
  int UNKNOWN_NUMBER;
  int VDBG;
  int TAG;
}
class CallTracker {
  int EVENT_THREE_WAY_DIAL_L2_RESULT_CDMA;
  int EVENT_CALL_WAITING_INFO_CDMA;
  int EVENT_EXIT_ECM_RESPONSE_CDMA;
  int EVENT_ECT_RESULT;
  int EVENT_SEPARATE_RESULT;
  int EVENT_CONFERENCE_RESULT;
  int EVENT_RADIO_NOT_AVAILABLE;
  int EVENT_RADIO_AVAILABLE;
  int EVENT_SWITCH_RESULT;
  int EVENT_GET_LAST_CALL_FAIL_CAUSE;
  int EVENT_OPERATION_COMPLETE;
  int EVENT_REPOLL_AFTER_DELAY;
  int EVENT_CALL_STATE_CHANGE;
  int EVENT_POLL_CALLS_RESULT;
  int cm;
  int lastRelevantPoll;
  int needsPoll;
  int pendingOperations;
  int POLL_DELAY_MSEC;
  int DBG_POLL;
}
class CallStateException {
}
class CallManager {
  int mHandler;
  int mPostDialCharacterRegistrants;
  int mServiceStateChangedRegistrants;
  int mSuppServiceFailedRegistrants;
  int mSubscriptionInfoReadyRegistrants;
  int mEcmTimerResetRegistrants;
  int mMmiCompleteRegistrants;
  int mMmiInitiateRegistrants;
  int mResendIncallMuteRegistrants;
  int mCdmaOtaStatusChangeRegistrants;
  int mSignalInfoRegistrants;
  int mDisplayInfoRegistrants;
  int mCallWaitingRegistrants;
  int mInCallVoicePrivacyOffRegistrants;
  int mInCallVoicePrivacyOnRegistrants;
  int mRingbackToneRegistrants;
  int mUnknownConnectionRegistrants;
  int mMmiRegistrants;
  int mDisconnectRegistrants;
  int mIncomingRingRegistrants;
  int mNewRingingConnectionRegistrants;
  int mPreciseCallStateRegistrants;
  int mDefaultPhone;
  int emptyConnections;
  int mForegroundCalls;
  int mBackgroundCalls;
  int mRingingCalls;
  int mPhones;
  int INSTANCE;
  int EVENT_POST_DIAL_CHARACTER;
  int EVENT_SERVICE_STATE_CHANGED;
  int EVENT_SUPP_SERVICE_FAILED;
  int EVENT_SUBSCRIPTION_INFO_READY;
  int EVENT_ECM_TIMER_RESET;
  int EVENT_MMI_COMPLETE;
  int EVENT_MMI_INITIATE;
  int EVENT_RESEND_INCALL_MUTE;
  int EVENT_CDMA_OTA_STATUS_CHANGE;
  int EVENT_SIGNAL_INFO;
  int EVENT_DISPLAY_INFO;
  int EVENT_CALL_WAITING;
  int EVENT_IN_CALL_VOICE_PRIVACY_OFF;
  int EVENT_IN_CALL_VOICE_PRIVACY_ON;
  int EVENT_RINGBACK_TONE;
  int EVENT_INCOMING_RING;
  int EVENT_UNKNOWN_CONNECTION;
  int EVENT_NEW_RINGING_CONNECTION;
  int EVENT_PRECISE_CALL_STATE_CHANGED;
  int EVENT_DISCONNECT;
  int VDBG;
  int DBG;
  int LOG_TAG;
}
class CallForwardInfo {
  int timeSeconds;
  int number;
  int toa;
  int serviceClass;
  int reason;
  int status;
}
class Call {
  int LOG_TAG;
  int isGeneric;
  int state;
  class State {
    int DISCONNECTING;
    int DISCONNECTED;
    int WAITING;
    int INCOMING;
    int ALERTING;
    int DIALING;
    int HOLDING;
    int ACTIVE;
    int IDLE;
  }
}
class BaseCommands {
  int sLteOnCdmaProductType;
  int sProductTypePattern;
  int sKernelCmdLine;
  int mRilVersion;
  int mPhoneType;
  int mCdmaSubscription;
  int mPreferredNetworkType;
  int mGsmBroadcastSmsRegistrant;
  int mRestrictedStateRegistrant;
  int mRingRegistrant;
  int mEmergencyCallbackModeRegistrant;
  int mIccSmsFullRegistrant;
  int mCatCallSetUpRegistrant;
  int mCatEventRegistrant;
  int mCatProCmdRegistrant;
  int mCatSessionEndRegistrant;
  int mSsnRegistrant;
  int mSmsStatusRegistrant;
  int mSmsOnSimRegistrant;
  int mUSSDRegistrant;
  int mSignalStrengthRegistrant;
  int mNITZTimeRegistrant;
  int mCdmaSmsRegistrant;
  int mGsmSmsRegistrant;
  int mIccRefreshRegistrants;
  int mRilConnectedRegistrants;
  int mExitEmergencyCallbackModeRegistrants;
  int mCdmaPrlChangedRegistrants;
  int mCdmaSubscriptionChangedRegistrants;
  int mResendIncallMuteRegistrants;
  int mRingbackToneRegistrants;
  int mT53AudCntrlInfoRegistrants;
  int mT53ClirInfoRegistrants;
  int mLineControlInfoRegistrants;
  int mRedirNumInfoRegistrants;
  int mNumberInfoRegistrants;
  int mSignalInfoRegistrants;
  int mDisplayInfoRegistrants;
  int mCallWaitingInfoRegistrants;
  int mOtaProvisionRegistrants;
  int mUnsolOemHookRawRegistrant;
  int mVoicePrivacyOffRegistrants;
  int mVoicePrivacyOnRegistrants;
  int mIccStatusChangedRegistrants;
  int mVoiceRadioTechChangedRegistrants;
  int mDataNetworkStateRegistrants;
  int mVoiceNetworkStateRegistrants;
  int mCallStateRegistrants;
  int mNotAvailRegistrants;
  int mOffOrNotAvailRegistrants;
  int mAvailRegistrants;
  int mOnRegistrants;
  int mRadioStateChangedRegistrants;
  int mStateMonitor;
  int mState;
  int mContext;
  int LOG_TAG;
}
class ApnSettingTest {
  int TYPES;
}
class ApnSetting {
  int bearer;
  int carrierEnabled;
  int roamingProtocol;
  int protocol;
  int numeric;
  int id;
  int types;
  int authType;
  int password;
  int user;
  int mmsPort;
  int mmsProxy;
  int mmsc;
  int port;
  int proxy;
  int apn;
  int carrier;
  int V2_FORMAT_REGEX;
}
class ApnContext {
  int mDependencyMet;
  int mDataEnabled;
  int mReason;
  int mDataConnectionAc;
  int mDataConnection;
  int mApnSetting;
  int mWaitingApnsPermanentFailureCountDown;
  int mWaitingApns;
  int mState;
  int mApnType;
  int DBG;
  int LOG_TAG;
}
class AdnRecordTest {
}
class AdnRecordLoader {
  int EVENT_UPDATE_RECORD_DONE;
  int EVENT_EF_LINEAR_RECORD_SIZE_DONE;
  int EVENT_ADN_LOAD_ALL_DONE;
  int EVENT_EXT_RECORD_LOAD_DONE;
  int EVENT_ADN_LOAD_DONE;
  int result;
  int adns;
  int recordNumber;
  int pin2;
  int userResponse;
  int pendingExtLoads;
  int extensionEF;
  int ef;
  int mFh;
  int LOG_TAG;
}
class AdnRecordCache {
  int EVENT_UPDATE_ADN_DONE;
  int EVENT_LOAD_ALL_ADN_LIKE_DONE;
  int userWriteResponse;
  int adnLikeWaiters;
  int adnLikeFiles;
  int mUsimPhoneBookManager;
  int mFh;
}
class AdnRecord {
  int CREATOR;
  int ADN_EXTENSION_ID;
  int ADN_CAPABILITY_ID;
  int ADN_DIALING_NUMBER_END;
  int ADN_DIALING_NUMBER_START;
  int ADN_TON_AND_NPI;
  int ADN_BCD_NUMBER_LENGTH;
  int MAX_EXT_CALLED_PARTY_LENGTH;
  int EXT_RECORD_TYPE_MASK;
  int EXT_RECORD_TYPE_ADDITIONAL_DATA;
  int EXT_RECORD_LENGTH_BYTES;
  int MAX_NUMBER_SIZE_BYTES;
  int FOOTER_SIZE_BYTES;
  int recordNumber;
  int efid;
  int extRecord;
  int emails;
  int number;
  int alphaTag;
  int LOG_TAG;
}
class ATResponseParserTest {
}
class ATResponseParser {
  int tokEnd;
  int tokStart;
  int next;
  int line;
}
class ATParseEx {
}
