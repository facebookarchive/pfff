package android.telephony;
class TelephonyManager {
  int DATA_SUSPENDED;
  int DATA_CONNECTED;
  int DATA_CONNECTING;
  int DATA_DISCONNECTED;
  int DATA_UNKNOWN;
  int DATA_ACTIVITY_DORMANT;
  int DATA_ACTIVITY_INOUT;
  int DATA_ACTIVITY_OUT;
  int DATA_ACTIVITY_IN;
  int DATA_ACTIVITY_NONE;
  int CALL_STATE_OFFHOOK;
  int CALL_STATE_RINGING;
  int CALL_STATE_IDLE;
  int SIM_STATE_READY;
  int SIM_STATE_NETWORK_LOCKED;
  int SIM_STATE_PUK_REQUIRED;
  int SIM_STATE_PIN_REQUIRED;
  int SIM_STATE_ABSENT;
  int SIM_STATE_UNKNOWN;
  int NETWORK_CLASS_4_G;
  int NETWORK_CLASS_3_G;
  int NETWORK_CLASS_2_G;
  int NETWORK_CLASS_UNKNOWN;
  int NETWORK_TYPE_HSPAP;
  int NETWORK_TYPE_EHRPD;
  int NETWORK_TYPE_LTE;
  int NETWORK_TYPE_EVDO_B;
  int NETWORK_TYPE_IDEN;
  int NETWORK_TYPE_HSPA;
  int NETWORK_TYPE_HSUPA;
  int NETWORK_TYPE_HSDPA;
  int NETWORK_TYPE_1xRTT;
  int NETWORK_TYPE_EVDO_A;
  int NETWORK_TYPE_EVDO_0;
  int NETWORK_TYPE_CDMA;
  int NETWORK_TYPE_UMTS;
  int NETWORK_TYPE_EDGE;
  int NETWORK_TYPE_GPRS;
  int NETWORK_TYPE_UNKNOWN;
  int PHONE_TYPE_SIP;
  int PHONE_TYPE_CDMA;
  int PHONE_TYPE_GSM;
  int PHONE_TYPE_NONE;
  int EXTRA_INCOMING_NUMBER;
  int EXTRA_STATE_OFFHOOK;
  int EXTRA_STATE_RINGING;
  int EXTRA_STATE_IDLE;
  int EXTRA_STATE;
  int ACTION_PHONE_STATE_CHANGED;
  int sInstance;
  int sRegistry;
  int sContext;
  int TAG;
}
class SmsMessage {
  class SubmitPdu {
    int encodedMessage;
    int encodedScAddress;
  }
  int mWrappedSmsMessage;
  int FORMAT_3GPP2;
  int FORMAT_3GPP;
  int MAX_USER_DATA_SEPTETS_WITH_HEADER;
  int MAX_USER_DATA_SEPTETS;
  int MAX_USER_DATA_BYTES_WITH_HEADER;
  int MAX_USER_DATA_BYTES;
  int ENCODING_KSC5601;
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
}
class SmsManager {
  int RESULT_ERROR_FDN_CHECK_FAILURE;
  int RESULT_ERROR_LIMIT_EXCEEDED;
  int RESULT_ERROR_NO_SERVICE;
  int RESULT_ERROR_NULL_PDU;
  int RESULT_ERROR_RADIO_OFF;
  int RESULT_ERROR_GENERIC_FAILURE;
  int STATUS_ON_ICC_UNSENT;
  int STATUS_ON_ICC_SENT;
  int STATUS_ON_ICC_UNREAD;
  int STATUS_ON_ICC_READ;
  int STATUS_ON_ICC_FREE;
  int sInstance;
}
class SmsCbMessage {
  int CREATOR;
  int mCmasWarningInfo;
  int mEtwsWarningInfo;
  int mPriority;
  int mBody;
  int mLanguage;
  int mServiceCategory;
  int mLocation;
  int mSerialNumber;
  int mGeographicalScope;
  int mMessageFormat;
  int MESSAGE_PRIORITY_EMERGENCY;
  int MESSAGE_PRIORITY_URGENT;
  int MESSAGE_PRIORITY_INTERACTIVE;
  int MESSAGE_PRIORITY_NORMAL;
  int MESSAGE_FORMAT_3GPP2;
  int MESSAGE_FORMAT_3GPP;
  int GEOGRAPHICAL_SCOPE_CELL_WIDE;
  int GEOGRAPHICAL_SCOPE_LA_WIDE;
  int GEOGRAPHICAL_SCOPE_PLMN_WIDE;
  int GEOGRAPHICAL_SCOPE_CELL_WIDE_IMMEDIATE;
  int LOG_TAG;
}
class SmsCbLocation {
  int CREATOR;
  int mCid;
  int mLac;
  int mPlmn;
}
class SmsCbEtwsInfo {
  int CREATOR;
  int mWarningSecurityInformation;
  int mActivatePopup;
  int mEmergencyUserAlert;
  int mWarningType;
  int ETWS_WARNING_TYPE_UNKNOWN;
  int ETWS_WARNING_TYPE_OTHER_EMERGENCY;
  int ETWS_WARNING_TYPE_TEST_MESSAGE;
  int ETWS_WARNING_TYPE_EARTHQUAKE_AND_TSUNAMI;
  int ETWS_WARNING_TYPE_TSUNAMI;
  int ETWS_WARNING_TYPE_EARTHQUAKE;
}
class SmsCbCmasInfo {
  int CREATOR;
  int mCertainty;
  int mUrgency;
  int mSeverity;
  int mResponseType;
  int mCategory;
  int mMessageClass;
  int CMAS_CERTAINTY_UNKNOWN;
  int CMAS_CERTAINTY_LIKELY;
  int CMAS_CERTAINTY_OBSERVED;
  int CMAS_URGENCY_UNKNOWN;
  int CMAS_URGENCY_EXPECTED;
  int CMAS_URGENCY_IMMEDIATE;
  int CMAS_SEVERITY_UNKNOWN;
  int CMAS_SEVERITY_SEVERE;
  int CMAS_SEVERITY_EXTREME;
  int CMAS_RESPONSE_TYPE_UNKNOWN;
  int CMAS_RESPONSE_TYPE_NONE;
  int CMAS_RESPONSE_TYPE_ASSESS;
  int CMAS_RESPONSE_TYPE_AVOID;
  int CMAS_RESPONSE_TYPE_MONITOR;
  int CMAS_RESPONSE_TYPE_EXECUTE;
  int CMAS_RESPONSE_TYPE_PREPARE;
  int CMAS_RESPONSE_TYPE_EVACUATE;
  int CMAS_RESPONSE_TYPE_SHELTER;
  int CMAS_CATEGORY_UNKNOWN;
  int CMAS_CATEGORY_OTHER;
  int CMAS_CATEGORY_CBRNE;
  int CMAS_CATEGORY_INFRA;
  int CMAS_CATEGORY_TRANSPORT;
  int CMAS_CATEGORY_ENV;
  int CMAS_CATEGORY_HEALTH;
  int CMAS_CATEGORY_FIRE;
  int CMAS_CATEGORY_RESCUE;
  int CMAS_CATEGORY_SECURITY;
  int CMAS_CATEGORY_SAFETY;
  int CMAS_CATEGORY_MET;
  int CMAS_CATEGORY_GEO;
  int CMAS_CLASS_UNKNOWN;
  int CMAS_CLASS_OPERATOR_DEFINED_USE;
  int CMAS_CLASS_CMAS_EXERCISE;
  int CMAS_CLASS_REQUIRED_MONTHLY_TEST;
  int CMAS_CLASS_CHILD_ABDUCTION_EMERGENCY;
  int CMAS_CLASS_SEVERE_THREAT;
  int CMAS_CLASS_EXTREME_THREAT;
  int CMAS_CLASS_PRESIDENTIAL_LEVEL_ALERT;
}
class SignalStrength {
  int CREATOR;
  int isGsm;
  int mLteCqi;
  int mLteRssnr;
  int mLteRsrq;
  int mLteRsrp;
  int mLteSignalStrength;
  int mEvdoSnr;
  int mEvdoEcio;
  int mEvdoDbm;
  int mCdmaEcio;
  int mCdmaDbm;
  int mGsmBitErrorRate;
  int mGsmSignalStrength;
  int INVALID_SNR;
  int SIGNAL_STRENGTH_NAMES;
  int NUM_SIGNAL_STRENGTH_BINS;
  int SIGNAL_STRENGTH_GREAT;
  int SIGNAL_STRENGTH_GOOD;
  int SIGNAL_STRENGTH_MODERATE;
  int SIGNAL_STRENGTH_POOR;
  int SIGNAL_STRENGTH_NONE_OR_UNKNOWN;
  int DBG;
  int LOG_TAG;
}
class ServiceState {
  int CREATOR;
  int mCdmaEriIconMode;
  int mCdmaEriIconIndex;
  int mCdmaDefaultRoamingIndicator;
  int mCdmaRoamingIndicator;
  int mSystemId;
  int mNetworkId;
  int mCssIndicator;
  int mRadioTechnology;
  int mIsEmergencyOnly;
  int mIsManualNetworkSelection;
  int mOperatorNumeric;
  int mOperatorAlphaShort;
  int mOperatorAlphaLong;
  int mRoaming;
  int mState;
  int REGISTRATION_STATE_ROAMING;
  int REGISTRATION_STATE_UNKNOWN;
  int REGISTRATION_STATE_REGISTRATION_DENIED;
  int REGISTRATION_STATE_NOT_REGISTERED_AND_SEARCHING;
  int REGISTRATION_STATE_HOME_NETWORK;
  int REGISTRATION_STATE_NOT_REGISTERED_AND_NOT_SEARCHING;
  int RIL_RADIO_TECHNOLOGY_GSM;
  int RIL_RADIO_TECHNOLOGY_HSPAP;
  int RIL_RADIO_TECHNOLOGY_LTE;
  int RIL_RADIO_TECHNOLOGY_EHRPD;
  int RIL_RADIO_TECHNOLOGY_EVDO_B;
  int RIL_RADIO_TECHNOLOGY_HSPA;
  int RIL_RADIO_TECHNOLOGY_HSUPA;
  int RIL_RADIO_TECHNOLOGY_HSDPA;
  int RIL_RADIO_TECHNOLOGY_EVDO_A;
  int RIL_RADIO_TECHNOLOGY_EVDO_0;
  int RIL_RADIO_TECHNOLOGY_1xRTT;
  int RIL_RADIO_TECHNOLOGY_IS95B;
  int RIL_RADIO_TECHNOLOGY_IS95A;
  int RIL_RADIO_TECHNOLOGY_UMTS;
  int RIL_RADIO_TECHNOLOGY_EDGE;
  int RIL_RADIO_TECHNOLOGY_GPRS;
  int RIL_RADIO_TECHNOLOGY_UNKNOWN;
  int STATE_POWER_OFF;
  int STATE_EMERGENCY_ONLY;
  int STATE_OUT_OF_SERVICE;
  int STATE_IN_SERVICE;
  int LOG_TAG;
}
class PhoneStateListener {
  int mHandler;
  int callback;
  int LISTEN_CELL_INFO;
  int LISTEN_OTASP_CHANGED;
  int LISTEN_SIGNAL_STRENGTHS;
  int LISTEN_DATA_ACTIVITY;
  int LISTEN_DATA_CONNECTION_STATE;
  int LISTEN_CALL_STATE;
  int LISTEN_CELL_LOCATION;
  int LISTEN_CALL_FORWARDING_INDICATOR;
  int LISTEN_MESSAGE_WAITING_INDICATOR;
  int LISTEN_SIGNAL_STRENGTH;
  int LISTEN_SERVICE_STATE;
  int LISTEN_NONE;
}
class PhoneNumberUtils {
  class CountryCallingCodeAndNewIndex {
    int newIndex;
    int countryCallingCode;
  }
  int CCC_LENGTH;
  int COUNTRY_CALLING_CALL;
  int NANP_LENGTH;
  int NANP_IDP_STRING;
  int PLUS_SIGN_STRING;
  int PLUS_SIGN_CHAR;
  int KEYPAD_MAP;
  int MIN_MATCH;
  int NANP_STATE_DASH;
  int NANP_STATE_ONE;
  int NANP_STATE_PLUS;
  int NANP_STATE_DIGIT;
  int NANP_COUNTRIES;
  int FORMAT_JAPAN;
  int FORMAT_NANP;
  int FORMAT_UNKNOWN;
  int GLOBAL_PHONE_NUMBER_PATTERN;
  int DBG;
  int LOG_TAG;
  int TOA_Unknown;
  int TOA_International;
  int CLIR_OFF;
  int CLIR_ON;
  int WILD;
  int WAIT;
  int PAUSE;
}
class PhoneNumberFormattingTextWatcher {
  int mFormatter;
  int mStopFormatting;
  int mSelfChange;
}
class NeighboringCellInfo {
  int CREATOR;
  int mNetworkType;
  int mPsc;
  int mLac;
  int mCid;
  int mRssi;
  int UNKNOWN_CID;
  int UNKNOWN_RSSI;
}
class LteCellIdentity {
  int CREATOR;
  int mTac;
  int mPci;
  int mCi;
  int mMnc;
  int mMcc;
}
class JapanesePhoneNumberFormatter {
  int FORMAT_MAP;
}
class GsmCellIdentity {
  int CREATOR;
  int mPsc;
  int mCid;
  int mLac;
  int mMnc;
  int mMcc;
}
class CellLocation {
}
class CellInfo {
  int CREATOR;
  int mCellIdentity;
  int mCellIdentityType;
  int mTimingAdvance;
  int mStrength;
  int mRegistered;
  int mTimeStampType;
  int mTimeStamp;
  int CELL_INFO_TIMESTAMP_TYPE_JAVA_RIL;
  int CELL_INFO_TIMESTAMP_TYPE_OEM_RIL;
  int CELL_INFO_TIMESTAMP_TYPE_MODEM;
  int CELL_INFO_TIMESTAMP_TYPE_ANTENNA;
  int CELL_INFO_TIMESTAMP_TYPE_UNKNOWN;
}
class CellIdentity {
  int mCellIdAttributes;
  int mCellIdType;
  int CELLID_TYPE_LTE;
  int CELLID_TYPE_CDMA;
  int CELLID_TYPE_GSM;
}
class CellBroadcastMessage {
  int CREATOR;
  int mIsRead;
  int mDeliveryTime;
  int mSmsCbMessage;
  int SMS_CB_MESSAGE_EXTRA;
}
class CdmaCellIdentity {
  int CREATOR;
  int mLatitude;
  int mLongitude;
  int mBasestationId;
  int mSystemId;
  int mNetworkId;
}
