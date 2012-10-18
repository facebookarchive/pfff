package com.android.internal.telephony.gsm;
class VoiceMailConstants {
  int SIZE;
  int TAG;
  int NUMBER;
  int NAME;
  int PARTNER_VOICEMAIL_PATH;
  int LOG_TAG;
  int CarrierVmMap;
}
class UsimServiceTableTest {
}
class UsimServiceTable {
  class UsimService {
    int NAS_CONFIG_BY_USIM;
    int UICC_ACCESS_TO_IMS;
    int EXTENDED_TERMINAL_APPLICATIONS;
    int IMS_COMMUNICATION_CONTROL_BY_USIM;
    int CSG_DISPLAY_CONTROL;
    int SM_OVER_IP;
    int OPERATOR_CSG_LISTS_AND_INDICATIONS;
    int ECALL_DATA;
    int HPLMN_DIRECT_ACCESS;
    int CALL_CONTROL_ON_EPS_PDN_CONNECTION_BY_USIM;
    int ALLOWED_CSG_LISTS_AND_INDICATIONS;
    int EPS_MOBILITY_MANAGEMENT_INFO;
    int IWLAN_LAST_REGISTERED_PLMN;
    int IWLAN_HPLMN_PRIORITY_INDICATION;
    int IWLAN_EQUIVALENT_HPLMN_PRESENTATION;
    int IWLAN_HOME_ID_LIST;
    int USIM_IP_CONNECTION_PARAMS;
    int PLMN_NETWORK_NAME_ICON;
    int SPN_ICON;
    int TERMINAL_APPLICATIONS;
    int GBA_LOCAL_KEY_ESTABLISHMENT;
    int OMA_BCAST_PROFILE;
    int LAST_RPLMN_SELECTION_INDICATION;
    int EQUIVALENT_HPLMN_PRESENTATION;
    int TERMINAL_PROFILE_AFTER_UICC_ACTIVATION;
    int EQUIVALENT_HPLMN;
    int DATA_DL_VIA_USSD;
    int MBMS_SECURITY;
    int GBA;
    int MM_STORAGE;
    int WLAN_REAUTH_IDENTITY;
    int VBS_SECURITY;
    int VGCS_SECURITY;
    int OPERATOR_WSID_LIST;
    int USER_WSID_LIST;
    int IWLAN_OPERATOR_PLMN_SELECT;
    int IWLAN_USER_PLMN_SELECT;
    int PSEUDONYM;
    int VBS_GROUP_ID_LIST;
    int VGCS_GROUP_ID_LIST;
    int NETWORK_INDICATION_OF_ALERTING;
    int MMS_CONNECTIVITY_PARAMS;
    int GPRS_CALL_CONTROL_BY_USIM;
    int MMS_NOTIFICATION_EXTENSION;
    int MMS_NOTIFICATION;
    int SERVICE_PROVIDER_DISPLAY_INFO;
    int IGNORED_2;
    int CFI_STATUS;
    int MWI_STATUS;
    int MBDN;
    int OPERATOR_PLMN_LIST;
    int PLMN_NETWORK_NAME;
    int EXTENSION_5;
    int HPLMN_SELECT;
    int OPERATOR_PLMN_SELECT;
    int MEXE;
    int INVESTIGATION_SCAN;
    int CPBCCH_INFO;
    int GSM_SECURITY_CONTEXT;
    int COOPERATIVE_NETWORK_LIST;
    int DEPERSONALISATION_CONTROL_KEYS;
    int APN_CONTROL_LIST;
    int ENABLED_SERVICES_TABLE;
    int IGNORED_1;
    int RUN_AT_COMMAND;
    int MO_SMS_CONTROL_BY_USIM;
    int CALL_CONTROL_BY_USIM;
    int DATA_DL_VIA_SMS_CB;
    int DATA_DL_VIA_SMS_PP;
    int GSM_ACCESS;
    int RFU;
    int EMLPP_AUTO_ANSWER;
    int EMLPP;
    int LOCALISED_SERVICE_AREAS;
    int IMAGE;
    int MSISDN;
    int USER_PLMN_SELECT;
    int SPN;
    int GROUP_ID_LEVEL_2;
    int GROUP_ID_LEVEL_1;
    int CB_MESSAGE_ID_RANGES;
    int CB_MESSAGE_ID;
    int CAP_CONFIG_PARAMS_2;
    int ADVICE_OF_CHARGE;
    int SM_SERVICE_PARAMS;
    int SM_STATUS_REPORTS;
    int SM_STORAGE;
    int INCOMING_CALL_INFO;
    int OUTGOING_CALL_INFO;
    int BDN_EXTENSION;
    int BDN;
    int SDN_EXTENSION;
    int SDN;
    int FDN_EXTENSION;
    int FDN;
    int PHONEBOOK;
  }
}
class UsimPhoneBookManager {
  class PbrFile {
    int mFileIds;
  }
  int USIM_EFCCP1_TAG;
  int USIM_EFEMAIL_TAG;
  int USIM_EFUID_TAG;
  int USIM_EFGSD_TAG;
  int USIM_EFAAS_TAG;
  int USIM_EFGRP_TAG;
  int USIM_EFPBC_TAG;
  int USIM_EFANR_TAG;
  int USIM_EFSNE_TAG;
  int USIM_EFEXT1_TAG;
  int USIM_EFIAP_TAG;
  int USIM_EFADN_TAG;
  int USIM_TYPE3_TAG;
  int USIM_TYPE2_TAG;
  int USIM_TYPE1_TAG;
  int EVENT_EMAIL_LOAD_DONE;
  int EVENT_IAP_LOAD_DONE;
  int EVENT_USIM_ADN_LOAD_DONE;
  int EVENT_PBR_LOAD_DONE;
  int mRefreshCache;
  int mEmailsForAdnRec;
  int mEmailFileRecord;
  int mIapFileRecord;
  int mEmailTagNumberInIap;
  int mEmailPresentInIap;
  int mPhoneBookRecords;
  int mLock;
  int mAdnCache;
  int mFh;
  int mIsPbrPresent;
  int mPbrFile;
  int DBG;
  int LOG_TAG;
}
class UsimDataDownloadTest {
  int SMS_PP_ENVELOPE_3_1_5;
  int SMS_PP_MESSAGE_3_1_5;
  int SMS_PP_ENVELOPE_3_1_1;
  int SMS_PP_MESSAGE_3_1_1;
  int mHandler;
  int mHandlerThread;
  int mCm;
  class TestHandlerThread {
    int mHandler;
  }
  int TAG;
}
class UsimDataDownloadHandler {
  int mCI;
  int EVENT_SEND_ENVELOPE_RESPONSE;
  int EVENT_START_DATA_DOWNLOAD;
  int DEV_ID_NETWORK;
  int DEV_ID_UICC;
  int BER_SMS_PP_DOWNLOAD_TAG;
  int TAG;
}
class UsimDataDownloadCommands {
  int mExpectingSendEnvelopeResponse;
  int mExpectingSendEnvelopeResponseSw2;
  int mExpectingSendEnvelopeResponseSw1;
  int mExpectingSendEnvelopeContents;
  int mExpectingSendEnvelope;
  int mExpectingAcknowledgeGsmSmsPdu;
  int mExpectingAcknowledgeGsmSmsFailureCause;
  int mExpectingAcknowledgeGsmSmsSuccess;
  int mExpectingAcknowledgeGsmSms;
  int TAG;
}
class SuppServiceNotification {
  int MT_CODE_ADDITIONAL_CALL_FORWARDED;
  int MT_CODE_DEFLECTED_CALL;
  int MT_CODE_CALL_CONNECTED_ECT;
  int MT_CODE_CALL_CONNECTING_ECT;
  int MT_CODE_FORWARD_CHECK_RECEIVED;
  int MT_CODE_ON_HOLD_CALL_RELEASED;
  int MT_CODE_MULTI_PARTY_CALL;
  int MT_CODE_CALL_RETRIEVED;
  int MT_CODE_CALL_ON_HOLD;
  int MT_CODE_CUG_CALL;
  int MT_CODE_FORWARDED_CALL;
  int MO_CODE_CALL_DEFLECTED;
  int MO_CODE_CLIR_SUPPRESSION_REJECTED;
  int MO_CODE_INCOMING_CALLS_BARRED;
  int MO_CODE_OUTGOING_CALLS_BARRED;
  int MO_CODE_CUG_CALL;
  int MO_CODE_CALL_IS_WAITING;
  int MO_CODE_CALL_FORWARDED;
  int MO_CODE_SOME_CF_ACTIVE;
  int MO_CODE_UNCONDITIONAL_CF_ACTIVE;
  int number;
  int type;
  int index;
  int code;
  int notificationType;
}
class SpnOverride {
  int PARTNER_SPN_OVERRIDE_PATH;
  int LOG_TAG;
  int CarrierSpnMap;
}
class SmsMessage {
  class PduParser {
    int mUserDataSize;
    int mUserDataSeptetPadding;
    int userData;
    int userDataHeader;
    int cur;
    int pdu;
  }
  class SubmitPdu {
  }
  int isStatusReportMessage;
  int status;
  int dischargeTimeMillis;
  int recipientAddress;
  int forSubmit;
  int automaticDeletion;
  int replyPathPresent;
  int dataCodingScheme;
  int protocolIdentifier;
  int mti;
  int messageClass;
  int LOG_TAG;
}
class SmsCbHeader {
  int mCmasInfo;
  int mEtwsInfo;
  int format;
  int nrOfPages;
  int pageIndex;
  int dataCodingScheme;
  int messageIdentifier;
  int serialNumber;
  int geographicalScope;
  int PDU_LENGTH_ETWS;
  int PDU_LENGTH_GSM;
  int MESSAGE_TYPE_CBS_MESSAGE;
  int FORMAT_ETWS_PRIMARY;
  int FORMAT_UMTS;
  int FORMAT_GSM;
  int PDU_HEADER_LENGTH;
}
class SmsCbConstants {
  int ETWS_WARNING_TYPE_OTHER;
  int ETWS_WARNING_TYPE_TEST;
  int ETWS_WARNING_TYPE_EARTHQUAKE_AND_TSUNAMI;
  int ETWS_WARNING_TYPE_TSUNAMI;
  int ETWS_WARNING_TYPE_EARTHQUAKE;
  int SERIAL_NUMBER_ETWS_EMERGENCY_USER_ALERT;
  int SERIAL_NUMBER_ETWS_ACTIVATE_POPUP;
  int MESSAGE_ID_PWS_LAST_IDENTIFIER;
  int MESSAGE_ID_CMAS_LAST_IDENTIFIER;
  int MESSAGE_ID_CMAS_ALERT_OPERATOR_DEFINED_USE;
  int MESSAGE_ID_CMAS_ALERT_EXERCISE;
  int MESSAGE_ID_CMAS_ALERT_REQUIRED_MONTHLY_TEST;
  int MESSAGE_ID_CMAS_ALERT_CHILD_ABDUCTION_EMERGENCY;
  int MESSAGE_ID_CMAS_ALERT_SEVERE_EXPECTED_LIKELY;
  int MESSAGE_ID_CMAS_ALERT_SEVERE_EXPECTED_OBSERVED;
  int MESSAGE_ID_CMAS_ALERT_SEVERE_IMMEDIATE_LIKELY;
  int MESSAGE_ID_CMAS_ALERT_SEVERE_IMMEDIATE_OBSERVED;
  int MESSAGE_ID_CMAS_ALERT_EXTREME_EXPECTED_LIKELY;
  int MESSAGE_ID_CMAS_ALERT_EXTREME_EXPECTED_OBSERVED;
  int MESSAGE_ID_CMAS_ALERT_EXTREME_IMMEDIATE_LIKELY;
  int MESSAGE_ID_CMAS_ALERT_EXTREME_IMMEDIATE_OBSERVED;
  int MESSAGE_ID_CMAS_ALERT_PRESIDENTIAL_LEVEL;
  int MESSAGE_ID_CMAS_FIRST_IDENTIFIER;
  int MESSAGE_ID_ETWS_OTHER_EMERGENCY_TYPE;
  int MESSAGE_ID_ETWS_TEST_MESSAGE;
  int MESSAGE_ID_ETWS_EARTHQUAKE_AND_TSUNAMI_WARNING;
  int MESSAGE_ID_ETWS_TSUNAMI_WARNING;
  int MESSAGE_ID_ETWS_EARTHQUAKE_WARNING;
  int MESSAGE_ID_ETWS_TYPE;
  int MESSAGE_ID_ETWS_TYPE_MASK;
  int MESSAGE_ID_PWS_FIRST_IDENTIFIER;
}
class SmsBroadcastConfigInfo {
  int selected;
  int toCodeScheme;
  int fromCodeScheme;
  int toServiceId;
  int fromServiceId;
}
class SimTlv {
  int hasValidTlvObject;
  int curDataLength;
  int curDataOffset;
  int curOffset;
  int tlvLength;
  int tlvOffset;
  int record;
}
class SimSmsInterfaceManager {
  class CellBroadcastRangeManager {
    int mConfigList;
  }
  int mHandler;
  int SMS_CB_CODE_SCHEME_MAX;
  int SMS_CB_CODE_SCHEME_MIN;
  int EVENT_SET_BROADCAST_CONFIG_DONE;
  int EVENT_SET_BROADCAST_ACTIVATION_DONE;
  int EVENT_UPDATE_DONE;
  int EVENT_LOAD_DONE;
  int mCellBroadcastRangeManager;
  int mCellBroadcastSubscriptions;
  int mSms;
  int mSuccess;
  int mLock;
  int DBG;
  int LOG_TAG;
}
class SimPhoneBookInterfaceManager {
  int LOG_TAG;
}
class SIMRecords {
  class Get_Spn_Fsm_State {
    int READ_SPN_SHORT_CPHS;
    int READ_SPN_CPHS;
    int READ_SPN_3GPP;
    int INIT;
    int IDLE;
  }
  int MCCMNC_CODES_HAVING_3DIGITS_MNC;
  int EVENT_GET_CSP_CPHS_DONE;
  int EVENT_GET_CFIS_DONE;
  int EVENT_SIM_REFRESH;
  int EVENT_SET_MSISDN_DONE;
  int EVENT_GET_INFO_CPHS_DONE;
  int EVENT_SET_CPHS_MAILBOX_DONE;
  int EVENT_GET_CFF_DONE;
  int EVENT_GET_SMS_DONE;
  int EVENT_SMS_ON_SIM;
  int EVENT_SET_MBDN_DONE;
  int EVENT_MARK_SMS_READ_DONE;
  int EVENT_GET_ALL_SMS_DONE;
  int EVENT_GET_SST_DONE;
  int EVENT_GET_PNN_DONE;
  int EVENT_UPDATE_DONE;
  int EVENT_GET_SPDI_DONE;
  int EVENT_GET_SPN_DONE;
  int EVENT_GET_CPHS_MAILBOX_DONE;
  int EVENT_GET_MSISDN_DONE;
  int EVENT_GET_AD_DONE;
  int EVENT_GET_VOICE_MAIL_INDICATOR_CPHS_DONE;
  int EVENT_GET_MWIS_DONE;
  int EVENT_GET_MBDN_DONE;
  int EVENT_GET_MBI_DONE;
  int EVENT_GET_ICCID_DONE;
  int EVENT_GET_IMSI_DONE;
  int EVENT_RADIO_OFF_OR_NOT_AVAILABLE;
  int CPHS_SST_MBN_ENABLED;
  int CPHS_SST_MBN_MASK;
  int CFF_LINE1_RESET;
  int CFF_LINE1_MASK;
  int CFF_UNCONDITIONAL_DEACTIVE;
  int CFF_UNCONDITIONAL_ACTIVE;
  int TAG_SHORT_NETWORK_NAME;
  int TAG_FULL_NETWORK_NAME;
  int TAG_SPDI_PLMN_LIST;
  int TAG_SPDI;
  int SPN_RULE_SHOW_PLMN;
  int SPN_RULE_SHOW_SPN;
  int mUsimServiceTable;
  int pnnHomeName;
  int spdiNetworks;
  int spnDisplayCondition;
  int mEfCfis;
  int mEfCff;
  int efCPHS_MWI;
  int efMWIS;
  int mCspPlmnEnabled;
  int mCphsInfo;
  int spnState;
  int callForwardingEnabled;
  int imsi;
  int mSpnOverride;
  int mVmConfig;
  int DBG;
  int CRASH_RIL;
  int LOG_TAG;
}
class SIMFileHandler {
  int LOG_TAG;
}
class GsmSmsCbTest {
  int etwsMessageTest;
  int etwsMessageCancel;
  int etwsMessageNormal;
  int sTestLocation;
  int TAG;
}
class GsmSmsCbMessage {
  int PDU_BODY_PAGE_LENGTH;
  int CARRIAGE_RETURN;
  int LANGUAGE_CODES_GROUP_2;
  int LANGUAGE_CODES_GROUP_0;
}
class GsmSmsAddress {
  int OFFSET_ADDRESS_VALUE;
  int OFFSET_TOA;
  int OFFSET_ADDRESS_LENGTH;
}
class GsmServiceStateTracker {
  int mAutoTimeZoneObserver;
  int mAutoTimeObserver;
  int mIntentReceiver;
  int CS_NOTIFICATION;
  int PS_NOTIFICATION;
  int CS_EMERGENCY_ENABLED;
  int CS_NORMAL_ENABLED;
  int CS_DISABLED;
  int CS_ENABLED;
  int PS_DISABLED;
  int PS_ENABLED;
  int DEFAULT_GPRS_CHECK_PERIOD_MILLIS;
  int curSpnRule;
  int curPlmn;
  int curSpn;
  int WAKELOCK_TAG;
  int mWakeLock;
  int mNotification;
  int mReportedGprsNoReg;
  int mStartedGprsRegCheck;
  int mNeedToRegForSimLoaded;
  int mSavedAtTime;
  int mSavedTime;
  int mSavedTimeZone;
  int mNitzUpdatedTime;
  int cr;
  int mGotCountryCode;
  int mZoneTime;
  int mZoneDst;
  int mZoneOffset;
  int mNeedFixZoneAfterNitz;
  int mEmergencyOnly;
  int mDataRoaming;
  int mGsmRoaming;
  int mNewReasonDataDenied;
  int mReasonDataDenied;
  int mNewMaxDataCalls;
  int mMaxDataCalls;
  int newGPRSState;
  int gprsState;
  int mPreferredNetworkType;
  int newCellLoc;
  int cellLoc;
  int phone;
  int DBG;
  int LOG_TAG;
}
class GsmSMSDispatcher {
  int mSmsCbPageMap;
  class SmsCbConcatInfo {
    int mLocation;
    int mHeader;
  }
  int mDataDownloadHandler;
  int EVENT_WRITE_SMS_COMPLETE;
  int EVENT_NEW_BROADCAST_SMS;
  int EVENT_NEW_SMS_STATUS_REPORT;
  int TAG;
}
class GsmMmiCode {
  int sTwoDigitNumberPattern;
  int MATCH_GROUP_DIALING_NUMBER;
  int MATCH_GROUP_PWD_CONFIRM;
  int MATCH_GROUP_SIC;
  int MATCH_GROUP_SIB;
  int MATCH_GROUP_SIA;
  int MATCH_GROUP_SERVICE_CODE;
  int MATCH_GROUP_ACTION;
  int MATCH_GROUP_POUND_STRING;
  int sPatternSuppService;
  int message;
  int state;
  int isUssdRequest;
  int isPendingUSSD;
  int pwd;
  int dialingNumber;
  int poundString;
  int sic;
  int sib;
  int sia;
  int sc;
  int action;
  int context;
  int phone;
  int EVENT_USSD_CANCEL_COMPLETE;
  int EVENT_SET_CFF_COMPLETE;
  int EVENT_QUERY_COMPLETE;
  int EVENT_USSD_COMPLETE;
  int EVENT_QUERY_CF_COMPLETE;
  int EVENT_GET_CLIR_COMPLETE;
  int EVENT_SET_COMPLETE;
  int SC_PUK2;
  int SC_PUK;
  int SC_PIN2;
  int SC_PIN;
  int SC_PWD;
  int SC_BA_MT;
  int SC_BA_MO;
  int SC_BA_ALL;
  int SC_BAICr;
  int SC_BAIC;
  int SC_BAOICxH;
  int SC_BAOIC;
  int SC_BAOC;
  int SC_WAIT;
  int SC_CF_All_Conditional;
  int SC_CF_All;
  int SC_CFNR;
  int SC_CFNRy;
  int SC_CFB;
  int SC_CFU;
  int SC_CLIR;
  int SC_CLIP;
  int ACTION_ERASURE;
  int ACTION_REGISTER;
  int ACTION_INTERROGATE;
  int ACTION_DEACTIVATE;
  int ACTION_ACTIVATE;
  int END_OF_USSD_COMMAND;
  int MAX_LENGTH_SHORT_CODE;
  int LOG_TAG;
}
class GsmDataConnectionTracker {
  int mPollNetStat;
  int mApnObserver;
  int DATA_STALL_NOT_SUSPECTED;
  int DATA_STALL_SUSPECTED;
  int canSetPreferApn;
  int APN_ID;
  int PREFERAPN_NO_UPDATE_URI;
  int INTENT_DATA_STALL_ALARM;
  int INTENT_RECONNECT_ALARM_EXTRA_TYPE;
  int INTENT_RECONNECT_ALARM;
  int POLL_PDP_MILLIS;
  class RecoveryAction {
    int RADIO_RESTART_WITH_PROP;
    int RADIO_RESTART;
    int REREGISTER;
    int CLEANUP;
    int GET_DATA_CALL_LIST;
  }
  int mResolver;
  int mReregisterOnReconnectFailure;
  class ApnChangeObserver {
  }
  int RADIO_TESTS;
  int LOG_TAG;
}
class GsmDataConnection {
  int mProfileId;
  int LOG_TAG;
}
class GsmConnection {
  class MyHandler {
  }
  int WAKE_LOCK_TIMEOUT_MILLIS;
  int PAUSE_DELAY_MILLIS;
  int PAUSE_DELAY_FIRST_MILLIS;
  int EVENT_WAKE_LOCK_TIMEOUT;
  int EVENT_NEXT_POST_DIAL;
  int EVENT_PAUSE_DONE;
  int EVENT_DTMF_DONE;
  int mPartialWakeLock;
  int h;
  int uusInfo;
  int numberPresentation;
  int postDialState;
  int cause;
  int nextPostDialChar;
  int holdingStartTime;
  int duration;
  int connectTimeReal;
  int disconnectTime;
  int connectTime;
  int createTime;
  int index;
  int disconnected;
  int isIncoming;
  int postDialString;
  int dialString;
  int address;
  int parent;
  int owner;
  int LOG_TAG;
}
class GsmCallTracker {
  int state;
  int desiredMute;
  int phone;
  int hangupPendingMO;
  int pendingMO;
  int backgroundCall;
  int foregroundCall;
  int ringingCall;
  int droppedDuringPoll;
  int voiceCallStartedRegistrants;
  int voiceCallEndedRegistrants;
  int connections;
  int MAX_CONNECTIONS_PER_CALL;
  int MAX_CONNECTIONS;
  int DBG_POLL;
  int REPEAT_POLLING;
  int LOG_TAG;
}
class GsmCall {
  int owner;
  int connections;
}
class GSMTestHandler {
  int FAIL_TIMEOUT_MILLIS;
  int mContext;
  int mGSMPhone;
  int sc;
  int mMsgConsumed;
  int mCurrentMessage;
  int mHandler;
}
class GSMPhoneTest {
  int ANY_MESSAGE;
  int EVENT_OEM_RIL_MESSAGE;
  int SERVICE_STATE_CHANGED;
  int SUPP_SERVICE_FAILED;
  int EVENT_IN_SERVICE;
  int EVENT_MMI_COMPLETE;
  int EVENT_MMI_INITIATE;
  int EVENT_SSN;
  int EVENT_DONE;
  int EVENT_POST_DIAL;
  int EVENT_CHANNEL_OPENED;
  int EVENT_RINGING;
  int EVENT_DISCONNECT;
  int EVENT_PHONE_STATE_CHANGED;
  int mHandler;
  int mGSMTestHandler;
  int mGSMPhone;
  int mRadioControl;
}
class GSMPhone {
  class NetworkSelectMessage {
    int operatorAlphaLong;
    int operatorNumeric;
    int message;
  }
  int mVmNumber;
  int mImeiSv;
  int mImei;
  int debugSocket;
  int debugPortThread;
  int mSsnRegistrants;
  int mPostDialHandler;
  int mSubInfo;
  int mSimSmsIntManager;
  int mSimPhoneBookIntManager;
  int mPendingMMIs;
  int mSST;
  int mCT;
  int VM_SIM_IMSI;
  int VM_NUMBER;
  int CIPHERING_KEY;
  int VDBG;
  int LOCAL_DEBUG;
  int LOG_TAG;
}
class CallFailCause {
  int ERROR_UNSPECIFIED;
  int FDN_BLOCKED;
  int CALL_BARRED;
  int ACM_LIMIT_EXCEEDED;
  int BEARER_NOT_AVAIL;
  int QOS_NOT_AVAIL;
  int CHANNEL_NOT_AVAIL;
  int SWITCHING_CONGESTION;
  int TEMPORARY_FAILURE;
  int NO_CIRCUIT_AVAIL;
  int NORMAL_UNSPECIFIED;
  int STATUS_ENQUIRY;
  int NUMBER_CHANGED;
  int USER_BUSY;
  int NORMAL_CLEARING;
  int UNOBTAINABLE_NUMBER;
}
