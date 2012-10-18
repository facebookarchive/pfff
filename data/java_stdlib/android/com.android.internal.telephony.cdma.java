package com.android.internal.telephony.cdma;
class TtyIntent {
  int TTY_PREFFERED_MODE;
  int TTY_PREFERRED_MODE_CHANGE_ACTION;
  int TTY_ENABLED;
  int TTY_ENABLED_CHANGE_ACTION;
  int TAG;
}
class SmsMessage {
  class SubmitPdu {
  }
  int mBearerData;
  int mEnvelope;
  int RETURN_ACK;
  int RETURN_NO_ACK;
  int status;
  int BEARER_DATA;
  int CAUSE_CODES;
  int BEARER_REPLY_OPTION;
  int DESTINATION_SUB_ADDRESS;
  int DESTINATION_ADDRESS;
  int ORIGINATING_SUB_ADDRESS;
  int ORIGINATING_ADDRESS;
  int SERVICE_CATEGORY;
  int TELESERVICE_IDENTIFIER;
  int LOGGABLE_TAG;
  int LOG_TAG;
}
class SignalToneUtil {
  int hm;
  int IS95_CONST_IR_SIG_TONE_ABBR_ALRT;
  int IS95_CONST_IR_SIG_IS54B_PBX_S_X4;
  int IS95_CONST_IR_SIG_IS54B_PBX_SLS;
  int IS95_CONST_IR_SIG_IS54B_PBX_SSL;
  int IS95_CONST_IR_SIG_IS54B_PBX_SS;
  int IS95_CONST_IR_SIG_IS54B_PBX_L;
  int IS95_CONST_IR_SIG_IS54B_S_X4;
  int IS95_CONST_IR_SIG_IS54B_SLS;
  int IS95_CONST_IR_SIG_IS54B_SS_2;
  int IS95_CONST_IR_SIG_IS54B_SSL;
  int IS95_CONST_IR_SIG_IS54B_SS;
  int IS95_CONST_IR_SIG_IS54B_L;
  int IS95_CONST_IR_SIG_IS54B_NO_TONE;
  int IS95_CONST_IR_SIG_TONE_NO_TONE;
  int IS95_CONST_IR_SIG_TONE_PIP;
  int IS95_CONST_IR_SIG_TONE_CALL_W;
  int IS95_CONST_IR_SIG_TONE_ANSWER;
  int IS95_CONST_IR_SIG_TONE_CONFIRM;
  int IS95_CONST_IR_SIG_TONE_BUSY;
  int IS95_CONST_IR_SIG_TONE_ABB_RE;
  int IS95_CONST_IR_SIG_TONE_REORDER;
  int IS95_CONST_IR_SIG_TONE_ABB_INT;
  int IS95_CONST_IR_SIG_TONE_INT;
  int IS95_CONST_IR_SIG_TONE_RING;
  int IS95_CONST_IR_SIG_TONE_DIAL;
  int IS95_CONST_IR_SIG_ISDN_OFF;
  int IS95_CONST_IR_SIG_ISDN_PAT_7;
  int IS95_CONST_IR_SIG_ISDN_PAT_6;
  int IS95_CONST_IR_SIG_ISDN_PAT_5;
  int IS95_CONST_IR_SIG_ISDN_PING;
  int IS95_CONST_IR_SIG_ISDN_PAT_3;
  int IS95_CONST_IR_SIG_ISDN_SP_PRI;
  int IS95_CONST_IR_SIG_ISDN_INTGRP;
  int IS95_CONST_IR_SIG_ISDN_NORMAL;
  int TAPIAMSSCDMA_SIGNAL_PITCH_UNKNOWN;
  int IS95_CONST_IR_ALERT_LOW;
  int IS95_CONST_IR_ALERT_HIGH;
  int IS95_CONST_IR_ALERT_MED;
  int IS95_CONST_IR_SIGNAL_USR_DEFD_ALERT;
  int IS95_CONST_IR_SIGNAL_IS54B;
  int IS95_CONST_IR_SIGNAL_ISDN;
  int IS95_CONST_IR_SIGNAL_TONE;
  int CDMA_INVALID_TONE;
}
class RuimSmsInterfaceManager {
  int mHandler;
  int EVENT_UPDATE_DONE;
  int EVENT_LOAD_DONE;
  int mSms;
  int mSuccess;
  int mLock;
  int DBG;
  int LOG_TAG;
}
class RuimRecords {
  int EVENT_RUIM_REFRESH;
  int EVENT_GET_SMS_DONE;
  int EVENT_SMS_ON_RUIM;
  int EVENT_MARK_SMS_READ_DONE;
  int EVENT_GET_ALL_SMS_DONE;
  int EVENT_GET_SST_DONE;
  int EVENT_UPDATE_DONE;
  int EVENT_GET_CDMA_SUBSCRIPTION_DONE;
  int EVENT_GET_ICCID_DONE;
  int EVENT_GET_DEVICE_IDENTITY_DONE;
  int EVENT_GET_IMSI_DONE;
  int EVENT_RADIO_OFF_OR_NOT_AVAILABLE;
  int mPrlVersion;
  int mMin2Min1;
  int mMyMobileNumber;
  int mImsi;
  int m_ota_commited;
  int DBG;
  int LOG_TAG;
}
class RuimPhoneBookInterfaceManager {
  int LOG_TAG;
}
class RuimFileHandler {
  int LOG_TAG;
}
class EriManager {
  int mEriFile;
  int isEriFileLoaded;
  int mEriFileSource;
  int mContext;
  int mPhone;
  int ERI_FROM_MODEM;
  int ERI_FROM_FILE_SYSTEM;
  int ERI_FROM_XML;
  int VDBG;
  int DBG;
  int LOG_TAG;
  class EriDisplayInformation {
    int mEriIconText;
    int mEriIconMode;
    int mEriIconIndex;
  }
  class EriFile {
    int mRoamIndTable;
    int mCallPromptId;
    int mEriFileType;
    int mNumberOfEriEntries;
    int mVersionNumber;
  }
}
class EriInfo {
  int mAlertId;
  int mCallPromptId;
  int mEriText;
  int mIconMode;
  int mIconIndex;
  int mRoamingIndicator;
  int ROAMING_ICON_MODE_FLASH;
  int ROAMING_ICON_MODE_NORMAL;
  int ROAMING_INDICATOR_FLASH;
  int ROAMING_INDICATOR_OFF;
  int ROAMING_INDICATOR_ON;
}
class CdmaSubscriptionSourceManager {
  int mCdmaSubscriptionSource;
  int mCdmaSubscriptionSourceChangedRegistrants;
  int mContext;
  int mCM;
  int sReferenceCount;
  int sReferenceCountMonitor;
  int sInstance;
  int PREFERRED_CDMA_SUBSCRIPTION;
  int SUBSCRIPTION_FROM_NV;
  int SUBSCRIPTION_FROM_RUIM;
  int SUBSCRIPTION_SOURCE_UNKNOWN;
  int EVENT_RADIO_ON;
  int EVENT_GET_CDMA_SUBSCRIPTION_SOURCE;
  int EVENT_CDMA_SUBSCRIPTION_SOURCE_CHANGED;
  int LOG_TAG;
}
class CdmaSmsCbTest {
  int CMAS_TEST_BEARER_DATA;
  int CAT_AMBER_ALERTS;
  int CAT_SEVERE_THREAT;
  int CAT_EXTREME_THREAT;
  int IS91_TEXT;
  int MONTHLY_TEST_ALERT;
  int AMBER_ALERT;
  int SEVERE_ALERT;
  int EXTREME_ALERT;
  int PRES_ALERT;
  int TEST_TEXT;
  int SUBPARAM_SERVICE_CATEGORY_PROGRAM_DATA;
  int SUBPARAM_LANGUAGE_INDICATOR;
  int SUBPARAM_PRIORITY_INDICATOR;
  int SUBPARAM_USER_DATA;
  int SUBPARAM_MESSAGE_IDENTIFIER;
}
class CdmaServiceStateTracker {
  int mAutoTimeZoneObserver;
  int mAutoTimeObserver;
  int currentCarrier;
  int cr;
  int mRegistrationDeniedReason;
  int mCdmaSSM;
  int isSubscriptionFromRuim;
  int isEriTextLoaded;
  int mIsMinInfoReady;
  int mPrlVersion;
  int mMin;
  int mHomeNetworkId;
  int mHomeSystemId;
  int mMdn;
  int mCurPlmn;
  int WAKELOCK_TAG;
  int mWakeLock;
  int mNeedToRegForRuimLoaded;
  int mSavedAtTime;
  int mSavedTime;
  int mSavedTimeZone;
  int mGotCountryCode;
  int mZoneTime;
  int mZoneDst;
  int mZoneOffset;
  int mNeedFixZone;
  int cdmaForSubscriptionInfoReadyRegistrants;
  int mRegistrationState;
  int mNewDataConnectionState;
  int mDataConnectionState;
  int mDefaultRoamingIndicator;
  int mIsInPrl;
  int mRoamingIndicator;
  int mCdmaRoaming;
  int mNitzUpdateDiff;
  int NITZ_UPDATE_DIFF_DEFAULT;
  int mNitzUpdateSpacing;
  int NITZ_UPDATE_SPACING_DEFAULT;
  int mCurrentOtaspMode;
  int UNACTIVATED_MIN_VALUE;
  int UNACTIVATED_MIN2_VALUE;
  int newCellLoc;
  int cellLoc;
  int phone;
  int LOG_TAG;
}
class CdmaSMSDispatcher {
  int mCheckForDuplicatePortsInOmadmWapPush;
  int mLastAcknowledgedSmsFingerprint;
  int mLastDispatchedSmsFingerprint;
  int TAG;
}
class CdmaMmiCode {
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
  int EVENT_SET_COMPLETE;
  int SC_PUK;
  int ACTION_REGISTER;
  int LOG_TAG;
}
class CdmaLteUiccRecords {
  class EfCsimEprlLoaded {
  }
  class EfCsimCdmaHomeLoaded {
  }
  class EfCsimImsimLoaded {
  }
  class EfCsimMdnLoaded {
  }
  class EfCsimSpnLoaded {
  }
  class EfCsimLiLoaded {
  }
  class EfPlLoaded {
  }
  int mIsimUiccRecords;
  int mHomeNetworkId;
  int mHomeSystemId;
  int mPrlVersion;
  int mMin;
  int mMdn;
  int mCsimSpnDisplayCondition;
  int mEFli;
  int mEFpl;
}
class CdmaLteUiccFileHandler {
  int LOG_TAG;
}
class CdmaLteServiceStateTracker {
  int mLteSS;
  int mCdmaLtePhone;
}
class CdmaInformationRecords {
  class CdmaT53AudioControlInfoRec {
    int downlink;
    int uplink;
  }
  class CdmaT53ClirInfoRec {
    int cause;
  }
  class CdmaLineControlInfoRec {
    int lineCtrlPowerDenial;
    int lineCtrlReverse;
    int lineCtrlToggle;
    int lineCtrlPolarityIncluded;
  }
  class CdmaRedirectingNumberInfoRec {
    int redirectingReason;
    int numberInfoRec;
    int REASON_CALL_FORWARDING_UNCONDITIONAL;
    int REASON_CALL_FORWARDING_BY_THE_CALLED_DTE;
    int REASON_CALLED_DTE_OUT_OF_ORDER;
    int REASON_CALL_FORWARDING_NO_REPLY;
    int REASON_CALL_FORWARDING_BUSY;
    int REASON_UNKNOWN;
  }
  class CdmaNumberInfoRec {
    int si;
    int pi;
    int numberPlan;
    int numberType;
    int number;
    int id;
  }
  class CdmaDisplayInfoRec {
    int alpha;
    int id;
  }
  class CdmaSignalInfoRec {
    int signal;
    int alertPitch;
    int signalType;
    int isPresent;
  }
  int RIL_CDMA_T53_AUDIO_CONTROL_INFO_REC;
  int RIL_CDMA_T53_RELEASE_INFO_REC;
  int RIL_CDMA_T53_CLIR_INFO_REC;
  int RIL_CDMA_EXTENDED_DISPLAY_INFO_REC;
  int RIL_CDMA_LINE_CONTROL_INFO_REC;
  int RIL_CDMA_REDIRECTING_NUMBER_INFO_REC;
  int RIL_CDMA_SIGNAL_INFO_REC;
  int RIL_CDMA_CONNECTED_NUMBER_INFO_REC;
  int RIL_CDMA_CALLING_PARTY_NUMBER_INFO_REC;
  int RIL_CDMA_CALLED_PARTY_NUMBER_INFO_REC;
  int RIL_CDMA_DISPLAY_INFO_REC;
  int record;
}
class CdmaDataConnectionTracker {
  int mPollNetStat;
  int mDefaultApnId;
  int mDunApnTypes;
  int mDefaultApnTypes;
  int mSupportedApnTypes;
  int DATA_CONNECTION_ACTIVE_PH_LINK_UP;
  int DATA_CONNECTION_ACTIVE_PH_LINK_DOWN;
  int DATA_CONNECTION_ACTIVE_PH_LINK_INACTIVE;
  int INTENT_DATA_STALL_ALARM;
  int INTENT_RECONNECT_ALARM;
  int DATA_CONNECTION_POOL_SIZE;
  int TIME_DELAYED_TO_RESTART_RADIO;
  int mPendingRestartRadio;
  int mPendingDataConnection;
  int mCdmaSSM;
  int mCdmaPhone;
  int LOG_TAG;
}
class CdmaDataConnection {
  int LOG_TAG;
}
class CdmaConnection {
  class MyHandler {
  }
  int PAUSE_DELAY_MILLIS;
  int WAKE_LOCK_TIMEOUT_MILLIS;
  int EVENT_WAKE_LOCK_TIMEOUT;
  int EVENT_NEXT_POST_DIAL;
  int EVENT_PAUSE_DONE;
  int EVENT_DTMF_DONE;
  int mPartialWakeLock;
  int h;
  int cnapNamePresentation;
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
  int cnapName;
  int disconnected;
  int isIncoming;
  int postDialString;
  int dialString;
  int address;
  int parent;
  int owner;
  int LOG_TAG;
}
class CdmaCallWaitingNotification {
  int signal;
  int alertPitch;
  int signalType;
  int isPresent;
  int numberPlan;
  int numberType;
  int namePresentation;
  int name;
  int numberPresentation;
  int number;
  int LOG_TAG;
}
class CdmaCallTracker {
  int mIsEcmTimerCanceled;
  int state;
  int pendingCallClirMode;
  int desiredMute;
  int phone;
  int mIsInEmergencyCall;
  int pendingCallInEcm;
  int hangupPendingMO;
  int pendingMO;
  int backgroundCall;
  int foregroundCall;
  int ringingCall;
  int droppedDuringPoll;
  int callWaitingRegistrants;
  int voiceCallStartedRegistrants;
  int voiceCallEndedRegistrants;
  int connections;
  int MAX_CONNECTIONS_PER_CALL;
  int MAX_CONNECTIONS;
  int DBG_POLL;
  int REPEAT_POLLING;
  int LOG_TAG;
}
class CdmaCall {
  int owner;
  int state;
  int connections;
}
class CallFailCause {
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
  int FDN_BLOCKED;
  int CALL_BARRED;
  int ACM_LIMIT_EXCEEDED;
  int NO_CIRCUIT_AVAIL;
  int NORMAL_UNSPECIFIED;
  int USER_BUSY;
  int NORMAL_CLEARING;
}
class CDMAPhone {
  int pOtaSpNumSchema;
  int INVALID_SYSTEM_SELECTION_CODE;
  int IS683_CONST_1900MHZ_F_BLOCK;
  int IS683_CONST_1900MHZ_E_BLOCK;
  int IS683_CONST_1900MHZ_D_BLOCK;
  int IS683_CONST_1900MHZ_C_BLOCK;
  int IS683_CONST_1900MHZ_B_BLOCK;
  int IS683_CONST_1900MHZ_A_BLOCK;
  int IS683_CONST_800MHZ_B_BAND;
  int IS683_CONST_800MHZ_A_BAND;
  int IS683A_SYS_SEL_CODE_OFFSET;
  int IS683A_SYS_SEL_CODE_NUM_DIGITS;
  int IS683A_FEATURE_CODE_NUM_DIGITS;
  int IS683A_FEATURE_CODE;
  int PROPERTY_CDMA_HOME_OPERATOR_NUMERIC;
  int mPostDialHandler;
  int mExitEcmRunnable;
  int mCarrierOtaSpNumSchema;
  int mMeid;
  int mEsn;
  int mImeiSv;
  int mImei;
  int mEcmExitRespRegistrant;
  int mIsPhoneInEcmState;
  int mEcmTimerResetRegistrants;
  int mEriFileLoadedRegistrants;
  int mWakeLock;
  int mEriManager;
  int mSubInfo;
  int mCdmaSubscriptionSource;
  int mRuimSmsInterfaceManager;
  int mRuimPhoneBookInterfaceManager;
  int mPendingMmis;
  int mCdmaSSM;
  int mSST;
  int mCT;
  int CANCEL_ECM_TIMER;
  int RESTART_ECM_TIMER;
  int mVmNumber;
  int VM_NUMBER_CDMA;
  int VM_COUNT_CDMA;
  int DEFAULT_ECM_EXIT_TIMER_VALUE;
  int VDBG;
  int DBG;
  int LOG_TAG;
}
class CDMALTEPhone {
  class NetworkSelectMessage {
    int operatorAlphaLong;
    int operatorNumeric;
    int message;
  }
  int m3gppSMS;
  int DBG;
  int LOG_TAG;
}
