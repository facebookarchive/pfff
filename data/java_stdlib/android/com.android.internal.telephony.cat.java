package com.android.internal.telephony.cat;
class ValueParser {
}
class ToneSettings {
  int CREATOR;
  int vibrate;
  int tone;
  int duration;
}
class Tone {
  int MELODY_8;
  int MELODY_7;
  int MELODY_6;
  int MELODY_5;
  int MELODY_4;
  int MELODY_3;
  int MELODY_2;
  int MELODY_1;
  int MESSAGE_RECEIVED;
  int QUESTION;
  int URGENT;
  int SAD;
  int HAPPY;
  int VIBRATE_ONLY;
  int CRITICAL_ALERT;
  int INCOMING_SMS;
  int INCOMING_SPEECH_CALL;
  int NEGATIVE_ACK;
  int POSITIVE_ACK;
  int GENERAL_BEEP;
  int RINGING;
  int CALL_WAITING;
  int ERROR_SPECIAL_INFO;
  int RADIO_PATH_NOT_AVAILABLE;
  int RADIO_PATH_ACK;
  int CONGESTION;
  int BUSY;
  int DIAL;
  int CREATOR;
  int mValue;
}
class TextMessage {
  int CREATOR;
  int duration;
  int userClear;
  int responseNeeded;
  int isHighPriority;
  int iconSelfExplanatory;
  int icon;
  int text;
  int title;
}
class TextColor {
  int BRIGHT_MAGENTA;
  int BRIGHT_BLUE;
  int BRIGHT_CYAN;
  int BRIGHT_GREEN;
  int BRIGHT_YELLOW;
  int BRIGHT_RED;
  int WHITE;
  int GRAY;
  int DARK_MAGENTA;
  int DARK_BLUE;
  int DARK_CYAN;
  int DARK_GREEN;
  int DARK_YELLOW;
  int DARK_RED;
  int DARK_GRAY;
  int BLACK;
  int mValue;
}
class TextAttribute {
  int color;
  int strikeThrough;
  int underlined;
  int italic;
  int bold;
  int size;
  int align;
  int length;
  int start;
}
class TextAlignment {
  int DEFAULT;
  int RIGHT;
  int CENTER;
  int LEFT;
  int mValue;
}
class RilMessageDecoder {
  class StateCmdParamsReady {
  }
  class StateStart {
  }
  int mStateCmdParamsReady;
  int mStateStart;
  int mCaller;
  int mCurrentRilMessage;
  int mCmdParamsFactory;
  int sInstance;
  int CMD_PARAMS_READY;
  int CMD_START;
}
class ResultException {
  int mExplanation;
  int mAdditionalInfo;
  int mResult;
}
class ResultCode {
  int MMS_ERROR;
  int FRAMES_ERROR;
  int ACCESS_TECH_UNABLE_TO_PROCESS;
  int BIP_ERROR;
  int USIM_CALL_CONTROL_PERMANENT;
  int MULTI_CARDS_CMD_ERROR;
  int USSD_RETURN_ERROR;
  int REQUIRED_VALUES_MISSING;
  int SMS_RP_ERROR;
  int SS_RETURN_ERROR;
  int CMD_NUM_NOT_KNOWN;
  int CMD_DATA_NOT_UNDERSTOOD;
  int CMD_TYPE_NOT_UNDERSTOOD;
  int BEYOND_TERMINAL_CAPABILITY;
  int MMS_TEMPORARY;
  int LAUNCH_BROWSER_ERROR;
  int NAA_CALL_CONTROL_TEMPORARY;
  int CONTRADICTION_WITH_TIMER;
  int USER_CLEAR_DOWN_CALL;
  int USER_NOT_ACCEPT;
  int NETWORK_CRNTLY_UNABLE_TO_PROCESS;
  int TERMINAL_CRNTLY_UNABLE_TO_PROCESS;
  int USSD_SS_SESSION_TERM_BY_USER;
  int HELP_INFO_REQUIRED;
  int NO_RESPONSE_FROM_USER;
  int BACKWARD_MOVE_BY_USER;
  int UICC_SESSION_TERM_BY_USER;
  int PRFRMD_TONE_NOT_PLAYED;
  int PRFRMD_NAA_NOT_ACTIVE;
  int PRFRMD_WITH_MODIFICATION;
  int PRFRMD_LIMITED_SERVICE;
  int PRFRMD_MODIFIED_BY_NAA;
  int PRFRMD_ICON_NOT_DISPLAYED;
  int PRFRMD_WITH_ADDITIONAL_EFS_READ;
  int PRFRMD_WITH_MISSING_INFO;
  int PRFRMD_WITH_PARTIAL_COMPREHENSION;
  int OK;
  int mCode;
}
class DTTZResponseData {
  int calendar;
}
class LanguageResponseData {
  int lang;
}
class GetInkeyInputResponseData {
  int GET_INKEY_NO;
  int GET_INKEY_YES;
  int mInData;
  int mYesNoResponse;
  int mIsYesNo;
  int mIsPacked;
  int mIsUcs2;
}
class SelectItemResponseData {
  int id;
}
class ResponseData {
}
class PresentationType {
  int NAVIGATION_OPTIONS;
  int DATA_VALUES;
  int NOT_SPECIFIED;
}
class Menu {
  int CREATOR;
  int itemsIconSelfExplanatory;
  int titleIconSelfExplanatory;
  int helpAvailable;
  int softKeyPreferred;
  int defaultItem;
  int titleIcon;
  int title;
  int presentationType;
  int titleAttrs;
  int items;
}
class LaunchBrowserMode {
  int LAUNCH_NEW_BROWSER;
  int USE_EXISTING_BROWSER;
  int LAUNCH_IF_NOT_ALREADY_LAUNCHED;
}
class Item {
  int CREATOR;
  int icon;
  int text;
  int id;
}
class Input {
  int CREATOR;
  int duration;
  int helpAvailable;
  int yesNo;
  int echo;
  int digitOnly;
  int packed;
  int ucs2;
  int maxLen;
  int minLen;
  int icon;
  int defaultText;
  int text;
}
class ImageDescriptor {
  int CODING_SCHEME_COLOUR;
  int CODING_SCHEME_BASIC;
  int length;
  int lowOffset;
  int highOffset;
  int imageId;
  int codingScheme;
  int height;
  int width;
}
class IconLoader {
  int CLUT_ENTRY_SIZE;
  int CLUT_LOCATION_OFFSET;
  int EVENT_READ_CLUT_DONE;
  int EVENT_READ_ICON_DONE;
  int EVENT_READ_EF_IMG_RECOED_DONE;
  int STATE_MULTI_ICONS;
  int STATE_SINGLE_ICON;
  int sLoader;
  int mIconsCache;
  int mIcons;
  int mCurrentRecordIndex;
  int mRecordNumbers;
  int mIconData;
  int mEndMsg;
  int mSimFH;
  int mRecordNumber;
  int mCurrentIcon;
  int mId;
  int mState;
}
class FontSize {
  int SMALL;
  int LARGE;
  int NORMAL;
  int mValue;
}
class Duration {
  int CREATOR;
  class TimeUnit {
    int TENTH_SECOND;
    int SECOND;
    int MINUTE;
    int mValue;
  }
  int timeUnit;
  int timeInterval;
}
class ComprehensionTlvTag {
  int TEXT_ATTRIBUTE;
  int BROWSER_TERMINATION_CAUSE;
  int URL;
  int LANGUAGE;
  int IMMEDIATE_RESPONSE;
  int ITEM_ICON_ID_LIST;
  int ICON_ID;
  int EVENT_LIST;
  int DEFAULT_TEXT;
  int HELP_REQUEST;
  int FILE_LIST;
  int RESPONSE_LENGTH;
  int ITEM_ID;
  int ITEM;
  int TONE;
  int TEXT_STRING;
  int SMS_TPDU;
  int USSD_STRING;
  int ADDRESS;
  int ALPHA_ID;
  int DURATION;
  int RESULT;
  int DEVICE_IDENTITIES;
  int COMMAND_DETAILS;
  int mValue;
}
class ComprehensionTlv {
  int mRawValue;
  int mValueIndex;
  int mLength;
  int mCr;
  int mTag;
  int LOG_TAG;
}
class CommandParamsFactory {
  int LANGUAGE_SETTING;
  int DTTZ_SETTING;
  int REFRESH_UICC_RESET;
  int REFRESH_NAA_INIT;
  int REFRESH_NAA_INIT_AND_FILE_CHANGE;
  int REFRESH_NAA_INIT_AND_FULL_FILE_CHANGE;
  int LOAD_MULTI_ICONS;
  int LOAD_SINGLE_ICON;
  int LOAD_NO_ICON;
  int MSG_ID_LOAD_ICON_DONE;
  int mCaller;
  int mIconLoadState;
  int mCmdParams;
  int mIconLoader;
  int sInstance;
}
class BIPClientParams {
  int bHasAlphaId;
  int textMsg;
}
class GetInputParams {
  int input;
}
class SelectItemParams {
  int loadTitleIcon;
  int menu;
}
class CallSetupParams {
  int callMsg;
  int confirmMsg;
}
class PlayToneParams {
  int settings;
  int textMsg;
}
class LaunchBrowserParams {
  int url;
  int mode;
  int confirmMsg;
}
class DisplayTextParams {
  int textMsg;
}
class CommandParams {
  int cmdDet;
}
class ItemsIconId {
  int selfExplanatory;
  int recordNumbers;
}
class IconId {
  int selfExplanatory;
  int recordNumber;
}
class DeviceIdentities {
  int destinationId;
  int sourceId;
}
class CommandDetails {
  int CREATOR;
  int commandQualifier;
  int typeOfCommand;
  int commandNumber;
  int compRequired;
}
class ValueObject {
}
class CatService {
  int STK_DEFAULT;
  int DEV_ID_NETWORK;
  int DEV_ID_TERMINAL;
  int DEV_ID_UICC;
  int DEV_ID_EARPIECE;
  int DEV_ID_DISPLAY;
  int DEV_ID_KEYPAD;
  int MSG_ID_ICC_RECORDS_LOADED;
  int MSG_ID_RIL_MSG_DECODED;
  int MSG_ID_SIM_READY;
  int MSG_ID_RESPONSE;
  int MSG_ID_REFRESH;
  int MSG_ID_CALL_SETUP;
  int MSG_ID_EVENT_NOTIFY;
  int MSG_ID_PROACTIVE_COMMAND;
  int MSG_ID_SESSION_END;
  int mStkAppInstalled;
  int mMsgDecoder;
  int mMenuCmd;
  int mCurrntCmd;
  int mContext;
  int mCmdIf;
  int sInstance;
  int sInstanceLock;
  int mIccRecords;
}
class RilMessage {
  int mResCode;
  int mData;
  int mId;
}
class CatResponseMessage {
  int usersConfirm;
  int usersYesNoSelection;
  int usersInput;
  int usersMenuSelection;
  int resCode;
  int cmdDet;
}
class CatLog {
  int DEBUG;
}
class CatException {
}
class CatCmdMessage {
  int CREATOR;
  class CallSettings {
    int callMsg;
    int confirmMsg;
  }
  class BrowserSettings {
    int mode;
    int url;
  }
  int mCallSettings;
  int mToneSettings;
  int mBrowserSettings;
  int mInput;
  int mMenu;
  int mTextMsg;
  int mCmdDet;
}
class BerTlv {
  int BER_EVENT_DOWNLOAD_TAG;
  int BER_MENU_SELECTION_TAG;
  int BER_PROACTIVE_COMMAND_TAG;
  int BER_UNKNOWN_TAG;
  int mCompTlvs;
  int mTag;
}
class AppInterface {
  class CommandType {
    int SEND_DATA;
    int RECEIVE_DATA;
    int CLOSE_CHANNEL;
    int OPEN_CHANNEL;
    int PROVIDE_LOCAL_INFORMATION;
    int SET_UP_CALL;
    int SET_UP_MENU;
    int SET_UP_IDLE_MODE_TEXT;
    int SET_UP_EVENT_LIST;
    int SEND_DTMF;
    int SEND_SMS;
    int SEND_USSD;
    int SEND_SS;
    int SELECT_ITEM;
    int REFRESH;
    int PLAY_TONE;
    int LAUNCH_BROWSER;
    int GET_INPUT;
    int GET_INKEY;
    int DISPLAY_TEXT;
    int mValue;
  }
  int CAT_SESSION_END_ACTION;
  int CAT_CMD_ACTION;
}
