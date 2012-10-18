package com.android.vcard;
class VCardUtils {
  int sEscapeIndicatorsV40;
  int sEscapeIndicatorsV30;
  int sUnAcceptableAsciiInV21WordSet;
  int sMobilePhoneLabelSet;
  int sKnownImPropNameMap_ItoS;
  int sKnownPhoneTypeMap_StoI;
  int sPhoneTypesUnknownToContactsSet;
  int sKnownPhoneTypesMap_ItoS;
  class TextUtilsPort {
  }
  class PhoneNumberUtilsPort {
  }
  class QuotedPrintableCodecPort {
    int ESCAPE_CHAR;
  }
  class DecoderException {
  }
  int LOG_TAG;
}
class VCardSourceDetector {
  int mSpecifiedCharset;
  int mVersion;
  int mParseType;
  int PARSE_TYPE_WINDOWS_MOBILE_V65_JP;
  int PARSE_TYPE_DOCOMO_FOMA;
  int PARSE_TYPE_MOBILE_PHONE_JP;
  int PARSE_TYPE_APPLE;
  int PARSE_TYPE_UNKNOWN;
  int TYPE_FOMA_CHARSET_SIGN;
  int FOMA_SIGNS;
  int WINDOWS_MOBILE_PHONE_SIGNS;
  int JAPANESE_MOBILE_PHONE_SIGNS;
  int APPLE_SIGNS;
  int LOG_TAG;
}
class VCardProperty {
  int mByteValue;
  int mValueList;
  int mRawValue;
  int mParameterMap;
  int mGroupList;
  int mName;
  int LOG_TAG;
}
class VCardPhoneNumberTranslationCallback {
}
class VCardParser_V40 {
  int mVCardParserImpl;
  int sAcceptableEncoding;
  int sKnownPropertyNameSet;
}
class VCardParser_V30 {
  int mVCardParserImpl;
  int sAcceptableEncoding;
  int sKnownPropertyNameSet;
}
class VCardParser_V21 {
  int mVCardParserImpl;
  int sAvailableEncoding;
  int sKnownValueSet;
  int sKnownTypeSet;
  int sKnownPropertyNameSet;
}
class VCardParserImpl_V40 {
}
class VCardParserImpl_V30 {
  int mEmittedAgentWarning;
  int mPreviousLine;
  int LOG_TAG;
}
class VCardParserImpl_V21 {
  int STATE_PARAMS_IN_DQUOTE;
  int STATE_PARAMS;
  int STATE_GROUP_OR_PROPERTY_NAME;
  int mUnknownValueSet;
  int mUnknownTypeSet;
  int mReader;
  int mCurrentCharset;
  int mCurrentEncoding;
  int mCanceled;
  int mInterpreterList;
  int mIntermediateCharset;
  int DEFAULT_CHARSET;
  int DEFAULT_ENCODING;
  class CustomBufferedReader {
    int mNextLine;
    int mNextLineIsValid;
    int mTime;
  }
  int LOG_TAG;
}
class VCardParser {
}
class VCardInterpreter {
}
class VCardEntryHandler {
}
class VCardEntryCounter {
  int mCount;
}
class VCardEntryConstructor {
  int mEntryHandlers;
  int mAccount;
  int mVCardType;
  int mCurrentEntry;
  int mEntryStack;
  int LOG_TAG;
}
class VCardEntryCommitter {
  int mCreatedUris;
  int mOperationList;
  int mCounter;
  int mTimeToCommit;
  int mContentResolver;
  int LOG_TAG;
}
class VCardEntry {
  int sEmptyList;
  int mChildren;
  int mAccount;
  int mVCardType;
  class InsertOperationConstrutor {
    int mBackReferenceIndex;
    int mOperationList;
  }
  class ToStringIterator {
    int mFirstElement;
    int mBuilder;
  }
  class IsIgnorableIterator {
    int mEmpty;
  }
  class EntryElementIterator {
  }
  int mAnniversary;
  int mBirthday;
  int mAndroidCustomDataList;
  int mNoteList;
  int mNicknameList;
  int mSipList;
  int mWebsiteList;
  int mPhotoList;
  int mImList;
  int mOrganizationList;
  int mPostalList;
  int mEmailList;
  int mPhoneList;
  int mNameData;
  class AndroidCustomData {
    int mDataList;
    int mMimeType;
  }
  class SipData {
    int mIsPrimary;
    int mLabel;
    int mType;
    int mAddress;
  }
  class AnniversaryData {
    int mAnniversary;
  }
  class BirthdayData {
    int mBirthday;
  }
  class WebsiteData {
    int mWebsite;
  }
  class NoteData {
    int mNote;
  }
  class NicknameData {
    int mNickname;
  }
  class PhotoData {
    int mHashCode;
    int mBytes;
    int mIsPrimary;
    int mFormat;
  }
  class ImData {
    int mIsPrimary;
    int mType;
    int mCustomProtocol;
    int mProtocol;
    int mAddress;
  }
  class OrganizationData {
    int mIsPrimary;
    int mType;
    int mPhoneticName;
    int mTitle;
    int mDepartmentName;
    int mOrganizationName;
  }
  class PostalData {
    int mVCardType;
    int mIsPrimary;
    int mLabel;
    int mType;
    int mCountry;
    int mPostalCode;
    int mRegion;
    int mLocalty;
    int mStreet;
    int mExtendedAddress;
    int mPobox;
    int ADDR_MAX_DATA_SIZE;
  }
  class EmailData {
    int mIsPrimary;
    int mLabel;
    int mType;
    int mAddress;
  }
  class PhoneData {
    int mIsPrimary;
    int mLabel;
    int mType;
    int mNumber;
  }
  class NameData {
    int displayName;
    int mSortString;
    int mPhoneticMiddle;
    int mPhoneticGiven;
    int mPhoneticFamily;
    int mFormatted;
    int mSuffix;
    int mPrefix;
    int mMiddle;
    int mGiven;
    int mFamily;
  }
  class EntryElement {
  }
  class EntryLabel {
    int ANDROID_CUSTOM;
    int ANNIVERSARY;
    int BIRTHDAY;
    int NOTE;
    int NICKNAME;
    int SIP;
    int WEBSITE;
    int PHOTO;
    int IM;
    int ORGANIZATION;
    int POSTAL_ADDRESS;
    int EMAIL;
    int PHONE;
    int NAME;
  }
  int sImMap;
  int DEFAULT_ORGANIZATION_TYPE;
  int LOG_TAG;
}
class VCardConstants {
  int MAX_CHARACTER_NUMS_BASE64_V30;
  int MAX_CHARACTER_NUMS_QP;
  int MAX_DATA_COLUMN;
  int PARAM_TYPE_X_IRMC_N;
  class ImportOnly {
    int PROPERTY_X_GOOGLE_TALK_WITH_SPACE;
    int PROPERTY_X_NICKNAME;
  }
  int PARAM_EXTRA_TYPE_COMPANY;
  int PARAM_SORT_AS;
  int PARAM_LANGUAGE;
  int PARAM_ADR_TYPE_INTL;
  int PARAM_ADR_TYPE_DOM;
  int PARAM_ADR_TYPE_PARCEL;
  int PARAM_PHONE_EXTRA_TYPE_OTHER;
  int PARAM_PHONE_EXTRA_TYPE_COMPANY_MAIN;
  int PARAM_PHONE_EXTRA_TYPE_ASSISTANT;
  int PARAM_PHONE_EXTRA_TYPE_TTY_TDD;
  int PARAM_PHONE_EXTRA_TYPE_RADIO;
  int PARAM_PHONE_EXTRA_TYPE_CALLBACK;
  int PARAM_ENCODING_B;
  int PARAM_ENCODING_BASE64;
  int PARAM_ENCODING_QP;
  int PARAM_ENCODING_8BIT;
  int PARAM_ENCODING_7BIT;
  int PARAM_TYPE_VIDEO;
  int PARAM_TYPE_BBS;
  int PARAM_TYPE_MSG;
  int PARAM_TYPE_MODEM;
  int PARAM_TYPE_TLX;
  int PARAM_TYPE_PAGER;
  int PARAM_TYPE_ISDN;
  int PARAM_TYPE_CAR;
  int PARAM_TYPE_PREF;
  int PARAM_ENCODING;
  int PARAM_CHARSET;
  int PARAM_VALUE;
  int PARAM_TYPE_INTERNET;
  int PARAM_TYPE_VOICE;
  int PARAM_TYPE_CELL;
  int PARAM_TYPE_FAX;
  int PARAM_TYPE_WORK;
  int PARAM_TYPE_HOME;
  int PARAM_TYPE;
  int PROPERTY_X_DCM_HMN_MODE;
  int PROPERTY_X_NO;
  int PROPERTY_X_REDUCTION;
  int PROPERTY_X_CLASS;
  int PROPERTY_X_ANDROID_CUSTOM;
  int PROPERTY_X_SKYPE_PSTNNUMBER;
  int PROPERTY_X_NETMEETING;
  int PROPERTY_X_QQ;
  int PROPERTY_X_SKYPE_USERNAME;
  int PROPERTY_X_GOOGLE_TALK;
  int PROPERTY_X_JABBER;
  int PROPERTY_X_ICQ;
  int PROPERTY_X_YAHOO;
  int PROPERTY_X_MSN;
  int PROPERTY_X_AIM;
  int PROPERTY_X_PHONETIC_LAST_NAME;
  int PROPERTY_X_PHONETIC_MIDDLE_NAME;
  int PROPERTY_X_PHONETIC_FIRST_NAME;
  int PROPERTY_X_SIP;
  int PROPERTY_CALURI;
  int PROPERTY_CLIENTPIDMAP;
  int PROPERTY_CATEGORIES;
  int PROPERTY_RELATED;
  int PROPERTY_PRODID;
  int PROPERTY_FBURL;
  int PROPERTY_XML;
  int PROPERTY_GENDER;
  int PROPERTY_AGENT;
  int PROPERTY_REV;
  int PROPERTY_END;
  int PROPERTY_IMPP;
  int PROPERTY_SORT_STRING;
  int PROPERTY_NICKNAME;
  int PROPERTY_NAME;
  int PROPERTY_ANNIVERSARY;
  int PROPERTY_BDAY;
  int PROPERTY_URL;
  int PROPERTY_LOGO;
  int PROPERTY_PHOTO;
  int PROPERTY_ROLE;
  int PROPERTY_TITLE;
  int PROPERTY_TEL;
  int PROPERTY_SOUND;
  int PROPERTY_ORG;
  int PROPERTY_NOTE;
  int PROPERTY_EMAIL;
  int PROPERTY_ADR;
  int PROPERTY_FN;
  int PROPERTY_N;
  int PROPERTY_VERSION;
  int PROPERTY_BEGIN;
  int VERSION_V40;
  int VERSION_V30;
  int VERSION_V21;
  int LOG_TAG;
}
class VCardConfig {
  int sJapaneseMobileTypeSet;
  int sVCardTypeMap;
  int VCARD_TYPE_DEFAULT;
  int VCARD_TYPE_DOCOMO_STR;
  int VCARD_TYPE_DOCOMO;
  int VCARD_TYPE_V21_JAPANESE_MOBILE_STR;
  int VCARD_TYPE_V21_JAPANESE_MOBILE;
  int VCARD_TYPE_V30_JAPANESE_STR;
  int VCARD_TYPE_V30_JAPANESE;
  int VCARD_TYPE_V21_JAPANESE_STR;
  int VCARD_TYPE_V21_JAPANESE;
  int VCARD_TYPE_V30_EUROPE_STR;
  int VCARD_TYPE_V30_EUROPE;
  int VCARD_TYPE_V21_EUROPE_STR;
  int VCARD_TYPE_V21_EUROPE;
  int VCARD_TYPE_V40_GENERIC_STR;
  int VCARD_TYPE_V40_GENERIC;
  int VCARD_TYPE_V30_GENERIC_STR;
  int VCARD_TYPE_V30_GENERIC;
  int VCARD_TYPE_V21_GENERIC_STR;
  int VCARD_TYPE_V21_GENERIC;
  int VCARD_TYPE_UNKNOWN;
  int FLAG_REFRAIN_IMAGE_EXPORT;
  int FLAG_REFRAIN_PHONE_NUMBER_FORMATTING;
  int FLAG_APPEND_TYPE_PARAM;
  int FLAG_CONVERT_PHONETIC_NAME_STRINGS;
  int FLAG_REFRAIN_QP_TO_NAME_PROPERTIES;
  int FLAG_DOCOMO;
  int FLAG_USE_DEFACT_PROPERTY;
  int FLAG_USE_ANDROID_PROPERTY;
  int NAME_ORDER_MASK;
  int NAME_ORDER_JAPANESE;
  int NAME_ORDER_EUROPE;
  int NAME_ORDER_DEFAULT;
  int VERSION_MASK;
  int VERSION_40;
  int VERSION_30;
  int VERSION_21;
  int DEFAULT_EXPORT_CHARSET;
  int DEFAULT_IMPORT_CHARSET;
  int DEFAULT_INTERMEDIATE_CHARSET;
  int LOG_LEVEL;
  int LOG_LEVEL_VERBOSE;
  int LOG_LEVEL_SHOW_WARNING;
  int LOG_LEVEL_PERFORMANCE_MEASUREMENT;
  int LOG_LEVEL_NONE;
  int LOG_TAG;
}
class VCardComposer {
  int mPhoneTranslationCallback;
  int sContactsProjection;
  int mTerminateCalled;
  int mErrorReason;
  int mInitDone;
  int mCharset;
  int mContentUriForRawContactsEntity;
  int mIdColumn;
  int mCursorSuppliedFromOutside;
  int mCursor;
  int mFirstVCardEmittedInDoCoMoCase;
  int mIsDoCoMo;
  int mContentResolver;
  int mVCardType;
  int sImMap;
  int UTF_8;
  int SHIFT_JIS;
  int NO_ERROR;
  int FAILURE_REASON_UNSUPPORTED_URI;
  int FAILURE_REASON_NOT_INITIALIZED;
  int FAILURE_REASON_NO_ENTRY;
  int FAILURE_REASON_FAILED_TO_GET_DATABASE_INFO;
  int DEBUG;
  int LOG_TAG;
}
class VCardBuilder {
  class PostalStruct {
    int addressData;
    int appendCharset;
    int reallyUseQuotedPrintable;
  }
  int sPostalTypePriorityMap;
  int mEndAppended;
  int mBuilder;
  int mVCardCharsetParameter;
  int mCharset;
  int mShouldAppendCharsetParam;
  int mNeedsToConvertPhoneticString;
  int mRefrainsQPToNameProperties;
  int mAppendTypeParamName;
  int mUsesDefactProperty;
  int mUsesAndroidProperty;
  int mShouldUseQuotedPrintable;
  int mIsDoCoMo;
  int mOnlyOneNoteFieldIsAvailable;
  int mIsJapaneseMobilePhone;
  int mIsV30OrV40;
  int mVCardType;
  int SHIFT_JIS;
  int VCARD_PARAM_ENCODING_BASE64_AS_B;
  int VCARD_PARAM_ENCODING_BASE64_V21;
  int VCARD_PARAM_ENCODING_QP;
  int VCARD_PARAM_EQUAL;
  int VCARD_WS;
  int VCARD_ITEM_SEPARATOR;
  int VCARD_DATA_SEPARATOR;
  int VCARD_END_OF_LINE;
  int VCARD_PARAM_SEPARATOR;
  int VCARD_DATA_PUBLIC;
  int VCARD_DATA_VCARD;
  int DEFAULT_EMAIL_TYPE;
  int DEFAULT_POSTAL_TYPE;
  int DEFAULT_PHONE_TYPE;
  int sAllowedAndroidPropertySet;
  int LOG_TAG;
}
class JapaneseUtils {
  int sHalfWidthMap;
}
