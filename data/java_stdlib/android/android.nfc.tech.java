package android.nfc.tech;
class TagTechnology {
  int MIFARE_ULTRALIGHT;
  int MIFARE_CLASSIC;
  int NDEF_FORMATABLE;
  int NDEF;
  int NFC_V;
  int NFC_F;
  int ISO_DEP;
  int NFC_B;
  int NFC_A;
}
class NfcV {
  int mDsfId;
  int mRespFlags;
  int EXTRA_DSFID;
  int EXTRA_RESP_FLAGS;
}
class NfcF {
  int mManufacturer;
  int mSystemCode;
  int EXTRA_PMM;
  int EXTRA_SC;
  int TAG;
}
class NfcB {
  int mProtInfo;
  int mAppData;
  int EXTRA_PROTINFO;
  int EXTRA_APPDATA;
}
class NfcA {
  int mAtqa;
  int mSak;
  int EXTRA_ATQA;
  int EXTRA_SAK;
  int TAG;
}
class NdefFormatable {
  int TAG;
}
class Ndef {
  int mNdefType;
  int mNdefMsg;
  int mCardState;
  int mMaxNdefSize;
  int ICODE_SLI;
  int MIFARE_CLASSIC;
  int NFC_FORUM_TYPE_4;
  int NFC_FORUM_TYPE_3;
  int NFC_FORUM_TYPE_2;
  int NFC_FORUM_TYPE_1;
  int UNKNOWN;
  int TYPE_ICODE_SLI;
  int TYPE_MIFARE_CLASSIC;
  int TYPE_4;
  int TYPE_3;
  int TYPE_2;
  int TYPE_1;
  int TYPE_OTHER;
  int EXTRA_NDEF_TYPE;
  int EXTRA_NDEF_CARDSTATE;
  int EXTRA_NDEF_MAXLENGTH;
  int EXTRA_NDEF_MSG;
  int NDEF_MODE_UNKNOWN;
  int NDEF_MODE_READ_WRITE;
  int NDEF_MODE_READ_ONLY;
  int TAG;
}
class MifareUltralight {
  int mType;
  int EXTRA_IS_UL_C;
  int MAX_PAGE_COUNT;
  int NXP_MANUFACTURER_ID;
  int PAGE_SIZE;
  int TYPE_ULTRALIGHT_C;
  int TYPE_ULTRALIGHT;
  int TYPE_UNKNOWN;
  int TAG;
}
class MifareClassic {
  int mSize;
  int mType;
  int mIsEmulated;
  int MAX_SECTOR_COUNT;
  int MAX_BLOCK_COUNT;
  int BLOCK_SIZE;
  int SIZE_MINI;
  int SIZE_4K;
  int SIZE_2K;
  int SIZE_1K;
  int TYPE_PRO;
  int TYPE_PLUS;
  int TYPE_CLASSIC;
  int TYPE_UNKNOWN;
  int KEY_NFC_FORUM;
  int KEY_MIFARE_APPLICATION_DIRECTORY;
  int KEY_DEFAULT;
  int TAG;
}
class IsoDep {
  int mHistBytes;
  int mHiLayerResponse;
  int EXTRA_HIST_BYTES;
  int EXTRA_HI_LAYER_RESP;
  int TAG;
}
class BasicTagTechnology {
  int mSelectedTechnology;
  int mIsConnected;
  int mTag;
  int TAG;
}
