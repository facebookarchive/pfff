package com.android.nfc_extras;
class NfcExecutionEnvironment {
  int EXTRA_MIFARE_BLOCK;
  int ACTION_MIFARE_ACCESS_DETECTED;
  int ACTION_EMV_CARD_REMOVAL;
  int EXTRA_APDU_BYTES;
  int ACTION_APDU_RECEIVED;
  int EXTRA_AID;
  int ACTION_AID_SELECTED;
  int mToken;
  int mExtras;
}
class NfcAdapterExtras {
  class CardEmulationRoute {
    int nfcEe;
    int route;
    int ROUTE_ON_WHEN_SCREEN_ON;
    int ROUTE_OFF;
  }
  int mPackageName;
  int mAdapter;
  int mRouteOnWhenScreenOn;
  int mEmbeddedEe;
  int sNfcExtras;
  int ROUTE_OFF;
  int sService;
  int ACTION_RF_FIELD_OFF_DETECTED;
  int ACTION_RF_FIELD_ON_DETECTED;
  int TAG;
}
