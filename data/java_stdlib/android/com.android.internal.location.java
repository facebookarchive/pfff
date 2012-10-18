package com.android.internal.location;
class GpsNetInitiatedHandler {
  int mNiNotification;
  class GpsNiResponse {
    int extras;
    int userResponse;
  }
  class GpsNiNotification {
    int extras;
    int textEncoding;
    int requestorIdEncoding;
    int text;
    int requestorId;
    int defaultResponse;
    int timeout;
    int privacyOverride;
    int needVerify;
    int needNotify;
    int niType;
    int notificationId;
  }
  int mIsHexInput;
  int mPopupImmediately;
  int visible;
  int mPlaySounds;
  int mLocationManager;
  int mContext;
  int GPS_ENC_UNKNOWN;
  int GPS_ENC_SUPL_UCS2;
  int GPS_ENC_SUPL_UTF8;
  int GPS_ENC_SUPL_GSM_DEFAULT;
  int GPS_ENC_NONE;
  int GPS_NI_PRIVACY_OVERRIDE;
  int GPS_NI_NEED_VERIFY;
  int GPS_NI_NEED_NOTIFY;
  int GPS_NI_RESPONSE_NORESP;
  int GPS_NI_RESPONSE_DENY;
  int GPS_NI_RESPONSE_ACCEPT;
  int GPS_NI_TYPE_UMTS_CTRL_PLANE;
  int GPS_NI_TYPE_UMTS_SUPL;
  int GPS_NI_TYPE_VOICE;
  int NI_EXTRA_CMD_RESPONSE;
  int NI_EXTRA_CMD_NOTIF_ID;
  int NI_RESPONSE_EXTRA_CMD;
  int NI_INTENT_KEY_DEFAULT_RESPONSE;
  int NI_INTENT_KEY_TIMEOUT;
  int NI_INTENT_KEY_MESSAGE;
  int NI_INTENT_KEY_TITLE;
  int NI_INTENT_KEY_NOTIF_ID;
  int ACTION_NI_VERIFY;
  int VERBOSE;
  int DEBUG;
  int TAG;
}
class DummyLocationProvider {
  int mAccuracy;
  int mPowerRequirement;
  int mSupportsBearing;
  int mSupportsSpeed;
  int mSupportsAltitude;
  int mHasMonetaryCost;
  int mRequiresCell;
  int mRequiresSatellite;
  int mRequiresNetwork;
  int mName;
  int TAG;
}
