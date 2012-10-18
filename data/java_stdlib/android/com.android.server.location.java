package com.android.server.location;
class PassiveProvider {
  int mTracking;
  int mLocationManager;
  int TAG;
}
class MockProvider {
  int TAG;
  int mEnabled;
  int mHasStatus;
  int mHasLocation;
  int mExtras;
  int mStatusUpdateTime;
  int mStatus;
  int mLocation;
  int mAccuracy;
  int mPowerRequirement;
  int mSupportsBearing;
  int mSupportsSpeed;
  int mSupportsAltitude;
  int mHasMonetaryCost;
  int mRequiresCell;
  int mRequiresSatellite;
  int mRequiresNetwork;
  int mLocationManager;
  int mName;
}
class LocationProviderProxy {
  class Connection {
    int mCachedAttributes;
    int mProvider;
  }
  int mNetworkInfo;
  int mNetworkState;
  int mMinTimeSource;
  int mMinTime;
  int mEnabled;
  int mLocationTracking;
  int mServiceConnection;
  int mMutex;
  int mHandler;
  int mIntent;
  int mName;
  int mContext;
  int SERVICE_ACTION;
  int TAG;
}
class LocationProviderInterface {
}
class LocationBasedCountryDetectorTest {
  class CountryListenerImpl {
    int mCountryCode;
    int mNotified;
  }
  class TestCountryDetector {
    int mAcceptableProviders;
    int mListeners;
    int mQueryLocationTimeout;
    int mCountry;
    int mLocation;
    int notifyCountry;
    int countryFoundLocker;
    int TOTAL_PROVIDERS;
  }
  int sEnabledProviders;
}
class LocationBasedCountryDetector {
  int mEnabledProviders;
  int mLocationManager;
  int mLocationListeners;
  int mQueryThread;
  int mTimer;
  int QUERY_LOCATION_TIMEOUT;
  int TAG;
}
class GpsXtraDownloader {
  int mNextServerIndex;
  int mXtraServers;
  int mContext;
  int DEBUG;
  int TAG;
}
class GpsLocationProvider {
  int mNmeaBuffer;
  int mSvCount;
  int mSvMasks;
  int mSvAzimuths;
  int mSvElevations;
  int mSnrs;
  int mSvs;
  int USED_FOR_FIX_MASK;
  int ALMANAC_MASK;
  int EPHEMERIS_MASK;
  int MAX_SVS;
  class GpsLocationProviderThread {
  }
  class ProviderHandler {
  }
  int mNetInitiatedListener;
  class Listener {
    int mSensors;
    int mListener;
  }
  int mBroadcastReciever;
  int mGpsStatusProvider;
  int RETRY_INTERVAL;
  int NTP_INTERVAL;
  int mClientUids;
  int mBatteryStats;
  int mTimeoutIntent;
  int mWakeupIntent;
  int mAlarmManager;
  int ALARM_TIMEOUT;
  int ALARM_WAKEUP;
  int mPendingListenerMessages;
  int mPendingMessageBits;
  int mWakeLock;
  int WAKELOCK_KEY;
  int mNIHandler;
  int mConnMgr;
  int mAGpsDataConnectionIpAddr;
  int mAGpsDataConnectionState;
  int mAGpsApn;
  int mInitializedLatch;
  int mHandler;
  int mThread;
  int mListeners;
  int mLocationExtras;
  int mLocation;
  int mLocationManager;
  int mNtpTime;
  int mContext;
  int mC2KServerPort;
  int mC2KServerHost;
  int mSuplServerPort;
  int mSuplServerHost;
  int mProperties;
  int mPositionMode;
  int mLastFixTime;
  int mTTFF;
  int mFixRequestTime;
  int mSupportsXtra;
  int mEngineCapabilities;
  int mSingleShot;
  int mStarted;
  int mFixInterval;
  int mEngineOn;
  int mNavigating;
  int mPeriodicTimeInjection;
  int mDownloadXtraDataPending;
  int mInjectNtpTimePending;
  int mNetworkAvailable;
  int mEnabled;
  int GPS_POLLING_THRESHOLD_INTERVAL;
  int NO_FIX_TIMEOUT;
  int RECENT_FIX_TIMEOUT;
  int mStatusUpdateTime;
  int mStatus;
  int mLocationFlags;
  int PROPERTIES_FILE;
  int AGPS_SETID_TYPE_MSISDN;
  int AGPS_SETID_TYPE_IMSI;
  int AGPS_SETID_TYPE_NONE;
  int AGPS_REG_LOCATION_TYPE_MAC;
  int AGPS_REF_LOCATION_TYPE_UMTS_CELLID;
  int AGPS_REF_LOCATION_TYPE_GSM_CELLID;
  int AGPS_RIL_REQUEST_REFLOC_MAC;
  int AGPS_RIL_REQUEST_REFLOC_CELLID;
  int AGPS_RIL_REQUEST_SETID_MSISDN;
  int AGPS_RIL_REQUEST_SETID_IMSI;
  int REQUEST_SINGLE_SHOT;
  int REMOVE_LISTENER;
  int ADD_LISTENER;
  int UPDATE_LOCATION;
  int DOWNLOAD_XTRA_DATA;
  int INJECT_NTP_TIME;
  int UPDATE_NETWORK_STATE;
  int ENABLE_TRACKING;
  int ENABLE;
  int CHECK_LOCATION;
  int AGPS_DATA_CONNECTION_OPEN;
  int AGPS_DATA_CONNECTION_OPENING;
  int AGPS_DATA_CONNECTION_CLOSED;
  int AGPS_TYPE_C2K;
  int AGPS_TYPE_SUPL;
  int GPS_CAPABILITY_ON_DEMAND_TIME;
  int GPS_CAPABILITY_SINGLE_SHOT;
  int GPS_CAPABILITY_MSA;
  int GPS_CAPABILITY_MSB;
  int GPS_CAPABILITY_SCHEDULING;
  int GPS_DELETE_ALL;
  int GPS_DELETE_CELLDB_INFO;
  int GPS_DELETE_RTI;
  int GPS_DELETE_SADATA;
  int GPS_DELETE_SVSTEER;
  int GPS_DELETE_SVDIR;
  int GPS_DELETE_HEALTH;
  int GPS_DELETE_UTC;
  int GPS_DELETE_IONO;
  int GPS_DELETE_TIME;
  int GPS_DELETE_POSITION;
  int GPS_DELETE_ALMANAC;
  int GPS_DELETE_EPHEMERIS;
  int LOCATION_HAS_ACCURACY;
  int LOCATION_HAS_BEARING;
  int LOCATION_HAS_SPEED;
  int LOCATION_HAS_ALTITUDE;
  int LOCATION_HAS_LAT_LONG;
  int LOCATION_INVALID;
  int GPS_AGPS_DATA_CONN_FAILED;
  int GPS_AGPS_DATA_CONN_DONE;
  int GPS_AGPS_DATA_CONNECTED;
  int GPS_RELEASE_AGPS_DATA_CONN;
  int GPS_REQUEST_AGPS_DATA_CONN;
  int GPS_STATUS_ENGINE_OFF;
  int GPS_STATUS_ENGINE_ON;
  int GPS_STATUS_SESSION_END;
  int GPS_STATUS_SESSION_BEGIN;
  int GPS_STATUS_NONE;
  int GPS_POSITION_RECURRENCE_SINGLE;
  int GPS_POSITION_RECURRENCE_PERIODIC;
  int GPS_POSITION_MODE_MS_ASSISTED;
  int GPS_POSITION_MODE_MS_BASED;
  int GPS_POSITION_MODE_STANDALONE;
  int VERBOSE;
  int DEBUG;
  int TAG;
}
class GeocoderProxy {
  class Connection {
    int mProvider;
  }
  int mServiceConnection;
  int mMutex;
  int mIntent;
  int mContext;
  int SERVICE_ACTION;
  int TAG;
}
class CountryDetectorBase {
  int mDetectedCountry;
  int mListener;
  int mContext;
  int mHandler;
}
class ComprehensiveCountryDetectorTest {
  class CountryListenerImpl {
    int mCountry;
    int mNotified;
  }
  class TestCountryDetector {
    int mNotifiedCountry;
    int listenerAdded;
    int mNotified;
    int mLocationBasedDetectorStopped;
    int mLocationBasedDetectorStarted;
    int COUNTRY_ISO;
  }
}
class ComprehensiveCountryDetector {
  int mLocationBasedCountryDetectionListener;
  int mTotalCountServiceStateChanges;
  int mCountServiceStateChanges;
  int mTotalTime;
  int mStopTime;
  int mStartTime;
  int mObject;
  int mLastCountryAddedToLogs;
  int mDebugLogs;
  int mPhoneStateListener;
  int mStopped;
  int mCountryFromLocation;
  int mTelephonyManager;
  int mCountry;
  int mLocationRefreshTimer;
  int mLocationBasedCountryDetector;
  int LOCATION_REFRESH_INTERVAL;
  int MAX_LENGTH_DEBUG_LOGS;
  int DEBUG;
  int TAG;
}
