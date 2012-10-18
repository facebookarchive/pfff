package android.location;
class LocationTest {
}
class LocationProvider {
  int AVAILABLE;
  int TEMPORARILY_UNAVAILABLE;
  int OUT_OF_SERVICE;
  int mService;
  int mName;
  int BAD_CHARS_REGEX;
  int TAG;
}
class LocationManagerTest {
  int manager;
  int LOG_TAG;
}
class LocationManager {
  class GpsStatusListenerTransport {
    int mGpsHandler;
    int mNmeaBuffer;
    class Nmea {
      int mNmea;
      int mTimestamp;
    }
    int NMEA_RECEIVED;
    int mNmeaListener;
    int mListener;
  }
  class ListenerTransport {
    int mListenerHandler;
    int mListener;
    int TYPE_PROVIDER_DISABLED;
    int TYPE_PROVIDER_ENABLED;
    int TYPE_STATUS_CHANGED;
    int TYPE_LOCATION_CHANGED;
  }
  int mListeners;
  int EXTRA_GPS_ENABLED;
  int GPS_FIX_CHANGE_ACTION;
  int PROVIDERS_CHANGED_ACTION;
  int GPS_ENABLED_CHANGE_ACTION;
  int KEY_LOCATION_CHANGED;
  int KEY_PROVIDER_ENABLED;
  int KEY_STATUS_CHANGED;
  int KEY_PROXIMITY_ENTERING;
  int PASSIVE_PROVIDER;
  int GPS_PROVIDER;
  int NETWORK_PROVIDER;
  int mGpsStatus;
  int mNmeaListeners;
  int mGpsStatusListeners;
  int mService;
  int TAG;
}
class LocationListener {
}
class Location {
  int CREATOR;
  int mResults;
  int mInitialBearing;
  int mDistance;
  int mLon2;
  int mLat2;
  int mLon1;
  int mLat1;
  int mExtras;
  int mAccuracy;
  int mHasAccuracy;
  int mBearing;
  int mHasBearing;
  int mSpeed;
  int mHasSpeed;
  int mAltitude;
  int mHasAltitude;
  int mLongitude;
  int mLatitude;
  int mTime;
  int mProvider;
  int FORMAT_SECONDS;
  int FORMAT_MINUTES;
  int FORMAT_DEGREES;
}
class GpsStatus {
  class NmeaListener {
  }
  class Listener {
  }
  int GPS_EVENT_SATELLITE_STATUS;
  int GPS_EVENT_FIRST_FIX;
  int GPS_EVENT_STOPPED;
  int GPS_EVENT_STARTED;
  int mSatelliteList;
  class SatelliteIterator {
    int mIndex;
    int mSatellites;
  }
  int mSatellites;
  int mTimeToFirstFix;
  int NUM_SATELLITES;
}
class GpsSatellite {
  int mAzimuth;
  int mElevation;
  int mSnr;
  int mPrn;
  int mUsedInFix;
  int mHasAlmanac;
  int mHasEphemeris;
  int mValid;
}
class GeocoderTest {
}
class GeocoderParams {
  int CREATOR;
  int mPackageName;
  int mLocale;
}
class Geocoder {
  int mService;
  int mParams;
  int TAG;
}
class Criteria {
  int CREATOR;
  int mCostAllowed;
  int mSpeedRequired;
  int mBearingRequired;
  int mAltitudeRequired;
  int mPowerRequirement;
  int mBearingAccuracy;
  int mSpeedAccuracy;
  int mVerticalAccuracy;
  int mHorizontalAccuracy;
  int ACCURACY_HIGH;
  int ACCURACY_MEDIUM;
  int ACCURACY_LOW;
  int ACCURACY_COARSE;
  int ACCURACY_FINE;
  int POWER_HIGH;
  int POWER_MEDIUM;
  int POWER_LOW;
  int NO_REQUIREMENT;
}
class CountryTester {
}
class CountryListener {
}
class CountryDetector {
  int mListeners;
  int mService;
  int TAG;
  class ListenerTransport {
    int mHandler;
    int mListener;
  }
}
class Country {
  int CREATOR;
  int mTimestamp;
  int mHashCode;
  int mSource;
  int mCountryIso;
  int COUNTRY_SOURCE_LOCALE;
  int COUNTRY_SOURCE_SIM;
  int COUNTRY_SOURCE_LOCATION;
  int COUNTRY_SOURCE_NETWORK;
}
class Address {
  int CREATOR;
  int mExtras;
  int mUrl;
  int mPhone;
  int mHasLongitude;
  int mHasLatitude;
  int mLongitude;
  int mLatitude;
  int mCountryName;
  int mCountryCode;
  int mPostalCode;
  int mPremises;
  int mSubThoroughfare;
  int mThoroughfare;
  int mSubLocality;
  int mLocality;
  int mSubAdminArea;
  int mAdminArea;
  int mMaxAddressLineIndex;
  int mAddressLines;
  int mFeatureName;
  int mLocale;
}
