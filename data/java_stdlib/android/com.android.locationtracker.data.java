package com.android.locationtracker.data;
class TrackerProvider {
  int mOpenHelper;
  class DatabaseHelper {
  }
  int LOG_TAG;
  int DB_VERSION;
  int TABLE_NAME;
  int DB_NAME;
  int CONTENT_URI;
}
class TrackerListHelper {
  class TrackerAdapter {
  }
  int SORT_ORDER;
  int mActivity;
}
class TrackerEntry {
  int mType;
  int mTag;
  int mTimestamp;
  class EntryType {
    int LOG_TYPE;
    int LOCATION_TYPE;
  }
  int LOCATION_DEBUG_KEYS;
  int NETWORK_LOCATION_TYPE_KEY;
  int NETWORK_LOCATION_SOURCE_KEY;
  int ATTRIBUTES_DATA_TYPE;
  int ATTRIBUTES;
  int BLOB_DATA;
  int REAL_DATA;
  int INT_DATA;
  int STRING_DATA;
  int DEBUG_INFO;
  int LOC_TIME;
  int DIST_NET_LOCATION;
  int BEARING;
  int SPEED;
  int ALTITUDE;
  int LONGITUDE;
  int LATITUDE;
  int ACCURACY;
  int ID_COL;
  int mLogMsg;
  int mDistFromNetLocation;
  int mLocation;
  int ENTRY_TYPE;
  int TAG;
  int TIMESTAMP;
}
class TrackerDataHelper {
  int NO_FORMATTER;
  int KML_FORMATTER;
  int CSV_FORMATTER;
  int mFormatter;
  int mContext;
}
class KMLFormatter {
  class LineBuilder {
    int mBuilder;
  }
}
class IFormatter {
}
class DateUtils {
}
class CSVFormatter {
  int DELIMITER;
}
