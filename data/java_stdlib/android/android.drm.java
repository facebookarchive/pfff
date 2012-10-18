package android.drm;
class ProcessedData {
  int mSubscriptionId;
  int mAccountId;
  int mData;
}
class DrmUtils {
  class ExtendedMetadataParser {
    int mMap;
  }
}
class DrmSupportInfo {
  int mDescription;
  int mMimeTypeList;
  int mFileSuffixList;
}
class DrmStore {
  class RightsStatus {
    int RIGHTS_NOT_ACQUIRED;
    int RIGHTS_EXPIRED;
    int RIGHTS_INVALID;
    int RIGHTS_VALID;
  }
  class Action {
    int DISPLAY;
    int EXECUTE;
    int PREVIEW;
    int OUTPUT;
    int TRANSFER;
    int RINGTONE;
    int PLAY;
    int DEFAULT;
  }
  class Playback {
    int RESUME;
    int PAUSE;
    int STOP;
    int START;
  }
  class DrmObjectType {
    int TRIGGER_OBJECT;
    int RIGHTS_OBJECT;
    int CONTENT;
    int UNKNOWN;
  }
  class ConstraintsColumns {
    int EXTENDED_METADATA;
    int LICENSE_AVAILABLE_TIME;
    int LICENSE_EXPIRY_TIME;
    int LICENSE_START_TIME;
    int REMAINING_REPEAT_COUNT;
    int MAX_REPEAT_COUNT;
  }
}
class DrmRights {
  int mSubscriptionId;
  int mAccountId;
  int mMimeType;
  int mData;
}
class DrmManagerClient {
  class InfoHandler {
    int INFO_EVENT_TYPE;
  }
  class EventHandler {
  }
  int mOnErrorListener;
  int mOnEventListener;
  int mOnInfoListener;
  int mEventHandler;
  int mInfoHandler;
  int mContext;
  int mReleased;
  int mNativeContext;
  int mUniqueId;
  int ACTION_PROCESS_DRM_INFO;
  int ACTION_REMOVE_ALL_RIGHTS;
  class OnErrorListener {
  }
  class OnEventListener {
  }
  class OnInfoListener {
  }
  int TAG;
  int mEventThread;
  int mInfoThread;
  int ERROR_UNKNOWN;
  int ERROR_NONE;
}
class DrmInfoStatus {
  int data;
  int mimeType;
  int infoType;
  int statusCode;
  int STATUS_ERROR;
  int STATUS_OK;
}
class DrmInfoRequest {
  int mRequestInformation;
  int mMimeType;
  int mInfoType;
  int SUBSCRIPTION_ID;
  int ACCOUNT_ID;
  int TYPE_RIGHTS_ACQUISITION_PROGRESS_INFO;
  int TYPE_RIGHTS_ACQUISITION_INFO;
  int TYPE_UNREGISTRATION_INFO;
  int TYPE_REGISTRATION_INFO;
}
class DrmInfoEvent {
  int TYPE_RIGHTS_REMOVED;
  int TYPE_ACCOUNT_ALREADY_REGISTERED;
  int TYPE_WAIT_FOR_RIGHTS;
  int TYPE_RIGHTS_INSTALLED;
  int TYPE_REMOVE_RIGHTS;
  int TYPE_ALREADY_REGISTERED_BY_ANOTHER_ACCOUNT;
}
class DrmInfo {
  int mAttributes;
  int mInfoType;
  int mMimeType;
  int mData;
}
class DrmEvent {
  int mAttributes;
  int mMessage;
  int mType;
  int mUniqueId;
  int DRM_INFO_OBJECT;
  int DRM_INFO_STATUS_OBJECT;
  int TYPE_DRM_INFO_PROCESSED;
  int TYPE_ALL_RIGHTS_REMOVED;
}
class DrmErrorEvent {
  int TYPE_ACQUIRE_DRM_INFO_FAILED;
  int TYPE_REMOVE_ALL_RIGHTS_FAILED;
  int TYPE_PROCESS_DRM_INFO_FAILED;
  int TYPE_NO_INTERNET_CONNECTION;
  int TYPE_OUT_OF_MEMORY;
  int TYPE_NOT_SUPPORTED;
  int TYPE_RIGHTS_RENEWAL_NOT_ALLOWED;
  int TYPE_RIGHTS_NOT_INSTALLED;
}
class DrmConvertedStatus {
  int offset;
  int convertedData;
  int statusCode;
  int STATUS_ERROR;
  int STATUS_INPUTDATA_ERROR;
  int STATUS_OK;
}
