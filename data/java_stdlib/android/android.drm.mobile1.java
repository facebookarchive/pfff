package android.drm.mobile1;
class DrmRightsManager {
  int singleton;
  int JNI_DRM_FAILURE;
  int JNI_DRM_SUCCESS;
  int DRM_MIMETYPE_MESSAGE;
  int DRM_MIMETYPE_RIGHTS_WBXML;
  int DRM_MIMETYPE_RIGHTS_XML;
  int DRM_MIMETYPE_RIGHTS_WBXML_STRING;
  int DRM_MIMETYPE_RIGHTS_XML_STRING;
}
class DrmRights {
  int roId;
  int JNI_DRM_FAILURE;
  int JNI_DRM_SUCCESS;
  int DRM_PERMISSION_PRINT;
  int DRM_PERMISSION_EXECUTE;
  int DRM_PERMISSION_DISPLAY;
  int DRM_PERMISSION_PLAY;
}
class DrmRawContent {
  class DrmInputStream {
    int b;
    int offset;
    int isClosed;
  }
  int rawType;
  int mediaType;
  int rightsIssuer;
  int id;
  int inDataLen;
  int inData;
  int JNI_DRM_UNKNOWN_DATA_LEN;
  int JNI_DRM_EOF;
  int JNI_DRM_FAILURE;
  int JNI_DRM_SUCCESS;
  int DRM_MIMETYPE_CONTENT;
  int DRM_MIMETYPE_MESSAGE;
  int DRM_UNKNOWN_DATA_LEN;
  int DRM_SEPARATE_DELIVERY_DM;
  int DRM_SEPARATE_DELIVERY;
  int DRM_COMBINED_DELIVERY;
  int DRM_FORWARD_LOCK;
  int DRM_MIMETYPE_CONTENT_STRING;
  int DRM_MIMETYPE_MESSAGE_STRING;
}
class DrmException {
}
class DrmConstraintInfo {
  int interval;
  int endDate;
  int startDate;
  int count;
}
