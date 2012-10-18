package android.hardware;
class SystemSensorManager {
  class ListenerDelegate {
    int mSensorAccuracies;
    int mFirstEvent;
    int mSensors;
    int mHandler;
    int mSensorList;
    int mSensorEventListener;
  }
  class SensorThread {
    class SensorThreadRunnable {
    }
    int mSensorsReady;
    int mThread;
  }
  int mMainLooper;
  int sPool;
  int sListeners;
  int sHandleToSensor;
  int sQueue;
  int sSensorThread;
  int sFullSensorsList;
  int sSensorModuleInitialized;
  int SENSOR_DISABLE;
}
class SerialPort {
  int mFileDescriptor;
  int mName;
  int mNativeContext;
  int TAG;
}
class SerialManager {
  int mService;
  int mContext;
  int TAG;
}
class SensorManager {
  class SensorEventPool {
    int mNumItemsInPool;
    int mPool;
    int mPoolSize;
  }
  int AXIS_MINUS_Z;
  int AXIS_MINUS_Y;
  int AXIS_MINUS_X;
  int AXIS_Z;
  int AXIS_Y;
  int AXIS_X;
  int SENSOR_STATUS_ACCURACY_HIGH;
  int SENSOR_STATUS_ACCURACY_MEDIUM;
  int SENSOR_STATUS_ACCURACY_LOW;
  int SENSOR_STATUS_UNRELIABLE;
  int SENSOR_DELAY_NORMAL;
  int SENSOR_DELAY_UI;
  int SENSOR_DELAY_GAME;
  int SENSOR_DELAY_FASTEST;
  int LIGHT_NO_MOON;
  int LIGHT_FULLMOON;
  int LIGHT_CLOUDY;
  int LIGHT_SUNRISE;
  int LIGHT_OVERCAST;
  int LIGHT_SHADE;
  int LIGHT_SUNLIGHT;
  int LIGHT_SUNLIGHT_MAX;
  int PRESSURE_STANDARD_ATMOSPHERE;
  int MAGNETIC_FIELD_EARTH_MIN;
  int MAGNETIC_FIELD_EARTH_MAX;
  int GRAVITY_THE_ISLAND;
  int GRAVITY_DEATH_STAR_I;
  int GRAVITY_PLUTO;
  int GRAVITY_NEPTUNE;
  int GRAVITY_URANUS;
  int GRAVITY_SATURN;
  int GRAVITY_JUPITER;
  int GRAVITY_MARS;
  int GRAVITY_MOON;
  int GRAVITY_EARTH;
  int GRAVITY_VENUS;
  int GRAVITY_MERCURY;
  int GRAVITY_SUN;
  int STANDARD_GRAVITY;
  int RAW_DATA_Z;
  int RAW_DATA_Y;
  int RAW_DATA_X;
  int RAW_DATA_INDEX;
  int DATA_Z;
  int DATA_Y;
  int DATA_X;
  int SENSOR_MAX;
  int SENSOR_MIN;
  int SENSOR_ALL;
  int SENSOR_ORIENTATION_RAW;
  int SENSOR_TRICORDER;
  int SENSOR_PROXIMITY;
  int SENSOR_LIGHT;
  int SENSOR_MAGNETIC_FIELD;
  int SENSOR_TEMPERATURE;
  int SENSOR_ACCELEROMETER;
  int SENSOR_ORIENTATION;
  int mLegacySensorManager;
  int mSensorListByType;
  int mTempMatrix;
  int TAG;
}
class SensorListener {
}
class SensorEventListener {
}
class SensorEvent {
  int timestamp;
  int accuracy;
  int sensor;
  int values;
}
class Sensor {
  int mMinDelay;
  int mPower;
  int mResolution;
  int mMaxRange;
  int mType;
  int mHandle;
  int mVersion;
  int mVendor;
  int mName;
  int TYPE_ALL;
  int TYPE_AMBIENT_TEMPERATURE;
  int TYPE_RELATIVE_HUMIDITY;
  int TYPE_ROTATION_VECTOR;
  int TYPE_LINEAR_ACCELERATION;
  int TYPE_GRAVITY;
  int TYPE_PROXIMITY;
  int TYPE_TEMPERATURE;
  int TYPE_PRESSURE;
  int TYPE_LIGHT;
  int TYPE_GYROSCOPE;
  int TYPE_ORIENTATION;
  int TYPE_MAGNETIC_FIELD;
  int TYPE_ACCELEROMETER;
}
class LegacySensorManager {
  class LmsFilter {
    int mIndex;
    int mT;
    int mV;
    int PREDICTION_TIME;
    int PREDICTION_RATIO;
    int COUNT;
    int SENSORS_RATE_MS;
  }
  class LegacyListener {
    int mYawfilter;
    int mSensors;
    int mTarget;
    int mValues;
  }
  int mLegacyListenersMap;
  int mSensorManager;
  int sRotation;
  int sWindowManager;
  int sInitialized;
}
class GeomagneticField {
  class LegendreTable {
    int mPDeriv;
    int mP;
  }
  int SCHMIDT_QUASI_NORM_FACTORS;
  int BASE_TIME;
  int DELTA_H;
  int DELTA_G;
  int H_COEFF;
  int G_COEFF;
  int EARTH_REFERENCE_RADIUS_KM;
  int EARTH_SEMI_MINOR_AXIS_KM;
  int EARTH_SEMI_MAJOR_AXIS_KM;
  int mGcRadiusKm;
  int mGcLongitudeRad;
  int mGcLatitudeRad;
  int mZ;
  int mY;
  int mX;
}
class Camera {
  class Parameters {
    int mMap;
    int PIXEL_FORMAT_BAYER_RGGB;
    int PIXEL_FORMAT_JPEG;
    int PIXEL_FORMAT_RGB565;
    int PIXEL_FORMAT_YUV420P;
    int PIXEL_FORMAT_YUV422I;
    int PIXEL_FORMAT_YUV420SP;
    int PIXEL_FORMAT_YUV422SP;
    int PREVIEW_FPS_MAX_INDEX;
    int PREVIEW_FPS_MIN_INDEX;
    int FOCUS_DISTANCE_FAR_INDEX;
    int FOCUS_DISTANCE_OPTIMAL_INDEX;
    int FOCUS_DISTANCE_NEAR_INDEX;
    int FOCUS_MODE_CONTINUOUS_PICTURE;
    int FOCUS_MODE_CONTINUOUS_VIDEO;
    int FOCUS_MODE_EDOF;
    int FOCUS_MODE_FIXED;
    int FOCUS_MODE_MACRO;
    int FOCUS_MODE_INFINITY;
    int FOCUS_MODE_AUTO;
    int SCENE_MODE_BARCODE;
    int SCENE_MODE_CANDLELIGHT;
    int SCENE_MODE_PARTY;
    int SCENE_MODE_SPORTS;
    int SCENE_MODE_FIREWORKS;
    int SCENE_MODE_STEADYPHOTO;
    int SCENE_MODE_SUNSET;
    int SCENE_MODE_SNOW;
    int SCENE_MODE_BEACH;
    int SCENE_MODE_THEATRE;
    int SCENE_MODE_NIGHT_PORTRAIT;
    int SCENE_MODE_NIGHT;
    int SCENE_MODE_LANDSCAPE;
    int SCENE_MODE_PORTRAIT;
    int SCENE_MODE_ACTION;
    int SCENE_MODE_AUTO;
    int FLASH_MODE_TORCH;
    int FLASH_MODE_RED_EYE;
    int FLASH_MODE_ON;
    int FLASH_MODE_AUTO;
    int FLASH_MODE_OFF;
    int ANTIBANDING_OFF;
    int ANTIBANDING_60HZ;
    int ANTIBANDING_50HZ;
    int ANTIBANDING_AUTO;
    int EFFECT_AQUA;
    int EFFECT_BLACKBOARD;
    int EFFECT_WHITEBOARD;
    int EFFECT_POSTERIZE;
    int EFFECT_SEPIA;
    int EFFECT_SOLARIZE;
    int EFFECT_NEGATIVE;
    int EFFECT_MONO;
    int EFFECT_NONE;
    int WHITE_BALANCE_SHADE;
    int WHITE_BALANCE_TWILIGHT;
    int WHITE_BALANCE_CLOUDY_DAYLIGHT;
    int WHITE_BALANCE_DAYLIGHT;
    int WHITE_BALANCE_WARM_FLUORESCENT;
    int WHITE_BALANCE_FLUORESCENT;
    int WHITE_BALANCE_INCANDESCENT;
    int WHITE_BALANCE_AUTO;
    int FALSE;
    int TRUE;
    int SUPPORTED_VALUES_SUFFIX;
    int KEY_VIDEO_STABILIZATION_SUPPORTED;
    int KEY_VIDEO_STABILIZATION;
    int KEY_VIDEO_SNAPSHOT_SUPPORTED;
    int KEY_RECORDING_HINT;
    int KEY_MAX_NUM_DETECTED_FACES_SW;
    int KEY_MAX_NUM_DETECTED_FACES_HW;
    int KEY_PREFERRED_PREVIEW_SIZE_FOR_VIDEO;
    int KEY_VIDEO_SIZE;
    int KEY_FOCUS_DISTANCES;
    int KEY_SMOOTH_ZOOM_SUPPORTED;
    int KEY_ZOOM_SUPPORTED;
    int KEY_ZOOM_RATIOS;
    int KEY_MAX_ZOOM;
    int KEY_ZOOM;
    int KEY_MAX_NUM_METERING_AREAS;
    int KEY_METERING_AREAS;
    int KEY_AUTO_WHITEBALANCE_LOCK_SUPPORTED;
    int KEY_AUTO_WHITEBALANCE_LOCK;
    int KEY_AUTO_EXPOSURE_LOCK_SUPPORTED;
    int KEY_AUTO_EXPOSURE_LOCK;
    int KEY_EXPOSURE_COMPENSATION_STEP;
    int KEY_MIN_EXPOSURE_COMPENSATION;
    int KEY_MAX_EXPOSURE_COMPENSATION;
    int KEY_EXPOSURE_COMPENSATION;
    int KEY_VERTICAL_VIEW_ANGLE;
    int KEY_HORIZONTAL_VIEW_ANGLE;
    int KEY_FOCAL_LENGTH;
    int KEY_MAX_NUM_FOCUS_AREAS;
    int KEY_FOCUS_AREAS;
    int KEY_FOCUS_MODE;
    int KEY_FLASH_MODE;
    int KEY_SCENE_MODE;
    int KEY_ANTIBANDING;
    int KEY_EFFECT;
    int KEY_WHITE_BALANCE;
    int KEY_GPS_PROCESSING_METHOD;
    int KEY_GPS_TIMESTAMP;
    int KEY_GPS_ALTITUDE;
    int KEY_GPS_LONGITUDE;
    int KEY_GPS_LATITUDE;
    int KEY_ROTATION;
    int KEY_JPEG_QUALITY;
    int KEY_JPEG_THUMBNAIL_QUALITY;
    int KEY_JPEG_THUMBNAIL_HEIGHT;
    int KEY_JPEG_THUMBNAIL_WIDTH;
    int KEY_JPEG_THUMBNAIL_SIZE;
    int KEY_PICTURE_FORMAT;
    int KEY_PICTURE_SIZE;
    int KEY_PREVIEW_FPS_RANGE;
    int KEY_PREVIEW_FRAME_RATE;
    int KEY_PREVIEW_FORMAT;
    int KEY_PREVIEW_SIZE;
  }
  class Area {
    int weight;
    int rect;
  }
  class Size {
    int height;
    int width;
  }
  class ErrorCallback {
  }
  int CAMERA_ERROR_SERVER_DIED;
  int CAMERA_ERROR_UNKNOWN;
  class Face {
    int mouth;
    int rightEye;
    int leftEye;
    int id;
    int score;
    int rect;
  }
  class FaceDetectionListener {
  }
  class OnZoomChangeListener {
  }
  class PictureCallback {
  }
  class ShutterCallback {
  }
  class AutoFocusMoveCallback {
  }
  class AutoFocusCallback {
  }
  class EventHandler {
    int mCamera;
  }
  class PreviewCallback {
  }
  class CameraInfo {
    int orientation;
    int facing;
    int CAMERA_FACING_FRONT;
    int CAMERA_FACING_BACK;
  }
  int CAMERA_FACE_DETECTION_SW;
  int CAMERA_FACE_DETECTION_HW;
  int ACTION_NEW_VIDEO;
  int ACTION_NEW_PICTURE;
  int mAutoFocusCallbackLock;
  int mFaceDetectionRunning;
  int mWithBuffer;
  int mOneShot;
  int mErrorCallback;
  int mFaceListener;
  int mZoomListener;
  int mAutoFocusMoveCallback;
  int mAutoFocusCallback;
  int mPostviewCallback;
  int mPreviewCallback;
  int mJpegCallback;
  int mRawImageCallback;
  int mShutterCallback;
  int mEventHandler;
  int mNativeContext;
  int CAMERA_MSG_FOCUS_MOVE;
  int CAMERA_MSG_PREVIEW_METADATA;
  int CAMERA_MSG_RAW_IMAGE_NOTIFY;
  int CAMERA_MSG_COMPRESSED_IMAGE;
  int CAMERA_MSG_RAW_IMAGE;
  int CAMERA_MSG_POSTVIEW_FRAME;
  int CAMERA_MSG_VIDEO_FRAME;
  int CAMERA_MSG_PREVIEW_FRAME;
  int CAMERA_MSG_ZOOM;
  int CAMERA_MSG_FOCUS;
  int CAMERA_MSG_SHUTTER;
  int CAMERA_MSG_ERROR;
  int TAG;
}
