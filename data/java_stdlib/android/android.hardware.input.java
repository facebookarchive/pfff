package android.hardware.input;
class KeyboardLayout {
  int CREATOR;
  int mCollection;
  int mLabel;
  int mDescriptor;
}
class InputManager {
  class InputDeviceVibrator {
    int mToken;
    int mDeviceId;
  }
  class InputDeviceListenerDelegate {
    int mListener;
  }
  class InputDevicesChangedListener {
  }
  class InputDeviceListener {
  }
  int INJECT_INPUT_EVENT_MODE_WAIT_FOR_FINISH;
  int INJECT_INPUT_EVENT_MODE_WAIT_FOR_RESULT;
  int INJECT_INPUT_EVENT_MODE_ASYNC;
  int DEFAULT_POINTER_SPEED;
  int MAX_POINTER_SPEED;
  int MIN_POINTER_SPEED;
  int META_DATA_KEYBOARD_LAYOUTS;
  int ACTION_QUERY_KEYBOARD_LAYOUTS;
  int mInputDeviceListeners;
  int mInputDevicesChangedListener;
  int mInputDevices;
  int mInputDevicesLock;
  int mIm;
  int sInstance;
  int MSG_DEVICE_CHANGED;
  int MSG_DEVICE_REMOVED;
  int MSG_DEVICE_ADDED;
  int DEBUG;
  int TAG;
}
