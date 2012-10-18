package com.android.server.input;
class PersistentDataStore {
  class InputDeviceState {
    int mKeyboardLayouts;
    int mCurrentKeyboardLayout;
  }
  int mDirty;
  int mLoaded;
  int mAtomicFile;
  int mInputDevices;
  int TAG;
}
class InputWindowHandle {
  int inputFeatures;
  int ownerUid;
  int ownerPid;
  int layer;
  int paused;
  int hasWallpaper;
  int hasFocus;
  int canReceiveKeys;
  int visible;
  int touchableRegion;
  int scaleFactor;
  int frameBottom;
  int frameRight;
  int frameTop;
  int frameLeft;
  int dispatchingTimeoutNanos;
  int layoutParamsType;
  int layoutParamsFlags;
  int name;
  int inputChannel;
  int windowState;
  int inputApplicationHandle;
  int ptr;
}
class InputManagerService {
  class VibratorToken {
    int mVibrating;
    int mTokenValue;
    int mToken;
    int mDeviceId;
  }
  class InputDevicesChangedListenerRecord {
    int mListener;
    int mPid;
  }
  class KeyboardLayoutVisitor {
  }
  class KeyboardLayoutDescriptor {
    int keyboardLayoutName;
    int receiverName;
    int packageName;
  }
  class InputFilterHost {
    int mDisconnected;
  }
  class InputManagerHandler {
  }
  class Callbacks {
  }
  int SW_KEYPAD_SLIDE;
  int SW_LID;
  int BTN_MOUSE;
  int KEY_STATE_VIRTUAL;
  int KEY_STATE_DOWN;
  int KEY_STATE_UP;
  int KEY_STATE_UNKNOWN;
  int INJECTION_TIMEOUT_MILLIS;
  int INPUT_EVENT_INJECTION_TIMED_OUT;
  int INPUT_EVENT_INJECTION_FAILED;
  int INPUT_EVENT_INJECTION_PERMISSION_DENIED;
  int INPUT_EVENT_INJECTION_SUCCEEDED;
  int mInputFilterHost;
  int mInputFilter;
  int mInputFilterLock;
  int mNextVibratorTokenValue;
  int mVibratorTokens;
  int mVibratorLock;
  int mSwitchedKeyboardLayoutToast;
  int mKeyboardLayoutIntent;
  int mKeyboardLayoutNotificationShown;
  int mTempFullKeyboards;
  int mTempInputDevicesChangedListenersToNotify;
  int mInputDevicesChangedListeners;
  int mInputDevices;
  int mInputDevicesChangedPending;
  int mInputDevicesLock;
  int mDataStore;
  int mNotificationManager;
  int mBluetoothService;
  int mSystemReady;
  int mHandler;
  int mCallbacks;
  int mContext;
  int mPtr;
  int MSG_RELOAD_DEVICE_ALIASES;
  int MSG_UPDATE_KEYBOARD_LAYOUTS;
  int MSG_RELOAD_KEYBOARD_LAYOUTS;
  int MSG_SWITCH_KEYBOARD_LAYOUT;
  int MSG_DELIVER_INPUT_DEVICES_CHANGED;
  int EXCLUDED_DEVICES_PATH;
  int DEBUG;
  int TAG;
}
class InputFilter {
  class Host {
  }
  class H {
  }
  int mOutboundInputEventConsistencyVerifier;
  int mInboundInputEventConsistencyVerifier;
  int mHost;
  int mH;
  int MSG_INPUT_EVENT;
  int MSG_UNINSTALL;
  int MSG_INSTALL;
}
class InputApplicationHandle {
  int dispatchingTimeoutNanos;
  int name;
  int appWindowToken;
  int ptr;
}
