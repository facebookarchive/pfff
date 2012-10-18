package com.android.future.usb;
class UsbManager {
  int mService;
  int mContext;
  int EXTRA_PERMISSION_GRANTED;
  int ACTION_USB_ACCESSORY_DETACHED;
  int ACTION_USB_ACCESSORY_ATTACHED;
  int TAG;
}
class UsbAccessory {
  int mSerial;
  int mUri;
  int mVersion;
  int mDescription;
  int mModel;
  int mManufacturer;
}
