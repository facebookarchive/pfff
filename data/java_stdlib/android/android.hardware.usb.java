package android.hardware.usb;
class UsbRequest {
  int mClientData;
  int mLength;
  int mBuffer;
  int mEndpoint;
  int mNativeContext;
  int TAG;
}
class UsbManager {
  int mService;
  int mContext;
  int EXTRA_PERMISSION_GRANTED;
  int EXTRA_ACCESSORY;
  int EXTRA_DEVICE;
  int USB_FUNCTION_ACCESSORY;
  int USB_FUNCTION_AUDIO_SOURCE;
  int USB_FUNCTION_PTP;
  int USB_FUNCTION_MTP;
  int USB_FUNCTION_RNDIS;
  int USB_FUNCTION_ADB;
  int USB_FUNCTION_MASS_STORAGE;
  int USB_CONFIGURED;
  int USB_CONNECTED;
  int ACTION_USB_ACCESSORY_DETACHED;
  int ACTION_USB_ACCESSORY_ATTACHED;
  int ACTION_USB_DEVICE_DETACHED;
  int ACTION_USB_DEVICE_ATTACHED;
  int ACTION_USB_STATE;
  int TAG;
}
class UsbInterface {
  int CREATOR;
  int mEndpoints;
  int mProtocol;
  int mSubclass;
  int mClass;
  int mId;
}
class UsbEndpoint {
  int CREATOR;
  int mInterval;
  int mMaxPacketSize;
  int mAttributes;
  int mAddress;
}
class UsbDeviceConnection {
  int mNativeContext;
  int mDevice;
  int TAG;
}
class UsbDevice {
  int CREATOR;
  int mInterfaces;
  int mProtocol;
  int mSubclass;
  int mClass;
  int mProductId;
  int mVendorId;
  int mName;
  int TAG;
}
class UsbConstants {
  int USB_SUBCLASS_VENDOR_SPEC;
  int USB_INTERFACE_SUBCLASS_BOOT;
  int USB_CLASS_VENDOR_SPEC;
  int USB_CLASS_APP_SPEC;
  int USB_CLASS_MISC;
  int USB_CLASS_WIRELESS_CONTROLLER;
  int USB_CLASS_VIDEO;
  int USB_CLASS_CONTENT_SEC;
  int USB_CLASS_CSCID;
  int USB_CLASS_CDC_DATA;
  int USB_CLASS_HUB;
  int USB_CLASS_MASS_STORAGE;
  int USB_CLASS_PRINTER;
  int USB_CLASS_STILL_IMAGE;
  int USB_CLASS_PHYSICA;
  int USB_CLASS_HID;
  int USB_CLASS_COMM;
  int USB_CLASS_AUDIO;
  int USB_CLASS_PER_INTERFACE;
  int USB_TYPE_RESERVED;
  int USB_TYPE_VENDOR;
  int USB_TYPE_CLASS;
  int USB_TYPE_STANDARD;
  int USB_TYPE_MASK;
  int USB_ENDPOINT_XFER_INT;
  int USB_ENDPOINT_XFER_BULK;
  int USB_ENDPOINT_XFER_ISOC;
  int USB_ENDPOINT_XFER_CONTROL;
  int USB_ENDPOINT_XFERTYPE_MASK;
  int USB_ENDPOINT_NUMBER_MASK;
  int USB_DIR_IN;
  int USB_DIR_OUT;
  int USB_ENDPOINT_DIR_MASK;
}
class UsbAccessory {
  int CREATOR;
  int SERIAL_STRING;
  int URI_STRING;
  int VERSION_STRING;
  int DESCRIPTION_STRING;
  int MODEL_STRING;
  int MANUFACTURER_STRING;
  int mSerial;
  int mUri;
  int mVersion;
  int mDescription;
  int mModel;
  int mManufacturer;
  int TAG;
}
