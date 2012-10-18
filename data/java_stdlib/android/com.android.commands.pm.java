package com.android.commands.pm;
class Pm {
  class ClearDataObserver {
    int result;
    int finished;
  }
  class PackageDeleteObserver {
    int result;
    int finished;
  }
  class PackageInstallObserver {
    int result;
    int finished;
  }
  int ROOT_UID;
  int PM_NOT_RUNNING_ERR;
  int mCurArgData;
  int mNextArg;
  int mArgs;
  int mResourceCache;
  int mPm;
}
