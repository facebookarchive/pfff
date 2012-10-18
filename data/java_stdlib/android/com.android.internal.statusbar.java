package com.android.internal.statusbar;
class StatusBarNotification {
  int CREATOR;
  int score;
  int notification;
  int initialPid;
  int uid;
  int tag;
  int id;
  int pkg;
}
class StatusBarIconList {
  int CREATOR;
  int mIcons;
  int mSlots;
}
class StatusBarIcon {
  int CREATOR;
  int contentDescription;
  int number;
  int visible;
  int iconLevel;
  int iconId;
  int iconPackage;
}
