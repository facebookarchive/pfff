package com.android.server.net;
class NetworkStatsService {
  class DefaultNetworkStatsSettings {
    int mResolver;
  }
  class DropBoxNonMonotonicObserver {
  }
  int mHandlerCallback;
  int mPhoneListener;
  int mLastPhoneNetworkType;
  int mLastPhoneState;
  int mAlertObserver;
  int mShutdownReceiver;
  int mRemovedReceiver;
  int mPollReceiver;
  int mTetherReceiver;
  int mConnReceiver;
  int mGlobalAlertBytes;
  int mPersistThreshold;
  int mSystemReady;
  int mHandler;
  int mHandlerThread;
  int mUidOperations;
  int mActiveUidCounterSet;
  int mXtStatsCached;
  int mDevStatsCached;
  int mUidTagRecorder;
  int mUidRecorder;
  int mXtRecorder;
  int mDevRecorder;
  int mNonMonotonicObserver;
  int mMobileIfaces;
  int mActiveIface;
  int mActiveIfaces;
  int mStatsLock;
  class NetworkStatsSettings {
    class Config {
      int deleteAgeMillis;
      int rotateAgeMillis;
      int bucketDuration;
    }
  }
  int PREFIX_UID_TAG;
  int PREFIX_UID;
  int PREFIX_XT;
  int PREFIX_DEV;
  int mPollIntent;
  int ACTION_NETWORK_STATS_UPDATED;
  int ACTION_NETWORK_STATS_POLL;
  int mConnManager;
  int mWakeLock;
  int mBaseDir;
  int mSystemDir;
  int mSettings;
  int mTeleManager;
  int mTime;
  int mAlarmManager;
  int mNetworkManager;
  int mContext;
  int TAG_NETSTATS_ERROR;
  int FLAG_PERSIST_FORCE;
  int FLAG_PERSIST_ALL;
  int FLAG_PERSIST_UID;
  int FLAG_PERSIST_NETWORK;
  int MSG_REGISTER_GLOBAL_ALERT;
  int MSG_UPDATE_IFACES;
  int MSG_PERFORM_POLL;
  int LOGV;
  int TAG;
}
class NetworkStatsRecorder {
  class RemoveUidRewriter {
    int mUid;
    int mTemp;
  }
  class CombiningRewriter {
    int mCollection;
  }
  int mComplete;
  int mPendingRewriter;
  int mSinceBoot;
  int mPending;
  int mLastSnapshot;
  int mPersistThresholdBytes;
  int mOnlyTags;
  int mBucketDuration;
  int mCookie;
  int mDropBox;
  int mObserver;
  int mRotator;
  int DUMP_BEFORE_DELETE;
  int TAG_NETSTATS_DUMP;
  int LOGV;
  int LOGD;
  int TAG;
}
class NetworkStatsCollectionTest {
  int TEST_IMSI;
  int TEST_FILE;
}
class NetworkStatsCollection {
  class Key {
    int hashCode;
    int tag;
    int set;
    int uid;
    int ident;
  }
  int mDirty;
  int mTotalBytes;
  int mEndMillis;
  int mStartMillis;
  int mBucketDuration;
  int mStats;
  int VERSION_UNIFIED_INIT;
  int VERSION_UID_WITH_SET;
  int VERSION_UID_WITH_TAG;
  int VERSION_UID_WITH_IDENT;
  int VERSION_UID_INIT;
  int VERSION_NETWORK_INIT;
  int FILE_MAGIC;
}
class NetworkPolicyManagerService {
  class XmlUtils {
  }
  int mHandlerCallback;
  int mConnReceiver;
  int mAlertObserver;
  int mWifiStateReceiver;
  int mWifiConfigReceiver;
  int mSnoozeWarningReceiver;
  int mAllowReceiver;
  int mStatsReceiver;
  int mPackageReceiver;
  int mScreenReceiver;
  int mProcessObserver;
  int mPolicyFile;
  int mHandler;
  int mHandlerThread;
  int mListeners;
  int mUidPidForeground;
  int mUidForeground;
  int mActiveNotifs;
  int mOverLimitNotified;
  int mMeteredIfaces;
  int mUidRules;
  int mAppPolicy;
  int mNetworkRules;
  int mNetworkPolicy;
  int mSuppressDefaultPolicy;
  int mRestrictBackground;
  int mScreenOn;
  int mRulesLock;
  int mNotifManager;
  int mConnManager;
  int mTime;
  int mNetworkManager;
  int mNetworkStats;
  int mPowerManager;
  int mActivityManager;
  int mContext;
  int MSG_SCREEN_ON_CHANGED;
  int MSG_ADVISE_PERSIST_THRESHOLD;
  int MSG_RESTRICT_BACKGROUND_CHANGED;
  int MSG_LIMIT_REACHED;
  int MSG_PROCESS_DIED;
  int MSG_FOREGROUND_ACTIVITIES_CHANGED;
  int MSG_METERED_IFACES_CHANGED;
  int MSG_RULES_CHANGED;
  int TIME_CACHE_MAX_AGE;
  int ACTION_SNOOZE_WARNING;
  int ACTION_ALLOW_BACKGROUND;
  int TAG_ALLOW_BACKGROUND;
  int ATTR_POLICY;
  int ATTR_APP_ID;
  int ATTR_UID;
  int ATTR_INFERRED;
  int ATTR_METERED;
  int ATTR_LAST_LIMIT_SNOOZE;
  int ATTR_LAST_WARNING_SNOOZE;
  int ATTR_LAST_SNOOZE;
  int ATTR_LIMIT_BYTES;
  int ATTR_WARNING_BYTES;
  int ATTR_CYCLE_TIMEZONE;
  int ATTR_CYCLE_DAY;
  int ATTR_NETWORK_ID;
  int ATTR_SUBSCRIBER_ID;
  int ATTR_NETWORK_TEMPLATE;
  int ATTR_RESTRICT_BACKGROUND;
  int ATTR_VERSION;
  int TAG_APP_POLICY;
  int TAG_UID_POLICY;
  int TAG_NETWORK_POLICY;
  int TAG_POLICY_LIST;
  int TYPE_LIMIT_SNOOZED;
  int TYPE_LIMIT;
  int TYPE_WARNING;
  int VERSION_LATEST;
  int VERSION_ADDED_NETWORK_ID;
  int VERSION_SWITCH_APP_ID;
  int VERSION_ADDED_INFERRED;
  int VERSION_ADDED_TIMEZONE;
  int VERSION_SPLIT_SNOOZE;
  int VERSION_ADDED_METERED;
  int VERSION_ADDED_RESTRICT_BACKGROUND;
  int VERSION_ADDED_SNOOZE;
  int VERSION_INIT;
  int LOGV;
  int LOGD;
  int TAG;
}
class NetworkIdentitySet {
  int VERSION_ADD_NETWORK_ID;
  int VERSION_ADD_ROAMING;
  int VERSION_INIT;
}
class NetworkAlertObserver {
}
