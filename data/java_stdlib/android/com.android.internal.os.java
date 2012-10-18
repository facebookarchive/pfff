package com.android.internal.os;
class ZygoteSecurityException {
}
class ZygoteInit {
  class MethodAndArgsCaller {
    int mArgs;
    int mMethod;
  }
  int ROOT_GID;
  int ROOT_UID;
  int UNPRIVILEGED_GID;
  int UNPRIVILEGED_UID;
  int PRELOAD_RESOURCES;
  int PRELOADED_CLASSES;
  int ZYGOTE_FORK_MODE;
  int GC_LOOP_COUNT;
  int mResources;
  int sServerSocket;
  int USAGE_STRING;
  int PRELOAD_GC_THRESHOLD;
  int LOG_BOOT_PROGRESS_PRELOAD_END;
  int LOG_BOOT_PROGRESS_PRELOAD_START;
  int ANDROID_SOCKET_ENV;
  int TAG;
}
class ZygoteConnection {
  class Arguments {
    int remainingArgs;
    int invokeWith;
    int rlimits;
    int effectiveCapabilities;
    int permittedCapabilities;
    int capabilitiesSpecified;
    int niceName;
    int runtimeInit;
    int classpath;
    int targetSdkVersionSpecified;
    int targetSdkVersion;
    int debugFlags;
    int peerWait;
    int gids;
    int gidSpecified;
    int gid;
    int uidSpecified;
    int uid;
  }
  int sPeerWaitSocket;
  int peer;
  int mSocketReader;
  int mSocketOutStream;
  int mSocket;
  int MAX_ZYGOTE_ARGC;
  int CONNECTION_TIMEOUT_MILLIS;
  int intArray2d;
  int TAG;
}
class WrapperInit {
  int TAG;
}
class SamplingProfilerIntegration {
  int startMillis;
  int samplingProfiler;
  int pending;
  int samplingProfilerDepth;
  int samplingProfilerMilliseconds;
  int snapshotWriter;
  int enabled;
  int SNAPSHOT_DIR;
  int TAG;
}
class RuntimeInit {
  class Arguments {
    int startArgs;
    int startClass;
  }
  class UncaughtHandler {
  }
  int mCrashing;
  int mApplicationObject;
  int initialized;
  int DEBUG;
  int TAG;
}
class ProcessStats {
  int sLoadComparator;
  class Stats {
    int removed;
    int added;
    int working;
    int active;
    int rel_majfaults;
    int rel_minfaults;
    int base_majfaults;
    int base_minfaults;
    int rel_stime;
    int rel_utime;
    int base_stime;
    int base_utime;
    int rel_uptime;
    int base_uptime;
    int nameWidth;
    int name;
    int baseName;
    int interesting;
    int workingThreads;
    int threadStats;
    int threadsDir;
    int cmdlineFile;
    int statFile;
    int pid;
  }
  int mCpuSpeeds;
  int mRelCpuSpeedTimes;
  int mCpuSpeedTimes;
  int mBuffer;
  int mFirst;
  int mWorkingProcsSorted;
  int mWorkingProcs;
  int mProcStats;
  int mCurThreadPids;
  int mCurPids;
  int mRelIdleTime;
  int mRelSoftIrqTime;
  int mRelIrqTime;
  int mRelIoWaitTime;
  int mRelSystemTime;
  int mRelUserTime;
  int mBaseIdleTime;
  int mBaseSoftIrqTime;
  int mBaseIrqTime;
  int mBaseIoWaitTime;
  int mBaseSystemTime;
  int mBaseUserTime;
  int mLastSampleRealTime;
  int mCurrentSampleRealTime;
  int mLastSampleTime;
  int mCurrentSampleTime;
  int mLoad15;
  int mLoad5;
  int mLoad1;
  int mIncludeThreads;
  int mLoadAverageData;
  int LOAD_AVERAGE_FORMAT;
  int mSystemCpuData;
  int SYSTEM_CPU_FORMAT;
  int mProcessFullStatsData;
  int mProcessFullStatsStringData;
  int PROCESS_FULL_STAT_VSIZE;
  int PROCESS_FULL_STAT_STIME;
  int PROCESS_FULL_STAT_UTIME;
  int PROCESS_FULL_STAT_MAJOR_FAULTS;
  int PROCESS_FULL_STAT_MINOR_FAULTS;
  int PROCESS_FULL_STATS_FORMAT;
  int mSinglePidStatsData;
  int mProcessStatsData;
  int PROCESS_STAT_STIME;
  int PROCESS_STAT_UTIME;
  int PROCESS_STAT_MAJOR_FAULTS;
  int PROCESS_STAT_MINOR_FAULTS;
  int PROCESS_STATS_FORMAT;
  int localLOGV;
  int DEBUG;
  int TAG;
}
class PowerProfile {
  int ATTR_NAME;
  int TAG_ARRAYITEM;
  int TAG_ARRAY;
  int TAG_ITEM;
  int TAG_DEVICE;
  int sPowerMap;
  int POWER_BATTERY_CAPACITY;
  int POWER_CPU_SPEEDS;
  int POWER_VIDEO;
  int POWER_AUDIO;
  int POWER_SCREEN_FULL;
  int POWER_RADIO_ACTIVE;
  int POWER_RADIO_SCANNING;
  int POWER_RADIO_ON;
  int POWER_SCREEN_ON;
  int POWER_BLUETOOTH_AT_CMD;
  int POWER_BLUETOOTH_ACTIVE;
  int POWER_BLUETOOTH_ON;
  int POWER_GPS_ON;
  int POWER_WIFI_ACTIVE;
  int POWER_WIFI_ON;
  int POWER_WIFI_SCAN;
  int POWER_CPU_ACTIVE;
  int POWER_CPU_AWAKE;
  int POWER_CPU_IDLE;
  int POWER_NONE;
}
class PkgUsageStats {
  int CREATOR;
  int componentResumeTimes;
  int usageTime;
  int launchCount;
  int packageName;
}
class LoggingPrintStreamTest {
  class TestPrintStream {
    int lines;
  }
  int out;
}
class LoggingPrintStream {
  int formatter;
  int decoder;
  int decodedChars;
  int encodedBytes;
  int builder;
}
class HandlerCaller {
  class Callback {
  }
  class MyHandler {
  }
  int mArgsPool;
  int mArgsPoolSize;
  int ARGS_POOL_MAX_SIZE;
  class SomeArgs {
    int argi6;
    int argi5;
    int argi4;
    int argi3;
    int argi2;
    int argi1;
    int arg4;
    int arg3;
    int arg2;
    int arg1;
    int next;
  }
  int mCallback;
  int mH;
  int mMainLooper;
  int mContext;
  int DEBUG;
  int TAG;
}
class DebugTest {
  int EXPECTED_GET_CALLERS;
  int EXPECTED_GET_CALLER;
}
class BinderInternal {
  class GcWatcher {
  }
  int mLastGcTime;
  int mGcWatcher;
}
class BatteryStatsImpl {
  int mNetworkDetailCache;
  int mNetworkSummaryCache;
  int CREATOR;
  int mWriteLock;
  int mPendingWrite;
  int BATTERY_PLUGGED_NONE;
  class Uid {
    class Pkg {
      class Serv {
        int mUnpluggedLaunches;
        int mUnpluggedStarts;
        int mUnpluggedStartTime;
        int mLastLaunches;
        int mLastStarts;
        int mLastStartTime;
        int mLoadedLaunches;
        int mLoadedStarts;
        int mLoadedStartTime;
        int mLaunches;
        int mLaunched;
        int mLaunchedSince;
        int mLaunchedTime;
        int mStarts;
        int mRunning;
        int mRunningSince;
        int mStartTime;
      }
      int mServiceStats;
      int mUnpluggedWakeups;
      int mLastWakeups;
      int mLoadedWakeups;
      int mWakeups;
    }
    class Proc {
      int mExcessivePower;
      int mSpeedBins;
      int mUnpluggedForegroundTime;
      int mUnpluggedStarts;
      int mUnpluggedSystemTime;
      int mUnpluggedUserTime;
      int mLastForegroundTime;
      int mLastStarts;
      int mLastSystemTime;
      int mLastUserTime;
      int mLoadedForegroundTime;
      int mLoadedStarts;
      int mLoadedSystemTime;
      int mLoadedUserTime;
      int mForegroundTime;
      int mStarts;
      int mSystemTime;
      int mUserTime;
    }
    class Sensor {
      int mTimer;
      int mHandle;
    }
    class Wakelock {
      int mTimerWindow;
      int mTimerFull;
      int mTimerPartial;
    }
    int mPids;
    int mPackageStats;
    int mProcessStats;
    int mSensorStats;
    int mWakelockStats;
    int mUserActivityCounters;
    int mVideoTurnedOnTimer;
    int mVideoTurnedOn;
    int mAudioTurnedOnTimer;
    int mAudioTurnedOn;
    int mWifiMulticastTimer;
    int mWifiMulticastEnabled;
    int mScanWifiLockTimer;
    int mScanWifiLockOut;
    int mFullWifiLockTimer;
    int mFullWifiLockOut;
    int mWifiRunningTimer;
    int mWifiRunning;
    int mStartedTcpBytesSent;
    int mStartedTcpBytesReceived;
    int mTcpBytesSentAtLastUnplug;
    int mTcpBytesReceivedAtLastUnplug;
    int mCurrentTcpBytesSent;
    int mCurrentTcpBytesReceived;
    int mLoadedTcpBytesSent;
    int mLoadedTcpBytesReceived;
    int mUid;
  }
  int mWifiMulticastNesting;
  int mWifiScanLockNesting;
  int mWifiFullLockNesting;
  int mGpsNesting;
  int mSensorNesting;
  int mWakeLockNesting;
  int mChangedStates;
  int mChangedBufferStates;
  class KernelWakelockStats {
    int mVersion;
    int mTotalTime;
    int mCount;
  }
  class StopwatchTimer {
    int mInList;
    int mTimeout;
    int mAcquireTime;
    int mUpdateTime;
    int mNesting;
    int mTimerPool;
    int mUid;
  }
  class SamplingTimer {
    int mUpdateVersion;
    int mTrackingReportedValues;
    int mInDischarge;
    int mUnpluggedReportedTotalTime;
    int mCurrentReportedTotalTime;
    int mUnpluggedReportedCount;
    int mCurrentReportedCount;
  }
  class Timer {
    int mUnpluggedTime;
    int mLastTime;
    int mLoadedTime;
    int mTotalTime;
    int mUnpluggedCount;
    int mLastCount;
    int mLoadedCount;
    int mCount;
    int mUnpluggables;
    int mType;
  }
  class SamplingCounter {
  }
  class Counter {
    int mPluggedCount;
    int mUnpluggedCount;
    int mLastCount;
    int mLoadedCount;
    int mUnpluggables;
    int mCount;
  }
  class Unpluggable {
  }
  int mMobileIfaces;
  int mNetworkStatsFactory;
  int mUidCache;
  int mProcWakelockFileStats;
  int mProcWakelocksData;
  int mProcWakelocksName;
  int PROC_WAKELOCKS_FORMAT;
  int sKernelWakelockUpdateVersion;
  int mKernelWakelockStats;
  int mPhoneSimStateRaw;
  int mPhoneServiceStateRaw;
  int mPhoneServiceState;
  int mBluetoothPingStart;
  int mBluetoothPingCount;
  int mRadioDataStart;
  int mRadioDataUptime;
  int mTotalDataRx;
  int mTotalDataTx;
  int mMobileDataRx;
  int mMobileDataTx;
  int mLastWriteTime;
  int mDischargeAmountScreenOffSinceCharge;
  int mDischargeAmountScreenOff;
  int mDischargeAmountScreenOnSinceCharge;
  int mDischargeAmountScreenOn;
  int mDischargeScreenOffUnplugLevel;
  int mDischargeScreenOnUnplugLevel;
  int mHighDischargeAmountSinceCharge;
  int mLowDischargeAmountSinceCharge;
  int mDischargeCurrentLevel;
  int mDischargeUnplugLevel;
  int mDischargeStartLevel;
  int mUnpluggedBatteryRealtime;
  int mUnpluggedBatteryUptime;
  int mTrackBatteryRealtimeStart;
  int mTrackBatteryPastRealtime;
  int mTrackBatteryUptimeStart;
  int mTrackBatteryPastUptime;
  int mOnBatteryInternal;
  int mOnBattery;
  int mBtHeadset;
  int mBluetoothOnTimer;
  int mBluetoothOn;
  int mGlobalWifiRunningTimer;
  int mGlobalWifiRunning;
  int mWifiOnUid;
  int mWifiOnTimer;
  int mWifiOn;
  int mPhoneDataConnectionsTimer;
  int mPhoneDataConnectionType;
  int mPhoneSignalScanningTimer;
  int mPhoneSignalStrengthsTimer;
  int mPhoneSignalStrengthBinRaw;
  int mPhoneSignalStrengthBin;
  int mVideoOnTimer;
  int mVideoOn;
  int mAudioOnTimer;
  int mAudioOn;
  int mPhoneOnTimer;
  int mPhoneOn;
  int mInputEventCounter;
  int mScreenBrightnessTimer;
  int mScreenBrightnessBin;
  int mScreenOnTimer;
  int mScreenOn;
  int mLastRealtime;
  int mRealtimeStart;
  int mRealtime;
  int mLastUptime;
  int mUptimeStart;
  int mUptime;
  int mBatteryLastRealtime;
  int mBatteryRealtime;
  int mBatteryLastUptime;
  int mBatteryUptime;
  int mStartCount;
  int mIteratingHistory;
  int mReadOverflow;
  int mHistoryIterator;
  int mHistoryCache;
  int mHistoryLastEnd;
  int mHistoryEnd;
  int mHistory;
  int mHistoryCur;
  int mLastHistoryTime;
  int mHistoryOverflow;
  int mHistoryBufferLastPos;
  int mHistoryReadTmp;
  int mHistoryLastLastWritten;
  int mHistoryLastWritten;
  int mHistoryBuffer;
  int MAX_MAX_HISTORY_BUFFER;
  int MAX_HISTORY_BUFFER;
  int mNumHistoryItems;
  int mRecordingHistory;
  int mHaveBatteryLevel;
  int mHistoryBaseTime;
  int mShuttingDown;
  int mUnpluggables;
  int mLastPartialTimers;
  int mWifiMulticastTimers;
  int mScanWifiLockTimers;
  int mFullWifiLockTimers;
  int mWifiRunningTimers;
  int mSensorTimers;
  int mWindowTimers;
  int mFullTimers;
  int mPartialTimers;
  int mUidStats;
  int mCallback;
  int mHandler;
  class MyHandler {
  }
  class BatteryCallback {
  }
  int DELAY_UPDATE_WAKELOCKS;
  int MSG_REPORT_POWER_CHANGE;
  int MSG_UPDATE_WAKELOCKS;
  int mFile;
  int sNumSpeedSteps;
  int BATCHED_WAKELOCK_NAME;
  int MAX_WAKELOCKS_PER_UID_IN_SYSTEM;
  int MAX_WAKELOCKS_PER_UID;
  int MAX_MAX_HISTORY_ITEMS;
  int MAX_HISTORY_ITEMS;
  int VERSION;
  int MAGIC;
  int USE_OLD_HISTORY;
  int DEBUG_HISTORY;
  int DEBUG;
  int TAG;
}
class AtomicFile {
  int mBackupName;
  int mBaseName;
}
class AndroidPrintStream {
  int tag;
  int priority;
}
