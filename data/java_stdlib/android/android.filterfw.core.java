package android.filterfw.core;
class VertexFrame {
  int vertexFrameId;
}
class SyncRunner {
  int TAG;
  int mLogVerbose;
  int mTimer;
  int mWakeCondition;
  int mWakeExecutor;
  int mDoneListener;
  int mScheduler;
}
class StreamPort {
  int mPersistent;
  int mFrame;
}
class StopWatchMap {
  int mStopWatches;
  int LOG_MFF_RUNNING_TIMES;
}
class StopWatch {
  int mNumCalls;
  int mTotalTime;
  int mStartTime;
  int mName;
  int TAG;
  int STOP_WATCH_LOGGING_PERIOD;
}
class SimpleScheduler {
}
class SimpleFrameManager {
}
class SimpleFrame {
  int mObject;
}
class ShaderProgram {
  int mTimer;
  int mGLEnvironment;
  int mMaxTileSize;
  int shaderProgramId;
}
class SerializedFrame {
  class DirectByteInputStream {
    int mSize;
    int mPos;
    int mBuffer;
  }
  class DirectByteOutputStream {
    int mDataOffset;
    int mOffset;
    int mBuffer;
  }
  int mObjectOut;
  int mByteOutputStream;
  int INITIAL_CAPACITY;
}
class Scheduler {
  int mGraph;
}
class RoundRobinScheduler {
  int mLastPos;
}
class RandomScheduler {
  int mRand;
}
class ProtocolException {
}
class ProgramVariable {
  int mVarName;
  int mProgram;
}
class ProgramPort {
  int mVarName;
}
class Program {
}
class OutputPort {
  int mBasePort;
  int mTargetPort;
}
class OneShotScheduler {
  int TAG;
  int mLogVerbose;
  int scheduled;
}
class NativeProgram {
  int mTornDown;
  int mHasResetFunction;
  int mHasGetValueFunction;
  int mHasSetValueFunction;
  int mHasTeardownFunction;
  int mHasInitFunction;
  int nativeProgramId;
}
class NativeFrame {
  int nativeFrameId;
}
class NativeBuffer {
  int mRefCount;
  int mOwnsData;
  int mAttachedFrame;
  int mSize;
  int mDataPointer;
}
class NativeAllocatorTag {
}
class MutableFrameFormat {
}
class KeyValueMap {
}
class InputPort {
  int mSourcePort;
}
class GraphRunner {
  int RESULT_ERROR;
  int RESULT_STOPPED;
  int RESULT_BLOCKED;
  int RESULT_SLEEPING;
  int RESULT_FINISHED;
  int RESULT_RUNNING;
  int RESULT_UNKNOWN;
  class OnRunnerDoneListener {
  }
  int mFilterContext;
}
class GLFrame {
  int mGLEnvironment;
  int mOwnsTexture;
  int glFrameId;
  int EXTERNAL_TEXTURE;
  int NEW_FBO_BINDING;
  int NEW_TEXTURE_BINDING;
  int EXISTING_FBO_BINDING;
  int EXISTING_TEXTURE_BINDING;
}
class GLFrameTimer {
  int mTimer;
}
class GLEnvironment {
  int mManageContext;
  int glEnvId;
}
class FrameManager {
  int mContext;
}
class FrameFormat {
  int mObjectClass;
  int mMetaData;
  int mDimensions;
  int mTarget;
  int mSize;
  int mBytesPerSample;
  int mBaseType;
  int SIZE_UNKNOWN;
  int BYTES_PER_SAMPLE_UNSPECIFIED;
  int SIZE_UNSPECIFIED;
  int TARGET_RS;
  int TARGET_VERTEXBUFFER;
  int TARGET_GPU;
  int TARGET_NATIVE;
  int TARGET_SIMPLE;
  int TARGET_UNSPECIFIED;
  int TYPE_OBJECT;
  int TYPE_POINTER;
  int TYPE_DOUBLE;
  int TYPE_FLOAT;
  int TYPE_INT32;
  int TYPE_INT16;
  int TYPE_BYTE;
  int TYPE_BIT;
  int TYPE_UNSPECIFIED;
}
class Frame {
  int mTimestamp;
  int mBindingId;
  int mBindingType;
  int mRefCount;
  int mReusable;
  int mReadOnly;
  int mFrameManager;
  int mFormat;
  int TIMESTAMP_UNKNOWN;
  int TIMESTAMP_NOT_SET;
  int NO_BINDING;
}
class FinalPort {
}
class FilterSurfaceView {
  int mSurfaceId;
  int mHeight;
  int mWidth;
  int mFormat;
  int mGLEnv;
  int mListener;
  int mState;
  int STATE_INITIALIZED;
  int STATE_CREATED;
  int STATE_ALLOCATED;
}
class FilterPort {
  int TAG;
  int mLogVerbose;
  int mChecksType;
  int mIsOpen;
  int mIsBlocking;
  int mPortFormat;
  int mName;
  int mFilter;
}
class FilterGraph {
  int TAG;
  int mLogVerbose;
  int mDiscardUnconnectedOutputs;
  int mTypeCheckMode;
  int mAutoBranchMode;
  int mIsReady;
  int TYPECHECK_STRICT;
  int TYPECHECK_DYNAMIC;
  int TYPECHECK_OFF;
  int AUTOBRANCH_UNSYNCED;
  int AUTOBRANCH_SYNCED;
  int AUTOBRANCH_OFF;
  int mPreconnections;
  int mNameMap;
  int mFilters;
}
class FilterFunction {
  class FrameHolderPort {
  }
  int mResultHolders;
  int mFilterIsSetup;
  int mFilterContext;
  int mFilter;
}
class FilterFactory {
  int mLogVerbose;
  int TAG;
  int mClassLoaderGuard;
  int mLibraries;
  int mCurrentClassLoader;
  int mPackages;
  int mSharedFactory;
}
class FilterContext {
  class OnFrameReceivedListener {
  }
  int mGraphs;
  int mStoredFrames;
  int mGLEnvironment;
  int mFrameManager;
}
class Filter {
  int TAG;
  int mLogVerbose;
  int mCurrentTimestamp;
  int mSleepDelay;
  int mIsOpen;
  int mStatus;
  int mFramesToSet;
  int mFramesToRelease;
  int mOutputPorts;
  int mInputPorts;
  int mOutputCount;
  int mInputCount;
  int mName;
  int STATUS_RELEASED;
  int STATUS_ERROR;
  int STATUS_FINISHED;
  int STATUS_SLEEPING;
  int STATUS_PROCESSING;
  int STATUS_PREPARED;
  int STATUS_UNPREPARED;
  int STATUS_PREINIT;
}
class FieldPort {
  int mValue;
  int mValueWaiting;
  int mHasFrame;
  int mField;
}
class CachedFrameManager {
  int mTimeStamp;
  int mStorageSize;
  int mStorageCapacity;
  int mAvailableFrames;
}
class AsyncRunner {
  int TAG;
  int mLogVerbose;
  class AsyncRunnerTask {
    int TAG;
  }
  class RunnerResult {
    int exception;
    int status;
  }
  int mException;
  int isProcessing;
  int mDoneListener;
  int mRunTask;
  int mRunner;
  int mSchedulerClass;
}
