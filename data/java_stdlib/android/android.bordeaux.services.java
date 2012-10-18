package android.bordeaux.services;
class StringFloat {
  int CREATOR;
  int value;
  int key;
}
class StochasticLinearRankerWithPrior {
  class Model {
    int priorParameters;
    int priorWeights;
    int uModel;
  }
  int mUseAutoAlpha;
  int mUsePrior;
  int mNumTrainPair;
  int mMinReqTrainingPair;
  int mPriorRankerPerf;
  int mUserRankerPerf;
  int mForgetRate;
  int mAutoAlpha;
  int mAlpha;
  int mPriorWeights;
  int SET_AUTO_ALPHA;
  int SET_NUM_TRAIN_PAIR;
  int SET_PRIOR_PERF;
  int SET_USER_PERF;
  int SET_MIN_TRAIN_PAIR;
  int SET_FORGET_RATE;
  int USE_AUTO_ALPHA;
  int SET_ALPHA;
  int USE_PRIOR;
  int EPSILON;
  int TAG;
}
class Learning_StochasticLinearRanker {
  int modelChangeCallback;
  int mLearningSlRanker;
  int TAG;
}
class Learning_MulticlassPA {
  class IntFloatArray {
    int floatArray;
    int indexArray;
  }
  int modelChangeCallback;
  int mMulticlassPA_learner;
}
class IntFloat {
  int CREATOR;
  int value;
  int index;
}
class IBordeauxLearner {
  class ModelChangeCallback {
  }
}
class BordeauxSessionStorage {
  class SessionDBHelper {
  }
  int mDbSessions;
  int mDbHelper;
  int DATABASE_CREATE;
  int DATABASE_VERSION;
  int SESSION_TABLE;
  int DATABASE_NAME;
  int COLUMN_TIME;
  int COLUMN_MODEL;
  int COLUMN_CLASS;
  int COLUMN_KEY;
  int TAG;
}
class BordeauxSessionManager {
  class LearningUpdateCallback {
    int mKey;
  }
  int mSessions;
  int mSavingThread;
  class PeriodicSave {
    int mQuit;
    int mSavingInterval;
  }
  class SessionKey {
    int value;
  }
  class Session {
    int modified;
    int learner;
    int learnerClass;
  }
  int mSessionStorage;
  int TAG;
}
class BordeauxService {
  int mBinder;
  int mSessionManager;
  int mNotificationManager;
  int mValue;
  int mCallbacks;
  int TAG;
}
class BordeauxRanker {
  int mRanker;
  int mName;
  int mContext;
  int RANKER_NOTAVAILABLE;
  int TAG;
}
class BordeauxManagerService {
  int mConnection;
  int mStarted;
  int mClassifier;
  int mRanker;
  int mService;
  int TAG;
}
class BordeauxClassifier {
  int mClassifier;
  int mName;
  int mContext;
  int TAG;
}
