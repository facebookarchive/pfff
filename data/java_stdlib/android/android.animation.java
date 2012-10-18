package android.animation;
class ViewPropertyAnimatorTest {
  class FutureReleaseListener {
    int mFuture;
  }
  class Canceler {
    int mFuture;
    int mAnim;
  }
  int mAnimator;
  int mListener;
  int mFuture;
  int mFutureListener;
  int mCanceled;
  int mRunning;
  int mStarted;
  int FUTURE_RELEASE_DELAY;
  int ANIM_MID_DELAY;
  int ANIM_MID_DURATION;
  int ANIM_DELAY;
  int ANIM_DURATION;
}
class ValueAnimatorEventsTest {
}
class ValueAnimator {
  class AnimatorUpdateListener {
  }
  class AnimationHandler {
    int mAnimationScheduled;
    int mChoreographer;
    int mReadyAnims;
    int mEndingAnims;
    int mDelayedAnims;
    int mPendingAnimations;
    int mAnimations;
  }
  int INFINITE;
  int REVERSE;
  int RESTART;
  int mValuesMap;
  int mValues;
  int mUpdateListeners;
  int mInterpolator;
  int mRepeatMode;
  int mRepeatCount;
  int mUnscaledStartDelay;
  int mStartDelay;
  int mUnscaledDuration;
  int mDuration;
  int mInitialized;
  int mStartListenersCalled;
  int mStarted;
  int mRunning;
  int mPlayingState;
  int mDelayStartTime;
  int mStartedDelay;
  int mCurrentFraction;
  int mCurrentIteration;
  int mPlayingBackwards;
  int sDefaultInterpolator;
  int sAnimationHandler;
  int mSeekTime;
  int mStartTime;
  int SEEKED;
  int RUNNING;
  int STOPPED;
  int sDurationScale;
}
class TypeEvaluator {
}
class TimeInterpolator {
}
class TimeAnimator {
  class TimeListener {
  }
  int mPreviousTime;
  int mListener;
}
class PropertyValuesHolder_Delegate {
}
class PropertyValuesHolder {
  class FloatPropertyValuesHolder {
    int mFloatAnimatedValue;
    int mFloatKeyframeSet;
    int mFloatProperty;
    int mJniSetter;
    int sJNISetterPropertyMap;
  }
  class IntPropertyValuesHolder {
    int mIntAnimatedValue;
    int mIntKeyframeSet;
    int mIntProperty;
    int mJniSetter;
    int sJNISetterPropertyMap;
  }
  int mAnimatedValue;
  int mEvaluator;
  int mTmpValueArray;
  int mPropertyMapLock;
  int sGetterPropertyMap;
  int sSetterPropertyMap;
  int DOUBLE_VARIANTS;
  int INTEGER_VARIANTS;
  int FLOAT_VARIANTS;
  int sFloatEvaluator;
  int sIntEvaluator;
  int mKeyframeSet;
  int mValueType;
  int mGetter;
  int mSetter;
  int mProperty;
  int mPropertyName;
}
class ObjectAnimatorEventsTest {
}
class LayoutTransition {
  class TransitionListener {
  }
  int mAnimateParentHierarchy;
  int mListeners;
  int mTransitionTypes;
  int staggerDelay;
  int layoutChangeListenerMap;
  int currentDisappearingAnimations;
  int currentAppearingAnimations;
  int currentChangingAnimations;
  int pendingAnimations;
  int mChangingInterpolator;
  int mChangingDisappearingInterpolator;
  int mChangingAppearingInterpolator;
  int mDisappearingInterpolator;
  int mAppearingInterpolator;
  int mChangingStagger;
  int mChangingDisappearingStagger;
  int mChangingAppearingStagger;
  int mChangingDelay;
  int mChangingDisappearingDelay;
  int mChangingAppearingDelay;
  int mDisappearingDelay;
  int mAppearingDelay;
  int mDisappearingDuration;
  int mAppearingDuration;
  int mChangingDuration;
  int mChangingDisappearingDuration;
  int mChangingAppearingDuration;
  int DEFAULT_DURATION;
  int defaultFadeOut;
  int defaultFadeIn;
  int defaultChangeOut;
  int defaultChangeIn;
  int defaultChange;
  int mChangingAnim;
  int mChangingDisappearingAnim;
  int mChangingAppearingAnim;
  int mAppearingAnim;
  int mDisappearingAnim;
  int FLAG_CHANGING;
  int FLAG_CHANGE_DISAPPEARING;
  int FLAG_CHANGE_APPEARING;
  int FLAG_DISAPPEARING;
  int FLAG_APPEARING;
  int CHANGING;
  int DISAPPEARING;
  int APPEARING;
  int CHANGE_DISAPPEARING;
  int CHANGE_APPEARING;
}
class KeyframeSet {
  int mEvaluator;
  int mKeyframes;
  int mInterpolator;
  int mLastKeyframe;
  int mFirstKeyframe;
  int mNumKeyframes;
}
class Keyframe {
  class FloatKeyframe {
    int mValue;
  }
  class IntKeyframe {
    int mValue;
  }
  class ObjectKeyframe {
    int mValue;
  }
  int mHasValue;
  int mInterpolator;
  int mValueType;
  int mFraction;
}
class IntKeyframeSet {
  int firstTime;
  int deltaValue;
  int lastValue;
  int firstValue;
}
class IntEvaluator {
}
class FutureWaiter {
}
class FloatKeyframeSet {
  int firstTime;
  int deltaValue;
  int lastValue;
  int firstValue;
}
class FloatEvaluator {
}
class EventsTest {
  class FutureReleaseListener {
    int mFuture;
  }
  class Ender {
    int mFuture;
    int mAnim;
  }
  class Canceler {
    int mFuture;
    int mAnim;
  }
  int mAnimator;
  int mListener;
  int mFuture;
  int mFutureListener;
  int mCanceled;
  int mRunning;
  int mStarted;
  int FUTURE_RELEASE_DELAY;
  int ANIM_MID_DELAY;
  int ANIM_MID_DURATION;
  int ANIM_DELAY;
  int ANIM_DURATION;
}
class BasicAnimatorActivity {
}
class ArgbEvaluator {
}
class AnimatorSetEventsTest {
  int yAnim;
  int xAnim;
  int button;
}
class AnimatorSet {
  class Builder {
    int mCurrentNode;
  }
  class Node {
    int done;
    int nodeDependents;
    int nodeDependencies;
    int tmpDependencies;
    int dependencies;
    int animation;
  }
  class Dependency {
    int rule;
    int node;
    int AFTER;
    int WITH;
  }
  class AnimatorSetListener {
    int mAnimatorSet;
  }
  class DependencyListener {
    int mRule;
    int mNode;
    int mAnimatorSet;
  }
  int mDuration;
  int mDelayAnim;
  int mStartDelay;
  int mStarted;
  int mTerminated;
  int mSetListener;
  int mNeedsSort;
  int mSortedNodes;
  int mNodes;
  int mNodeMap;
  int mPlayingSet;
}
class AnimatorListenerAdapter {
}
class AnimatorInflater {
  int VALUE_TYPE_CUSTOM;
  int VALUE_TYPE_COLOR;
  int VALUE_TYPE_INT;
  int VALUE_TYPE_FLOAT;
  int SEQUENTIALLY;
  int TOGETHER;
}
class Animator {
  class AnimatorListener {
  }
  int mListeners;
}
class AnimationThread {
  int mListener;
  int mQueue;
  int mSession;
  class MessageBundle {
    int mUptimeMillis;
    int mMessage;
    int mTarget;
  }
}
