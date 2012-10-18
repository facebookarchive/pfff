package android.preference;
class VolumePreference {
  class SeekBarVolumizer {
    int mVolumeObserver;
    int mVolumeBeforeMute;
    int mSeekBar;
    int mLastProgress;
    int mRingtone;
    int mOriginalStreamVolume;
    int mStreamType;
    int mAudioManager;
    int mHandler;
    int mContext;
  }
  class SavedState {
    int CREATOR;
    int mVolumeStore;
  }
  class VolumeStore {
    int originalVolume;
    int volume;
  }
  int mSeekBarVolumizer;
  int mStreamType;
  int TAG;
}
class TwoStatePreference {
  class SavedState {
    int CREATOR;
    int checked;
  }
  int mDisableDependentsState;
  int mSendClickAccessibilityEvent;
  int mChecked;
  int mSummaryOff;
  int mSummaryOn;
}
class SwitchPreference {
  class Listener {
  }
  int mListener;
  int mSwitchOff;
  int mSwitchOn;
}
class SeekBarPreference {
  class SavedState {
    int CREATOR;
    int max;
    int progress;
  }
  int mTrackingTouch;
  int mMax;
  int mProgress;
}
class SeekBarDialogPreference {
  int mMyIcon;
  int TAG;
}
class RingtonePreference {
  int mRequestCode;
  int mShowSilent;
  int mShowDefault;
  int mRingtoneType;
  int TAG;
}
class PreferenceScreen {
  class SavedState {
    int CREATOR;
    int dialogBundle;
    int isDialogShowing;
  }
  int mListView;
  int mDialog;
  int mRootAdapter;
}
class PreferenceManager {
  class OnActivityDestroyListener {
  }
  class OnActivityStopListener {
  }
  class OnActivityResultListener {
  }
  class OnPreferenceTreeClickListener {
  }
  int mOnPreferenceTreeClickListener;
  int mPreferencesScreens;
  int mActivityDestroyListeners;
  int mActivityStopListeners;
  int mActivityResultListeners;
  int mPreferenceScreen;
  int mSharedPreferencesMode;
  int mSharedPreferencesName;
  int mNoCommit;
  int mEditor;
  int mSharedPreferences;
  int mNextRequestCode;
  int mNextId;
  int mContext;
  int mFragment;
  int mActivity;
  int KEY_HAS_SET_DEFAULT_VALUES;
  int METADATA_KEY_PREFERENCES;
  int TAG;
}
class PreferenceInflater {
  int mPreferenceManager;
  int EXTRA_TAG_NAME;
  int INTENT_TAG_NAME;
  int TAG;
}
class PreferenceGroupAdapter {
  class PreferenceLayout {
    int name;
    int widgetResId;
    int resId;
  }
  int mSyncRunnable;
  int mHandler;
  int mIsSyncing;
  int mHasReturnedViewTypeCount;
  int mTempPreferenceLayout;
  int mPreferenceLayouts;
  int mPreferenceList;
  int mPreferenceGroup;
  int TAG;
}
class PreferenceGroup {
  int mAttachedToActivity;
  int mCurrentPreferenceOrder;
  int mOrderingAsAdded;
  int mPreferenceList;
}
class PreferenceFrameLayout {
  class LayoutParams {
    int removeBorders;
  }
  int mPaddingApplied;
  int mBorderRight;
  int mBorderLeft;
  int mBorderBottom;
  int mBorderTop;
  int DEFAULT_BORDER_RIGHT;
  int DEFAULT_BORDER_LEFT;
  int DEFAULT_BORDER_BOTTOM;
  int DEFAULT_BORDER_TOP;
}
class PreferenceFragment {
  int mListOnKeyListener;
  class OnPreferenceStartFragmentCallback {
  }
  int mRequestFocus;
  int mHandler;
  int MSG_BIND_PREFERENCES;
  int FIRST_REQUEST_CODE;
  int mInitDone;
  int mHavePrefs;
  int mList;
  int mPreferenceManager;
  int PREFERENCES_TAG;
}
class PreferenceCategory {
  int TAG;
}
class PreferenceActivity {
  class Header {
    int CREATOR;
    int extras;
    int intent;
    int fragmentArguments;
    int fragment;
    int iconRes;
    int breadCrumbShortTitle;
    int breadCrumbShortTitleRes;
    int breadCrumbTitle;
    int breadCrumbTitleRes;
    int summary;
    int summaryRes;
    int title;
    int titleRes;
    int id;
  }
  int HEADER_ID_UNDEFINED;
  class HeaderAdapter {
    int mInflater;
    class HeaderViewHolder {
      int summary;
      int title;
      int icon;
    }
  }
  int mHandler;
  int MSG_BUILD_HEADERS;
  int MSG_BIND_PREFERENCES;
  int FIRST_REQUEST_CODE;
  int mNextButton;
  int mSavedInstanceState;
  int mPreferenceManager;
  int mCurHeader;
  int mSinglePane;
  int mFragmentBreadCrumbs;
  int mPrefsContainer;
  int mListFooter;
  int mHeaders;
  int EXTRA_PREFS_SET_BACK_TEXT;
  int EXTRA_PREFS_SET_NEXT_TEXT;
  int EXTRA_PREFS_SHOW_SKIP;
  int EXTRA_PREFS_SHOW_BUTTON_BAR;
  int BACK_STACK_PREFS;
  int EXTRA_NO_HEADERS;
  int EXTRA_SHOW_FRAGMENT_SHORT_TITLE;
  int EXTRA_SHOW_FRAGMENT_TITLE;
  int EXTRA_SHOW_FRAGMENT_ARGUMENTS;
  int EXTRA_SHOW_FRAGMENT;
  int PREFERENCES_TAG;
  int CUR_HEADER_TAG;
  int HEADERS_TAG;
}
class Preference {
  class BaseSavedState {
    int CREATOR;
  }
  class OnPreferenceChangeInternalListener {
  }
  class OnPreferenceClickListener {
  }
  class OnPreferenceChangeListener {
  }
  int mBaseMethodCalled;
  int mDependents;
  int mListener;
  int mHasSpecifiedLayout;
  int mWidgetLayoutResId;
  int mLayoutResId;
  int mShouldDisableView;
  int mDependencyMet;
  int mDefaultValue;
  int mDependencyKey;
  int mPersistent;
  int mRequiresKey;
  int mSelectable;
  int mEnabled;
  int mExtras;
  int mFragment;
  int mIntent;
  int mKey;
  int mIcon;
  int mIconResId;
  int mSummary;
  int mTitleRes;
  int mTitle;
  int mOrder;
  int mOnClickListener;
  int mOnChangeListener;
  int mId;
  int mPreferenceManager;
  int mContext;
  int DEFAULT_ORDER;
}
class OnDependencyChangeListener {
}
class MultiSelectListPreference {
  class SavedState {
    int CREATOR;
    int values;
  }
  int mPreferenceChanged;
  int mNewValues;
  int mValues;
  int mEntryValues;
  int mEntries;
}
class MultiCheckPreference {
  class SavedState {
    int CREATOR;
    int values;
  }
  int mSummary;
  int mOrigValues;
  int mSetValues;
  int mEntryValues;
  int mEntries;
}
class ListPreferenceTest {
}
class ListPreference {
  class SavedState {
    int CREATOR;
    int value;
  }
  int mClickedDialogEntryIndex;
  int mSummary;
  int mValue;
  int mEntryValues;
  int mEntries;
}
class GenericInflater {
  class FactoryMerger {
    int mF2;
    int mF1;
  }
  class Factory {
  }
  class Parent {
  }
  int mDefaultPackage;
  int sConstructorMap;
  int mConstructorSignature;
  int mConstructorArgs;
  int mFactory;
  int mFactorySet;
  int mContext;
  int DEBUG;
}
class EditTextPreference {
  class SavedState {
    int CREATOR;
    int text;
  }
  int mText;
  int mEditText;
}
class DialogPreference {
  class SavedState {
    int CREATOR;
    int dialogBundle;
    int isDialogShowing;
  }
  int mWhichButtonClicked;
  int mDialog;
  int mDialogLayoutResId;
  int mNegativeButtonText;
  int mPositiveButtonText;
  int mDialogIcon;
  int mDialogMessage;
  int mDialogTitle;
  int mBuilder;
}
class CheckBoxPreference {
}
