package android.appwidget;
class AppWidgetProviderInfo {
  int CREATOR;
  int resizeMode;
  int previewImage;
  int autoAdvanceViewId;
  int icon;
  int label;
  int configure;
  int initialLayout;
  int updatePeriodMillis;
  int minResizeHeight;
  int minResizeWidth;
  int minHeight;
  int minWidth;
  int provider;
  int RESIZE_BOTH;
  int RESIZE_VERTICAL;
  int RESIZE_HORIZONTAL;
  int RESIZE_NONE;
}
class AppWidgetProvider {
}
class AppWidgetManager {
  int mDisplayMetrics;
  int mContext;
  int sService;
  int sManagerCache;
  int META_DATA_APPWIDGET_PROVIDER;
  int ACTION_APPWIDGET_ENABLED;
  int ACTION_APPWIDGET_DISABLED;
  int ACTION_APPWIDGET_DELETED;
  int ACTION_APPWIDGET_OPTIONS_CHANGED;
  int ACTION_APPWIDGET_UPDATE;
  int INVALID_APPWIDGET_ID;
  int EXTRA_CUSTOM_EXTRAS;
  int EXTRA_CUSTOM_INFO;
  int EXTRA_APPWIDGET_PROVIDER;
  int EXTRA_APPWIDGET_IDS;
  int EXTRA_APPWIDGET_OPTIONS;
  int OPTION_APPWIDGET_MAX_HEIGHT;
  int OPTION_APPWIDGET_MAX_WIDTH;
  int OPTION_APPWIDGET_MIN_HEIGHT;
  int OPTION_APPWIDGET_MIN_WIDTH;
  int EXTRA_APPWIDGET_ID;
  int ACTION_APPWIDGET_CONFIGURE;
  int ACTION_APPWIDGET_BIND;
  int ACTION_APPWIDGET_PICK;
  int TAG;
}
class AppWidgetHostView {
  class ParcelableSparseArray {
    int CREATOR;
  }
  int mOldPaint;
  int mOld;
  int mFadeStartTime;
  int mLayoutId;
  int mViewMode;
  int mView;
  int mInfo;
  int mAppWidgetId;
  int mRemoteContext;
  int mContext;
  int sInflaterFilter;
  int FADE_DURATION;
  int VIEW_MODE_DEFAULT;
  int VIEW_MODE_ERROR;
  int VIEW_MODE_CONTENT;
  int VIEW_MODE_NOINIT;
  int CROSSFADE;
  int LOGD;
  int TAG;
}
class AppWidgetHost {
  int mViews;
  int mCallbacks;
  int mHostId;
  int mHandler;
  class UpdateHandler {
  }
  class Callbacks {
  }
  int mPackageName;
  int mContext;
  int mDisplayMetrics;
  int sService;
  int sServiceLock;
  int HANDLE_VIEW_DATA_CHANGED;
  int HANDLE_PROVIDER_CHANGED;
  int HANDLE_UPDATE;
}
