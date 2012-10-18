package com.android.dumprendertree2.ui;
class DirListActivity {
  class DirListAdapter {
    int mItems;
    int mContext;
  }
  class ListItem {
    int mIsDirectory;
    int mName;
    int mRelativePath;
  }
  class LoadListItemsThread {
    int mRelativePath;
    int mHandler;
  }
  int mCurrentDirPath;
  int mListView;
  int sProgressDialog;
  int NO_RESPONSE_MESSAGE;
  int MSG_SHOW_PROGRESS_DIALOG;
  int MSG_LOADED_ITEMS;
  int DIALOG_RUN_ABORT_DIR;
  int PROGRESS_DIALOG_DELAY_MS;
  int MEAN_TITLE_CHAR_SIZE;
  int LOG_TAG;
}
