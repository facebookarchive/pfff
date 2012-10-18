package com.android.common.widget;
class GroupingListAdapterTests {
  int mAdapter;
  int mNextId;
  int mCursor;
  int GROUPING_COLUMN_INDEX;
  int PROJECTION;
}
class GroupingListAdapter {
  int mDataSetObserver;
  int mChangeObserver;
  int mPositionMetadata;
  int mLastCachedGroup;
  int mLastCachedCursorPosition;
  int mLastCachedListPosition;
  int mPositionCache;
  int mGroupMetadata;
  int mGroupCount;
  int mRowIdColumnIndex;
  int mCount;
  int mCursor;
  int mContext;
  class PositionMetadata {
    int listPosition;
    int groupPosition;
    int childCount;
    int cursorPosition;
    int isExpanded;
    int itemType;
  }
  int ITEM_TYPE_IN_GROUP;
  int ITEM_TYPE_GROUP_HEADER;
  int ITEM_TYPE_STANDALONE;
  int EXPANDED_GROUP_MASK;
  int GROUP_SIZE_MASK;
  int GROUP_OFFSET_MASK;
  int GROUP_METADATA_ARRAY_INCREMENT;
  int GROUP_METADATA_ARRAY_INITIAL_SIZE;
}
class CompositeCursorAdapterTest {
  class TestCompositeCursorAdapter {
    int mRequests;
  }
}
class CompositeCursorAdapter {
  int mNotificationNeeded;
  int mNotificationsEnabled;
  int mCacheValid;
  int mCount;
  int mSize;
  int mPartitions;
  int mContext;
  class Partition {
    int count;
    int idColumnIndex;
    int cursor;
    int hasHeader;
    int showIfEmpty;
  }
  int INITIAL_CAPACITY;
}
