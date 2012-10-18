package com.android.layoutlib.bridge.impl.binding;
class FakeExpandableAdapter {
  int mChildrenTypes;
  int mGroupTypes;
}
class FakeAdapter {
  int mTypes;
}
class BaseAdapter {
  int mItems;
  int mSkipCallbackParser;
  int mAdapterRef;
  int mCallback;
  int mBinding;
  class AdapterItem {
    int mChildren;
    int mPositionPerType;
    int mFullPosition;
    int mType;
    int mItem;
  }
}
