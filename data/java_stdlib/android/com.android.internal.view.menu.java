package com.android.internal.view.menu;
class SubMenuBuilder {
  int mItem;
  int mParentMenu;
}
class MenuView {
  class ItemView {
  }
}
class MenuPresenter {
  class Callback {
  }
}
class MenuPopupHelper {
  class MenuAdapter {
    int mExpandedIndex;
    int mAdapterMenu;
  }
  int mMeasureParent;
  int mForceShowIcon;
  int mPresenterCallback;
  int mAdapter;
  int mTreeObserver;
  int mOverflowOnly;
  int mAnchorView;
  int mPopupMaxWidth;
  int mMenu;
  int mPopup;
  int mInflater;
  int mContext;
  int ITEM_LAYOUT;
  int TAG;
}
class MenuItemImpl {
  int sSpaceShortcutLabel;
  int sDeleteShortcutLabel;
  int sEnterShortcutLabel;
  int sPrependShortcutLabel;
  int mMenuInfo;
  int NO_ICON;
  int mIsActionViewExpanded;
  int mOnActionExpandListener;
  int mActionProvider;
  int mActionView;
  int mShowAsAction;
  int IS_ACTION;
  int ENABLED;
  int HIDDEN;
  int EXCLUSIVE;
  int CHECKED;
  int CHECKABLE;
  int mFlags;
  int mClickListener;
  int mItemCallback;
  int mSubMenu;
  int mMenu;
  int mIconResId;
  int mIconDrawable;
  int mShortcutAlphabeticChar;
  int mShortcutNumericChar;
  int mIntent;
  int mTitleCondensed;
  int mTitle;
  int mOrdering;
  int mCategoryOrder;
  int mGroup;
  int mId;
  int SHOW_AS_ACTION_MASK;
  int TAG;
}
class MenuDialogHelper {
  int mPresenterCallback;
  int mPresenter;
  int mDialog;
  int mMenu;
}
class MenuBuilder {
  class ItemInvoker {
  }
  class Callback {
  }
  int mExpandedItem;
  int mPresenters;
  int mTempShortcutItemList;
  int mIsClosing;
  int mOptionalIconsVisible;
  int mItemsChangedWhileDispatchPrevented;
  int mPreventDispatchingItemsChanged;
  int mFrozenViewStates;
  int mHeaderView;
  int mHeaderIcon;
  int mHeaderTitle;
  int mCurrentMenuInfo;
  int mDefaultShowAsAction;
  int mIsActionItemsStale;
  int mNonActionItems;
  int mActionItems;
  int mIsVisibleItemsStale;
  int mVisibleItems;
  int mItems;
  int mCallback;
  int mShortcutsVisible;
  int mQwertyMode;
  int mResources;
  int mContext;
  int sCategoryToOrder;
  int EXPANDED_ACTION_VIEW_ID;
  int ACTION_VIEW_STATES_KEY;
  int PRESENTER_KEY;
  int TAG;
}
class ListMenuPresenter {
  class MenuAdapter {
    int mExpandedIndex;
  }
  int VIEWS_TAG;
  int mId;
  int mAdapter;
  int mCallback;
  int mItemLayoutRes;
  int mThemeRes;
  int mItemIndexOffset;
  int mMenuView;
  int mMenu;
  int mInflater;
  int mContext;
  int TAG;
}
class ListMenuItemView {
  int mForceShowIcon;
  int mInflater;
  int mMenuType;
  int mPreserveIconSpacing;
  int mTextAppearanceContext;
  int mTextAppearance;
  int mBackground;
  int mShortcutView;
  int mCheckBox;
  int mTitleView;
  int mRadioButton;
  int mIconView;
  int mItemData;
  int TAG;
}
class IconMenuView {
  class LayoutParams {
    int maxNumItemsOnRow;
    int desiredWidth;
    int bottom;
    int right;
    int top;
    int left;
  }
  class SavedState {
    int CREATOR;
    int focusedPosition;
  }
  int mLayoutNumRows;
  int mLayout;
  int mLastChildrenCaptionMode;
  int mMenuBeingLongpressed;
  int mHasStaleChildren;
  int mAnimations;
  int mItemBackground;
  int mMoreIcon;
  int mVerticalDividerRects;
  int mVerticalDividerWidth;
  int mVerticalDivider;
  int mHorizontalDividerRects;
  int mHorizontalDividerHeight;
  int mHorizontalDivider;
  int mNumActualItemsShown;
  int mMaxItemsPerRow;
  int mMaxItems;
  int mMaxRows;
  int mRowHeight;
  int mMenu;
  int ITEM_CAPTION_CYCLE_DELAY;
}
class IconMenuPresenter {
  class SubMenuPresenterCallback {
  }
  int OPEN_SUBMENU_KEY;
  int VIEWS_TAG;
  int mOpenSubMenu;
  int mSubMenuPresenterCallback;
  int mOpenSubMenuId;
  int mMaxItems;
  int mMoreView;
}
class IconMenuItemView {
  int sPrependShortcutLabel;
  int mShortcutCaption;
  int mShortcutCaptionMode;
  int mPositionIconOutput;
  int mPositionIconAvailable;
  int mDisabledAlpha;
  int mTextAppearanceContext;
  int mTextAppearance;
  int mIcon;
  int mItemData;
  int mItemInvoker;
  int mIconMenuView;
  int NO_ALPHA;
}
class ExpandedMenuView {
  int mAnimations;
  int mMenu;
}
class ContextMenuBuilder {
}
class BaseMenuPresenter {
  int mId;
  int mMenuView;
  int mItemLayoutRes;
  int mMenuLayoutRes;
  int mCallback;
  int mInflater;
  int mSystemInflater;
  int mMenu;
  int mContext;
  int mSystemContext;
}
class ActionMenuView {
  class LayoutParams {
    int expanded;
    int preventEdgeOffset;
    int expandable;
    int extraPixels;
    int cellsUsed;
    int isOverflowButton;
  }
  class ActionMenuChildView {
  }
  int mMaxItemHeight;
  int mMeasuredExtraWidth;
  int mGeneratedItemPadding;
  int mMinCellSize;
  int mFormatItemsWidth;
  int mFormatItems;
  int mPresenter;
  int mReserveOverflow;
  int mMenu;
  int GENERATED_ITEM_PADDING;
  int MIN_CELL_SIZE;
  int TAG;
}
class ActionMenuPresenter {
  class OpenOverflowRunnable {
    int mPopup;
  }
  class PopupPresenterCallback {
  }
  class ActionButtonSubmenu {
    int mSubMenu;
  }
  class OverflowPopup {
  }
  class OverflowMenuButton {
  }
  class SavedState {
    int CREATOR;
    int openSubMenuId;
  }
  int mOpenSubMenuId;
  int mPopupPresenterCallback;
  int mPostedOpenRunnable;
  int mActionButtonPopup;
  int mOverflowPopup;
  int mScrapActionButtonView;
  int mActionButtonGroups;
  int mMinCellSize;
  int mExpandedActionViewsExclusive;
  int mWidthLimitSet;
  int mStrictWidthLimit;
  int mMaxItemsSet;
  int mMaxItems;
  int mActionItemWidthLimit;
  int mWidthLimit;
  int mReserveOverflowSet;
  int mReserveOverflow;
  int mOverflowButton;
  int TAG;
}
class ActionMenuItemView {
  int mSavedPaddingLeft;
  int mMinWidth;
  int mExpandedFormat;
  int mAllowTextWithIcon;
  int mItemInvoker;
  int mIcon;
  int mTitle;
  int mItemData;
  int TAG;
}
class ActionMenuItem {
  int ENABLED;
  int HIDDEN;
  int EXCLUSIVE;
  int CHECKED;
  int CHECKABLE;
  int mFlags;
  int NO_ICON;
  int mClickListener;
  int mContext;
  int mIconResId;
  int mIconDrawable;
  int mShortcutAlphabeticChar;
  int mShortcutNumericChar;
  int mIntent;
  int mTitleCondensed;
  int mTitle;
  int mOrdering;
  int mCategoryOrder;
  int mGroup;
  int mId;
}
class ActionMenu {
  int mItems;
  int mIsQwerty;
  int mContext;
}
