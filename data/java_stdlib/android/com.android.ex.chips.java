package com.android.ex.chips;
class SingleRecipientArrayAdapter {
  int mLayoutInflater;
  int mLayoutId;
}
class RecipientEntry {
  int mPhotoBytes;
  int mPhotoThumbnailUri;
  int mIsDivider;
  int mDataId;
  int mContactId;
  int mDestinationLabel;
  int mDestinationType;
  int mDestination;
  int mDisplayName;
  int mIsFirstLevel;
  int mEntryType;
  int ENTRY_TYPE_SIZE;
  int ENTRY_TYPE_PERSON;
  int INVALID_DESTINATION_TYPE;
  int GENERATED_CONTACT;
  int INVALID_CONTACT;
}
class RecipientEditTextView {
  class RecipientChipShadow {
    int mChip;
  }
  class MoreImageSpan {
  }
  class IndividualReplacementTask {
  }
  class RecipientReplacementTask {
  }
  class RecipientTextWatcher {
  }
  int mDelayedShrink;
  int mHandlePendingChips;
  int mIndividualReplacements;
  int mAddTextWatcher;
  int mDragEnabled;
  int mTriedGettingScrollView;
  int mScrollView;
  int mTextWatcher;
  int mCheckedItem;
  int mAlternatesListener;
  int mCopyAddress;
  int mCopyDialog;
  int mGestureDetector;
  int mShouldShrink;
  int mRemovedSpans;
  int mTemporaryRecipients;
  int mAddressPopup;
  int mAlternatesPopup;
  int mNoChips;
  int mPendingChipsCount;
  int mHandler;
  int mPendingChips;
  int mMoreItem;
  int mMoreChip;
  int mDefaultContactPhoto;
  int mAlternatesLayout;
  int mSelectedChip;
  int mValidator;
  int mTokenizer;
  int mChipPadding;
  int mLineSpacingExtra;
  int mChipFontSize;
  int mChipHeight;
  int mChipBackgroundPressed;
  int mInvalidChipBackground;
  int mChipDelete;
  int mChipBackground;
  int sSelectedTextColor;
  int MAX_CHIPS_PARSED;
  int CHIP_LIMIT;
  int DISMISS_DELAY;
  int DISMISS;
  int TAG;
  int COMMIT_CHAR_SPACE;
  int COMMIT_CHAR_SEMICOLON;
  int COMMIT_CHAR_COMMA;
}
class RecipientChip {
  int mOriginalText;
  int mSelected;
  int mEntry;
  int mDataId;
  int mContactId;
  int mValue;
  int mDisplay;
}
class RecipientAlternatesAdapterTest {
}
class RecipientAlternatesAdapter {
  class OnCheckedItemChangedListener {
  }
  int mQuery;
  int QUERY_TYPE_PHONE;
  int QUERY_TYPE_EMAIL;
  int TAG;
  int mCheckedItemChangedListener;
  int mCheckedItemPosition;
  int mCurrentId;
  int mLayoutInflater;
  int MAX_LOOKUPS;
}
class Queries {
  class Query {
    int DISPLAY_NAME_SOURCE;
    int PHOTO_THUMBNAIL_URI;
    int DATA_ID;
    int CONTACT_ID;
    int DESTINATION_LABEL;
    int DESTINATION_TYPE;
    int DESTINATION;
    int NAME;
    int mContentUri;
    int mContentFilterUri;
    int mProjection;
  }
  int EMAIL;
  int PHONE;
}
class ChipsUtil {
}
class ChipsTest {
  class TestBaseRecipientAdapter {
  }
  class MockRecipientEditTextView {
  }
  class BaseMockRecipientEditTextView {
  }
  int mEditable;
  int mTokenizer;
  int mMockEntries;
  int mMockRecips;
}
class BaseRecipientAdapter {
  int mDelayedMessageHandler;
  class DelayedMessageHandler {
  }
  int mPhotoCacheMap;
  int mCurrentConstraint;
  int mRemainingDirectoryCount;
  int mEntries;
  int mExistingDestinations;
  int mNonAggregatedEntries;
  int mEntryMap;
  int mHandler;
  int mPreferredMaxResultCount;
  int mAccount;
  int mInflater;
  int mContentResolver;
  int mContext;
  class DirectoryFilter {
    int mLimit;
    int mParams;
  }
  class DefaultFilter {
  }
  class DefaultFilterResult {
    int paramsList;
    int existingDestinations;
    int nonAggregatedEntries;
    int entryMap;
    int entries;
  }
  class TemporaryEntry {
    int displayNameSource;
    int thumbnailUriString;
    int dataId;
    int contactId;
    int destinationLabel;
    int destinationType;
    int destination;
    int displayName;
  }
  class DirectoryListQuery {
    int TYPE_RESOURCE_ID;
    int PACKAGE_NAME;
    int DISPLAY_NAME;
    int ACCOUNT_TYPE;
    int ACCOUNT_NAME;
    int ID;
    int PROJECTION;
    int URI;
  }
  class PhotoQuery {
    int PHOTO;
    int PROJECTION;
  }
  class DirectorySearchParams {
    int filter;
    int constraint;
    int accountType;
    int accountName;
    int displayName;
    int directoryType;
    int directoryId;
  }
  int mQueryType;
  int mQuery;
  int QUERY_TYPE_PHONE;
  int QUERY_TYPE_EMAIL;
  int MESSAGE_SEARCH_PENDING;
  int MESSAGE_SEARCH_PENDING_DELAY;
  int PHOTO_CACHE_SIZE;
  int PRIMARY_ACCOUNT_TYPE;
  int PRIMARY_ACCOUNT_NAME;
  int ALLOWANCE_FOR_DUPLICATES;
  int DEFAULT_PREFERRED_MAX_RESULT_COUNT;
  int DEBUG;
  int TAG;
}
class AccountSpecifier {
}
