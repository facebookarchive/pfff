package android.accounts;
class OperationCanceledException {
}
class OnAccountsUpdateListener {
}
class NetworkErrorException {
}
class IAccountAuthenticatorCache {
}
class GrantCredentialsPermissionActivity {
  int mInflater;
  int mResultBundle;
  int mUid;
  int mAuthTokenType;
  int mAccount;
  int EXTRAS_REQUESTING_UID;
  int EXTRAS_PACKAGES;
  int EXTRAS_ACCOUNT_TYPE_LABEL;
  int EXTRAS_RESPONSE;
  int EXTRAS_AUTH_TOKEN_TYPE;
  int EXTRAS_AUTH_TOKEN_LABEL;
  int EXTRAS_ACCOUNT;
}
class ChooseTypeAndAccountActivity {
  int mOkButton;
  int mSelectedItemIndex;
  int mExistingAccounts;
  int mPendingRequest;
  int mAccounts;
  int SELECTED_ITEM_NONE;
  int KEY_INSTANCE_STATE_SELECTED_ADD_ACCOUNT;
  int KEY_INSTANCE_STATE_SELECTED_ACCOUNT_NAME;
  int KEY_INSTANCE_STATE_EXISTING_ACCOUNTS;
  int KEY_INSTANCE_STATE_PENDING_REQUEST;
  int REQUEST_ADD_ACCOUNT;
  int REQUEST_CHOOSE_TYPE;
  int REQUEST_NULL;
  int EXTRA_DESCRIPTION_TEXT_OVERRIDE;
  int EXTRA_ALWAYS_PROMPT_FOR_ACCOUNT;
  int EXTRA_SELECTED_ACCOUNT;
  int EXTRA_ADD_ACCOUNT_AUTH_TOKEN_TYPE_STRING;
  int EXTRA_ADD_ACCOUNT_REQUIRED_FEATURES_STRING_ARRAY;
  int EXTRA_ADD_ACCOUNT_OPTIONS_BUNDLE;
  int EXTRA_ALLOWABLE_ACCOUNT_TYPES_STRING_ARRAY;
  int EXTRA_ALLOWABLE_ACCOUNTS_ARRAYLIST;
  int TAG;
}
class ChooseAccountTypeActivity {
  class AccountArrayAdapter {
    int mInfos;
    int mLayoutInflater;
  }
  class ViewHolder {
    int text;
    int icon;
  }
  class AuthInfo {
    int drawable;
    int name;
    int desc;
  }
  int mAuthenticatorInfosToDisplay;
  int mTypeToAuthenticatorInfo;
  int TAG;
}
class ChooseAccountActivity {
  class AccountArrayAdapter {
    int mInfos;
    int mLayoutInflater;
  }
  class ViewHolder {
    int text;
    int icon;
  }
  class AccountInfo {
    int drawable;
    int name;
  }
  int mTypeToAuthDescription;
  int mResult;
  int mAccountManagerResponse;
  int mAccounts;
  int TAG;
}
class AuthenticatorException {
}
class AuthenticatorDescription {
  int CREATOR;
  int customTokens;
  int packageName;
  int accountPreferencesId;
  int smallIconId;
  int iconId;
  int labelId;
  int type;
}
class AccountsException {
}
class AccountManagerServiceTest {
  class MyAccountManagerService {
  }
  class MyMockPackageManager {
  }
  class MyMockContext {
  }
  class MockAccountAuthenticatorCache {
    int mServices;
  }
  class AccountSorter {
  }
  int mAms;
}
class AccountManagerService {
  class DatabaseHelper {
  }
  class MessageHandler {
  }
  class Session {
    int mAccounts;
    int mStripAuthTokenFromResult;
    int mAuthenticator;
    int mNumErrors;
    int mNumRequestContinued;
    int mNumResults;
    int mCreationTime;
    int mExpectActivityLaunch;
    int mAccountType;
    int mResponse;
  }
  class GetAccountsByTypeAndFeatureSession {
    int mCurrentAccount;
    int mAccountsWithFeatures;
    int mAccountsOfType;
    int mFeatures;
  }
  class RemoveAccountSession {
    int mAccount;
  }
  class TestFeaturesSession {
    int mAccount;
    int mFeatures;
  }
  int EMPTY_ACCOUNT_ARRAY;
  int sThis;
  int mUsers;
  class UserAccounts {
    int authTokenCache;
    int userDataCache;
    int accountCache;
    int cacheLock;
    int signinRequiredNotificationIds;
    int credentialsPermissionNotificationIds;
    int openHelper;
    int userId;
  }
  int mNotificationIds;
  int mSessions;
  int COLUMNS_EXTRAS_KEY_AND_VALUE;
  int SELECTION_USERDATA_BY_ACCOUNT;
  int COLUMNS_AUTHTOKENS_TYPE_AND_AUTHTOKEN;
  int SELECTION_AUTHTOKENS_BY_ACCOUNT;
  int COUNT_OF_MATCHING_GRANTS;
  int ACCOUNTS_CHANGED_INTENT;
  int ACCOUNT_TYPE_COUNT_PROJECTION;
  int META_VALUE;
  int META_KEY;
  int TABLE_META;
  int EXTRAS_VALUE;
  int EXTRAS_KEY;
  int EXTRAS_ACCOUNTS_ID;
  int EXTRAS_ID;
  int TABLE_EXTRAS;
  int GRANTS_GRANTEE_UID;
  int GRANTS_AUTH_TOKEN_TYPE;
  int GRANTS_ACCOUNTS_ID;
  int TABLE_GRANTS;
  int AUTHTOKENS_AUTHTOKEN;
  int AUTHTOKENS_TYPE;
  int AUTHTOKENS_ACCOUNTS_ID;
  int AUTHTOKENS_ID;
  int TABLE_AUTHTOKENS;
  int ACCOUNTS_PASSWORD;
  int ACCOUNTS_TYPE_COUNT;
  int ACCOUNTS_TYPE;
  int ACCOUNTS_NAME;
  int ACCOUNTS_ID;
  int TABLE_ACCOUNTS;
  int mAuthenticatorCache;
  int MESSAGE_TIMED_OUT;
  int mMessageHandler;
  int mMessageThread;
  int mPackageManager;
  int mContext;
  int DATABASE_VERSION;
  int DATABASE_NAME;
  int TIMEOUT_DELAY_MS;
  int TAG;
}
class AccountManagerResponse {
  int CREATOR;
  int mResponse;
}
class AccountManagerFuture {
}
class AccountManagerCallback {
}
class AccountManager {
  int mAccountsChangedBroadcastReceiver;
  int mAccountsUpdatedListeners;
  class GetAuthTokenByTypeAndFeaturesTask {
    int mNumAccounts;
    int mMyCallback;
    int mLoginOptions;
    int mAddAccountOptions;
    int mFeatures;
    int mAuthTokenType;
    int mAccountType;
    int mFuture;
  }
  class Future2Task {
    int mCallback;
  }
  class BaseFutureTask {
    class Response {
    }
    int mHandler;
    int mResponse;
  }
  class AmsTask {
    class Response {
    }
    int mActivity;
    int mCallback;
    int mHandler;
    int mResponse;
  }
  int LOGIN_ACCOUNTS_CHANGED_ACTION;
  int mMainHandler;
  int mService;
  int mContext;
  int AUTHENTICATOR_ATTRIBUTES_NAME;
  int AUTHENTICATOR_META_DATA_NAME;
  int ACTION_AUTHENTICATOR_INTENT;
  int KEY_NOTIFY_ON_FAILURE;
  int KEY_ANDROID_PACKAGE_NAME;
  int KEY_CALLER_PID;
  int KEY_CALLER_UID;
  int KEY_USERDATA;
  int KEY_ERROR_MESSAGE;
  int KEY_ERROR_CODE;
  int KEY_BOOLEAN_RESULT;
  int KEY_AUTH_TOKEN_LABEL;
  int KEY_AUTH_FAILED_MESSAGE;
  int KEY_AUTHENTICATOR_TYPES;
  int KEY_ACCOUNT_MANAGER_RESPONSE;
  int KEY_ACCOUNT_AUTHENTICATOR_RESPONSE;
  int KEY_ACCOUNTS;
  int KEY_PASSWORD;
  int KEY_INTENT;
  int KEY_AUTHTOKEN;
  int KEY_ACCOUNT_TYPE;
  int KEY_ACCOUNT_NAME;
  int ERROR_CODE_BAD_REQUEST;
  int ERROR_CODE_BAD_ARGUMENTS;
  int ERROR_CODE_UNSUPPORTED_OPERATION;
  int ERROR_CODE_INVALID_RESPONSE;
  int ERROR_CODE_CANCELED;
  int ERROR_CODE_NETWORK_ERROR;
  int ERROR_CODE_REMOTE_EXCEPTION;
  int TAG;
}
class AccountAuthenticatorResponse {
  int CREATOR;
  int mAccountAuthenticatorResponse;
  int TAG;
}
class AccountAuthenticatorCache {
  class MySerializer {
  }
  int sSerializer;
  int TAG;
}
class AccountAuthenticatorActivity {
  int mResultBundle;
  int mAccountAuthenticatorResponse;
}
class AccountAndUser {
  int userId;
  int account;
}
class Account {
  int CREATOR;
  int type;
  int name;
}
class AbstractAccountAuthenticator {
  int mTransport;
  class Transport {
  }
  int mContext;
  int TAG;
}
