package javax.security.auth.callback;
class UnsupportedCallbackException {
  int callback;
}
class TextOutputCallback {
  int message;
  int messageType;
  int ERROR;
  int WARNING;
  int INFORMATION;
}
class TextInputCallback {
  int inputText;
  int defaultText;
  int prompt;
}
class PasswordCallback {
  int inputPassword;
  int echoOn;
  int prompt;
}
class NameCallback {
  int inputName;
  int defaultName;
  int prompt;
}
class LanguageCallback {
  int locale;
}
class ConfirmationCallback {
  int selection;
  int options;
  int defaultOption;
  int optionType;
  int messageType;
  int prompt;
  int ERROR;
  int WARNING;
  int INFORMATION;
  int OK;
  int CANCEL;
  int NO;
  int YES;
  int OK_CANCEL_OPTION;
  int YES_NO_CANCEL_OPTION;
  int YES_NO_OPTION;
  int UNSPECIFIED_OPTION;
}
class ChoiceCallback {
  int selections;
  int multipleSelectionsAllowed;
  int defaultChoice;
  int choices;
  int prompt;
}
class CallbackHandler {
}
class Callback {
}
