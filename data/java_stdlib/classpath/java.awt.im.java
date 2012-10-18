package java.awt.im;
class InputSubset {
  int FULLWIDTH_DIGITS;
  int FULLWIDTH_LATIN;
  int HALFWIDTH_KATAKANA;
  int HANJA;
  int KANJI;
  int SIMPLIFIED_HANZI;
  int TRADITIONAL_HANZI;
  int LATIN_DIGITS;
  int LATIN;
}
class InputMethodRequests {
}
class InputMethodHighlight {
  int style;
  int variation;
  int state;
  int selected;
  int SELECTED_CONVERTED_TEXT_HIGHLIGHT;
  int UNSELECTED_CONVERTED_TEXT_HIGHLIGHT;
  int SELECTED_RAW_TEXT_HIGHLIGHT;
  int UNSELECTED_RAW_TEXT_HIGHLIGHT;
  int CONVERTED_TEXT;
  int RAW_TEXT;
}
class InputContext {
  int subsets;
  int recent;
  int im;
  int descriptors;
}
