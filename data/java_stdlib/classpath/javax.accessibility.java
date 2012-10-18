package javax.accessibility;
class AccessibleValue {
}
class AccessibleTextSequence {
  int endIndex;
  int startIndex;
  int text;
}
class AccessibleText {
  int SENTENCE;
  int WORD;
  int CHARACTER;
}
class AccessibleTableModelChange {
  int DELETE;
  int UPDATE;
  int INSERT;
}
class AccessibleTable {
}
class AccessibleStreamable {
}
class AccessibleStateSet {
  int states;
}
class AccessibleState {
  int TRANSIENT;
  int MULTI_LINE;
  int SINGLE_LINE;
  int HORIZONTAL;
  int VERTICAL;
  int VISIBLE;
  int TRUNCATED;
  int SHOWING;
  int SELECTED;
  int SELECTABLE;
  int MULTISELECTABLE;
  int RESIZABLE;
  int OPAQUE;
  int MODAL;
  int MANAGES_DESCENDANTS;
  int INDETERMINATE;
  int ICONIFIED;
  int FOCUSED;
  int FOCUSABLE;
  int ENABLED;
  int EXPANDED;
  int COLLAPSED;
  int EXPANDABLE;
  int EDITABLE;
  int CHECKED;
  int BUSY;
  int ARMED;
  int PRESSED;
  int ACTIVE;
}
class AccessibleSelection {
}
class AccessibleRole {
  int HTML_CONTAINER;
  int RULER;
  int PROGRESS_MONITOR;
  int PARAGRAPH;
  int HEADER;
  int FOOTER;
  int EDITBAR;
  int GROUP_BOX;
  int FONT_CHOOSER;
  int SPIN_BOX;
  int DATE_EDITOR;
  int STATUS_BAR;
  int UNKNOWN;
  int SWING_COMPONENT;
  int AWT_COMPONENT;
  int TOOL_TIP;
  int TOOL_BAR;
  int TREE;
  int TEXT;
  int TABLE;
  int SPLIT_PANE;
  int SLIDER;
  int VIEWPORT;
  int SCROLL_BAR;
  int SCROLL_PANE;
  int ROW_HEADER;
  int RADIO_BUTTON;
  int CHECK_BOX;
  int TOGGLE_BUTTON;
  int PUSH_BUTTON;
  int PASSWORD_TEXT;
  int PROGRESS_BAR;
  int PANEL;
  int PAGE_TAB;
  int PAGE_TAB_LIST;
  int SEPARATOR;
  int MENU_ITEM;
  int MENU;
  int POPUP_MENU;
  int MENU_BAR;
  int LIST_ITEM;
  int LIST;
  int LAYERED_PANE;
  int GLASS_PANE;
  int ROOT_PANE;
  int LABEL;
  int ICON;
  int HYPERLINK;
  int FILLER;
  int FILE_CHOOSER;
  int DIRECTORY_PANE;
  int COLOR_CHOOSER;
  int DIALOG;
  int FRAME;
  int WINDOW;
  int OPTION_PANE;
  int DESKTOP_PANE;
  int INTERNAL_FRAME;
  int DESKTOP_ICON;
  int COMBO_BOX;
  int CANVAS;
  int COLUMN_HEADER;
  int ALERT;
}
class AccessibleResourceBundle {
}
class AccessibleRelationSet {
  int relations;
}
class AccessibleRelation {
  int targets;
  int EMPTY_TARGETS;
  int SUBWINDOW_OF_PROPERTY;
  int SUBWINDOW_OF;
  int PARENT_WINDOW_OF_PROPERTY;
  int PARENT_WINDOW_OF;
  int FLOWS_TO_PROPERTY;
  int FLOWS_TO;
  int FLOWS_FROM_PROPERTY;
  int FLOWS_FROM;
  int EMBEDS_PROPERTY;
  int EMBEDS;
  int EMBEDDED_BY_PROPERTY;
  int EMBEDDED_BY;
  int CHILD_NODE_OF_PROPERTY;
  int CHILD_NODE_OF;
  int CONTROLLED_BY_PROPERTY;
  int CONTROLLER_FOR_PROPERTY;
  int MEMBER_OF_PROPERTY;
  int LABELED_BY_PROPERTY;
  int LABEL_FOR_PROPERTY;
  int CONTROLLED_BY;
  int CONTROLLER_FOR;
  int MEMBER_OF;
  int LABELED_BY;
  int LABEL_FOR;
}
class AccessibleKeyBinding {
}
class AccessibleIcon {
}
class AccessibleHypertext {
}
class AccessibleHyperlink {
}
class AccessibleExtendedText {
  int ATTRIBUTE_RUN;
  int LINE;
}
class AccessibleExtendedTable {
}
class AccessibleExtendedComponent {
}
class AccessibleEditableText {
}
class AccessibleContext {
  int listeners;
  int accessibleDescription;
  int accessibleName;
  int accessibleParent;
  int ACCESSIBLE_TEXT_ATTRIBUTES_CHANGED;
  int ACCESSIBLE_INVALIDATE_CHILDREN;
  int ACCESSIBLE_COMPONENT_BOUNDS_CHANGED;
  int ACCESSIBLE_HYPERTEXT_OFFSET;
  int ACCESSIBLE_ACTION_PROPERTY;
  int ACCESSIBLE_TABLE_COLUMN_DESCRIPTION_CHANGED;
  int ACCESSIBLE_TABLE_COLUMN_HEADER_CHANGED;
  int ACCESSIBLE_TABLE_ROW_DESCRIPTION_CHANGED;
  int ACCESSIBLE_TABLE_ROW_HEADER_CHANGED;
  int ACCESSIBLE_TABLE_MODEL_CHANGED;
  int ACCESSIBLE_TABLE_SUMMARY_CHANGED;
  int ACCESSIBLE_TABLE_CAPTION_CHANGED;
  int ACCESSIBLE_ACTIVE_DESCENDANT_PROPERTY;
  int ACCESSIBLE_CHILD_PROPERTY;
  int ACCESSIBLE_VISIBLE_DATA_PROPERTY;
  int ACCESSIBLE_CARET_PROPERTY;
  int ACCESSIBLE_TEXT_PROPERTY;
  int ACCESSIBLE_SELECTION_PROPERTY;
  int ACCESSIBLE_VALUE_PROPERTY;
  int ACCESSIBLE_STATE_PROPERTY;
  int ACCESSIBLE_DESCRIPTION_PROPERTY;
  int ACCESSIBLE_NAME_PROPERTY;
}
class AccessibleComponent {
}
class AccessibleBundle {
  int key;
}
class AccessibleAttributeSequence {
  int endIndex;
  int startIndex;
  int attributes;
}
class AccessibleAction {
  int TOGGLE_POPUP;
  int CLICK;
  int TOGGLE_EXPAND;
  int INCREMENT;
  int DECREMENT;
}
class Accessible {
}
