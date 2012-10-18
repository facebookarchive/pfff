package java.awt.event;
class WindowStateListener {
}
class WindowListener {
}
class WindowFocusListener {
}
class WindowEvent {
  int newState;
  int oldState;
  int opposite;
  int WINDOW_LAST;
  int WINDOW_STATE_CHANGED;
  int WINDOW_LOST_FOCUS;
  int WINDOW_GAINED_FOCUS;
  int WINDOW_DEACTIVATED;
  int WINDOW_ACTIVATED;
  int WINDOW_DEICONIFIED;
  int WINDOW_ICONIFIED;
  int WINDOW_CLOSED;
  int WINDOW_CLOSING;
  int WINDOW_OPENED;
  int WINDOW_FIRST;
  int serialVersionUID;
}
class WindowAdapter {
}
class TextListener {
}
class TextEvent {
  int TEXT_VALUE_CHANGED;
  int TEXT_LAST;
  int TEXT_FIRST;
  int serialVersionUID;
}
class PaintEvent {
  int updateRect;
  int UPDATE;
  int PAINT;
  int PAINT_LAST;
  int PAINT_FIRST;
  int serialVersionUID;
}
class MouseWheelListener {
}
class MouseWheelEvent {
  int wheelRotation;
  int scrollAmount;
  int scrollType;
  int WHEEL_BLOCK_SCROLL;
  int WHEEL_UNIT_SCROLL;
  int serialVersionUID;
}
class MouseMotionListener {
}
class MouseMotionAdapter {
}
class MouseListener {
}
class MouseEvent {
  int popupTrigger;
  int button;
  int clickCount;
  int absY;
  int absX;
  int y;
  int x;
  int MOUSE_WHEEL;
  int MOUSE_DRAGGED;
  int BUTTON3;
  int BUTTON2;
  int BUTTON1;
  int NOBUTTON;
  int MOUSE_EXITED;
  int MOUSE_ENTERED;
  int MOUSE_MOVED;
  int MOUSE_RELEASED;
  int MOUSE_PRESSED;
  int MOUSE_CLICKED;
  int MOUSE_LAST;
  int MOUSE_FIRST;
  int serialVersionUID;
}
class MouseAdapter {
}
class KeyListener {
}
class KeyEvent {
  int isProxyActive;
  int keyLocation;
  int keyChar;
  int keyCode;
  int KEY_LOCATION_NUMPAD;
  int KEY_LOCATION_RIGHT;
  int KEY_LOCATION_LEFT;
  int KEY_LOCATION_STANDARD;
  int KEY_LOCATION_UNKNOWN;
  int CHAR_UNDEFINED;
  int VK_UNDEFINED;
  int VK_WINDOWS;
  int VK_CONTEXT_MENU;
  int VK_BEGIN;
  int VK_ALT_GRAPH;
  int VK_COMPOSE;
  int VK_STOP;
  int VK_PROPS;
  int VK_FIND;
  int VK_AGAIN;
  int VK_UNDO;
  int VK_PASTE;
  int VK_COPY;
  int VK_CUT;
  int VK_INPUT_METHOD_ON_OFF;
  int VK_KANA_LOCK;
  int VK_JAPANESE_ROMAN;
  int VK_JAPANESE_HIRAGANA;
  int VK_JAPANESE_KATAKANA;
  int VK_CODE_INPUT;
  int VK_PREVIOUS_CANDIDATE;
  int VK_ALL_CANDIDATES;
  int VK_ROMAN_CHARACTERS;
  int VK_HALF_WIDTH;
  int VK_FULL_WIDTH;
  int VK_HIRAGANA;
  int VK_KATAKANA;
  int VK_ALPHANUMERIC;
  int VK_KANJI;
  int VK_KANA;
  int VK_MODECHANGE;
  int VK_ACCEPT;
  int VK_NONCONVERT;
  int VK_CONVERT;
  int VK_FINAL;
  int VK_UNDERSCORE;
  int VK_RIGHT_PARENTHESIS;
  int VK_PLUS;
  int VK_NUMBER_SIGN;
  int VK_LEFT_PARENTHESIS;
  int VK_INVERTED_EXCLAMATION_MARK;
  int VK_EXCLAMATION_MARK;
  int VK_EURO_SIGN;
  int VK_DOLLAR;
  int VK_CIRCUMFLEX;
  int VK_COLON;
  int VK_AT;
  int VK_BRACERIGHT;
  int VK_BRACELEFT;
  int VK_GREATER;
  int VK_LESS;
  int VK_QUOTEDBL;
  int VK_ASTERISK;
  int VK_AMPERSAND;
  int VK_DEAD_SEMIVOICED_SOUND;
  int VK_DEAD_VOICED_SOUND;
  int VK_DEAD_IOTA;
  int VK_DEAD_OGONEK;
  int VK_DEAD_CEDILLA;
  int VK_DEAD_CARON;
  int VK_DEAD_DOUBLEACUTE;
  int VK_DEAD_ABOVERING;
  int VK_DEAD_DIAERESIS;
  int VK_DEAD_ABOVEDOT;
  int VK_DEAD_BREVE;
  int VK_DEAD_MACRON;
  int VK_DEAD_TILDE;
  int VK_DEAD_CIRCUMFLEX;
  int VK_DEAD_ACUTE;
  int VK_DEAD_GRAVE;
  int VK_KP_RIGHT;
  int VK_KP_LEFT;
  int VK_KP_DOWN;
  int VK_KP_UP;
  int VK_QUOTE;
  int VK_BACK_QUOTE;
  int VK_META;
  int VK_HELP;
  int VK_INSERT;
  int VK_PRINTSCREEN;
  int VK_F24;
  int VK_F23;
  int VK_F22;
  int VK_F21;
  int VK_F20;
  int VK_F19;
  int VK_F18;
  int VK_F17;
  int VK_F16;
  int VK_F15;
  int VK_F14;
  int VK_F13;
  int VK_F12;
  int VK_F11;
  int VK_F10;
  int VK_F9;
  int VK_F8;
  int VK_F7;
  int VK_F6;
  int VK_F5;
  int VK_F4;
  int VK_F3;
  int VK_F2;
  int VK_F1;
  int VK_SCROLL_LOCK;
  int VK_NUM_LOCK;
  int VK_DELETE;
  int VK_DIVIDE;
  int VK_DECIMAL;
  int VK_SUBTRACT;
  int VK_SEPARATOR;
  int VK_SEPARATER;
  int VK_ADD;
  int VK_MULTIPLY;
  int VK_NUMPAD9;
  int VK_NUMPAD8;
  int VK_NUMPAD7;
  int VK_NUMPAD6;
  int VK_NUMPAD5;
  int VK_NUMPAD4;
  int VK_NUMPAD3;
  int VK_NUMPAD2;
  int VK_NUMPAD1;
  int VK_NUMPAD0;
  int VK_CLOSE_BRACKET;
  int VK_BACK_SLASH;
  int VK_OPEN_BRACKET;
  int VK_Z;
  int VK_Y;
  int VK_X;
  int VK_W;
  int VK_V;
  int VK_U;
  int VK_T;
  int VK_S;
  int VK_R;
  int VK_Q;
  int VK_P;
  int VK_O;
  int VK_N;
  int VK_M;
  int VK_L;
  int VK_K;
  int VK_J;
  int VK_I;
  int VK_H;
  int VK_G;
  int VK_F;
  int VK_E;
  int VK_D;
  int VK_C;
  int VK_B;
  int VK_A;
  int VK_EQUALS;
  int VK_SEMICOLON;
  int VK_9;
  int VK_8;
  int VK_7;
  int VK_6;
  int VK_5;
  int VK_4;
  int VK_3;
  int VK_2;
  int VK_1;
  int VK_0;
  int VK_SLASH;
  int VK_PERIOD;
  int VK_MINUS;
  int VK_COMMA;
  int VK_DOWN;
  int VK_RIGHT;
  int VK_UP;
  int VK_LEFT;
  int VK_HOME;
  int VK_END;
  int VK_PAGE_DOWN;
  int VK_PAGE_UP;
  int VK_SPACE;
  int VK_ESCAPE;
  int VK_CAPS_LOCK;
  int VK_PAUSE;
  int VK_ALT;
  int VK_CONTROL;
  int VK_SHIFT;
  int VK_CLEAR;
  int VK_CANCEL;
  int VK_TAB;
  int VK_BACK_SPACE;
  int VK_ENTER;
  int KEY_RELEASED;
  int KEY_PRESSED;
  int KEY_TYPED;
  int KEY_LAST;
  int KEY_FIRST;
  int serialVersionUID;
}
class KeyAdapter {
}
class ItemListener {
}
class ItemEvent {
  int stateChange;
  int item;
  int DESELECTED;
  int SELECTED;
  int ITEM_STATE_CHANGED;
  int ITEM_LAST;
  int ITEM_FIRST;
  int serialVersionUID;
}
class InvocationEvent {
  int when;
  int throwable;
  int exception;
  int catchExceptions;
  int notifier;
  int runnable;
  int INVOCATION_LAST;
  int INVOCATION_DEFAULT;
  int INVOCATION_FIRST;
  int serialVersionUID;
}
class InputMethodListener {
}
class InputMethodEvent {
  int visiblePosition;
  int caret;
  int committedCharacterCount;
  int text;
  int when;
  int INPUT_METHOD_LAST;
  int CARET_POSITION_CHANGED;
  int INPUT_METHOD_TEXT_CHANGED;
  int INPUT_METHOD_FIRST;
  int serialVersionUID;
}
class InputEvent {
  int modifiersEx;
  int modifiers;
  int when;
  int CONVERT_MASK;
  int ALT_GRAPH_DOWN_MASK;
  int BUTTON3_DOWN_MASK;
  int BUTTON2_DOWN_MASK;
  int BUTTON1_DOWN_MASK;
  int ALT_DOWN_MASK;
  int META_DOWN_MASK;
  int CTRL_DOWN_MASK;
  int SHIFT_DOWN_MASK;
  int BUTTON3_MASK;
  int BUTTON2_MASK;
  int BUTTON1_MASK;
  int ALT_GRAPH_MASK;
  int ALT_MASK;
  int META_MASK;
  int CTRL_MASK;
  int SHIFT_MASK;
  int serialVersionUID;
}
class HierarchyListener {
}
class HierarchyEvent {
  int changeFlags;
  int changedParent;
  int changed;
  int SHOWING_CHANGED;
  int DISPLAYABILITY_CHANGED;
  int PARENT_CHANGED;
  int HIERARCHY_LAST;
  int ANCESTOR_RESIZED;
  int ANCESTOR_MOVED;
  int HIERARCHY_CHANGED;
  int HIERARCHY_FIRST;
  int serialVersionUID;
}
class HierarchyBoundsListener {
}
class HierarchyBoundsAdapter {
}
class FocusListener {
}
class FocusEvent {
  int opposite;
  int temporary;
  int FOCUS_LOST;
  int FOCUS_GAINED;
  int FOCUS_LAST;
  int FOCUS_FIRST;
  int serialVersionUID;
}
class FocusAdapter {
}
class ContainerListener {
}
class ContainerEvent {
  int child;
  int COMPONENT_REMOVED;
  int COMPONENT_ADDED;
  int CONTAINER_LAST;
  int CONTAINER_FIRST;
  int serialVersionUID;
}
class ContainerAdapter {
}
class ComponentListener {
}
class ComponentEvent {
  int COMPONENT_HIDDEN;
  int COMPONENT_SHOWN;
  int COMPONENT_RESIZED;
  int COMPONENT_MOVED;
  int COMPONENT_LAST;
  int COMPONENT_FIRST;
  int serialVersionUID;
}
class ComponentAdapter {
}
class AdjustmentListener {
}
class AdjustmentEvent {
  int isAdjusting;
  int value;
  int adjustmentType;
  int adjustable;
  int TRACK;
  int BLOCK_INCREMENT;
  int BLOCK_DECREMENT;
  int UNIT_DECREMENT;
  int UNIT_INCREMENT;
  int ADJUSTMENT_VALUE_CHANGED;
  int ADJUSTMENT_LAST;
  int ADJUSTMENT_FIRST;
  int serialVersionUID;
}
class ActionListener {
}
class ActionEvent {
  int when;
  int modifiers;
  int actionCommand;
  int ACTION_PERFORMED;
  int ACTION_LAST;
  int ACTION_FIRST;
  int ALT_MASK;
  int META_MASK;
  int CTRL_MASK;
  int SHIFT_MASK;
  int serialVersionUID;
}
class AWTEventListenerProxy {
  int mask;
}
class AWTEventListener {
}
