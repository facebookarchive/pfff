package java.awt;
class Window {
  class WindowFlipBufferStrategy {
  }
  class WindowBltBufferStrategy {
  }
  class AccessibleAWTWindow {
    int serialVersionUID;
  }
  int next_window_number;
  int windowFocusOwner;
  int shown;
  int windowStateListener;
  int windowFocusListener;
  int windowListener;
  int ownedWindows;
  int alwaysOnTop;
  int focusableWindowState;
  int state;
  int windowSerializedDataVersion;
  int warningString;
  int serialVersionUID;
}
class Transparency {
  int TRANSLUCENT;
  int BITMASK;
  int OPAQUE;
}
class Toolkit {
  int lightweightPeer;
  int awtEventListeners;
  int desktopPropsSupport;
  int desktopProperties;
  int props;
  int toolkit;
  int default_toolkit_name;
}
class TexturePaint {
  int anchor;
  int texture;
}
class TextField {
  class AccessibleAWTTextField {
    int serialVersionUID;
  }
  int action_listeners;
  int echoChar;
  int columns;
  int serialVersionUID;
  int next_textfield_number;
}
class TextComponent {
  class AccessibleAWTTextComponent {
    int serialVersionUID;
  }
  int textListener;
  int text;
  int selectionEnd;
  int selectionStart;
  int editable;
  int serialVersionUID;
}
class TextArea {
  class AccessibleAWTTextArea {
    int serialVersionUID;
  }
  int next_text_number;
  int scrollbarVisibility;
  int rows;
  int columns;
  int serialVersionUID;
  int SCROLLBARS_NONE;
  int SCROLLBARS_HORIZONTAL_ONLY;
  int SCROLLBARS_VERTICAL_ONLY;
  int SCROLLBARS_BOTH;
}
class SystemColor {
  int infoText;
  int info;
  int scrollbar;
  int controlDkShadow;
  int controlShadow;
  int controlLtHighlight;
  int controlHighlight;
  int controlText;
  int control;
  int textInactiveText;
  int textHighlightText;
  int textHighlight;
  int textText;
  int text;
  int menuText;
  int menu;
  int windowText;
  int windowBorder;
  int window;
  int inactiveCaptionBorder;
  int inactiveCaptionText;
  int inactiveCaption;
  int activeCaptionBorder;
  int activeCaptionText;
  int activeCaption;
  int desktop;
  int colors;
  int NUM_COLORS;
  int INFO_TEXT;
  int INFO;
  int SCROLLBAR;
  int CONTROL_DK_SHADOW;
  int CONTROL_SHADOW;
  int CONTROL_LT_HIGHLIGHT;
  int CONTROL_HIGHLIGHT;
  int CONTROL_TEXT;
  int CONTROL;
  int TEXT_INACTIVE_TEXT;
  int TEXT_HIGHLIGHT_TEXT;
  int TEXT_HIGHLIGHT;
  int TEXT_TEXT;
  int TEXT;
  int MENU_TEXT;
  int MENU;
  int WINDOW_TEXT;
  int WINDOW_BORDER;
  int WINDOW;
  int INACTIVE_CAPTION_BORDER;
  int INACTIVE_CAPTION_TEXT;
  int INACTIVE_CAPTION;
  int ACTIVE_CAPTION_BORDER;
  int ACTIVE_CAPTION_TEXT;
  int ACTIVE_CAPTION;
  int DESKTOP;
  int serialVersionUID;
}
class Stroke {
}
class Shape {
}
class Scrollbar {
  class AccessibleAWTScrollBar {
    int serialVersionUID;
  }
  int next_scrollbar_number;
  int valueIsAdjusting;
  int adjustment_listeners;
  int visibleAmount;
  int value;
  int orientation;
  int minimum;
  int maximum;
  int pageIncrement;
  int lineIncrement;
  int serialVersionUID;
  int VERTICAL;
  int HORIZONTAL;
}
class ScrollPaneAdjustable {
  int valueIsAdjusting;
  int adjustmentListener;
  int blockIncrement;
  int unitIncrement;
  int visibleAmount;
  int maximum;
  int minimum;
  int value;
  int orientation;
  int sp;
  int serialVersionUID;
}
class ScrollPane {
  class AccessibleAWTScrollPane {
    int serialVersionUID;
  }
  int wheelScrollingEnabled;
  int scrollPosition;
  int scrollbarDisplayPolicy;
  int vAdjustable;
  int hAdjustable;
  int serialVersionUID;
  int next_scrollpane_number;
  int SCROLLBARS_NEVER;
  int SCROLLBARS_ALWAYS;
  int SCROLLBARS_AS_NEEDED;
}
class Robot {
  int peer;
  int autoDelay;
  int waitForIdle;
}
class RenderingHints {
  int VALUE_STROKE_PURE;
  int VALUE_STROKE_NORMALIZE;
  int VALUE_STROKE_DEFAULT;
  int KEY_STROKE_CONTROL;
  int VALUE_COLOR_RENDER_DEFAULT;
  int VALUE_COLOR_RENDER_QUALITY;
  int VALUE_COLOR_RENDER_SPEED;
  int KEY_COLOR_RENDERING;
  int VALUE_ALPHA_INTERPOLATION_DEFAULT;
  int VALUE_ALPHA_INTERPOLATION_QUALITY;
  int VALUE_ALPHA_INTERPOLATION_SPEED;
  int KEY_ALPHA_INTERPOLATION;
  int VALUE_INTERPOLATION_BICUBIC;
  int VALUE_INTERPOLATION_BILINEAR;
  int VALUE_INTERPOLATION_NEAREST_NEIGHBOR;
  int KEY_INTERPOLATION;
  int VALUE_FRACTIONALMETRICS_DEFAULT;
  int VALUE_FRACTIONALMETRICS_ON;
  int VALUE_FRACTIONALMETRICS_OFF;
  int KEY_FRACTIONALMETRICS;
  int VALUE_TEXT_ANTIALIAS_DEFAULT;
  int VALUE_TEXT_ANTIALIAS_OFF;
  int VALUE_TEXT_ANTIALIAS_ON;
  int KEY_TEXT_ANTIALIASING;
  int VALUE_DITHER_DEFAULT;
  int VALUE_DITHER_ENABLE;
  int VALUE_DITHER_DISABLE;
  int KEY_DITHERING;
  int VALUE_RENDER_DEFAULT;
  int VALUE_RENDER_QUALITY;
  int VALUE_RENDER_SPEED;
  int KEY_RENDERING;
  int VALUE_ANTIALIAS_DEFAULT;
  int VALUE_ANTIALIAS_OFF;
  int VALUE_ANTIALIAS_ON;
  int KEY_ANTIALIASING;
  int hintMap;
  class KeyImpl {
    int v3;
    int v2;
    int v1;
    int description;
  }
  class Key {
    int key;
  }
}
class Rectangle {
  int height;
  int width;
  int y;
  int x;
  int serialVersionUID;
}
class PrintJob {
}
class PrintGraphics {
}
class PopupMenu {
  class AccessibleAWTPopupMenu {
    int serialVersionUID;
  }
  int serialVersionUID;
  int next_popup_number;
}
class Polygon {
  int BIG_VALUE;
  int bounds;
  int ypoints;
  int xpoints;
  int npoints;
  int serialVersionUID;
}
class PointerInfo {
  int p;
  int gd;
}
class Point {
  int y;
  int x;
  int serialVersionUID;
}
class Panel {
  class AccessibleAWTPanel {
    int serialVersionUID;
  }
  int next_panel_number;
  int initialUpdateConsumed;
  int initialSystemUpdateDone;
  int context;
  int serialVersionUID;
}
class PaintContext {
}
class Paint {
}
class PageAttributes {
  int resolutionScale;
  int resolutionY;
  int resolutionX;
  int quality;
  int origin;
  int orientation;
  int media;
  int color;
  class PrintQualityType {
    int DRAFT;
    int NORMAL;
    int HIGH;
    int NAMES;
  }
  class OriginType {
    int PRINTABLE;
    int PHYSICAL;
    int NAMES;
  }
  class OrientationRequestedType {
    int LANDSCAPE;
    int PORTRAIT;
    int NAMES;
  }
  class MediaType {
    int PERSONAL;
    int MONARCH;
    int ITALY;
    int INVITE;
    int ENV_PERSONAL;
    int ENV_MONARCH;
    int ENV_ITALY;
    int ENV_INVITE;
    int ENV_14;
    int ENV_12;
    int ENV_11;
    int ENV_10;
    int ENV_9;
    int ENV_6X9;
    int ENV_7X9;
    int ENV_9X11;
    int ENV_9X12;
    int ENV_10X13;
    int ENV_10X14;
    int ENV_10X15;
    int LEGAL;
    int NOTE;
    int LETTER;
    int TABLOID;
    int STATEMENT;
    int ISO_DESIGNATED_LONG_ENVELOPE;
    int ISO_C10_ENVELOPE;
    int C10;
    int ISO_C9_ENVELOPE;
    int C9;
    int ISO_C8_ENVELOPE;
    int C8;
    int ISO_C7_ENVELOPE;
    int C7;
    int ISO_C6_ENVELOPE;
    int C6;
    int ISO_C5_ENVELOPE;
    int C5;
    int ISO_C4_ENVELOPE;
    int C4;
    int ISO_C3_ENVELOPE;
    int C3;
    int ISO_C2_ENVELOPE;
    int C2;
    int ISO_C1_ENVELOPE;
    int C1;
    int ISO_C0_ENVELOPE;
    int C0;
    int B10;
    int B9;
    int B8;
    int B7;
    int B6;
    int ISO_B5_ENVELOPE;
    int B5;
    int ISO_B4_ENVELOPE;
    int B4;
    int B3;
    int B2;
    int B1;
    int B0;
    int A10;
    int A9;
    int A8;
    int A7;
    int A6;
    int A5;
    int A4;
    int A3;
    int A2;
    int A1;
    int A0;
    int PERSONAL_ENVELOPE;
    int MONARCH_ENVELOPE;
    int ITALY_ENVELOPE;
    int INVITE_ENVELOPE;
    int NA_NUMBER_14_ENVELOPE;
    int NA_NUMBER_12_ENVELOPE;
    int NA_NUMBER_11_ENVELOPE;
    int NA_NUMBER_10_ENVELOPE;
    int NA_NUMBER_9_ENVELOPE;
    int NA_6X9_ENVELOPE;
    int NA_7X9_ENVELOPE;
    int NA_9X11_ENVELOPE;
    int NA_9X12_ENVELOPE;
    int NA_10X13_ENVELOPE;
    int NA_10X14_ENVELOPE;
    int NA_10X15_ENVELOPE;
    int E;
    int D;
    int C;
    int B;
    int A;
    int QUARTO;
    int NA_LEGAL;
    int NA_LETTER;
    int LEDGER;
    int INVOICE;
    int FOLIO;
    int EXECUTIVE;
    int ISO_DESIGNATED_LONG;
    int ISO_C10;
    int ISO_C9;
    int ISO_C8;
    int ISO_C7;
    int ISO_C6;
    int ISO_C5;
    int ISO_C4;
    int ISO_C3;
    int ISO_C2;
    int ISO_C1;
    int ISO_C0;
    int JIS_B10;
    int JIS_B9;
    int JIS_B8;
    int JIS_B7;
    int JIS_B6;
    int JIS_B5;
    int JIS_B4;
    int JIS_B3;
    int JIS_B2;
    int JIS_B1;
    int JIS_B0;
    int ISO_B10;
    int ISO_B9;
    int ISO_B8;
    int ISO_B7;
    int ISO_B6;
    int ISO_B5;
    int ISO_B4;
    int ISO_B3;
    int ISO_B2;
    int ISO_B1;
    int ISO_B0;
    int ISO_A10;
    int ISO_A9;
    int ISO_A8;
    int ISO_A7;
    int ISO_A6;
    int ISO_A5;
    int ISO_A4;
    int ISO_A3;
    int ISO_A2;
    int ISO_A1;
    int ISO_A0;
    int ISO_2A0;
    int ISO_4A0;
    int NAMES;
  }
  class ColorType {
    int MONOCHROME;
    int COLOR;
    int NAMES;
  }
}
class MouseInfo {
  int peer;
}
class MenuShortcut {
  int keyName;
  int usesShift;
  int key;
  int serialVersionUID;
}
class MenuItem {
  class AccessibleAWTMenuItem {
    int serialVersionUID;
  }
  int action_listeners;
  int shortcut;
  int label;
  int eventMask;
  int enabled;
  int actionCommand;
  int serialVersionUID;
  int next_menuitem_number;
}
class MenuContainer {
}
class MenuComponent {
  class AccessibleAWTMenuComponent {
    int serialVersionUID;
  }
  int focusListener;
  int newEventsOnly;
  int nameExplicitlySet;
  int accessibleContext;
  int toolkit;
  int tree_lock;
  int peer;
  int parent;
  int name;
  int font;
  int serialVersionUID;
}
class MenuBar {
  class AccessibleAWTMenuBar {
    int serialVersionUID;
  }
  int menus;
  int helpMenu;
  int next_menubar_number;
  int serialVersionUID;
}
class Menu {
  class AccessibleAWTMenu {
    int serialVersionUID;
  }
  int separatorLabel;
  int menuSerializedDataVersion;
  int isHelpMenu;
  int tearOff;
  int items;
  int serialVersionUID;
  int next_menu_number;
}
class MediaTracker {
  class MediaEntry {
    int height;
    int width;
    int status;
    int next;
    int image;
    int id;
  }
  int serialVersionUID;
  int head;
  int target;
  int COMPLETE;
  int ERRORED;
  int ABORTED;
  int LOADING;
}
class List {
  class AccessibleAWTList {
    class AccessibleAWTListChild {
      int indexInParent;
      int parent;
      int serialVersionUID;
    }
    int serialVersionUID;
  }
  int action_listeners;
  int item_listeners;
  int visibleIndex;
  int selected;
  int rows;
  int multipleMode;
  int items;
  int serialVersionUID;
  int next_list_number;
}
class LightweightDispatcher {
  int mouseEventTarget;
  int lastTarget;
  int instances;
}
class LayoutManager2 {
}
class LayoutManager {
}
class Label {
  int nextLabelNumber;
  class AccessibleAWTLabel {
    int serialVersionUID;
  }
  int text;
  int alignment;
  int serialVersionUID;
  int RIGHT;
  int CENTER;
  int LEFT;
}
class KeyboardFocusManager {
  int focusRequests;
  int keyEventPostProcessors;
  int keyEventDispatchers;
  int vetoableChangeSupport;
  int propertyChangeSupport;
  int defaultFocusKeys;
  int defaultPolicy;
  int currentFocusCycleRoots;
  int currentActiveWindows;
  int currentFocusedWindows;
  int currentPermanentFocusOwners;
  int currentFocusOwners;
  int currentKeyboardFocusManagers;
  int DEFAULT_BACKWARD_KEYS;
  int DEFAULT_FORWARD_KEYS;
  int DOWN_CYCLE_TRAVERSAL_KEYS;
  int UP_CYCLE_TRAVERSAL_KEYS;
  int BACKWARD_TRAVERSAL_KEYS;
  int FORWARD_TRAVERSAL_KEYS;
}
class KeyEventPostProcessor {
}
class KeyEventDispatcher {
}
class JobAttributes {
  int sides;
  int printer;
  int toPage;
  int fromPage;
  int pageRanges;
  int multiple;
  int minPage;
  int maxPage;
  int filename;
  int dialog;
  int destination;
  int selection;
  int copies;
  class SidesType {
    int TWO_SIDED_SHORT_EDGE;
    int TWO_SIDED_LONG_EDGE;
    int ONE_SIDED;
    int NAMES;
  }
  class MultipleDocumentHandlingType {
    int SEPARATE_DOCUMENTS_UNCOLLATED_COPIES;
    int SEPARATE_DOCUMENTS_COLLATED_COPIES;
    int NAMES;
  }
  class DialogType {
    int NONE;
    int NATIVE;
    int COMMON;
    int NAMES;
  }
  class DestinationType {
    int PRINTER;
    int FILE;
    int NAMES;
  }
  class DefaultSelectionType {
    int SELECTION;
    int RANGE;
    int ALL;
    int NAMES;
  }
}
class ItemSelectable {
}
class Insets {
  int right;
  int bottom;
  int left;
  int top;
  int serialVersionUID;
}
class ImageCapabilities {
  int accelerated;
}
class Image {
  int accelerationPriority;
  int SCALE_AREA_AVERAGING;
  int SCALE_REPLICATE;
  int SCALE_SMOOTH;
  int SCALE_FAST;
  int SCALE_DEFAULT;
  int UndefinedProperty;
}
class IllegalComponentStateException {
  int serialVersionUID;
}
class HeadlessException {
  int serialVersionUID;
}
class GridLayout {
  int vgap;
  int hgap;
  int rows;
  int cols;
  int serialVersionUID;
}
class GridBagLayoutInfo {
  int rowWeights;
  int colWeights;
  int rowHeights;
  int colWidths;
  int rows;
  int cols;
  int pos_y;
  int pos_x;
  int serialVersionUID;
}
class GridBagLayout {
  int rowHeights;
  int rowWeights;
  int columnWidths;
  int columnWeights;
  int defaultConstraints;
  int layoutInfo;
  int internalcomptable;
  int comptable;
  int MAXGRIDSIZE;
  int PREFERREDSIZE;
  int MINSIZE;
  int serialVersionUID;
}
class GridBagConstraints {
  int weighty;
  int weightx;
  int ipady;
  int ipadx;
  int insets;
  int gridy;
  int gridx;
  int gridwidth;
  int gridheight;
  int fill;
  int anchor;
  int LAST_LINE_END;
  int LAST_LINE_START;
  int FIRST_LINE_END;
  int FIRST_LINE_START;
  int LINE_END;
  int LINE_START;
  int PAGE_END;
  int PAGE_START;
  int REMAINDER;
  int RELATIVE;
  int NORTHWEST;
  int WEST;
  int SOUTHWEST;
  int SOUTH;
  int SOUTHEAST;
  int EAST;
  int NORTHEAST;
  int NORTH;
  int CENTER;
  int VERTICAL;
  int HORIZONTAL;
  int BOTH;
  int NONE;
  int serialVersionUID;
}
class GraphicsEnvironment {
  int localGraphicsEnvironment;
}
class GraphicsDevice {
  int mode;
  int fullScreenOldBounds;
  int full_screen;
  int TYPE_IMAGE_BUFFER;
  int TYPE_PRINTER;
  int TYPE_RASTER_SCREEN;
}
class GraphicsConfiguration {
  int bufferCapabilities;
  int imageCapabilities;
}
class GraphicsConfigTemplate {
  int UNNECESSARY;
  int PREFERRED;
  int REQUIRED;
  int serialVersionUID;
}
class Graphics2D {
}
class Graphics {
}
class GradientPaint {
  int cyclic;
  int c2;
  int y2;
  int x2;
  int c1;
  int y1;
  int x1;
}
class Frame {
  class AccessibleAWTFrame {
    int serialVersionUID;
  }
  int weakFramesQueue;
  int weakFrames;
  int next_frame_number;
  int undecorated;
  int maximizedBounds;
  int title;
  int state;
  int resizable;
  int ownedWindows;
  int menuBar;
  int mbManagement;
  int icon;
  int frameSerializedDataVersion;
  int serialVersionUID;
  int NORMAL;
  int MAXIMIZED_VERT;
  int MAXIMIZED_HORIZ;
  int MAXIMIZED_BOTH;
  int ICONIFIED;
  int MOVE_CURSOR;
  int HAND_CURSOR;
  int W_RESIZE_CURSOR;
  int E_RESIZE_CURSOR;
  int S_RESIZE_CURSOR;
  int N_RESIZE_CURSOR;
  int NE_RESIZE_CURSOR;
  int NW_RESIZE_CURSOR;
  int SE_RESIZE_CURSOR;
  int SW_RESIZE_CURSOR;
  int WAIT_CURSOR;
  int TEXT_CURSOR;
  int CROSSHAIR_CURSOR;
  int DEFAULT_CURSOR;
}
class FontMetrics {
  int gRC;
  int font;
  int serialVersionUID;
}
class FontFormatException {
  int serialVersionUID;
}
class Font {
  int hashCode;
  int peer;
  int serialVersionUID;
  int style;
  int pointSize;
  int size;
  int name;
  int SERIF;
  int SANS_SERIF;
  int MONOSPACED;
  int DIALOG_INPUT;
  int DIALOG;
  int LAYOUT_NO_LIMIT_CONTEXT;
  int LAYOUT_NO_START_CONTEXT;
  int LAYOUT_RIGHT_TO_LEFT;
  int LAYOUT_LEFT_TO_RIGHT;
  int TYPE1_FONT;
  int TRUETYPE_FONT;
  int HANGING_BASELINE;
  int CENTER_BASELINE;
  int ROMAN_BASELINE;
  int ITALIC;
  int BOLD;
  int PLAIN;
}
class FocusTraversalPolicy {
}
class FlowLayout {
  int vgap;
  int hgap;
  int align;
  int serialVersionUID;
  int TRAILING;
  int LEADING;
  int RIGHT;
  int CENTER;
  int LEFT;
}
class FileDialog {
  int next_file_dialog_number;
  int mode;
  int filter;
  int file;
  int dir;
  int serialVersionUID;
  int SAVE;
  int LOAD;
}
class EventQueue {
  int nativeLoopRunning;
  int dispatchThread;
  int lastWhen;
  int currentEvent;
  int prev;
  int next;
  int queues;
  class Queue {
    int queueTail;
    int queueHead;
  }
  int LOW_PRIORITY;
  int NORM_PRIORITY;
}
class EventDispatchThread {
  int queue;
  int dispatchThreadNum;
  int DEFAULT_PRIORITY;
}
class Event {
  int y;
  int x;
  int when;
  int target;
  int modifiers;
  int key;
  int id;
  int evt;
  int consumed;
  int clickCount;
  int arg;
  int WINDOW_MOVED;
  int WINDOW_ICONIFY;
  int WINDOW_EXPOSE;
  int WINDOW_DESTROY;
  int WINDOW_DEICONIFY;
  int UP;
  int TAB;
  int SCROLL_PAGE_UP;
  int SCROLL_PAGE_DOWN;
  int SCROLL_LOCK;
  int SCROLL_LINE_UP;
  int SCROLL_LINE_DOWN;
  int SCROLL_END;
  int SCROLL_BEGIN;
  int SCROLL_ABSOLUTE;
  int SAVE_FILE;
  int RIGHT;
  int PRINT_SCREEN;
  int PGUP;
  int PGDN;
  int PAUSE;
  int NUM_LOCK;
  int MOUSE_UP;
  int MOUSE_MOVE;
  int MOUSE_EXIT;
  int MOUSE_ENTER;
  int MOUSE_DRAG;
  int MOUSE_DOWN;
  int LOST_FOCUS;
  int LOAD_FILE;
  int LIST_SELECT;
  int LIST_DESELECT;
  int LEFT;
  int KEY_RELEASE;
  int KEY_PRESS;
  int KEY_ACTION_RELEASE;
  int KEY_ACTION;
  int INSERT;
  int HOME;
  int GOT_FOCUS;
  int F9;
  int F8;
  int F7;
  int F6;
  int F5;
  int F4;
  int F3;
  int F2;
  int F12;
  int F11;
  int F10;
  int F1;
  int ESCAPE;
  int ENTER;
  int END;
  int DOWN;
  int DELETE;
  int CAPS_LOCK;
  int BACK_SPACE;
  int ACTION_EVENT;
  int ALT_MASK;
  int META_MASK;
  int CTRL_MASK;
  int SHIFT_MASK;
  int serialVersionUID;
}
class DisplayMode {
  int refreshRate;
  int bitDepth;
  int height;
  int width;
  int REFRESH_RATE_UNKNOWN;
  int BIT_DEPTH_MULTI;
}
class Dimension {
  int height;
  int width;
  int serialVersionUID;
}
class Dialog {
  class AccessibleAWTDialog {
    int serialVersionUID;
  }
  int next_dialog_number;
  int eq2;
  int blocked;
  int undecorated;
  int title;
  int resizable;
  int modal;
  int serialVersionUID;
  class ModalityType {
    int TOOLKIT_MODAL;
    int MODELESS;
    int DOCUMENT_MODAL;
    int APPLICATION_MODAL;
  }
  class ModalExclusionType {
    int TOOLKIT_EXCLUDE;
    int NO_EXCLUDE;
    int APPLICATION_EXCLUDE;
  }
}
class Desktop {
  int peer;
  class Action {
    int PRINT;
    int OPEN;
    int MAIL;
    int EDIT;
    int BROWSE;
  }
}
class DefaultKeyboardFocusManager {
  int delayRequests;
  int waitForKeyStroke;
  class EventDelayRequest {
    int focusedComp;
    int timestamp;
    int enqueuedKeyEvents;
  }
}
class DefaultFocusTraversalPolicy {
  int serialVersionUID;
}
class Cursor {
  int type;
  int name;
  int predefined;
  int PREDEFINED_COUNT;
  int CUSTOM_CURSOR;
  int NAMES;
  int MOVE_CURSOR;
  int HAND_CURSOR;
  int E_RESIZE_CURSOR;
  int W_RESIZE_CURSOR;
  int S_RESIZE_CURSOR;
  int N_RESIZE_CURSOR;
  int NE_RESIZE_CURSOR;
  int NW_RESIZE_CURSOR;
  int SE_RESIZE_CURSOR;
  int SW_RESIZE_CURSOR;
  int WAIT_CURSOR;
  int TEXT_CURSOR;
  int CROSSHAIR_CURSOR;
  int DEFAULT_CURSOR;
  int serialVersionUID;
}
class ContainerOrderFocusTraversalPolicy {
  int implicitDownCycleTraversal;
  int serialVersionUID;
}
class Container {
  class AccessibleAWTContainer {
    class AccessibleContainerHandler {
    }
    int accessibleContainerHandler;
    int serialVersionUID;
  }
  class GfxPrintAllVisitor {
    int INSTANCE;
  }
  class GfxPaintAllVisitor {
    int INSTANCE;
  }
  class GfxPrintVisitor {
    int INSTANCE;
  }
  class GfxPaintVisitor {
    int INSTANCE;
  }
  class GfxVisitor {
  }
  int focusTraversalKeys;
  int focusTraversalPolicy;
  int containerListener;
  int containerSerializedDataVersion;
  int focusTraversalPolicyProvider;
  int focusCycleRoot;
  int layoutMgr;
  int component;
  int ncomponents;
  int serialVersionUID;
}
class CompositeContext {
}
class Composite {
}
class ComponentOrientation {
  int orientation;
  int UNKNOWN;
  int RIGHT_TO_LEFT;
  int LEFT_TO_RIGHT;
  int LEFT_TO_RIGHT_ID;
  int HORIZONTAL_ID;
  int UNKNOWN_ID;
  int serialVersionUID;
}
class Component {
  class FlipBufferStrategy {
    int height;
    int width;
    int validatedContents;
    int drawVBuffer;
    int drawBuffer;
    int caps;
    int numBuffers;
  }
  class BltBufferStrategy {
    int frontBuffer;
    int height;
    int width;
    int validatedContents;
    int backBuffers;
    int caps;
  }
  class AccessibleAWTComponent {
    class AccessibleAWTFocusHandler {
    }
    class AccessibleAWTComponentHandler {
    }
    int accessibleAWTFocusHandler;
    int accessibleAWTComponentHandler;
    int serialVersionUID;
  }
  class HeavyweightInLightweightListener {
  }
  int redrawRate;
  int incrementalDraw;
  int pendingFocusRequest;
  int numHierarchyBoundsListeners;
  int numHierarchyListeners;
  int bufferStrategy;
  int graphicsConfig;
  int componentOrientation;
  int peer;
  int parent;
  int hierarchyBoundsListener;
  int hierarchyListener;
  int inputMethodListener;
  int mouseWheelListener;
  int mouseMotionListener;
  int mouseListener;
  int keyListener;
  int focusListener;
  int componentListener;
  int accessibleContext;
  int componentSerializedDataVersion;
  int isPacked;
  int changeSupport;
  int eventMask;
  int newEventsOnly;
  int prefSizeSet;
  int prefSize;
  int maxSizeSet;
  int maxSize;
  int minSizeSet;
  int minSize;
  int focusTraversalKeysEnabled;
  int focusTraversalKeys;
  int isFocusTraversableOverridden;
  int focusable;
  int nameExplicitlySet;
  int name;
  int popups;
  int dropTarget;
  int valid;
  int enabled;
  int visible;
  int ignoreRepaint;
  int locale;
  int cursor;
  int peerFont;
  int font;
  int background;
  int foreground;
  int height;
  int width;
  int y;
  int x;
  int DEFAULT_MAX_SIZE;
  int treeLock;
  int LEFT_ALIGNMENT;
  int RIGHT_ALIGNMENT;
  int BOTTOM_ALIGNMENT;
  int CENTER_ALIGNMENT;
  int TOP_ALIGNMENT;
  int serialVersionUID;
}
class ColorPaintContext {
  class ColorRaster {
  }
  int cachedRaster;
  int colorModel;
  int color;
}
class Color {
  int context;
  int cs;
  int falpha;
  int fvalue;
  int frgbvalue;
  int value;
  int BRIGHT_SCALE;
  int ALPHA_MASK;
  int BLUE_MASK;
  int GREEN_MASK;
  int RED_MASK;
  int BLUE;
  int blue;
  int CYAN;
  int cyan;
  int MAGENTA;
  int magenta;
  int GREEN;
  int green;
  int YELLOW;
  int yellow;
  int ORANGE;
  int orange;
  int PINK;
  int pink;
  int RED;
  int red;
  int BLACK;
  int black;
  int DARK_GRAY;
  int darkGray;
  int GRAY;
  int gray;
  int LIGHT_GRAY;
  int lightGray;
  int WHITE;
  int white;
  int serialVersionUID;
}
class Choice {
  class AccessibleAWTChoice {
    int serialVersionUID;
  }
  int item_listeners;
  int selectedIndex;
  int pItems;
  int serialVersionUID;
  int next_choice_number;
}
class CheckboxMenuItem {
  class AccessibleAWTCheckboxMenuItem {
    int serialVersionUID;
  }
  int item_listeners;
  int state;
  int serialVersionUID;
  int next_chkmenuitem_number;
}
class CheckboxGroup {
  int selectedCheckbox;
  int serialVersionUID;
}
class Checkbox {
  class AccessibleAWTCheckbox {
    int serialVersionUID;
  }
  int next_checkbox_number;
  int item_listeners;
  int state;
  int label;
  int group;
  int serialVersionUID;
}
class CardLayout {
  int PREF;
  int MAX;
  int MIN;
  int PREV;
  int NEXT;
  int LAST;
  int FIRST;
  int tab;
  int vgap;
  int hgap;
  int serialVersionUID;
}
class Canvas {
  class CanvasFlipBufferStrategy {
  }
  class CanvasBltBufferStrategy {
  }
  class AccessibleAWTCanvas {
    int serialVersionUID;
  }
  int bufferStrategy;
  int next_canvas_number;
  int serialVersionUID;
}
class Button {
  class AccessibleAWTButton {
    int serialVersionUID;
  }
  int next_button_number;
  int action_listeners;
  int label;
  int actionCommand;
  int serialVersionUID;
}
class BufferCapabilities {
  int flip;
  int back;
  int front;
  class FlipContents {
    int COPIED;
    int PRIOR;
    int BACKGROUND;
    int UNDEFINED;
    int NAMES;
  }
}
class BorderLayout {
  int PREF;
  int MAX;
  int MIN;
  int vgap;
  int hgap;
  int lastItem;
  int firstItem;
  int lastLine;
  int firstLine;
  int center;
  int west;
  int east;
  int south;
  int north;
  int serialVersionUID;
  int LINE_END;
  int LINE_START;
  int PAGE_END;
  int PAGE_START;
  int AFTER_LINE_ENDS;
  int BEFORE_LINE_BEGINS;
  int AFTER_LAST_LINE;
  int BEFORE_FIRST_LINE;
  int CENTER;
  int WEST;
  int EAST;
  int SOUTH;
  int NORTH;
}
class BasicStroke {
  int end;
  int start;
  int phase;
  int dash;
  int limit;
  int join;
  int cap;
  int width;
  int CAP_SQUARE;
  int CAP_ROUND;
  int CAP_BUTT;
  int JOIN_BEVEL;
  int JOIN_ROUND;
  int JOIN_MITER;
}
class AttributeValue {
  int names;
  int value;
}
class AlphaComposite {
  int alpha;
  int rule;
  int Xor;
  int DstAtop;
  int SrcAtop;
  int DstOut;
  int SrcOut;
  int DstIn;
  int SrcIn;
  int DstOver;
  int SrcOver;
  int Dst;
  int Src;
  int Clear;
  int XOR;
  int DST_ATOP;
  int SRC_ATOP;
  int DST_OUT;
  int SRC_OUT;
  int DST_IN;
  int SRC_IN;
  int DST_OVER;
  int SRC_OVER;
  int DST;
  int SRC;
  int CLEAR;
  int cache;
}
class Adjustable {
  int NO_ORIENTATION;
  int VERTICAL;
  int HORIZONTAL;
}
class ActiveEvent {
}
class AWTPermission {
  int serialVersionUID;
}
class AWTKeyStroke {
  int onKeyRelease;
  int modifiers;
  int keyCode;
  int keyChar;
  int vktable;
  int ctor;
  int recent;
  int cache;
  int MODIFIERS_MASK;
  int serialVersionUID;
}
class AWTException {
  int serialVersionUID;
}
class AWTEventMulticaster {
  int b;
  int a;
}
class AWTEvent {
  int RESERVED_ID_MAX;
  int WINDOW_FOCUS_EVENT_MASK;
  int WINDOW_STATE_EVENT_MASK;
  int MOUSE_WHEEL_EVENT_MASK;
  int HIERARCHY_BOUNDS_EVENT_MASK;
  int HIERARCHY_EVENT_MASK;
  int INVOCATION_EVENT_MASK;
  int PAINT_EVENT_MASK;
  int INPUT_ENABLED_EVENT_MASK;
  int INPUT_METHOD_EVENT_MASK;
  int TEXT_EVENT_MASK;
  int ITEM_EVENT_MASK;
  int ADJUSTMENT_EVENT_MASK;
  int ACTION_EVENT_MASK;
  int WINDOW_EVENT_MASK;
  int MOUSE_MOTION_EVENT_MASK;
  int MOUSE_EVENT_MASK;
  int KEY_EVENT_MASK;
  int FOCUS_EVENT_MASK;
  int CONTAINER_EVENT_MASK;
  int COMPONENT_EVENT_MASK;
  int isFocusManagerEvent;
  int bdata;
  int queueNext;
  int consumed;
  int id;
  int serialVersionUID;
}
class AWTError {
  int serialVersionUID;
}
