package javax.swing;
class WindowConstants {
  int EXIT_ON_CLOSE;
  int DISPOSE_ON_CLOSE;
  int HIDE_ON_CLOSE;
  int DO_NOTHING_ON_CLOSE;
}
class ViewportLayout {
  int serialVersionUID;
}
class UnsupportedLookAndFeelException {
}
class UIManager {
  int listeners;
  int lookAndFeelDefaults;
  int currentUIDefaults;
  int currentLookAndFeel;
  int auxLookAndFeels;
  int installed;
  int serialVersionUID;
  class MultiplexUIDefaults {
    int fallback;
    class MultiplexEnumeration {
      int i;
      int enums;
    }
  }
  class LookAndFeelInfo {
    int clazz;
    int name;
  }
}
class UIDefaults {
  int serialVersionUID;
  class ProxyLazyValue {
    int inner;
  }
  class LazyValue {
  }
  class LazyInputMap {
    int bind;
  }
  class ActiveValue {
  }
  int propertyChangeSupport;
  int defaultLocale;
  int bundles;
}
class TransferHandler {
  int recognizer;
  int propertyName;
  int visualRepresentation;
  int sourceActions;
  int pasteAction;
  int cutAction;
  int copyAction;
  int COPY_OR_MOVE;
  int MOVE;
  int COPY;
  int NONE;
  int COMMAND_PASTE;
  int COMMAND_CUT;
  int COMMAND_COPY;
  int serialVersionUID;
  class SwingDragHandler {
    int autoscrolls;
  }
  class SwingDragGestureRecognizer {
  }
  class TransferAction {
    int command;
  }
  class PropertyTransferable {
    int property;
    int component;
  }
}
class ToolTipManager {
  int popup;
  int currentPoint;
  int toolTipText;
  int currentTip;
  int currentComponent;
  int shared;
  int heavyWeightPopupEnabled;
  int lightWeightPopupEnabled;
  int enabled;
  int insideTimer;
  int exitTimer;
  int enterTimer;
  class insideTimerAction {
  }
  class outsideTimerAction {
  }
  class stillInsideTimerAction {
  }
}
class Timer {
  int queueLock;
  int queue;
  int task;
  int ticks;
  int initialDelay;
  int delay;
  int repeats;
  int coalesce;
  int listenerList;
  int logTimers;
  int timer;
  int drainer;
  int serialVersionUID;
  class Task {
  }
}
class SwingUtilities {
  class OwnerFrame {
  }
  int ownerFrame;
}
class SwingConstants {
  int PREVIOUS;
  int NEXT;
  int TRAILING;
  int LEADING;
  int VERTICAL;
  int HORIZONTAL;
  int NORTH_WEST;
  int WEST;
  int SOUTH_WEST;
  int SOUTH;
  int SOUTH_EAST;
  int EAST;
  int NORTH_EAST;
  int NORTH;
  int RIGHT;
  int BOTTOM;
  int LEFT;
  int TOP;
  int CENTER;
}
class SpringLayout {
  class DeferredHeight {
    int c;
  }
  class DeferredWidth {
    int c;
  }
  class DeferredDimension {
    int value;
  }
  class DeferredSpring {
    int c;
    int edgeName;
    int sl;
  }
  class Constraints {
    int h;
    int v;
    int south;
    int east;
    int width;
    int height;
    int y;
    int x;
  }
  int constraintsMap;
  int WEST;
  int SOUTH;
  int NORTH;
  int EAST;
}
class Spring {
  class MaxSpring {
    int value;
    int s2;
    int s1;
  }
  class MinusSpring {
    int s;
  }
  class AddSpring {
    int value;
    int s2;
    int s1;
  }
  class SimpleSpring {
    int value;
    int max;
    int pref;
    int min;
  }
  int UNSET;
}
class SpinnerNumberModel {
  int stepSize;
  int maximum;
  int minimum;
  int value;
  int serialVersionUID;
}
class SpinnerModel {
}
class SpinnerListModel {
  int index;
  int list;
  int serialVersionUID;
}
class SpinnerDateModel {
  int serialVersionUID;
  int calendarField;
  int end;
  int start;
  int date;
}
class SortingFocusTraversalPolicy {
  int implicitDownCycleTraversal;
  int comparator;
}
class SizeSequence {
  int sizes;
}
class SizeRequirements {
  int alignment;
  int maximum;
  int preferred;
  int minimum;
  int serialVersionUID;
}
class SingleSelectionModel {
}
class Scrollable {
}
class ScrollPaneLayout {
  int hsbPolicy;
  int vsbPolicy;
  int upperRight;
  int upperLeft;
  int lowerRight;
  int lowerLeft;
  int colHead;
  int rowHead;
  int hsb;
  int vsb;
  int viewport;
  class UIResource {
  }
  int serialVersionUID;
}
class ScrollPaneConstants {
  int HORIZONTAL_SCROLLBAR_ALWAYS;
  int HORIZONTAL_SCROLLBAR_NEVER;
  int HORIZONTAL_SCROLLBAR_AS_NEEDED;
  int VERTICAL_SCROLLBAR_ALWAYS;
  int VERTICAL_SCROLLBAR_NEVER;
  int VERTICAL_SCROLLBAR_AS_NEEDED;
  int HORIZONTAL_SCROLLBAR_POLICY;
  int VERTICAL_SCROLLBAR_POLICY;
  int UPPER_TRAILING_CORNER;
  int UPPER_LEADING_CORNER;
  int LOWER_TRAILING_CORNER;
  int LOWER_LEADING_CORNER;
  int UPPER_RIGHT_CORNER;
  int UPPER_LEFT_CORNER;
  int LOWER_RIGHT_CORNER;
  int LOWER_LEFT_CORNER;
  int COLUMN_HEADER;
  int ROW_HEADER;
  int HORIZONTAL_SCROLLBAR;
  int VERTICAL_SCROLLBAR;
  int VIEWPORT;
}
class RootPaneContainer {
}
class RepaintManager {
  int doubleBufferMaximumSize;
  int offscreenBuffers;
  int doubleBufferingEnabled;
  int invalidComponents;
  int repaintWorker;
  int dirtyComponentsWork;
  int dirtyComponents;
  class RepaintWorker {
    int live;
  }
  int rectCache;
  int currentRepaintManagers;
  class RepaintWorkerEvent {
  }
}
class Renderer {
}
class ProgressMonitorInputStream {
  int read;
  int monitor;
}
class ProgressMonitor {
  class TimerListener {
    int first;
    int lastProgress;
    int timestamp;
  }
  int canceled;
  int timer;
  int progressDialog;
  int noteLabel;
  int progressBar;
  int progress;
  int max;
  int min;
  int millisToPopup;
  int millisToDecideToPopup;
  int message;
  int note;
  int component;
  int accessibleContext;
}
class PopupFactory {
  int sharedFactory;
}
class Popup {
  class LightweightPopup {
    int layeredPane;
    int panel;
    int y;
    int x;
    int contents;
    int owner;
  }
  class JWindowPopup {
    int contents;
    int window;
  }
}
class OverlayLayout {
  int spansY;
  int spansX;
  int offsetsY;
  int offsetsX;
  int yTotal;
  int xTotal;
  int yChildren;
  int xChildren;
  int target;
  int serialVersionUID;
}
class MutableComboBoxModel {
}
class MenuSelectionManager {
  int selectedPath;
  int manager;
  int listenerList;
  int changeEvent;
}
class MenuElement {
}
class LookAndFeel {
}
class ListSelectionModel {
  int MULTIPLE_INTERVAL_SELECTION;
  int SINGLE_INTERVAL_SELECTION;
  int SINGLE_SELECTION;
}
class ListModel {
}
class ListCellRenderer {
}
class LayoutFocusTraversalPolicy {
  int serialVersionUID;
  class LayoutComparator {
  }
}
class KeyboardManager {
  int menuBarLookup;
  int topLevelLookup;
  int manager;
}
class KeyStroke {
  int serialVersionUID;
}
class JWindow {
  int accessibleContext;
  int rootPaneCheckingEnabled;
  int rootPane;
  int serialVersionUID;
  class AccessibleJWindow {
  }
}
class JViewport {
  int isPaintRoot;
  int sizeChanged;
  int damaged;
  int cachedBlitPaint;
  int cachedBlitSize;
  int cachedBlitTo;
  int cachedBlitFrom;
  int viewListener;
  int scrollMode;
  int changeEvent;
  int lastPaintPosition;
  int backingStoreImage;
  int backingStore;
  int isViewSizeSet;
  int scrollUnderway;
  int defaultScrollMode;
  int serialVersionUID;
  int BACKINGSTORE_SCROLL_MODE;
  int BLIT_SCROLL_MODE;
  int SIMPLE_SCROLL_MODE;
  class ViewListener {
    int serialVersionUID;
  }
  class AccessibleJViewport {
  }
}
class JTree {
  int clientShowsRootHandlesSet;
  int clientScrollsOnExpandSet;
  int clientRowHeightSet;
  int selectionRedirector;
  int treeModelListener;
  int visibleRowCount;
  int treeModel;
  int toggleClickCount;
  int showsRootHandles;
  int selectionModel;
  int scrollsOnExpand;
  int rowHeight;
  int rootVisible;
  int largeModel;
  int invokesStopCellEditing;
  int editable;
  int cellRenderer;
  int cellEditor;
  int nodeStates;
  int anchorSelectionPath;
  int expandsSelectedPaths;
  int dragEnabled;
  int COLLAPSED;
  int EXPANDED;
  int EXPANDS_SELECTED_PATHS_PROPERTY;
  int LEAD_SELECTION_PATH_PROPERTY;
  int ANCHOR_SELECTION_PATH_PROPERTY;
  int VISIBLE_ROW_COUNT_PROPERTY;
  int TREE_MODEL_PROPERTY;
  int TOGGLE_CLICK_COUNT_PROPERTY;
  int SHOWS_ROOT_HANDLES_PROPERTY;
  int SELECTION_MODEL_PROPERTY;
  int SCROLLS_ON_EXPAND_PROPERTY;
  int ROW_HEIGHT_PROPERTY;
  int ROOT_VISIBLE_PROPERTY;
  int LARGE_MODEL_PROPERTY;
  int INVOKES_STOP_CELL_EDITING_PROPERTY;
  int EDITABLE_PROPERTY;
  int CELL_RENDERER_PROPERTY;
  int CELL_EDITOR_PROPERTY;
  int serialVersionUID;
  class EmptySelectionModel {
    int sharedInstance;
    int serialVersionUID;
  }
  class TreeSelectionRedirector {
    int serialVersionUID;
  }
  class TreeModelHandler {
  }
  class DynamicUtilTreeNode {
    int hasChildren;
    int loadedChildren;
    int childValue;
  }
  class AccessibleJTree {
    class AccessibleJTreeNode {
      int cursor;
      int mod;
      int actionList;
      int selectionList;
      int states;
      int acc;
      int tp;
      int tree;
    }
  }
}
class JToolTip {
  int component;
  int text;
  class AccessibleJToolTip {
    int serialVersionUID;
  }
  int serialVersionUID;
}
class JToolBar {
  int orientation;
  int rollover;
  int floatable;
  int margin;
  int paintBorder;
  int serialVersionUID;
  class Separator {
    int serialVersionUID;
  }
  class DefaultToolBarLayout {
  }
  class AccessibleJToolBar {
    int serialVersionUID;
  }
}
class JToggleButton {
  int serialVersionUID;
  class ToggleButtonModel {
    int serialVersionUID;
  }
  class AccessibleJToggleButton {
    int serialVersionUID;
  }
}
class JTextPane {
}
class JTextField {
  int horizontalVisibility;
  int actionPropertyChangeListener;
  int actionCommand;
  int action;
  int align;
  int columns;
  int notifyAction;
  int actions;
  int serialVersionUID;
  class AccessibleJTextField {
    int serialVersionUID;
  }
}
class JTextArea {
  int wrapStyleWord;
  int tabSize;
  int lineWrap;
  int columns;
  int rows;
  int serialVersionUID;
  class AccessibleJTextArea {
  }
}
class JTable {
  int booleanInvertingEditor;
  int rowHeights;
  int clientRowHeightSet;
  int rectCache;
  int surrendersFocusOnKeystroke;
  int tableColumnPropertyChangeHandler;
  int tableHeader;
  int showVerticalLines;
  int showHorizontalLines;
  int SELECTION_FOREGROUND_CHANGED_PROPERTY;
  int selectionForeground;
  int SELECTION_BACKGROUND_CHANGED_PROPERTY;
  int selectionBackground;
  int preferredViewportSize;
  int gridColor;
  int dragEnabled;
  int cellEditor;
  int selectionModel;
  int columnModel;
  int dataModel;
  int cellSelectionEnabled;
  int rowSelectionAllowed;
  int rowMargin;
  int rowHeight;
  int autoResizeMode;
  int autoCreateColumnsFromModel;
  int editorComp;
  int editingRow;
  int editingColumn;
  int defaultRenderersByColumnClass;
  int defaultEditorsByColumnClass;
  int AUTO_RESIZE_LAST_COLUMN;
  int AUTO_RESIZE_ALL_COLUMNS;
  int AUTO_RESIZE_SUBSEQUENT_COLUMNS;
  int AUTO_RESIZE_NEXT_COLUMN;
  int AUTO_RESIZE_OFF;
  int this_table;
  int serialVersionUID;
  class TableTextField {
  }
  class IconCellRenderer {
  }
  class NumberCellRenderer {
  }
  class FloatCellRenderer {
  }
  class DoubleCellRenderer {
  }
  class DateCellRenderer {
  }
  class BooleanCellRenderer {
    int checkBox;
  }
  class TableColumnPropertyChangeHandler {
  }
  class AccessibleJTable {
    int columnDescriptions;
    int rowDescriptions;
    int summary;
    int caption;
    int lastSelectedColumn;
    int lastSelectedRow;
    class AccessibleJTableHeaderCell {
      int columnIndex;
      int header;
    }
    class AccessibleTableHeader {
      int header;
    }
    class AccessibleJTableModelChange {
      int lastColumn;
      int firstColumn;
      int lastRow;
      int firstRow;
      int type;
    }
    class AccessibleJTableCell {
      int index;
      int column;
      int row;
      int table;
    }
  }
}
class JTabbedPane {
  int tabs;
  int layoutPolicy;
  int tabPlacement;
  int WRAP_TAB_LAYOUT;
  int SCROLL_TAB_LAYOUT;
  int model;
  int changeListener;
  int changeEvent;
  int serialVersionUID;
  class Page {
    int serialVersionUID;
    int underlinedChar;
    int mnemonicKey;
    int fg;
    int bg;
    int title;
    int enabled;
    int disabledIcon;
    int icon;
    int component;
    int tip;
  }
  class ModelListener {
    int serialVersionUID;
  }
  class AccessibleJTabbedPane {
    int serialVersionUID;
  }
}
class JSplitPane {
  int clientOneTouchExpandableSet;
  int clientDividerSizeSet;
  int resizeWeight;
  int dividerLocation;
  int rightComponent;
  int leftComponent;
  int orientation;
  int lastDividerLocation;
  int dividerSize;
  int oneTouchExpandable;
  int continuousLayout;
  int VERTICAL_SPLIT;
  int TOP;
  int RIGHT;
  int RESIZE_WEIGHT_PROPERTY;
  int ORIENTATION_PROPERTY;
  int ONE_TOUCH_EXPANDABLE_PROPERTY;
  int LEFT;
  int LAST_DIVIDER_LOCATION_PROPERTY;
  int HORIZONTAL_SPLIT;
  int DIVIDER_SIZE_PROPERTY;
  int DIVIDER_LOCATION_PROPERTY;
  int DIVIDER;
  int CONTINUOUS_LAYOUT_PROPERTY;
  int BOTTOM;
  int serialVersionUID;
  class AccessibleJSplitPane {
    int serialVersionUID;
  }
}
class JSpinner {
  int serialVersionUID;
  int editor;
  int model;
  class ModelListener {
  }
  class DateEditorFormatter {
  }
  class DateEditor {
    int serialVersionUID;
  }
  class ListEditor {
  }
  class NumberEditorFormatter {
  }
  class NumberEditor {
    int serialVersionUID;
  }
  class DefaultEditor {
    int serialVersionUID;
    int ftf;
    int spinner;
  }
}
class JSlider {
  int changeEvent;
  int changeListener;
  int isInverted;
  int orientation;
  int snapToTicks;
  int minorTickSpacing;
  int majorTickSpacing;
  int sliderModel;
  int labelTable;
  int paintLabels;
  int paintTrack;
  int paintTicks;
  class AccessibleJSlider {
    int serialVersionUID;
  }
  int serialVersionUID;
  class LabelUIResource {
  }
}
class JSeparator {
  int orientation;
  int serialVersionUID;
  class AccessibleJSeparator {
    int serialVersionUID;
  }
}
class JScrollPane {
  class ScrollBar {
    int serialVersionUID;
  }
  int wheelScrollingEnabled;
  int viewportBorder;
  int viewport;
  int verticalScrollBarPolicy;
  int verticalScrollBar;
  int horizontalScrollBarPolicy;
  int horizontalScrollBar;
  int upperRight;
  int upperLeft;
  int lowerRight;
  int lowerLeft;
  int rowHeader;
  int columnHeader;
  int serialVersionUID;
  class AccessibleJScrollPane {
    int viewPort;
  }
}
class JScrollBar {
  int sbChangeListener;
  int unitIncrement;
  int orientation;
  int model;
  int blockIncrement;
  int serialVersionUID;
  class ScrollBarChangeListener {
  }
  class AccessibleJScrollBar {
    int serialVersionUID;
  }
}
class JRootPane {
  int windowDecorationStyle;
  int defaultReleaseAction;
  int defaultPressAction;
  int defaultButton;
  int contentPane;
  int menuBar;
  int layeredPane;
  int glassPane;
  int WARNING_DIALOG;
  int QUESTION_DIALOG;
  int FILE_CHOOSER_DIALOG;
  int COLOR_CHOOSER_DIALOG;
  int ERROR_DIALOG;
  int INFORMATION_DIALOG;
  int PLAIN_DIALOG;
  int FRAME;
  int NONE;
  int serialVersionUID;
  class RootLayout {
    int menuBarBounds;
    int contentPaneBounds;
    int layeredPaneBounds;
    int glassPaneBounds;
    int serialVersionUID;
  }
  class AccessibleJRootPane {
    int serialVersionUID;
  }
}
class JRadioButtonMenuItem {
  class AccessibleJRadioButtonMenuItem {
    int serialVersionUID;
  }
  int uiClassID;
  int serialVersionUID;
}
class JRadioButton {
  class AccessibleJRadioButton {
    int serialVersionUID;
  }
  int serialVersionUID;
}
class JProgressBar {
  int changeListener;
  int changeEvent;
  int paintString;
  int progressString;
  int model;
  int paintBorder;
  int orientation;
  int indeterminate;
  int serialVersionUID;
  class AccessibleJProgressBar {
    int serialVersionUID;
  }
}
class JPopupMenu {
  class ActionChangeListener {
  }
  class AccessibleJPopupMenu {
    int serialVersionUID;
  }
  class Separator {
  }
  int visible;
  int popupLocationY;
  int popupLocationX;
  int popup;
  int selectionModel;
  int lightWeightPopupEnabled;
  int margin;
  int label;
  int invoker;
  int DefaultLightWeightPopupEnabled;
  int borderPainted;
  int serialVersionUID;
}
class JPasswordField {
  int echoChar;
  class AccessibleJPasswordField {
    int serialVersionUID;
  }
}
class JPanel {
  class AccessibleJPanel {
  }
}
class JOptionPane {
  class ValuePropertyHandler {
    int dialog;
  }
  int privFrame;
  int wantsInput;
  int value;
  int selectionValues;
  int optionType;
  int options;
  int messageType;
  int message;
  int inputValue;
  int initialValue;
  int initialSelectionValue;
  int icon;
  int UNINITIALIZED_VALUE;
  int WANTS_INPUT_PROPERTY;
  int VALUE_PROPERTY;
  int SELECTION_VALUES_PROPERTY;
  int OPTIONS_PROPERTY;
  int OPTION_TYPE_PROPERTY;
  int MESSAGE_TYPE_PROPERTY;
  int MESSAGE_PROPERTY;
  int INPUT_VALUE_PROPERTY;
  int INITIAL_VALUE_PROPERTY;
  int INITIAL_SELECTION_VALUE_PROPERTY;
  int ICON_PROPERTY;
  int WARNING_MESSAGE;
  int QUESTION_MESSAGE;
  int PLAIN_MESSAGE;
  int INFORMATION_MESSAGE;
  int ERROR_MESSAGE;
  int YES_OPTION;
  int YES_NO_OPTION;
  int YES_NO_CANCEL_OPTION;
  int OK_OPTION;
  int OK_CANCEL_OPTION;
  int NO_OPTION;
  int DEFAULT_OPTION;
  int CLOSED_OPTION;
  int CANCEL_OPTION;
  int serialVersionUID;
  class AccessibleJOptionPane {
    int serialVersionUID;
  }
}
class JMenuItem {
  class AccessibleJMenuItem {
    int selected;
    int pressed;
    int focusOwner;
    int armed;
    int serialVersionUID;
  }
  int isDragging;
  int accelerator;
  int serialVersionUID;
}
class JMenuBar {
  int margin;
  int borderPainted;
  int selectionModel;
  int serialVersionUID;
  class AccessibleJMenuBar {
  }
}
class JMenu {
  class ActionChangedListener {
    int menuItem;
  }
  class WinListener {
    int serialVersionUID;
  }
  class AccessibleJMenu {
    int serialVersionUID;
  }
  int menuChangeListener;
  int menuLocation;
  int popupListener;
  int delay;
  int menuEvent;
  int popupMenu;
  int serialVersionUID;
  class MenuChangeListener {
    int selected;
  }
}
class JList {
  int listListener;
  class ListListener {
  }
  int visibleRowCount;
  int selectionModel;
  int selectionForeground;
  int selectionBackground;
  int prototypeCellValue;
  int model;
  int layoutOrientation;
  int fixedCellHeight;
  int fixedCellWidth;
  int cellRenderer;
  int dragEnabled;
  int HORIZONTAL_WRAP;
  int VERTICAL_WRAP;
  int VERTICAL;
  int serialVersionUID;
  class AccessibleJList {
    class AccessibleJListChild {
      int cursor;
      int listIndex;
      int parent;
    }
  }
}
class JLayeredPane {
  int componentToLayer;
  int DRAG_LAYER;
  int POPUP_LAYER;
  int MODAL_LAYER;
  int PALETTE_LAYER;
  int DEFAULT_LAYER;
  int FRAME_CONTENT_LAYER;
  int LAYER_PROPERTY;
  int serialVersionUID;
  class AccessibleJLayeredPane {
  }
}
class JLabel {
  int iconTextGap;
  int displayedMnemonicIndex;
  int displayedMnemonic;
  int disabledIcon;
  int icon;
  int verticalTextPosition;
  int verticalAlignment;
  int horizontalTextPosition;
  int horizontalAlignment;
  int text;
  int labelFor;
  int LABEL_PROPERTY;
  int serialVersionUID;
  class AccessibleJLabel {
  }
}
class JInternalFrame {
  int wasIcon;
  int isFirstTimeVisible;
  int defaultCloseOperation;
  int defaultFocus;
  int storedBounds;
  int title;
  int rootPane;
  int frameIcon;
  int desktopIcon;
  int resizable;
  int rootPaneCheckingEnabled;
  int maximizable;
  int isSelected;
  int isMaximum;
  int isIcon;
  int isClosed;
  int iconable;
  int closable;
  int TITLE_PROPERTY;
  int ROOT_PANE_PROPERTY;
  int MENU_BAR_PROPERTY;
  int LAYERED_PANE_PROPERTY;
  int IS_SELECTED_PROPERTY;
  int IS_MAXIMUM_PROPERTY;
  int IS_ICON_PROPERTY;
  int IS_CLOSED_PROPERTY;
  int GLASS_PANE_PROPERTY;
  int FRAME_ICON_PROPERTY;
  int CONTENT_PANE_PROPERTY;
  class JDesktopIcon {
    int frame;
    int serialVersionUID;
    class AccessibleJDesktopIcon {
      int serialVersionUID;
    }
  }
  class AccessibleJInternalFrame {
    int serialVersionUID;
  }
  int serialVersionUID;
}
class JFrame {
  int rootPaneCheckingEnabled;
  int rootPane;
  int accessibleContext;
  int closeAction;
  int defaultLookAndFeelDecorated;
  int serialVersionUID;
  int EXIT_ON_CLOSE;
  class AccessibleJFrame {
  }
}
class JFormattedTextField {
  int editValid;
  int formatter;
  int formatterFactory;
  int focusLostBehavior;
  int value;
  int PERSIST;
  int REVERT;
  int COMMIT_OR_REVERT;
  int COMMIT;
  class AbstractFormatterFactory {
  }
  class AbstractFormatter {
    int textField;
    int serialVersionUID;
  }
  int serialVersionUID;
}
class JFileChooser {
  class AccessibleJFileChooser {
  }
  int dragEnabled;
  int selectedFile;
  int selectedFiles;
  int currentFilter;
  int currentDir;
  int controlButtonsShown;
  int fv;
  int fileSelectionMode;
  int fileHiding;
  int multiSelection;
  int retval;
  int dialogType;
  int dialogTitle;
  int isAcceptAll;
  int choosableFilters;
  int approveButtonToolTipText;
  int approveButtonText;
  int approveButtonMnemonic;
  int accessory;
  int fsv;
  int accessibleContext;
  int CHOOSABLE_FILE_FILTER_CHANGED_PROPERTY;
  int DIALOG_TYPE_CHANGED_PROPERTY;
  int DIALOG_TITLE_CHANGED_PROPERTY;
  int ACCEPT_ALL_FILE_FILTER_USED_CHANGED_PROPERTY;
  int ACCESSORY_CHANGED_PROPERTY;
  int FILE_SELECTION_MODE_CHANGED_PROPERTY;
  int FILE_FILTER_CHANGED_PROPERTY;
  int FILE_HIDING_CHANGED_PROPERTY;
  int FILE_VIEW_CHANGED_PROPERTY;
  int FILE_SYSTEM_VIEW_CHANGED_PROPERTY;
  int MULTI_SELECTION_ENABLED_CHANGED_PROPERTY;
  int SELECTED_FILES_CHANGED_PROPERTY;
  int SELECTED_FILE_CHANGED_PROPERTY;
  int DIRECTORY_CHANGED_PROPERTY;
  int CONTROL_BUTTONS_ARE_SHOWN_CHANGED_PROPERTY;
  int APPROVE_BUTTON_MNEMONIC_CHANGED_PROPERTY;
  int APPROVE_BUTTON_TOOL_TIP_TEXT_CHANGED_PROPERTY;
  int APPROVE_BUTTON_TEXT_CHANGED_PROPERTY;
  int APPROVE_SELECTION;
  int CANCEL_SELECTION;
  int FILES_AND_DIRECTORIES;
  int DIRECTORIES_ONLY;
  int FILES_ONLY;
  int ERROR_OPTION;
  int APPROVE_OPTION;
  int CANCEL_OPTION;
  int CUSTOM_DIALOG;
  int SAVE_DIALOG;
  int OPEN_DIALOG;
  int serialVersionUID;
}
class JEditorPane {
  int loader;
  int editorMap;
  int registerMap;
  int editorKits;
  int focus_root;
  int editorKit;
  int serialVersionUID;
  class PageLoader {
    int page;
    int old;
    int in;
    int doc;
  }
  class PageStream {
    int cancelled;
  }
  class PlainEditorKit {
  }
  class EditorKitMapping {
    int classLoader;
    int className;
  }
  class JEditorPaneAccessibleHypertextSupport {
    class HTMLLink {
      int element;
    }
  }
  class AccessibleJEditorPaneHTML {
  }
  class AccessibleJEditorPane {
  }
}
class JDialog {
  int decorated;
  int closeAction;
  int rootPaneCheckingEnabled;
  int rootPane;
  int accessibleContext;
  int serialVersionUID;
  class AccessibleJDialog {
  }
}
class JDesktopPane {
  class AccessibleJDesktopPane {
    int serialVersionUID;
  }
  int clientDragModeSet;
  int dragMode;
  int desktopManager;
  int selectedFrame;
  int OUTLINE_DRAG_MODE;
  int LIVE_DRAG_MODE;
  int serialVersionUID;
}
class JComponent {
  class ActionListenerProxy {
    int bindingCommandName;
    int target;
  }
  int clientAutoscrollsSet;
  int clientOpaqueSet;
  int paintChild;
  int WHEN_IN_FOCUSED_WINDOW;
  int WHEN_ANCESTOR_OF_FOCUSED_COMPONENT;
  int WHEN_FOCUSED;
  int UNDEFINED_CONDITION;
  int TOOL_TIP_TEXT_KEY;
  int defaultLocale;
  int rectCache;
  int dragBufferInitialized;
  int dragBuffer;
  int paintingTile;
  int transferHandler;
  int inputVerifier;
  int verifyInputWhenFocusTarget;
  int actionMap;
  int inputMap_whenInFocusedWindow;
  int inputMap_whenAncestorOfFocused;
  int inputMap_whenFocused;
  int clientProperties;
  int vetoableChangeSupport;
  int listenerList;
  int isRepainting;
  int paintingDoubleBuffered;
  int autoscrolls;
  int requestFocusEnabled;
  int ui;
  int opaque;
  int debugGraphicsOptions;
  int doubleBuffered;
  int inheritsPopupMenu;
  int componentPopupMenu;
  int border;
  int alignmentY;
  int alignmentX;
  class AccessibleJComponent {
    int accessibleFocusHandler;
    int accessibleContainerHandler;
    int serialVersionUID;
    class AccessibleContainerHandler {
    }
    class AccessibleFocusHandler {
    }
  }
  int accessibleContext;
  int serialVersionUID;
}
class JComboBox {
  class DefaultKeySelectionManager {
  }
  class AccessibleJComboBox {
    int serialVersionUID;
  }
  int prototypeDisplayValue;
  int action;
  int lightWeightPopupEnabled;
  int actionCommand;
  int keySelectionManager;
  int selectedItemReminder;
  int isEditable;
  int maximumRowCount;
  int editor;
  int renderer;
  int dataModel;
  int DEFAULT_MAXIMUM_ROW_COUNT;
  class KeySelectionManager {
  }
  int serialVersionUID;
}
class JColorChooser {
  class DefaultResetListener {
    int init;
    int chooser;
  }
  class DefaultOKCancelListener {
    int dialog;
  }
  int accessibleContext;
  int CHOOSER_PANELS_PROPERTY;
  int PREVIEW_PANEL_PROPERTY;
  int SELECTION_MODEL_PROPERTY;
  int dragEnabled;
  int chooserPanels;
  int previewPanel;
  int selectionModel;
  class AccessibleJColorChooser {
    int serialVersionUID;
  }
  int serialVersionUID;
}
class JCheckBoxMenuItem {
  class AccessibleJCheckBoxMenuItem {
    int serialVersionUID;
  }
  int selectedObjects;
  int state;
  int uiClassID;
  int serialVersionUID;
}
class JCheckBox {
  int borderPaintedFlat;
  int BORDER_PAINTED_FLAT_CHANGED_PROPERTY;
  int serialVersionUID;
  class AccessibleJCheckBox {
  }
}
class JButton {
  int defaultCapable;
  int serialVersionUID;
  class AccessibleJButton {
  }
}
class JApplet {
  int rootPaneCheckingEnabled;
  int rootPane;
  int serialVersionUID;
  int accessibleContext;
  class AccessibleJApplet {
  }
}
class InternalFrameFocusTraversalPolicy {
}
class InputVerifier {
}
class InputMap {
  int parent;
  int inputMap;
  int serialVersionUID;
}
class ImageIcon {
  int accessibleContext;
  int loadStatus;
  int observer;
  int description;
  int image;
  int id;
  int tracker;
  int component;
  int serialVersionUID;
  class AccessibleImageIcon {
    int serialVersionUID;
  }
}
class Icon {
}
class GrayFilter {
  int p;
  int b;
}
class FocusManager {
  int FOCUS_MANAGER_CLASS_PROPERTY;
  class WrappingFocusManager {
    int wrapped;
  }
}
class DesktopManager {
}
class DefaultSingleSelectionModel {
  int index;
  int listenerList;
  int changeEvent;
  int serialVersionUID;
}
class DefaultListSelectionModel {
  int setLeadCalledFromAdd;
  int oldSel;
  int sel;
  int valueIsAdjusting;
  int leadAnchorNotificationEnabled;
  int anchorSelectionIndex;
  int leadSelectionIndex;
  int selectionMode;
  int listenerList;
  int serialVersionUID;
}
class DefaultListModel {
  int elements;
  int serialVersionUID;
}
class DefaultListCellRenderer {
  int noFocusBorder;
  class UIResource {
  }
  int serialVersionUID;
}
class DefaultFocusManager {
  int historyStack;
}
class DefaultDesktopManager {
  int iconRects;
  int pane;
  int dragCache;
  int currentDragMode;
  int WAS_ICON_ONCE_PROPERTY;
  int serialVersionUID;
}
class DefaultComboBoxModel {
  int selectedItem;
  int list;
  int serialVersionUID;
}
class DefaultCellEditor {
  int clickCountToStart;
  int delegate;
  int editorComponent;
  class JCheckBoxDelegate {
    int serialVersionUID;
  }
  class JComboBoxDelegate {
    int serialVersionUID;
  }
  class JTextFieldDelegate {
    int serialVersionUID;
  }
  class EditorDelegate {
    int value;
    int serialVersionUID;
  }
  int serialVersionUID;
}
class DefaultButtonModel {
  int actionCommand;
  int mnemonic;
  int group;
  int changeEvent;
  int listenerList;
  int stateMask;
  int SELECTED;
  int ROLLOVER;
  int PRESSED;
  int ENABLED;
  int ARMED;
  int serialVersionUID;
}
class DefaultBoundedRangeModel {
  int isAdjusting;
  int maximum;
  int minimum;
  int extent;
  int value;
  int listenerList;
  int changeEvent;
  int serialVersionUID;
}
class DebugGraphics {
  int yOffset;
  int xOffset;
  int graphicsID;
  int debugOptions;
  int buffer;
  int graphics;
  int counter;
  int debugLogStream;
  int debugFlashTime;
  int debugFlashCount;
  int debugFlashColor;
  int NONE_OPTION;
  int BUFFERED_OPTION;
  int FLASH_OPTION;
  int LOG_OPTION;
}
class ComponentInputMap {
  int component;
}
class CompatibilityFocusTraversalPolicy {
  int backward;
  int forward;
  int fallback;
}
class ComboBoxModel {
}
class ComboBoxEditor {
}
class CellRendererPane {
  int accessibleContext;
  class AccessibleCellRendererPane {
    int serialVersionUID;
  }
  int serialVersionUID;
}
class CellEditor {
}
class ButtonModel {
}
class ButtonGroup {
  int sel;
  int buttons;
  int serialVersionUID;
}
class BoxLayout {
  int spansY;
  int spansX;
  int offsetsY;
  int offsetsX;
  int yTotal;
  int xTotal;
  int yChildren;
  int xChildren;
  int way;
  int container;
  int serialVersionUID;
  int PAGE_AXIS;
  int LINE_AXIS;
  int Y_AXIS;
  int X_AXIS;
}
class Box {
  class Filler {
    int max;
    int pref;
    int min;
    class AccessibleBoxFiller {
      int serialVersionUID;
    }
    int serialVersionUID;
  }
  class AccessibleBox {
    int serialVersionUID;
  }
  int serialVersionUID;
}
class BoundedRangeModel {
}
class BorderFactory {
}
class ActionMap {
  int parent;
  int actionMap;
  int serialVersionUID;
}
class Action {
  int MNEMONIC_KEY;
  int ACTION_COMMAND_KEY;
  int ACCELERATOR_KEY;
  int SMALL_ICON;
  int SHORT_DESCRIPTION;
  int NAME;
  int LONG_DESCRIPTION;
  int DEFAULT;
}
class AbstractSpinnerModel {
  int listenerList;
  int changeEvent;
}
class AbstractListModel {
  int listenerList;
  int serialVersionUID;
}
class AbstractCellEditor {
  int changeEvent;
  int listenerList;
  int serialVersionUID;
}
class AbstractButton {
  class AccessibleAbstractButton {
    int serialVersionUID;
  }
  int VERTICAL_TEXT_POSITION_CHANGED_PROPERTY;
  int VERTICAL_ALIGNMENT_CHANGED_PROPERTY;
  int TEXT_CHANGED_PROPERTY;
  int SELECTED_ICON_CHANGED_PROPERTY;
  int ROLLOVER_SELECTED_ICON_CHANGED_PROPERTY;
  int ROLLOVER_ICON_CHANGED_PROPERTY;
  int ROLLOVER_ENABLED_CHANGED_PROPERTY;
  int PRESSED_ICON_CHANGED_PROPERTY;
  int MODEL_CHANGED_PROPERTY;
  int MNEMONIC_CHANGED_PROPERTY;
  int MARGIN_CHANGED_PROPERTY;
  int ICON_CHANGED_PROPERTY;
  int HORIZONTAL_TEXT_POSITION_CHANGED_PROPERTY;
  int HORIZONTAL_ALIGNMENT_CHANGED_PROPERTY;
  int FOCUS_PAINTED_CHANGED_PROPERTY;
  int DISABLED_SELECTED_ICON_CHANGED_PROPERTY;
  int DISABLED_ICON_CHANGED_PROPERTY;
  int CONTENT_AREA_FILLED_CHANGED_PROPERTY;
  int BORDER_PAINTED_CHANGED_PROPERTY;
  int clientContentAreaFilledSet;
  int clientIconTextGapSet;
  int clientRolloverEnabledSet;
  int clientBorderPaintedSet;
  int changeEvent;
  int actionPropertyChangeListener;
  int multiClickThreshhold;
  int eventHandler;
  int changeListener;
  int itemListener;
  int actionListener;
  int mnemonicIndex;
  int margin;
  int model;
  int action;
  int rollOverEnabled;
  int contentAreaFilled;
  int focusPainted;
  int borderPainted;
  int verticalTextPosition;
  int horizontalTextPosition;
  int horizontalAlignment;
  int verticalAlignment;
  int iconTextGap;
  int text;
  int current_icon;
  int rolloverSelectedIcon;
  int rolloverIcon;
  int disabledSelectedIcon;
  int selectedIcon;
  int disabledIcon;
  int pressed_icon;
  int default_icon;
  class EventHandler {
  }
  class ButtonChangeListener {
    int serialVersionUID;
  }
  int serialVersionUID;
}
class AbstractAction {
  int store;
  int changeSupport;
  int enabled;
  int serialVersionUID;
}
