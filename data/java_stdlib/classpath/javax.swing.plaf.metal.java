package javax.swing.plaf.metal;
class OceanTheme {
  int INACTIVE_CONTROL_TEXT;
  int SECONDARY3;
  int SECONDARY2;
  int SECONDARY1;
  int PRIMARY3;
  int PRIMARY2;
  int PRIMARY1;
  int BLACK;
}
class MetalUtils {
  int darkColor;
  int lightColor;
  int pattern2D;
}
class MetalTreeUI {
  int lineStyleListener;
  int lineStyle;
  int LINE_STYLE_HORIZONTAL;
  int LINE_STYLE_ANGLED;
  int LINE_STYLE_NONE;
  int LINE_STYLE_VALUE_HORIZONTAL;
  int LINE_STYLE_VALUE_ANGLED;
  int LINE_STYLE_VALUE_NONE;
  int LINE_STYLE_PROPERTY;
  class LineStyleListener {
  }
}
class MetalToolTipUI {
  int inactiveBorder;
  int activeBorder;
  int acceleratorForeground;
  int acceleratorFont;
  int acceleratorDelimiter;
  int acceleratorString;
  int isAcceleratorHidden;
  int instance;
  int padSpaceBetweenStrings;
}
class MetalToolBarUI {
  class MetalDockingListener {
  }
  int rolloverListener;
  int contListener;
  class MetalRolloverListener {
  }
  class MetalContainerListener {
  }
}
class MetalToggleButtonUI {
  int disabledTextColor;
  int selectColor;
  int focusColor;
}
class MetalTheme {
  int WHITE;
  int BLACK;
}
class MetalTextFieldUI {
}
class MetalTabbedPaneUI {
  int tabsOpaque;
  int hg;
  int tabAreaBackground;
  int selectHighlight;
  int selectColor;
  int minTabWidth;
  class TabbedPaneLayout {
  }
}
class MetalSplitPaneUI {
}
class MetalSplitPaneDivider {
  int orientation;
  int splitPane;
  int light;
  int dark;
  class MetalOneTouchButton {
    int direction;
    int colors;
    int RIGHT;
    int LEFT;
  }
  int BUTTON_SPRITE_R;
  int BUTTON_SPRITE_L;
}
class MetalSliderUI {
  int filledSlider;
  int SLIDER_FILL;
  int TICK_BUFFER;
  int vertThumbIcon;
  int horizThumbIcon;
  int tickLength;
  int trackWidth;
  int darkShadowColor;
  int highlightColor;
  int thumbColor;
  class MetalPropertyListener {
  }
}
class MetalSeparatorUI {
  int instance;
}
class MetalScrollPaneUI {
}
class MetalScrollButton {
  int freeStanding;
  int buttonWidth;
  int maximumSize;
}
class MetalScrollBarUI {
  int scrollBarShadowColor;
  int isFreeStanding;
  int scrollBarWidth;
  int decreaseButton;
  int increaseButton;
  int MIN_THUMB_SIZE_FREE_STANDING;
  int MIN_THUMB_SIZE;
  int FREE_STANDING_PROP;
  class MetalScrollBarPropertyChangeHandler {
  }
}
class MetalRootPaneUI {
  int instance;
  class MetalRootLayout {
    int titlePane;
    int prefSize;
    int titlePaneBounds;
    int menuBarBounds;
    int contentPaneBounds;
    int layeredPaneBounds;
    int glassPaneBounds;
  }
  class MetalTitlePane {
    int windowMenu;
    int menuBar;
    int maximizeAction;
    int iconifyAction;
    int closeAction;
    int title;
    int selectedTitleColor;
    int notSelectedTitleColor;
    int closeIcon;
    int iconIcon;
    int maxIcon;
    int minIcon;
    int maxButton;
    int iconButton;
    int closeButton;
    int rootPane;
    class MetalTitlePaneLayout {
    }
    class PaneButton {
    }
    class MaximizeAction {
    }
    class IconifyAction {
    }
    class CloseAction {
    }
    class MouseHandler {
      int lastDragLocation;
    }
  }
  class MetalFrameBorder {
  }
}
class MetalRadioButtonUI {
  int disabledTextColor;
  int selectColor;
  int focusColor;
}
class MetalProgressBarUI {
}
class MetalPopupMenuSeparatorUI {
  int instance;
}
class MetalMenuBarUI {
}
class MetalLookAndFeel {
  int theme;
  int serialVersionUID;
}
class MetalLabelUI {
  int metalLabelUI;
}
class MetalInternalFrameUI {
  int paletteListener;
  int IS_PALETTE;
}
class MetalInternalFrameTitlePane {
  int title;
  int paletteTitleHeight;
  int paletteCloseIcon;
  int isPalette;
  class MetalTitlePaneLayout {
  }
  class MetalInternalFrameTitlePanePropertyChangeHandler {
  }
}
class MetalIconFactory {
  int verticalSliderThumbIcon;
  int horizontalSliderThumbIcon;
  int treeHardDriveIcon;
  int treeFloppyDriveIcon;
  int treeComputerIcon;
  int internalFrameDefaultMenuIcon;
  int radioButtonMenuItemIcon;
  int radioButtonIcon;
  int fileChooserUpFolderIcon;
  int fileChooserNewFolderIcon;
  int fileChooserListViewIcon;
  int fileChooserHomeFolderIcon;
  int fileChooserDetailViewIcon;
  int checkBoxMenuItemIcon;
  int checkBoxIcon;
  class TreeComputerIcon {
  }
  class TreeFloppyDriveIcon {
  }
  class TreeHardDriveIcon {
  }
  class TreeLeafIcon {
  }
  class TreeFolderIcon {
  }
  class TreeControlIcon {
    int collapsed;
    int isLight;
  }
  class VerticalSliderThumbIcon {
    int gradientMask;
  }
  class InternalFrameMinimizeIcon {
  }
  class InternalFrameMaximizeIcon {
  }
  class InternalFrameAltMaximizeIcon {
    int size;
  }
  class InternalFrameDefaultMenuIcon {
  }
  class InternalFrameCloseIcon {
    int size;
  }
  class HorizontalSliderThumbIcon {
    int gradientMask;
  }
  class RadioButtonMenuItemIcon {
  }
  class RadioButtonIcon {
    int gradientMask;
  }
  class PaletteCloseIcon {
  }
  class FolderIcon16 {
  }
  class FileIcon16 {
  }
  class FileChooserUpFolderIcon {
  }
  class FileChooserNewFolderIcon {
  }
  class FileChooserListViewIcon {
  }
  class FileChooserHomeFolderIcon {
  }
  class FileChooserDetailViewIcon {
  }
  class CheckBoxMenuItemIcon {
  }
  int menuItemArrow;
  int menuArrow;
  int LIGHT;
  int DARK;
}
class MetalFileChooserUI {
  class ButtonLayout {
    int GAP;
  }
  class VerticalMidLayout {
  }
  int tableClickList;
  int singleClickList;
  int doubleClickList;
  int listSelList;
  int dirLabel;
  int look;
  int save;
  int scrollPane;
  int startEditing;
  int listView;
  int actionMap;
  int filterModel;
  int fileTablePanel;
  int fileListPanel;
  int fileTable;
  int fileList;
  int approveButton;
  int buttonPanel;
  int bottomPanel;
  int controls;
  int topPanel;
  int filterLabel;
  int fileTextField;
  int fileLabel;
  int directoryModel;
  int directoryComboBox;
  int directoryLabel;
  class TableClickListener {
    class EditingActionListener {
    }
    int editField;
    int editFile;
    int lastSelected;
    int fc;
    int table;
  }
  class SingleClickListener {
    class EditingActionListener {
    }
    int editField;
    int lastSelected;
    int fc;
    int editFile;
    int list;
  }
  class MetalFileChooserSelectionListener {
  }
  class FilterComboBoxRenderer {
  }
  class FilterComboBoxModel {
    int selected;
    int filters;
  }
  class FileRenderer {
  }
  class IndentIcon {
    int depth;
    int icon;
    int INDENT;
  }
  class DirectoryComboBoxRenderer {
    int indentIcon;
  }
  class DirectoryComboBoxAction {
  }
  class DirectoryComboBoxModel {
    int selectedIndex;
    int items;
  }
  class MetalFileChooserPropertyChangeListener {
  }
  class DetailViewActionListener {
  }
  class ListViewActionListener {
  }
  class TableFileRenderer {
  }
}
class MetalDesktopIconUI {
}
class MetalComboBoxUI {
  class MetalComboPopup {
  }
  class MetalPropertyChangeListener {
  }
  class MetalComboBoxLayoutManager {
  }
}
class MetalComboBoxIcon {
}
class MetalComboBoxEditor {
  int editorBorderInsets;
  class EditorTextField {
  }
  class UIResource {
  }
  class MetalComboBoxEditorBorder {
  }
}
class MetalComboBoxButton {
  int iconOnly;
  int comboIcon;
  int rendererPane;
  int listBox;
  int comboBox;
}
class MetalCheckBoxUI {
  int instance;
}
class MetalCheckBoxIcon {
  int border;
}
class MetalButtonUI {
  int disabledTextColor;
  int selectColor;
  int focusColor;
  int sharedUI;
}
class MetalButtonListener {
}
class MetalBorders {
  class TableHeaderBorder {
    int editorBorderInsets;
  }
  class ToolBarBorder {
  }
  class ToggleButtonBorder {
  }
  class PopupMenuBorder {
    int borderInsets;
  }
  class RolloverMarginBorder {
    int borderInsets;
  }
  class RolloverButtonBorder {
  }
  class ScrollPaneBorder {
    int insets;
  }
  class MenuBarBorder {
    int borderInsets;
  }
  class MenuItemBorder {
    int borderInsets;
  }
  class OptionDialogBorder {
  }
  class InternalFrameBorder {
    int borderInsets;
  }
  class TextFieldBorder {
  }
  class PaletteBorder {
    int borderInsets;
  }
  class Flush3DBorder {
    int borderInsets;
  }
  class DesktopIconBorder {
  }
  class ButtonBorder {
    int borderInsets;
  }
  int marginBorder;
  int rolloverBorder;
  int textBorder;
  int textFieldBorder;
  int toolbarButtonBorder;
  int desktopIconBorder;
  int toggleButtonBorder;
  int buttonBorder;
}
class DefaultMetalTheme {
  int MENU_TEXT_FONT;
  int CONTROL_TEXT_FONT;
  int BOLD_MENU_TEXT_FONT;
  int PLAIN_MENU_TEXT_FONT;
  int BOLD_CONTROL_TEXT_FONT;
  int PLAIN_CONTROL_TEXT_FONT;
  int WINDOW_TITLE_FONT;
  int USER_TEXT_FONT;
  int SYSTEM_TEXT_FONT;
  int SUB_TEXT_FONT;
  int SECONDARY3;
  int SECONDARY2;
  int SECONDARY1;
  int PRIMARY3;
  int PRIMARY2;
  int PRIMARY1;
}
