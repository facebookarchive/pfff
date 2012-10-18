package javax.swing.text;
class ZoneView {
  int loadedZones;
  int maxZonesLoaded;
  int maximumZoneSize;
  class Zone {
    int p1;
    int p0;
  }
}
class WrappedPlainView {
  class WrappedLine {
    int numLines;
  }
  class WrappedLineCreator {
  }
  int lineBuffer;
  int tabSize;
  int tabBase;
  int lineHeight;
  int selectionEnd;
  int selectionStart;
  int viewFactory;
  int wordWrap;
  int metrics;
  int disabledColor;
  int unselectedColor;
  int selectedColor;
}
class ViewFactory {
}
class View {
  int parent;
  int elt;
  int Y_AXIS;
  int X_AXIS;
  int GoodBreakWeight;
  int ForcedBreakWeight;
  int ExcellentBreakWeight;
  int BadBreakWeight;
}
class Utilities {
}
class TextAction {
  class VerticalMovementAction {
    int dir;
  }
  class HorizontalMovementAction {
    int dir;
  }
}
class TableView {
  int columnRequirements;
  int columnSpans;
  int columnOffsets;
  class TableCell {
    int column;
    int row;
  }
  class TableRow {
  }
}
class TabableView {
}
class TabStop {
  int leader;
  int align;
  int pos;
  int LEAD_EQUALS;
  int LEAD_THICKLINE;
  int LEAD_UNDERLINE;
  int LEAD_HYPHENS;
  int LEAD_DOTS;
  int LEAD_NONE;
  int ALIGN_BAR;
  int ALIGN_DECIMAL;
  int ALIGN_CENTER;
  int ALIGN_RIGHT;
  int ALIGN_LEFT;
  int serialVersionUID;
}
class TabSet {
  int tabs;
  int serialVersionUID;
}
class TabExpander {
}
class StyledEditorKit {
  int viewFactory;
  int caretTracker;
  int inputAttributes;
  int currentRun;
  class CaretTracker {
  }
  class StyledViewFactory {
  }
  class StyledTextAction {
  }
  class FontFamilyAction {
    int family;
  }
  class FontSizeAction {
    int size;
  }
  class ForegroundAction {
    int fg;
  }
  class AlignmentAction {
    int a;
  }
  class BoldAction {
  }
  class ItalicAction {
  }
  class UnderlineAction {
  }
  int serialVersionUID;
}
class StyledDocument {
}
class StyleContext {
  class SimpleFontSpec {
    int size;
    int style;
    int family;
  }
  int attributeSetPool;
  int search;
  int styles;
  int readAttributeKeys;
  int writeAttributeKeys;
  int compressionThreshold;
  int defaultStyleContext;
  int sharedFonts;
  int sharedAttributeSets;
  int DEFAULT_STYLE;
  class SmallAttributeSet {
    int resolveParent;
    int attrs;
  }
  class NamedStyle {
    int attributes;
    int listenerList;
    int changeEvent;
    int serialVersionUID;
  }
  int serialVersionUID;
}
class StyleConstants {
  class ParagraphConstants {
    int TabSet;
    int SpaceBelow;
    int SpaceAbove;
    int RightIndent;
    int Orientation;
    int LineSpacing;
    int LeftIndent;
    int FirstLineIndent;
    int Alignment;
  }
  class FontConstants {
    int Size;
    int Italic;
    int Family;
    int Bold;
  }
  class ColorConstants {
    int Background;
    int Foreground;
  }
  class CharacterConstants {
    int Underline;
    int Superscript;
    int Subscript;
    int StrikeThrough;
    int Italic;
    int IconAttribute;
    int Foreground;
    int Size;
    int Family;
    int ComponentAttribute;
    int Bold;
    int BidiLevel;
    int Background;
  }
  int keyname;
  int keys;
  int ResolveAttribute;
  int NameAttribute;
  int ModelAttribute;
  int ComposedTextAttribute;
  int IconElementName;
  int ComponentElementName;
  int TabSet;
  int SpaceBelow;
  int SpaceAbove;
  int RightIndent;
  int Orientation;
  int LineSpacing;
  int LeftIndent;
  int FirstLineIndent;
  int Alignment;
  int Underline;
  int Superscript;
  int Subscript;
  int StrikeThrough;
  int Size;
  int Italic;
  int IconAttribute;
  int Foreground;
  int FontSize;
  int FontFamily;
  int Family;
  int ComponentAttribute;
  int Bold;
  int BidiLevel;
  int Background;
  int ALIGN_JUSTIFIED;
  int ALIGN_RIGHT;
  int ALIGN_CENTER;
  int ALIGN_LEFT;
}
class Style {
}
class StringContent {
  int queueOfDeath;
  int EMPTY;
  class StickyPosition {
    int mark;
  }
  class RemoveUndo {
    int positions;
    int undoString;
    int len;
    int start;
  }
  class InsertUndo {
    int positions;
    int redoContent;
    int length;
    int start;
  }
  int marks;
  int count;
  int content;
  int serialVersionUID;
  class Mark {
    int refCount;
    int mark;
  }
  class UndoPosRef {
    int undoOffset;
    int mark;
  }
}
class SimpleAttributeSet {
  int tab;
  int EMPTY;
  int serialVersionUID;
}
class Segment {
  int offset;
  int count;
  int array;
  int current;
  int partialReturn;
}
class Position {
  class Bias {
    int name;
    int Forward;
    int Backward;
  }
}
class PlainView {
  int tabSize;
  int tabBase;
  int lineBuffer;
  int metrics;
  int longestLine;
  int maxLineLength;
  int font;
  int selectionEnd;
  int selectionStart;
  int disabledColor;
  int unselectedColor;
  int selectedColor;
}
class PlainDocument {
  int rootElement;
  int tabSizeAttribute;
  int lineLimitAttribute;
  int serialVersionUID;
}
class PasswordView {
  int oneCharBuffer;
}
class ParagraphView {
  int tabSet;
  int lineSpacing;
  int justification;
  int firstLineIndent;
  class Row {
  }
}
class NumberFormatter {
}
class NavigationFilter {
  class FilterBypass {
  }
}
class MutableAttributeSet {
}
class MaskFormatter {
  int maskLength;
  int hexString;
  int valueContainsLiteralCharacters;
  int placeHolderChar;
  int placeHolder;
  int validChars;
  int invalidChars;
  int mask;
  int HEX_CHAR;
  int ANYTHING_CHAR;
  int LETTER_CHAR;
  int ALPHANUM_CHAR;
  int LOWERCASE_CHAR;
  int UPPERCASE_CHAR;
  int ESCAPE_CHAR;
  int NUM_CHAR;
}
class LayoutQueue {
  int list;
  int defaultQueue;
}
class LayeredHighlighter {
  class LayerPainter {
  }
}
class LabelView {
  int valid;
  int superscript;
  int subscript;
  int underline;
  int strikeThrough;
  int font;
  int foreground;
  int background;
}
class Keymap {
}
class JTextComponent {
  int dragEnabled;
  int margin;
  int selectionColor;
  int selectedTextColor;
  int disabledTextColor;
  int caretColor;
  int highlighter;
  int editable;
  int caret;
  int doc;
  int navigationFilter;
  int focusAccelerator;
  int keymap;
  int keymaps;
  int defaultTransferHandler;
  int FOCUS_ACCELERATOR_KEY;
  int DEFAULT_KEYMAP;
  int serialVersionUID;
  class DefaultTransferHandler {
  }
  class DefaultKeymap {
    int defaultAction;
    int map;
    int parent;
    int name;
  }
  class KeymapActionMap {
    int map;
  }
  class KeymapWrapper {
    int map;
  }
  class KeyBinding {
    int actionName;
    int key;
  }
  class AccessibleJTextComponent {
    int caretDot;
    int serialVersionUID;
  }
}
class InternationalFormatter {
  int maximum;
  int minimum;
  int format;
  int serialVersionUID;
}
class IconView {
}
class Highlighter {
  class HighlightPainter {
  }
  class Highlight {
  }
}
class GlyphView {
  int cached;
  int tabExpander;
  int tabX;
  int length;
  int offset;
  int glyphPainter;
  class DefaultGlyphPainter {
    int fontMetrics;
  }
  class J2DGlyphPainter {
    int textLayout;
  }
  class GlyphPainter {
  }
}
class GapContent {
  int queueOfDeath;
  int searchMark;
  int garbageMarks;
  int marks;
  int gapEnd;
  int gapStart;
  int buffer;
  int DEFAULT_BUFSIZE;
  int serialVersionUID;
  class UndoRemove {
    int positions;
    int text;
    int where;
  }
  class InsertUndo {
    int positions;
    int text;
    int length;
    int where;
  }
  class UndoPosRef {
    int undoOffset;
    int mark;
  }
  class Mark {
    int mark;
  }
  class GapContentPosition {
    int mark;
  }
}
class FlowView {
  int strategy;
  int layoutPool;
  int layoutSpan;
  int sharedStrategy;
  class LogicalView {
  }
  class FlowStrategy {
  }
}
class FieldView {
  int cachedSpan;
  int horizontalVisibility;
}
class EmptyAttributeSet {
}
class ElementIterator {
  int stack;
  int root;
  class ElementRef {
    int index;
    int element;
  }
}
class Element {
}
class EditorKit {
  int serialVersionUID;
}
class DocumentFilter {
  class FilterBypass {
  }
}
class Document {
  int TitleProperty;
  int StreamDescriptionProperty;
}
class DefaultTextUI {
}
class DefaultStyledDocument {
  int styleChangeListener;
  int buffer;
  int BUFFER_SIZE_DEFAULT;
  int serialVersionUID;
  class StyleChangeListener {
  }
  class SectionElement {
  }
  class ElementBuffer {
    int offsetLastIndexReplace;
    int offsetLastIndex;
    int edits;
    int recreateLeafs;
    int insertPath;
    int elementStack;
    int createdFracture;
    int fracturedChild;
    int fracturedParent;
    int pos;
    int length;
    int endOffset;
    int offset;
    int root;
    int serialVersionUID;
    class Edit {
      int isFracture;
      int added;
      int removed;
      int index;
      int e;
    }
  }
  class ElementSpec {
    int attributes;
    int content;
    int length;
    int offset;
    int direction;
    int type;
    int JoinFractureDirection;
    int OriginateDirection;
    int JoinNextDirection;
    int JoinPreviousDirection;
    int ContentType;
    int EndTagType;
    int StartTagType;
  }
  class AttributeUndoableEdit {
    int element;
    int isReplacing;
    int newAttributes;
    int copy;
  }
}
class DefaultHighlighter {
  int drawsLayeredHighlights;
  int highlights;
  int textComponent;
  int DefaultPainter;
  class LayerHighlightEntry {
    int paintRect;
  }
  class HighlightEntry {
    int painter;
    int p1;
    int p0;
  }
  class DefaultHighlightPainter {
    int color;
  }
}
class DefaultFormatterFactory {
  int nullFormatter;
  int displayFormatter;
  int editFormatter;
  int defaultFormatter;
}
class DefaultFormatter {
  int valueClass;
  int allowsInvalid;
  int overwriteMode;
  int commitsOnValidEdit;
  int serialVersionUID;
  class FormatterDocumentFilter {
  }
}
class DefaultEditorKit {
  int defaultActions;
  int writableAction;
  int upAction;
  int selectWordAction;
  int selectParagraphAction;
  int selectLineAction;
  int selectionUpAction;
  int selectionPreviousWordAction;
  int selectionNextWordAction;
  int selectionForwardAction;
  int selectionEndWordAction;
  int selectionEndParagraphAction;
  int selectionEndLineAction;
  int selectionEndAction;
  int selectionDownAction;
  int selectionBeginWordAction;
  int selectionBeginParagraphAction;
  int selectionBeginLineAction;
  int selectionBeginAction;
  int selectionBackwardAction;
  int selectAllAction;
  int readOnlyAction;
  int previousWordAction;
  int pasteAction;
  int pageUpAction;
  int pageDownAction;
  int nextWordAction;
  int insertTabAction;
  int insertContentAction;
  int insertBreakAction;
  int forwardAction;
  int endWordAction;
  int endParagraphAction;
  int EndOfLineStringProperty;
  int endLineAction;
  int endAction;
  int downAction;
  int deletePrevCharAction;
  int deleteNextCharAction;
  int defaultKeyTypedAction;
  int cutAction;
  int copyAction;
  int beginWordAction;
  int beginParagraphAction;
  int beginLineAction;
  int beginAction;
  int beepAction;
  int backwardAction;
  int serialVersionUID;
  class InsertTabAction {
  }
  class InsertContentAction {
  }
  class InsertBreakAction {
  }
  class DefaultKeyTypedAction {
  }
  class PasteAction {
  }
  class CutAction {
  }
  class CopyAction {
  }
  class BeepAction {
  }
  class EndAction {
  }
  class BeginAction {
  }
  class BeginLineAction {
  }
  class EndLineAction {
  }
  class DeleteNextCharAction {
  }
  class DeletePrevCharAction {
  }
  class BackwardAction {
  }
  class ForwardAction {
  }
  class UpAction {
  }
  class DownAction {
  }
  class SelectionBackwardAction {
  }
  class SelectionForwardAction {
  }
  class SelectionUpAction {
  }
  class SelectionDownAction {
  }
  class SelectWordAction {
  }
  class SelectLineAction {
  }
  class SelectionEndLineAction {
  }
  class SelectionBeginLineAction {
  }
  class SelectionEndAction {
  }
  class SelectionBeginAction {
  }
  class SelectAllAction {
  }
  class NextWordAction {
  }
  class PreviousWordAction {
  }
  class EndWordAction {
  }
  class BeginWordAction {
  }
  class SelectionEndWordAction {
  }
  class SelectionBeginWordAction {
  }
  class SelectionNextWordAction {
  }
  class SelectionPreviousWordAction {
  }
}
class DefaultCaret {
  int bypass;
  int blinkListener;
  int blinkTimer;
  int highlightEntry;
  int active;
  int visible;
  int magicCaretPosition;
  int mark;
  int dot;
  int blinkRate;
  int selectionVisible;
  int textComponent;
  int propertyChangeListener;
  int documentListener;
  int listenerList;
  int changeEvent;
  int policy;
  int UPDATE_WHEN_ON_EDT;
  int NEVER_UPDATE;
  int ALWAYS_UPDATE;
  int serialVersionUID;
  class PropertyChangeHandler {
  }
  class DocumentHandler {
  }
  class BlinkTimerListener {
    int ignoreNextEvent;
  }
  class Bypass {
  }
  int componentWithSelection;
}
class DateFormatter {
  int serialVersionUID;
}
class CompositeView {
  int right;
  int left;
  int bottom;
  int top;
  int insideAllocation;
  int numChildren;
  int children;
}
class ComponentView {
  int interceptor;
  int comp;
  class Interceptor {
    int alignY;
    int alignX;
    int max;
    int pref;
    int min;
  }
}
class ChangedCharSetException {
  int m_charSetKey;
  int m_charSetSpec;
  int serialVersionUID;
}
class Caret {
}
class BoxView {
  int clipRect;
  int tmpRect;
  int span;
  int requirements;
  int offsets;
  int spans;
  int requirementsValid;
  int layoutValid;
  int myAxis;
}
class BadLocationException {
  int offset;
  int serialVersionUID;
}
class AttributeSet {
  int ResolveAttribute;
  int NameAttribute;
  class ParagraphAttribute {
  }
  class FontAttribute {
  }
  class ColorAttribute {
  }
  class CharacterAttribute {
  }
}
class AsyncBoxView {
  int locator;
  int flushTask;
  int minorSpan;
  int majorSpan;
  int minorChanged;
  int majorChanged;
  int prefReq;
  int minReq;
  int changing;
  int childStates;
  int rightInset;
  int estimatedMajorSpan;
  int leftInset;
  int bottomInset;
  int topInset;
  int majorAxis;
  class FlushTask {
  }
  class ChildState {
    int maximum;
    int minorOffset;
    int minorSpan;
    int majorOffset;
    int majorSpan;
    int preferred;
    int minimum;
    int childSizeValid;
    int majorValid;
    int minorValid;
    int childView;
  }
  class ChildLocator {
    int childAlloc;
    int lastAlloc;
    int lastValidOffset;
  }
}
class AbstractWriter {
  int lineSeparatorChars;
  int lineSeparator;
  int endOffset;
  int startOffset;
  int indented;
  int indentLevel;
  int indentSpace;
  int canWrapLines;
  int lineLength;
  int maxLineLength;
  int document;
  int iter;
  int writer;
  int NEWLINE;
}
class AbstractDocument {
  class Bypass {
  }
  class BidiElement {
  }
  class BidiRootElement {
  }
  class LeafElement {
    int endPos;
    int startPos;
    int serialVersionUID;
  }
  class ElementEdit {
    int added;
    int removed;
    int index;
    int elem;
    int serialVersionUID;
  }
  class DefaultDocumentEvent {
    int modified;
    int changes;
    int type;
    int length;
    int offset;
    int THRESHOLD;
    int serialVersionUID;
  }
  class BranchElement {
    int lastIndex;
    int numChildren;
    int children;
    int serialVersionUID;
  }
  class AbstractElement {
    int tree_children;
    int tree_parent;
    int element_parent;
    int attributes;
    int offset;
    int count;
    int serialVersionUID;
  }
  class Content {
  }
  class AttributeContext {
  }
  int notifyListeners;
  int bidiRoot;
  int bypass;
  int numWriters;
  int numReaders;
  int currentWriter;
  int listenerList;
  int properties;
  int documentFilter;
  int context;
  int content;
  int I18N;
  int AsyncLoadPriority;
  int BidiRootName;
  int ElementNameAttribute;
  int SectionElementName;
  int ParagraphElementName;
  int ContentElementName;
  int BidiElementName;
  int BAD_LOCATION;
  int serialVersionUID;
}
