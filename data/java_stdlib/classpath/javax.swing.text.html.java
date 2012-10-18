package javax.swing.text.html;
class ViewAttributeSet {
  int styleSheet;
  int view;
}
class TableView {
  int tmpRect;
  int cellSpacing;
  int gridValid;
  int width;
  int numColumns;
  int columnWidths;
  int columnSpans;
  int columnOffsets;
  int totalColumnRequirements;
  int columnRequirements;
  int attributes;
  class CellView {
    int rowSpan;
    int colSpan;
  }
  class RowView {
    int rowIndex;
    int overlap;
  }
}
class StyleSheet {
  class ListPainter {
    int tmpRect;
    int type;
    int styleSheet;
    int attributes;
  }
  class BoxPainter {
    int background;
    int bottomPadding;
    int topPadding;
    int rightPadding;
    int leftPadding;
    int border;
    int bottomInset;
    int topInset;
    int rightInset;
    int leftInset;
  }
  int resolvedStyles;
  int css;
  int linked;
  int baseFontSize;
  int base;
  class CSSStyle {
    int selector;
    int precedence;
    int PREC_USER_IMPORTANT;
    int PREC_AUTHOR_IMPORTANT;
    int PREC_AUTHOR_NORMAL;
    int PREC_NORM;
    int PREC_UA;
  }
  class CSSStyleSheetParserCallback {
    int precedence;
    int styles;
  }
}
class SelectListModel {
  int initialSelection;
  int selectionModel;
}
class SelectComboBoxModel {
  int initial;
}
class ResetableToggleButtonModel {
  int initial;
}
class ResetablePlainDocument {
  int initial;
}
class ResetableModel {
}
class ParagraphView {
  int cssHeight;
  int cssWidth;
  int painter;
  int attributes;
}
class Option {
  int selected;
  int label;
  int attributes;
}
class ObjectView {
}
class NullView {
}
class MultiStyle {
  int attributes;
  int name;
}
class MultiAttributeSet {
  int multi;
  class MultiNameEnumeration {
    int current;
    int index;
  }
}
class MinimalHTMLWriter {
  int inFontTag;
  int tagStack;
  int doc;
}
class ListView {
  int painter;
}
class InlineView {
  int nowrap;
  int longestWord;
  int attributes;
}
class ImageView {
  int attributes;
  int spans;
  int observer;
  int height;
  int width;
  int loading;
  int haveHeight;
  int haveWidth;
  int reloadProperties;
  int reloadImage;
  int imageState;
  int image;
  int loadOnDemand;
  class Observer {
  }
}
class HTMLWriter {
  int fg_pass_end_elem;
  int fg_pass_start_elem;
  int endElem;
  int startElem;
  int htmlFragmentParentHashSet;
  int doc_len_remaining;
  int doc_offset_remaining;
  int doc_len;
  int doc_pos;
  int html_entity_escape_str_arr;
  int html_entity_char_arr;
  int new_line_str;
  int openEmbeddedTagHashSet;
  int htmlDoc;
  int outWriter;
}
class HTMLFrameHyperlinkEvent {
  int target_frame;
}
class HTMLEditorKit {
  int autoFormSubmission;
  int editorPane;
  int inputAttributes;
  int contentType;
  int linkController;
  int parser;
  int defaultCursor;
  int linkCursor;
  int viewFactory;
  int styleSheet;
  int defaultActions;
  int PARA_INDENT_RIGHT;
  int PARA_INDENT_LEFT;
  int LOGICAL_STYLE_ACTION;
  int IMG_BORDER;
  int IMG_ALIGN_TOP;
  int IMG_ALIGN_MIDDLE;
  int IMG_ALIGN_BOTTOM;
  int FONT_CHANGE_SMALLER;
  int FONT_CHANGE_BIGGER;
  int COLOR_ACTION;
  int ITALIC_ACTION;
  int BOLD_ACTION;
  int DEFAULT_CSS;
  int serialVersionUID;
  class ParserCallback {
    int IMPLIED;
  }
  class Parser {
  }
  class HTMLFactory {
  }
  class HTMLTextAction {
  }
  class InsertHTMLTextAction {
    int parentTag;
    int html;
    int alternateParentTag;
    int alternateAddTag;
    int addTag;
  }
  class LinkController {
    int lastAnchorElement;
  }
}
class HTMLDocument {
  class HTMLReader {
    class TitleAction {
    }
    class StyleAction {
    }
    class MetaAction {
    }
    class MapAction {
    }
    class LinkAction {
    }
    class HeadAction {
    }
    class BaseAction {
    }
    class ConvertAction {
    }
    class AreaAction {
    }
    class SpecialAction {
    }
    class PreAction {
    }
    class ParagraphAction {
    }
    class IsindexAction {
    }
    class HiddenAction {
    }
    class FormTagAction {
    }
    class FormAction {
    }
    class CharacterAction {
    }
    class BlockAction {
    }
    class TagAction {
    }
    int threshold;
    int buttonGroups;
    int numOptions;
    int option;
    int selectModel;
    int textAreaDocument;
    int styles;
    int inTextArea;
    int inStyleTag;
    int inPreTag;
    int debug;
    int insertTagEncountered;
    int insertTag;
    int offset;
    int pushDepth;
    int popDepth;
    int endHTMLEncountered;
    int tagToAction;
    int charAttrStack;
    int parseStack;
    int parseBuffer;
    int charAttr;
    int GROW_THRESHOLD;
    int MAX_THRESHOLD;
  }
  class RunElement {
  }
  class BlockElement {
  }
  class Iterator {
  }
  class LeafIterator {
    int it;
    int doc;
    int tag;
  }
  int baseTarget;
  int frameDocument;
  int parser;
  int tokenThreshold;
  int preservesUnknownTags;
  int baseURL;
  int AdditionalComments;
}
class HTML {
  int attrMap;
  int tagMap;
  int SYNTHETIC;
  int PREFORMATTED;
  int BLOCK;
  int BREAKS;
  int NULL_ATTRIBUTE_VALUE;
  class UnknownTag {
    int serialVersionUID;
  }
  class Tag {
    int flags;
    int name;
    int IMPLIED;
    int CONTENT;
    int COMMENT;
    int TOTAL_SYNTHETIC_TAGS;
    int VAR;
    int UL;
    int U;
    int TT;
    int TR;
    int TITLE;
    int TH;
    int TEXTAREA;
    int TD;
    int TABLE;
    int SUP;
    int SUB;
    int STYLE;
    int STRONG;
    int STRIKE;
    int SPAN;
    int SMALL;
    int SELECT;
    int SCRIPT;
    int SAMP;
    int S;
    int PRE;
    int PARAM;
    int P;
    int OPTION;
    int OL;
    int OBJECT;
    int NOFRAMES;
    int NOBR;
    int META;
    int MENU;
    int MAP;
    int LINK;
    int LI;
    int KBD;
    int ISINDEX;
    int INPUT;
    int IMG;
    int I;
    int HTML;
    int HR;
    int HEAD;
    int H6;
    int H5;
    int H4;
    int H3;
    int H2;
    int H1;
    int FRAMESET;
    int FRAME;
    int FORM;
    int FONT;
    int EM;
    int DT;
    int DL;
    int DIV;
    int DIR;
    int DFN;
    int DD;
    int CODE;
    int CITE;
    int CENTER;
    int CAPTION;
    int BR;
    int BODY;
    int BLOCKQUOTE;
    int BIG;
    int BASEFONT;
    int BASE;
    int B;
    int AREA;
    int APPLET;
    int ADDRESS;
    int A;
  }
  class Attribute {
    int name;
    int DYNAMIC_CLASS;
    int PSEUDO_CLASS;
    int WIDTH;
    int VSPACE;
    int VLINK;
    int VERSION;
    int VALUETYPE;
    int VALUE;
    int VALIGN;
    int USEMAP;
    int TYPE;
    int TITLE;
    int TEXT;
    int TARGET;
    int STYLE;
    int START;
    int STANDBY;
    int SRC;
    int SIZE;
    int SHAPES;
    int SHAPE;
    int SELECTED;
    int SCROLLING;
    int ROWSPAN;
    int ROWS;
    int REV;
    int REL;
    int PROMPT;
    int NOWRAP;
    int NOSHADE;
    int NORESIZE;
    int NOHREF;
    int NAME;
    int N;
    int MULTIPLE;
    int METHOD;
    int MEDIA;
    int MAXLENGTH;
    int MARGINWIDTH;
    int MARGINHEIGHT;
    int LOWSRC;
    int LINK;
    int LANGUAGE;
    int LANG;
    int ISMAP;
    int ID;
    int HTTPEQUIV;
    int HSPACE;
    int HREF;
    int HEIGHT;
    int HALIGN;
    int FRAMEBORDER;
    int FACE;
    int ENDTAG;
    int ENCTYPE;
    int DUMMY;
    int DIR;
    int DECLARE;
    int DATA;
    int COORDS;
    int CONTENT;
    int COMPACT;
    int COMMENT;
    int COLSPAN;
    int COLS;
    int COLOR;
    int CODETYPE;
    int CODEBASE;
    int CODE;
    int CLEAR;
    int CLASSID;
    int CLASS;
    int CHECKED;
    int CELLSPACING;
    int CELLPADDING;
    int BORDER;
    int BGCOLOR;
    int BACKGROUND;
    int ARCHIVE;
    int ALT;
    int ALINK;
    int ALIGN;
    int ACTION;
  }
}
class HRuleView {
  class Beginning {
    int breakOffset;
  }
  int HEIGHT;
  int nullView;
}
class FrameView {
}
class FrameSetView {
  int numViews;
  int percent;
  int relative;
  int absolute;
  class FrameSetRow {
    int row;
  }
}
class FormView {
  int maxIsPreferred;
  int RESET;
  int SUBMIT;
  class SubmitThread {
    int data;
  }
  class MouseEventListener {
  }
}
class FormSubmitEvent {
  int data;
  int method;
  class MethodType {
    int GET;
    int POST;
  }
}
class CSSParser {
  int readWS;
  int tokenBufferLength;
  int tokenBuffer;
  int callback;
  int encounteredRuleSet;
  int reader;
  int stackCount;
  int unitStack;
  int pushedChar;
  int didPushChar;
  int charMapping;
  int END;
  int PAREN_CLOSE;
  int PAREN_OPEN;
  int BRACE_CLOSE;
  int BRACE_OPEN;
  int BRACKET_CLOSE;
  int BRACKET_OPEN;
  int IDENTIFIER;
  class CSSParserCallback {
  }
}
class CSSBorder {
  int bottomStyle;
  int rightColor;
  int leftColor;
  int bottomColor;
  int topColor;
  int topStyle;
  int rightStyle;
  int leftStyle;
  int bottom;
  int top;
  int right;
  int left;
  int STYLE_OUTSET;
  int STYLE_INSET;
  int STYLE_RIDGE;
  int STYLE_GROOVE;
  int STYLE_DOUBLE;
  int STYLE_SOLID;
  int STYLE_DASHED;
  int STYLE_DOTTED;
  int STYLE_HIDDEN;
  int STYLE_NONE;
  int STYLE_NOT_SET;
}
class CSS {
  class Attribute {
    int attributeMap;
    int defaultValue;
    int isInherited;
    int attStr;
    int BOTTOM;
    int TOP;
    int RIGHT;
    int LEFT;
    int POSITION;
    int BORDER_SPACING;
    int BORDER_RIGHT_COLOR;
    int BORDER_LEFT_COLOR;
    int BORDER_BOTTOM_COLOR;
    int BORDER_TOP_COLOR;
    int BORDER_RIGHT_STYLE;
    int BORDER_LEFT_STYLE;
    int BORDER_BOTTOM_STYLE;
    int BORDER_TOP_STYLE;
    int WORD_SPACING;
    int WIDTH;
    int WHITE_SPACE;
    int VERTICAL_ALIGN;
    int TEXT_TRANSFORM;
    int TEXT_INDENT;
    int TEXT_DECORATION;
    int TEXT_ALIGN;
    int PADDING_TOP;
    int PADDING_RIGHT;
    int PADDING_LEFT;
    int PADDING_BOTTOM;
    int PADDING;
    int MARGIN_TOP;
    int MARGIN_RIGHT;
    int MARGIN_LEFT;
    int MARGIN_BOTTOM;
    int MARGIN;
    int LIST_STYLE_TYPE;
    int LIST_STYLE_POSITION;
    int LIST_STYLE_IMAGE;
    int LIST_STYLE;
    int LINE_HEIGHT;
    int LETTER_SPACING;
    int HEIGHT;
    int FONT_WEIGHT;
    int FONT_VARIANT;
    int FONT_STYLE;
    int FONT_SIZE;
    int FONT_FAMILY;
    int FONT;
    int FLOAT;
    int DISPLAY;
    int COLOR;
    int CLEAR;
    int BORDER_WIDTH;
    int BORDER_TOP_WIDTH;
    int BORDER_TOP;
    int BORDER_STYLE;
    int BORDER_RIGHT_WIDTH;
    int BORDER_RIGHT;
    int BORDER_LEFT_WIDTH;
    int BORDER_LEFT;
    int BORDER_COLOR;
    int BORDER_BOTTOM_WIDTH;
    int BORDER_BOTTOM;
    int BORDER;
    int BACKGROUND_REPEAT;
    int BACKGROUND_POSITION;
    int BACKGROUND_IMAGE;
    int BACKGROUND_COLOR;
    int BACKGROUND_ATTACHMENT;
    int BACKGROUND;
  }
}
class BlockView {
  int positionInfo;
  int cssSpans;
  int painter;
  int attributes;
  class PositionInfo {
    int bottom;
    int top;
    int right;
    int left;
    int type;
    int FIXED;
    int ABSOLUTE;
    int RELATIVE;
    int STATIC;
  }
}
class BRView {
}
