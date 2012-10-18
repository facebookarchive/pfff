package java.awt.font;
class TransformAttribute {
  int affineTransform;
  int serialVersionUID;
}
class TextMeasurer {
  int numChars;
  int totalLayout;
  int frc;
  int text;
}
class TextLayout {
  class CaretPolicy {
  }
  int DEFAULT_CARET_POLICY;
  int hash;
  int visualToLogical;
  int logicalToVisual;
  int bidi;
  int hasWhitespace;
  int leftToRight;
  int charIndices;
  int naturalBounds;
  int totalAdvance;
  int lm;
  int boundsCache;
  int length;
  int offset;
  int string;
  int frc;
  int runs;
  class Run {
    int location;
    int runEnd;
    int runStart;
    int font;
    int glyphVector;
  }
}
class TextHitInfo {
  int leadingEdge;
  int charIndex;
}
class TextAttribute {
  int WIDTH_SEMI_EXTENDED;
  int WIDTH_SEMI_CONDENSED;
  int WIDTH_REGULAR;
  int WIDTH_EXTENDED;
  int WIDTH_CONDENSED;
  int WIDTH;
  int WEIGHT_ULTRABOLD;
  int WEIGHT_SEMIBOLD;
  int WEIGHT_REGULAR;
  int WEIGHT_MEDIUM;
  int WEIGHT_LIGHT;
  int WEIGHT_HEAVY;
  int WEIGHT_EXTRABOLD;
  int WEIGHT_EXTRA_LIGHT;
  int WEIGHT_DEMILIGHT;
  int WEIGHT_DEMIBOLD;
  int WEIGHT_BOLD;
  int WEIGHT;
  int UNDERLINE_ON;
  int UNDERLINE_LOW_TWO_PIXEL;
  int UNDERLINE_LOW_ONE_PIXEL;
  int UNDERLINE_LOW_GRAY;
  int UNDERLINE_LOW_DOTTED;
  int UNDERLINE_LOW_DASHED;
  int UNDERLINE;
  int TRANSFORM;
  int SWAP_COLORS_ON;
  int SWAP_COLORS;
  int SUPERSCRIPT_SUPER;
  int SUPERSCRIPT_SUB;
  int SUPERSCRIPT;
  int STRIKETHROUGH_ON;
  int STRIKETHROUGH;
  int SIZE;
  int RUN_DIRECTION_RTL;
  int RUN_DIRECTION_LTR;
  int RUN_DIRECTION;
  int POSTURE_REGULAR;
  int POSTURE_OBLIQUE;
  int POSTURE;
  int NUMERIC_SHAPING;
  int JUSTIFICATION_NONE;
  int JUSTIFICATION_FULL;
  int JUSTIFICATION;
  int INPUT_METHOD_UNDERLINE;
  int INPUT_METHOD_HIGHLIGHT;
  int FOREGROUND;
  int FONT;
  int FAMILY;
  int CHAR_REPLACEMENT;
  int BIDI_EMBEDDING;
  int BACKGROUND;
  int serialVersionUID;
}
class ShapeGraphicAttribute {
  int bounds;
  int stroke;
  int shape;
  int STROKE;
  int FILL;
}
class OpenType {
  int TAG_VMTX;
  int TAG_VHEA;
  int TAG_VDMX;
  int TAG_TYP1;
  int TAG_TRAK;
  int TAG_PROP;
  int TAG_PREP;
  int TAG_POST;
  int TAG_PCLT;
  int TAG_OS2;
  int TAG_OPBD;
  int TAG_NAME;
  int TAG_MORT;
  int TAG_MMSD;
  int TAG_MMFX;
  int TAG_MAXP;
  int TAG_LTSH;
  int TAG_LOCA;
  int TAG_LCAR;
  int TAG_KERN;
  int TAG_JUST;
  int TAG_JSTF;
  int TAG_HMTX;
  int TAG_HHEA;
  int TAG_HEAD;
  int TAG_HDMX;
  int TAG_GVAR;
  int TAG_GSUB;
  int TAG_GPOS;
  int TAG_GLYF;
  int TAG_GDEF;
  int TAG_GASP;
  int TAG_FVAR;
  int TAG_FPGM;
  int TAG_FMTX;
  int TAG_FEAT;
  int TAG_FDSC;
  int TAG_EBSC;
  int TAG_EBLC;
  int TAG_EBDT;
  int TAG_DSIG;
  int TAG_CVT;
  int TAG_CVAR;
  int TAG_CMAP;
  int TAG_CFF;
  int TAG_BSLN;
  int TAG_BLOC;
  int TAG_BDAT;
  int TAG_BASE;
  int TAG_AVAR;
  int TAG_ACNT;
}
class NumericShaper {
  int mask;
  int key;
  int zeroDigits;
  int TIBETAN;
  int THAI;
  int TELUGU;
  int TAMIL;
  int ORIYA;
  int MYANMAR;
  int MONGOLIAN;
  int MALAYALAM;
  int LAO;
  int KHMER;
  int KANNADA;
  int GURMUKHI;
  int GUJARATI;
  int EUROPEAN;
  int ETHIOPIC;
  int EASTERN_ARABIC;
  int DEVANAGARI;
  int BENGALI;
  int ARABIC;
  int ALL_RANGES;
  int serialVersionUID;
}
class MultipleMaster {
}
class LineMetrics {
}
class LineBreakMeasurer {
  int numChars;
  int tm;
  int position;
  int text;
}
class ImageGraphicAttribute {
  int originY;
  int originX;
  int image;
}
class GraphicAttribute {
  int alignment;
  int TOP_ALIGNMENT;
  int ROMAN_BASELINE;
  int HANGING_BASELINE;
  int CENTER_BASELINE;
  int BOTTOM_ALIGNMENT;
}
class GlyphVector {
  int FLAG_RUN_RTL;
  int FLAG_MASK;
  int FLAG_HAS_TRANSFORMS;
  int FLAG_HAS_POSITION_ADJUSTMENTS;
  int FLAG_COMPLEX_GLYPHS;
}
class GlyphMetrics {
  int glyphType;
  int bounds;
  int advanceY;
  int advanceX;
  int horizontal;
  int WHITESPACE;
  int STANDARD;
  int LIGATURE;
  int COMPONENT;
  int COMBINING;
}
class GlyphJustificationInfo {
  int shrinkRightLimit;
  int shrinkLeftLimit;
  int shrinkAbsorb;
  int shrinkPriority;
  int growRightLimit;
  int growLeftLimit;
  int growAbsorb;
  int growPriority;
  int weight;
  int PRIORITY_NONE;
  int PRIORITY_INTERCHAR;
  int PRIORITY_WHITESPACE;
  int PRIORITY_KASHIDA;
}
class FontRenderContext {
  int usesFractionalMetrics;
  int isAntiAliased;
  int affineTransform;
}
