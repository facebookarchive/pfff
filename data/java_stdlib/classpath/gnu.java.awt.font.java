package gnu.java.awt.font;
class OpenTypeFontPeer {
  int fontDelegate;
  int IDENDITY;
  class XFontMetrics {
    int cachedPoint;
  }
  class XLineMetrics {
    int fontRenderContext;
    int glyphVector;
    int font;
  }
  int fontToFileMap;
  int availableFontNames;
  int fontProperties;
}
class GNUGlyphVector {
  int cleanOutline;
  int layoutFlags;
  int transforms;
  int pos;
  int valid;
  int transform;
  int fontSize;
  int glyphs;
  int renderContext;
  int font;
  int fontDelegate;
}
class FontFactory {
}
class FontDelegate {
  int FLAG_NO_HINT_WEAK_POINTS;
  int FLAG_NO_HINT_STRONG_POINTS;
  int FLAG_NO_HINT_EDGE_POINTS;
  int FLAG_NO_HINT_VERTICAL;
  int FLAG_NO_HINT_HORIZONTAL;
  int FLAG_FITTED;
}
