package gnu.java.awt.font.opentype;
class Scaler {
}
class OpenTypeFontFactory {
}
class OpenTypeFont {
  int nameTable;
  int hinter;
  int glyphNamer;
  int cmap;
  int scaler;
  int emsPerUnit;
  int unitsPerEm;
  int version;
  int tableLength;
  int tableStart;
  int tableTag;
  int numGlyphs;
  int buf;
  int TAG_ZAPF;
  int TAG_TTCF;
  int TAG_TRUE;
  int TAG_SFNT;
  int TAG_OTTO;
}
class NameDecoder {
  int microsoftLanguageCodes;
  int macLanguageCodes;
  int PLATFORM_MICROSOFT;
  int PLATFORM_MACINTOSH;
  int NAME_POSTSCRIPT_CID;
  int NAME_SAMPLE_TEXT;
  int NAME_FULL_MACCOMPATIBLE;
  int NAME_PREFERRED_SUBFAMILY;
  int NAME_PREFERRED_FAMILY;
  int NAME_LICENSE_URL;
  int NAME_LICENSE;
  int NAME_DESIGNER_URL;
  int NAME_VENDOR_URL;
  int NAME_DESCRIPTION;
  int NAME_DESIGNER;
  int NAME_MANUFACTURER;
  int NAME_TRADEMARK;
  int NAME_POSTSCRIPT;
  int NAME_VERSION;
  int NAME_FULL;
  int NAME_UNIQUE;
  int NAME_SUBFAMILY;
  int NAME_FAMILY;
  int NAME_COPYRIGHT;
}
class MacResourceFork {
  class Resource {
    int buf;
    int dataOffset;
    int nameOffset;
    int attribute;
    int id;
    int type;
  }
  int buf;
  int resources;
  int types;
}
class Hinter {
}
class GlyphNamer {
  int AGLFN_NAMES;
  int AGLFN_NAME_OFFSET;
  int AGLFN_GLYPHS;
  int STANDARD_POSTSCRIPT_GLYPH_NAMES;
  int glyphCharacterCodes;
  int glyphNames;
  int postFormat;
  int zapfExtraInfo;
  int zapfOffsets;
  int zapfTable;
  int postTable;
}
class CharGlyphMap {
  class Type12 {
    int data;
    int numGroups;
  }
  class Type4 {
    int numSegments;
    int rangeID;
    int idDelta;
    int firstChar;
    int lastChar;
  }
  class Type0 {
    int UPPER_TURKISH;
    int UPPER_ROMANIAN;
    int UPPER_ROMAN;
    int UPPER_ICELANDIC;
    int UPPER_HEBREW;
    int UPPER_GREEK;
    int UPPER_FARSI;
    int UPPER_CYRILLIC;
    int UPPER_CROATIAN;
    int UPPER_EAST_EUROPEAN_ROMAN;
    int UPPER_ARABIC;
    int glyphToUCS2;
  }
  class Dummy {
  }
  int PLATFORM_MICROSOFT;
  int PLATFORM_MACINTOSH;
  int PLATFORM_UNICODE;
}
