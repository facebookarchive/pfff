package gnu.java.awt.font.opentype.truetype;
class ZonePathIterator {
  int floats;
  int type;
  int contourStart;
  int curPoint;
  int numPoints;
  int zone;
  int state;
  int EMIT_MOVETO;
  int EMIT_CLOSE;
  int EMIT_SEGMENT;
}
class Zone {
  int shearY;
  int shearX;
  int scaleY;
  int scaleX;
  int numPoints;
  int points;
}
class VirtualMachine {
  int INST_NAME;
  int unitsPerEm;
  int cachedPixelsPerEM;
  int roundThreshold;
  int roundPhase;
  int roundPeriod;
  int ignoreCVTProgram;
  int executeGlyphInstructions;
  int glyphZone;
  int twilightZone;
  int zp2;
  int zp1;
  int zp0;
  int singleWidthValue;
  int scanType;
  int scanControl;
  int rp2;
  int rp1;
  int rp0;
  int dualY;
  int dualX;
  int projY;
  int projX;
  int minimumDistance;
  int loop;
  int freeY;
  int freeX;
  int deltaShift;
  int deltaBase;
  int cvtCutIn;
  int antialiased;
  int shearY;
  int shearX;
  int scaleY;
  int scaleX;
  int deviceTransform;
  int pointSize;
  int numTwilightPoints;
  int preProgram;
  int fontProgram;
  int engineCompensation;
  int cvt;
  int controlValueTable;
  int fdefEntryPoint;
  int fdefBuffer;
  int sp;
  int maxStackElements;
  int stack;
  int storage;
  int ONE_214;
  int TRACE_EXECUTION;
  int PATENTED_HINTING;
}
class TrueTypeScaler {
  int unitsPerEm;
  int glyphZone;
  int glyphMeasurer;
  int glyphLoader;
  int glyfTable;
}
class Point {
  int outDir;
  int inDir;
  int next;
  int prev;
  int flags;
  int v;
  int u;
  int y;
  int x;
  int scaledY;
  int scaledX;
  int origY;
  int origX;
  int DIR_DOWN;
  int DIR_UP;
  int DIR_LEFT;
  int DIR_RIGHT;
  int FLAG_DONE_Y;
  int FLAG_DONE_X;
  int FLAG_INFLECTION;
  int FLAG_WEAK_INTERPOLATION;
  int FLAG_CONTOUR_END;
  int FLAG_ON_CURVE;
  int FLAG_TOUCHED_Y;
  int FLAG_TOUCHED_X;
}
class GlyphMeasurer {
  int horizontalLineGap;
  int verticalDescent;
  int horizontalDescent;
  int verticalAscent;
  int horizontalAscent;
  int numLongVerticalMetricsEntries;
  int numLongHorizontalMetricsEntries;
  int verticalGlyphMetrics;
  int horizontalGlyphMetrics;
}
class GlyphLocator {
  class FourByte {
    int indexToLoc;
  }
  class TwoByte {
    int indexToLoc;
  }
  int glyfTable;
}
class GlyphLoader {
  int UNSCALED_COMPONENT_OFFSET;
  int SCALED_COMPONENT_OFFSET;
  int OVERLAP_COMPOUND;
  int USE_MY_METRICS;
  int WE_HAVE_INSTRUCTIONS;
  int WE_HAVE_A_TWO_BY_TWO;
  int WE_HAVE_AN_X_AND_Y_SCALE;
  int MORE_COMPONENTS;
  int WE_HAVE_A_SCALE;
  int ROUND_XY_TO_GRID;
  int ARGS_ARE_XY_VALUES;
  int ARGS_ARE_WORDS;
  int pointFlags;
  int contourEndPoints;
  int unitsPerEm;
  int vm;
  int glyphMeasurer;
  int glyphLocator;
}
class Fixed {
  int ONE;
}
