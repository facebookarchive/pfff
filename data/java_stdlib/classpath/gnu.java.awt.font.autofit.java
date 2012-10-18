package gnu.java.awt.font.autofit;
class Width {
  int fit;
  int cur;
  int org;
}
class Utils {
  int ANGLE_2PI;
  int ANGLE_PI4;
  int ANGLE_PI2;
  int ANGLE_PI;
  int ATAN;
  int ATAN_BITS;
}
class Segment {
  int edge;
  int edgeNext;
  int len;
  int score;
  int maxPos;
  int minPos;
  int contour;
  int last;
  int first;
  int pos;
  int numLinked;
  int serif;
  int link;
  int flags;
  int dir;
  int FLAG_EDGE_DONE;
  int FLAG_EDGE_SERIF;
  int FLAG_EDGE_ROUND;
  int FLAG_EDGE_NORMAL;
}
class ScriptMetrics {
  int scaler;
  int script;
}
class Script {
}
class LatinMetrics {
  int unitsPerEm;
  int axis;
}
class LatinBlue {
  int flags;
  int shoot;
  int ref;
  int FLAG_ADJUSTMENT;
  int FLAG_TOP;
  int FLAG_BLUE_ACTIVE;
}
class LatinAxis {
  int orgScale;
  int orgDelta;
  int blueCount;
  int blues;
  int edgeDistanceTreshold;
  int widths;
  int widthCount;
  int delta;
  int scale;
}
class Latin {
  int IDENTITY;
  int TEST_CHARS;
  int BLUE_MAX;
  int SMALL_MINOR;
  int SMALL_BOTTOM;
  int SMALL_TOP;
  int SMALL_F_TOP;
  int CAPITAL_BOTTOM;
  int CAPITAL_TOP;
  int MAX_TEST_CHARS;
  int MAX_WIDTHS;
}
class HintScaler {
  int renderMode;
  int face;
  int yDelta;
  int yScale;
  int xDelta;
  int xScale;
}
class GlyphHints {
  int flags;
  int metrics;
  int maxContours;
  int numContours;
  int contours;
  int maxPoints;
  int numPoints;
  int points;
  int axis;
  int yDelta;
  int yScale;
  int xDelta;
  int xScale;
}
class Edge {
  int scale;
  int pos;
  int blueEdge;
  int dir;
  int flags;
  int serif;
  int link;
  int opos;
  int last;
  int first;
  int fpos;
}
class Constants {
  int DIR_DOWN;
  int DIR_UP;
  int DIR_LEFT;
  int DIR_RIGHT;
  int DIR_NONE;
  int DIMENSION_MAX;
  int DIMENSION_VERT;
  int DIMENSION_HORZ;
}
class AxisHints {
  int edges;
  int numEdges;
  int numSegments;
  int majorDir;
  int segments;
}
class AutoHinter {
  int scaler;
  int hints;
  int metrics;
  int latinScript;
}
