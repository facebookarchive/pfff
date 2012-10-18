package java.awt.geom;
class RoundRectangle2D {
  class Float {
    int height;
    int width;
    int y;
    int x;
    int arcwidth;
    int archeight;
  }
  class Double {
    int height;
    int width;
    int y;
    int x;
    int arcwidth;
    int archeight;
  }
}
class RectangularShape {
}
class Rectangle2D {
  class Float {
    int height;
    int width;
    int y;
    int x;
  }
  class Double {
    int height;
    int width;
    int y;
    int x;
  }
  int OUT_BOTTOM;
  int OUT_RIGHT;
  int OUT_TOP;
  int OUT_LEFT;
}
class QuadCurve2D {
  class Float {
    int y2;
    int x2;
    int ctrly;
    int ctrlx;
    int y1;
    int x1;
  }
  class Double {
    int y2;
    int x2;
    int ctrly;
    int ctrlx;
    int y1;
    int x1;
  }
  int EPSILON;
  int BIG_VALUE;
}
class Point2D {
  class Float {
    int y;
    int x;
  }
  class Double {
    int y;
    int x;
  }
}
class PathIterator {
  int SEG_CLOSE;
  int SEG_CUBICTO;
  int SEG_QUADTO;
  int SEG_LINETO;
  int SEG_MOVETO;
  int WIND_NON_ZERO;
  int WIND_EVEN_ODD;
}
class NoninvertibleTransformException {
  int serialVersionUID;
}
class Line2D {
  class Float {
    int y2;
    int x2;
    int y1;
    int x1;
  }
  class Double {
    int y2;
    int x2;
    int y1;
    int x1;
  }
}
class IllegalPathStateException {
  int serialVersionUID;
}
class GeneralPath {
  class GeneralPathIterator {
    int pos;
    int transform;
    int path;
    int NUM_COORDS;
  }
  int index;
  int subpath;
  int ypoints;
  int xpoints;
  int types;
  int rule;
  int BIG_VALUE;
  int INIT_SIZE;
  int WIND_NON_ZERO;
  int WIND_EVEN_ODD;
}
class FlatteningPathIterator {
  int done;
  int srcPosY;
  int srcPosX;
  int srcSegType;
  int scratch;
  int recLevel;
  int stackSize;
  int stack;
  int recursionLimit;
  int flatnessSq;
  int srcIter;
}
class Ellipse2D {
  class Float {
    int y;
    int x;
    int width;
    int height;
  }
  class Double {
    int y;
    int x;
    int width;
    int height;
  }
}
class Dimension2D {
}
class CubicCurve2D {
  class Float {
    int y2;
    int x2;
    int ctrly2;
    int ctrlx2;
    int ctrly1;
    int ctrlx1;
    int y1;
    int x1;
  }
  class Double {
    int y2;
    int x2;
    int ctrly2;
    int ctrlx2;
    int ctrly1;
    int ctrlx1;
    int y1;
    int x1;
  }
  int EPSILON;
  int BIG_VALUE;
}
class Area {
  class CubicSegment {
    int cp2;
    int cp1;
  }
  class QuadSegment {
    int cp;
  }
  class LineSegment {
  }
  class Segment {
    int node;
    int next;
    int P2;
    int P1;
  }
  class Intersection {
    int seg;
    int tb;
    int ta;
    int p;
  }
  class AreaIterator {
    class IteratorSegment {
      int coords;
      int type;
    }
    int at;
    int index;
    int segments;
  }
  int windingRule;
  int ccIntersections;
  int holes;
  int solids;
  int PE_EPSILON;
  int RS_EPSILON;
  int EPSILON;
}
class Arc2D {
  class Float {
    int extent;
    int start;
    int height;
    int width;
    int y;
    int x;
  }
  class Double {
    int extent;
    int start;
    int height;
    int width;
    int y;
    int x;
  }
  class ArcIterator {
    int type;
    int extent;
    int start;
    int h;
    int w;
    int y;
    int x;
    int xform;
    int limit;
    int current;
  }
  int type;
  int PIE;
  int CHORD;
  int OPEN;
}
class AffineTransform {
  int type;
  int m12;
  int m02;
  int m11;
  int m01;
  int m10;
  int m00;
  int TYPE_GENERAL_TRANSFORM;
  int TYPE_MASK_ROTATION;
  int TYPE_GENERAL_ROTATION;
  int TYPE_QUADRANT_ROTATION;
  int TYPE_FLIP;
  int TYPE_MASK_SCALE;
  int TYPE_GENERAL_SCALE;
  int TYPE_UNIFORM_SCALE;
  int TYPE_TRANSLATION;
  int TYPE_IDENTITY;
  int serialVersionUID;
}
