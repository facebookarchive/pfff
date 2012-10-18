package gnu.java.awt.java2d;
class TexturePaintContext {
  int transform;
  int paintRaster;
  int image;
}
class TextCacheKey {
  int font;
  int fontRenderContext;
  int string;
}
class ShapeWrapper {
  int shape;
}
class ShapeCache {
  int polyline;
  int polygon;
  int arc;
  int ellipse;
  int roundRect;
  int rect;
  int line;
}
class Segment {
  int radius;
  int last;
  int next;
  int first;
  int P2;
  int P1;
}
class ScanlineCoverage {
  int iterator;
  int maxCoverage;
  int maxX;
  int minX;
  int lastPrev;
  int last;
  int currentPrev;
  int current;
  int head;
  class Coverage {
    int next;
    int pixelCoverage;
    int covDelta;
    int xPos;
  }
  class Range {
    int coverage;
    int length;
    int xPos;
  }
  class Iterator {
    int handledPixelCoverage;
    int currentCoverage;
    int currentItem;
    int range;
  }
}
class ScanlineConverter {
  int scanlineCoverage;
  int maxX;
  int minX;
  int maxY;
  int minY;
  int edgePoolLast;
  int edgePool;
  int activeEdges;
  int coords;
  int halfStep;
  int yResolution;
  int resolution;
  int upperBounds;
  int scanlines;
  int numScanlines;
  int ONE;
  int FIXED_DIGITS;
}
class Scanline {
  int edges;
}
class RasterGraphics {
  int colorModel;
  int raster;
}
class QuadSegment {
  int cp;
}
class PolyEdgeComparator {
  int y;
}
class PolyEdge {
  int scanlineNext;
  int poolNext;
  int isClip;
  int xIntersection;
  int slope;
  int y1;
  int x1;
  int y0;
  int x0;
}
class Pixelizer {
}
class PixelCoverage {
  int last;
  int current;
  int head;
  class Bucket {
    int prev;
    int next;
    int yCov;
    int xCov;
    int xPos;
  }
}
class LineSegment {
}
class ImagePaint {
  int transform;
  int image;
  class ImagePaintContext {
    int target;
  }
}
class CubicSegment {
  int cp2;
  int cp1;
}
class AlphaCompositeContext {
  int fd;
  int fs;
  int dstColorModel;
  int srcColorModel;
  int composite;
}
class ActiveEdges {
  int numActiveEdges;
  int activeEdges;
}
class AbstractGraphics2D {
  int STANDARD_HINTS;
  int STANDARD_STROKE;
  int isOptimized;
  int destinationRaster;
  int renderingHints;
  int clip;
  int stroke;
  int composite;
  int font;
  int isForegroundColorNull;
  int foreground;
  int background;
  int paintContext;
  int paint;
  int transform;
  int searchTextKey;
  int gvCache;
  int scanlineConverters;
  int shapeCache;
  int GV_CACHE_SIZE;
  int FONT;
  int DEFAULT_TEXT_AA;
  int imageCache;
}
