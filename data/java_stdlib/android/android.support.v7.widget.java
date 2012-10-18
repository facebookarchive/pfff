package android.support.v7.widget;
class ViewGroup {
  int MEASURED_STATE_MASK;
  int MEASURED_STATE_TOO_SMALL;
}
class Space {
}
class GridLayout {
  int CAN_STRETCH;
  int INFLEXIBLE;
  int FILL;
  int BASELINE;
  int CENTER;
  int RIGHT;
  int LEFT;
  int END;
  int START;
  int BOTTOM;
  int TOP;
  int TRAILING;
  int LEADING;
  int UNDEFINED_ALIGNMENT;
  class Alignment {
  }
  class Spec {
    int alignment;
    int span;
    int startDefined;
    int UNDEFINED;
  }
  class Interval {
    int max;
    int min;
  }
  class Bounds {
    int flexibility;
    int after;
    int before;
  }
  class PackedMap {
    int values;
    int keys;
    int index;
  }
  class Assoc {
    int valueType;
    int keyType;
  }
  class MutableInt {
    int value;
  }
  class Arc {
    int valid;
    int value;
    int span;
  }
  class LayoutParams {
    int columnSpec;
    int rowSpec;
    int GRAVITY;
    int ROW_SPAN;
    int ROW;
    int COLUMN_SPAN;
    int COLUMN;
    int BOTTOM_MARGIN;
    int RIGHT_MARGIN;
    int TOP_MARGIN;
    int LEFT_MARGIN;
    int MARGIN;
    int DEFAULT_SPAN_SIZE;
    int DEFAULT_SPAN;
    int DEFAULT_COLUMN;
    int DEFAULT_ROW;
    int DEFAULT_MARGIN;
    int DEFAULT_HEIGHT;
    int DEFAULT_WIDTH;
  }
  class Axis {
    int parentMax;
    int parentMin;
    int orderPreserved;
    int locationsValid;
    int locations;
    int arcsValid;
    int arcs;
    int trailingMarginsValid;
    int trailingMargins;
    int leadingMarginsValid;
    int leadingMargins;
    int backwardLinksValid;
    int backwardLinks;
    int forwardLinksValid;
    int forwardLinks;
    int groupBoundsValid;
    int groupBounds;
    int maxIndex;
    int definedCount;
    int horizontal;
    int COMPLETE;
    int PENDING;
    int NEW;
  }
  int lastLayoutParamsHashCode;
  int defaultGap;
  int alignmentMode;
  int useDefaultMargins;
  int orientation;
  int layoutParamsValid;
  int verticalAxis;
  int horizontalAxis;
  int COLUMN_ORDER_PRESERVED;
  int ROW_ORDER_PRESERVED;
  int ALIGNMENT_MODE;
  int USE_DEFAULT_MARGINS;
  int COLUMN_COUNT;
  int ROW_COUNT;
  int ORIENTATION;
  int DEFAULT_ALIGNMENT_MODE;
  int DEFAULT_ORDER_PRESERVED;
  int DEFAULT_USE_DEFAULT_MARGINS;
  int DEFAULT_COUNT;
  int DEFAULT_ORIENTATION;
  int UNINITIALIZED_HASH;
  int DEFAULT_CONTAINER_MARGIN;
  int MAX_SIZE;
  int DEBUG;
  int TAG;
  int ALIGN_MARGINS;
  int ALIGN_BOUNDS;
  int UNDEFINED;
  int VERTICAL;
  int HORIZONTAL;
}
