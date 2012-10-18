package gnu.java.awt.peer.gtk;
class VolatileImageGraphics {
  int buffer;
  int owner;
}
class GtkWindowPeer {
  int height;
  int width;
  int y;
  int x;
  int windowState;
  int GDK_WINDOW_TYPE_HINT_DESKTOP;
  int GDK_WINDOW_TYPE_HINT_DOCK;
  int GDK_WINDOW_TYPE_HINT_UTILITY;
  int GDK_WINDOW_TYPE_HINT_SPLASHSCREEN;
  int GDK_WINDOW_TYPE_HINT_TOOLBAR;
  int GDK_WINDOW_TYPE_HINT_MENU;
  int GDK_WINDOW_TYPE_HINT_DIALOG;
  int GDK_WINDOW_TYPE_HINT_NORMAL;
}
class GtkVolatileImage {
  int nativePointer;
  int gdkColorModel;
  int component;
  int caps;
  int height;
  int width;
}
class GtkToolkit {
  int imageCache;
  int fontCache;
  class LRUCache {
    int max_entries;
  }
  int initializedGlobalIDs;
  int q;
  int GTK_LOCK;
}
class GtkTextFieldPeer {
}
class GtkTextAreaPeer {
  int DEFAULT_COLS;
  int DEFAULT_ROWS;
}
class GtkSelection {
  int bytes;
  int bytesDelivered;
  int uris;
  int urisDelivered;
  int image;
  int imagePointer;
  int imageDelivered;
  int text;
  int textDelivered;
  int dataFlavors;
  int mimeTypesDelivered;
  int requestInProgress;
  int clipboard;
  int requestLock;
}
class GtkScrollbarPeer {
}
class GtkScrollPanePeer {
}
class GtkPopupMenuPeer {
}
class GtkPanelPeer {
}
class GtkMouseInfoPeer {
  int gde;
}
class GtkMenuPeer {
}
class GtkMenuItemPeer {
}
class GtkMenuComponentPeer {
}
class GtkMenuBarPeer {
  int hasHelpMenu;
}
class GtkMainThread {
  int mainThread;
  int runningLock;
  int running;
  int nWindowsLock;
  int numberOfWindows;
}
class GtkListPeer {
}
class GtkLabelPeer {
}
class GtkImageConsumer {
  int source;
  int pixelCache;
  int properties;
  int height;
  int width;
  int target;
}
class GtkImage {
  int pixbufLock;
  int errorImage;
  int nativeModel;
  int source;
  int errorLoading;
  int observers;
  int pixbuf;
  int isLoaded;
  int props;
  int height;
  int width;
}
class GtkGenericPeer {
  int globalRef;
  int widget;
  int awtWidget;
  int next_native_state;
  int native_state;
}
class GtkFramePeer {
  int menuBar;
  int menuBarHeight;
}
class GtkFileDialogPeer {
  int filter;
  int currentDirectory;
  int currentFile;
  int FS;
}
class GtkEmbeddedWindowPeer {
}
class GtkDialogPeer {
}
class GtkCursor {
  int hotspot;
  int image;
}
class GtkContainerPeer {
  int c;
}
class GtkComponentPeer {
  class RepaintTimerTask {
    int awtComponent;
    int height;
    int width;
    int y;
    int x;
    int repaintTimer;
  }
  int currentPaintArea;
  int insets;
  int awtComponent;
  int caps;
  int backBuffer;
}
class GtkClipboardNotifier {
  int notifier;
  int announcePrimaryChange;
  int announceClipboardChange;
}
class GtkClipboard {
  int canCache;
  int filesMimeType;
  int imageMimeType;
  int stringMimeType;
  int selection;
  int clipboard;
}
class GtkChoicePeer {
  int selected;
}
class GtkCheckboxPeer {
  int groupMap;
  int currentState;
  int current_group;
}
class GtkCheckboxMenuItemPeer {
}
class GtkCanvasPeer {
}
class GtkButtonPeer {
}
class GdkScreenGraphicsDevice {
  class X11DisplayMode {
    int height;
    int width;
    int rates;
  }
  int screen;
  int fixedDisplayMode;
  int displayModes;
  int idString;
  int env;
  int configurations;
  int bounds;
  int oldWindowBounds;
  int oldWindowDecorationState;
  int fullscreenWindow;
  int native_state;
}
class GdkRobotPeer {
  int cm;
}
class GdkPixbufDecoder {
  class GdkPixbufReader {
    int ext;
    int height;
    int width;
    int defaultModel;
    int bufferedImage;
    int dec;
  }
  class GdkPixbufWriter {
    int exception;
    int data;
    int DATADONE;
    int ext;
  }
  class GdkPixbufReaderSpi {
  }
  class GdkPixbufWriterSpi {
  }
  int writerSpi;
  int readerSpi;
  int imageFormatSpecs;
  class ImageFormatSpec {
    int extensions;
    int mimeTypes;
    int writable;
    int name;
  }
  int cm;
  int nativeDecoder;
  int curr;
  int needsClose;
  int native_state;
  int pixbufLock;
}
class GdkGraphicsEnvironment {
  int display;
  int devices;
  int defaultDevice;
  int native_state;
}
class GdkGraphicsConfiguration {
  int translucentColorModel;
  int bitmaskColorModel;
  int opaqueColorModel;
  int gdkScreenGraphicsDevice;
}
class GdkFontPeer {
  class GdkFontLineMetrics {
    int nchars;
  }
  int nativeFont;
  int nameTable;
  int metrics;
  int underlineThickness;
  int underlineOffset;
  int height;
  int maxAdvance;
  int maxDescent;
  int maxAscent;
  int descent;
  int ascent;
  int FONT_METRICS_UNDERLINE_THICKNESS;
  int FONT_METRICS_UNDERLINE_OFFSET;
  int FONT_METRICS_HEIGHT;
  int FONT_METRICS_MAX_ADVANCE;
  int FONT_METRICS_MAX_DESCENT;
  int FONT_METRICS_DESCENT;
  int FONT_METRICS_MAX_ASCENT;
  int FONT_METRICS_ASCENT;
  int metricsCache;
  int native_state;
  class GdkFontMetrics {
  }
  int textLayoutCache;
  int DEFAULT_CTX;
}
class FreetypeGlyphVector {
  int metricsCache;
  int glyphTransforms;
  int fontSet;
  int glyphCodes;
  int nGlyphs;
  int frc;
  int s;
  int glyphPositions;
  int logicalBounds;
  int peer;
  int font;
}
class ComponentGraphicsCopy {
  int height;
  int width;
  int gtkimage;
  int component;
}
class ComponentGraphics {
  int ONE;
  int hasLock;
  int componentBuffer;
  int buffer;
  int cairo_t;
  int component;
  int hasXRenderExtension;
}
class CairoSurfaceGraphics {
  int cairo_t;
  int buffer;
  int surface;
}
class CairoSurface {
  int cairoCM_opaque;
  int cairoCM_pre;
  int cairoColorModel;
  int sharedBuffer;
  int surfacePointer;
  int height;
  int width;
}
class CairoGraphics2D {
  int ALPHA_INTERPOLATION_DEFAULT;
  int ALPHA_INTERPOLATION_QUALITY;
  int ALPHA_INTERPOLATION_SPEED;
  int INTERPOLATION_BICUBIC;
  int INTERPOLATION_BILINEAR;
  int INTERPOLATION_NEAREST;
  int argb32;
  int rgb32;
  int draw3DRectStroke;
  int originalClip;
  int firstClip;
  int shiftDrawCalls;
  int ignoreAA;
  int antialias;
  int hints;
  int compCtx;
  int comp;
  int font;
  int transform;
  int clip;
  int bg;
  int fg;
  int stroke;
  int customPaint;
  int paint;
  int nativePointer;
}
class BufferedImageGraphics {
  int hasAlpha;
  int hasFastCM;
  int cairo_t;
  int bufferedImages;
  int surface;
  int imageHeight;
  int imageWidth;
  int buffer;
  int image;
}
class AsyncImage {
  int observers;
  int realImage;
  class Loader {
    int url;
  }
  class NullImageSource {
    int consumers;
  }
}
