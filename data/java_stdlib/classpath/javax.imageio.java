package javax.imageio;
class ImageWriter {
  int warningLocales;
  int warningListeners;
  int progressListeners;
  int output;
  int originatingProvider;
  int locale;
  int availableLocales;
  int aborted;
}
class ImageWriteParam {
  int tilingSet;
  int tilingMode;
  int tileWidth;
  int tileHeight;
  int tileGridYOffset;
  int tileGridXOffset;
  int progressiveMode;
  int preferredTileSizes;
  int locale;
  int compressionTypes;
  int compressionType;
  int compressionQuality;
  int compressionMode;
  int canWriteTiles;
  int canWriteProgressive;
  int canWriteCompressed;
  int canOffsetTiles;
  int MODE_COPY_FROM_METADATA;
  int MODE_EXPLICIT;
  int MODE_DEFAULT;
  int MODE_DISABLED;
}
class ImageTypeSpecifier {
  int sampleModel;
  int colorModel;
}
class ImageTranscoder {
}
class ImageReader {
  int warningLocales;
  int warningListeners;
  int updateListeners;
  int seekForwardOnly;
  int progressListeners;
  int originatingProvider;
  int minIndex;
  int locale;
  int input;
  int ignoreMetadata;
  int availableLocales;
  int aborted;
}
class ImageReadParam {
  int sourceRenderSize;
  int numProgressivePasses;
  int minProgressivePass;
  int destinationBands;
  int destination;
  int canSetSourceRenderSize;
}
class ImageIO {
  int useCache;
  int cacheDirectory;
  class ImageWriterIterator {
    int writerExtension;
    int it;
  }
  class ImageReaderIterator {
    int readerExtension;
    int it;
  }
  class TranscoderFilter {
    int writer;
    int reader;
  }
  class WriterObjectFilter {
    int formatName;
    int type;
  }
  class WriterSuffixFilter {
    int fileSuffix;
  }
  class WriterMIMETypeFilter {
    int MIMEType;
  }
  class WriterFormatFilter {
    int formatName;
  }
  class ReaderSuffixFilter {
    int fileSuffix;
  }
  class ReaderObjectFilter {
    int object;
  }
  class ReaderMIMETypeFilter {
    int MIMEType;
  }
  class ReaderFormatFilter {
    int formatName;
  }
}
class IIOParamController {
}
class IIOParam {
  int no_controller;
  int subsamplingYOffset;
  int subsamplingXOffset;
  int sourceYSubsampling;
  int sourceXSubsampling;
  int sourceRegion;
  int sourceBands;
  int destinationType;
  int destinationOffset;
  int defaultController;
  int controller;
}
class IIOImage {
  int thumbnails;
  int raster;
  int metadata;
  int image;
}
class IIOException {
}
