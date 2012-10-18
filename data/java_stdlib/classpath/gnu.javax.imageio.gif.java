package gnu.javax.imageio.gif;
class GIFImageReaderSpi {
  int extraImageMetadataFormatClassNames;
  int extraImageMetadataFormatNames;
  int nativeImageMetadataFormatClassName;
  int nativeImageMetadataFormatName;
  int supportsStandardImageMetadataFormat;
  int extraStreamMetadataFormatClassNames;
  int extraStreamMetadataFormatNames;
  int nativeStreamMetadataFormatClassName;
  int nativeStreamMetadataFormatName;
  int supportsStandardStreamMetadataFormat;
  int writerSpiNames;
  int MIMETypes;
  int suffixes;
  int names;
  int readerClassName;
  int version;
  int vendorName;
}
class GIFImageReader {
  int file;
}
class GIFFile {
  class GIFException {
  }
  int animationFrames;
  int loops;
  int isLooped;
  int currentBits;
  int remainingBits;
  int comment;
  int dataBlockIndex;
  int duration;
  int compressedData;
  int raster;
  int transparentIndex;
  int undraw;
  int hasTransparency;
  int interlaced;
  int localPalette;
  int hasGlobalColorMap;
  int globalPalette;
  int nColors;
  int bgIndex;
  int globalHeight;
  int globalWidth;
  int height;
  int width;
  int y;
  int x;
  int UNDRAW_RESTORE_PREVIOUS;
  int UNDRAW_RESTORE_BACKGROUND;
  int UNDRAW_OVERWRITE;
  int EXTENSION_APPLICATION;
  int EXTENSION_GCONTROL;
  int EXTENSION_COMMENT;
  int TERMINATOR;
  int LOCAL;
  int EXTENSION;
  int nsBlock;
}
