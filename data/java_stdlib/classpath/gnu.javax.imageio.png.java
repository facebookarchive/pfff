package gnu.javax.imageio.png;
class PNGTime {
  int date;
}
class PNGPhys {
  int usesRatio;
  int ratio;
  int y;
  int x;
}
class PNGPalette {
  int blue;
  int green;
  int red;
}
class PNGImageReaderSpi {
  int INPUT_TYPES;
  int WRITER_SPI_NAMES;
  int MIME_TYPES;
  int SUFFIXES;
  int NAMES;
  int READER_CLASSNAME;
  int VERSION;
  int VENDOR_NAME;
  int SIGNATURE;
}
class PNGImageReader {
  int imageTypes;
  int image;
  int pngFile;
}
class PNGICCProfile {
  int genericName;
  int profile;
  int name;
}
class PNGHeader {
  int RGB_WITH_ALPHA;
  int GRAYSCALE_WITH_ALPHA;
  int INDEXED;
  int RGB;
  int GRAYSCALE;
  int INTERLACE_ADAM7;
  int INTERLACE_NONE;
  int interlace;
  int filter;
  int compression;
  int colorType;
  int depth;
  int height;
  int width;
}
class PNGGamma {
  int gamma;
}
class PNGFilter {
  int FILTER_PAETH;
  int FILTER_AVERAGE;
  int FILTER_UP;
  int FILTER_SUB;
  int FILTER_NONE;
}
class PNGFile {
  int sourceImage;
  int encoder;
  int decoder;
  int height;
  int width;
  int hasPalette;
  int header;
  int chunks;
  int endChunk;
  int signature;
}
class PNGException {
}
class PNGEncoder {
  int profile;
  int rawData;
  int bpp;
  int stride;
  int palette;
  int header;
  int defaultChunkSize;
}
class PNGDecoder {
  int bpp;
  int readFilter;
  int inflater;
  int stride;
  int currentScanline;
  int length;
  int offset;
  int filterType;
  int lastScanline;
  int scanline;
  int raster;
  int header;
}
class PNGData {
  int offset;
}
class PNGChunk {
  int crc;
  int data;
  int type;
  int TYPE_PROFILE;
  int TYPE_GAMMA;
  int TYPE_PHYS;
  int TYPE_END;
  int TYPE_TIME;
  int TYPE_DATA;
  int TYPE_PALETTE;
  int TYPE_HEADER;
  int crcTable;
}
