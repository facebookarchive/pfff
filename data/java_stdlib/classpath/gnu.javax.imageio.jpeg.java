package gnu.javax.imageio.jpeg;
class ZigZag {
  int ZIGZAG_8X8_MAP;
  int ZIGZAG_BACKWARD;
  int ZIGZAG_FORWARD;
}
class YCbCr_ColorSpace {
}
class JPEGScan {
  int components;
  int numOfComponentBlocks;
  int numOfComponents;
  int maxH;
  int maxV;
  int maxWidth;
  int maxHeight;
}
class JPEGMarkerFoundException {
}
class JPEGMarker {
  int DNL;
  int EOI;
  int SOI;
  int COM;
  int DRI;
  int SOS;
  int DQT;
  int DHT;
  int SOF15;
  int SOF14;
  int SOF13;
  int SOF11;
  int SOF10;
  int SOF9;
  int SOF7;
  int SOF6;
  int SOF5;
  int SOF3;
  int SOF2;
  int SOF1;
  int SOF0;
  int RST7;
  int RST6;
  int RST5;
  int RST4;
  int RST3;
  int RST2;
  int RST1;
  int RST0;
  int APP15;
  int APP14;
  int APP13;
  int APP12;
  int APP11;
  int APP10;
  int APP9;
  int APP8;
  int APP7;
  int APP6;
  int APP5;
  int APP4;
  int APP3;
  int APP2;
  int APP1;
  int APP0;
  int X00;
  int XFF;
  int JFXX_THREE_BPP;
  int JFXX_ONE_BPP;
  int JFXX_JPEG;
  int JFIF_X;
  int JFIF_I;
  int JFIF_F;
  int JFIF_J;
}
class JPEGImageReaderSpi {
  int readerSpi;
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
class JPEGImageReader {
  int decoder;
}
class JPEGImageInputStream {
  int marker;
  int in;
}
class JPEGFrame {
  int components;
  int height;
  int width;
  int componentCount;
  int colorMode;
  int precision;
  int JPEG_COLOR_CMYK;
  int JPEG_COLOR_YCbCr;
  int JPEG_COLOR_RGB;
  int JPEG_COLOR_GRAY;
}
class JPEGException {
}
class JPEGDecoder {
  int qTables;
  int acTables;
  int dcTables;
  int jpegFrames;
  int jpegStream;
  int JFXX_FIXED_LENGTH;
  int JFIF_FIXED_LENGTH;
  int MINOR_VERSION;
  int MAJOR_VERSION;
  int marker;
  int height;
  int width;
  int image;
  int thumbnail;
  int Ythumbnail;
  int Xthumbnail;
  int Ydensity;
  int Xdensity;
  int units;
  int minorVersion;
  int majorVersion;
}
class JPEGComponent {
  int data;
  int previousDC;
  int quantizationTable;
  int DCTable;
  int ACTable;
  int maxH;
  int maxV;
  int height;
  int width;
  int quant_id;
  int component_id;
  int factorV;
  int factorH;
}
class HuffmanTable {
  int lastk;
  int JPEG_AC_TABLE;
  int JPEG_DC_TABLE;
  int bits;
  int huffval;
  int maxcode;
  int mincode;
  int valptr;
  int EHUFSI;
  int EHUFCO;
  int huffsize;
  int huffcode;
  int HUFFMAN_MAX_TABLES;
}
class DCT {
  int cT;
  int c;
}
