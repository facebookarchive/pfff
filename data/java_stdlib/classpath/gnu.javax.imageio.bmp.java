package gnu.javax.imageio.bmp;
class EncodeRLE8 {
  int DELTA;
  int EOB;
  int EOL;
  int ESCAPE;
  int offset;
  int fileHeader;
  int infoHeader;
}
class EncodeRLE4 {
  int DELTA;
  int EOB;
  int EOL;
  int ESCAPE;
  int offset;
  int fileHeader;
  int infoHeader;
}
class EncodeRGB8 {
  int offset;
  int fileHeader;
  int infoHeader;
}
class EncodeRGB4 {
  int offset;
  int fileHeader;
  int infoHeader;
}
class EncodeRGB32 {
  int offset;
  int fileHeader;
  int infoHeader;
}
class EncodeRGB24 {
  int offset;
  int fileHeader;
  int infoHeader;
}
class EncodeRGB16 {
  int offset;
  int fileHeader;
  int infoHeader;
}
class EncodeRGB1 {
  int offset;
  int fileHeader;
  int infoHeader;
}
class DecodeRLE8 {
  int DELTA;
  int EOB;
  int EOL;
  int ESCAPE;
}
class DecodeRLE4 {
  int DELTA;
  int EOB;
  int EOL;
  int ESCAPE;
}
class DecodeRGB8 {
}
class DecodeRGB4 {
}
class DecodeRGB24 {
}
class DecodeRGB1 {
}
class DecodeBF32 {
  int useDefaultMasks;
  int bitmasks;
}
class DecodeBF16 {
  int useDefaultMasks;
  int bitmasks;
}
class BMPInfoHeader {
  int BI_BITFIELDS;
  int BI_RLE4;
  int BI_RLE8;
  int BI_RGB;
  int SIZE;
  int biClrImportant;
  int biClrUsed;
  int biYPelsPerMeter;
  int biXPelsPerMeter;
  int biSizeImage;
  int biCompression;
  int biBitCount;
  int biPlanes;
  int biHeight;
  int biWidth;
  int biSize;
}
class BMPImageWriterSpi {
  int writerInstance;
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
  int readerSpiNames;
  int MIMETypes;
  int suffixes;
  int names;
  int writerClassName;
  int version;
  int vendorName;
}
class BMPImageWriter {
  int infoHeader;
  int fileHeader;
  int encoder;
}
class BMPImageReaderSpi {
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
class BMPImageReader {
  int decoder;
  int fileHeader;
  int infoHeader;
}
class BMPFileHeader {
  int BITMAPINFOHEADER_SIZE;
  int SIZE;
  int bfOffBits;
  int bfSize;
  int bfType;
}
class BMPException {
}
class BMPEncoder {
}
class BMPDecoder {
  int offset;
  int fileHeader;
  int infoHeader;
}
