package java.awt.image;
class WritableRenderedImage {
}
class WritableRaster {
}
class VolatileImage {
  int transparency;
  int IMAGE_INCOMPATIBLE;
  int IMAGE_RESTORED;
  int IMAGE_OK;
}
class TileObserver {
}
class SinglePixelPackedSampleModel {
  int sampleSize;
  int bitOffsets;
  int bitMasks;
  int scanlineStride;
}
class ShortLookupTable {
  int data;
}
class SampleModel {
  int dataType;
  int numBands;
  int height;
  int width;
}
class RescaleOp {
  int hints;
  int offsets;
  int scale;
}
class ReplicateScaleFilter {
  int outpixbuf;
  int srccols;
  int srcrows;
  int srcWidth;
  int srcHeight;
  int destWidth;
  int destHeight;
}
class RenderedImage {
}
class RasterOp {
}
class RasterFormatException {
  int serialVersionUID;
}
class Raster {
  int parent;
  int numDataElements;
  int numBands;
  int sampleModelTranslateY;
  int sampleModelTranslateX;
  int height;
  int width;
  int minY;
  int minX;
  int dataBuffer;
  int sampleModel;
}
class RGBImageFilter {
  int canFilterIndexColorModel;
  int newmodel;
  int origmodel;
}
class PixelInterleavedSampleModel {
}
class PixelGrabber {
  int grabbing;
  int grabberThread;
  int consumerStatus;
  int observerStatus;
  int ip;
  int bytes_delivered;
  int byte_pixel_buffer;
  int ints_delivered;
  int int_pixel_buffer;
  int props;
  int hints;
  int model;
  int forceRGB;
  int scansize;
  int height;
  int width;
  int offset;
  int y;
  int x;
}
class PackedColorModel {
  int shifts;
  int masks;
}
class MultiPixelPackedSampleModel {
  int numElems;
  int numberOfBits;
  int elemBits;
  int dataBitOffset;
  int sampleSize;
  int bitOffsets;
  int bitMasks;
  int scanlineStride;
}
class MemoryImageSource {
  int consumers;
  int props;
  int cm;
  int pixelb;
  int scansize;
  int offset;
  int height;
  int width;
  int pixeli;
  int fullbuffers;
  int animated;
}
class LookupTable {
  int numComponents;
  int offset;
}
class LookupOp {
  int hints;
  int lut;
}
class Kernel {
  int data;
  int height;
  int width;
}
class IndexColorModel {
  int validBits;
  int rgb;
  int trans;
  int opaque;
  int map_size;
}
class ImagingOpException {
  int serialVersionUID;
}
class ImageProducer {
}
class ImageObserver {
  int ABORT;
  int ERROR;
  int ALLBITS;
  int FRAMEBITS;
  int SOMEBITS;
  int PROPERTIES;
  int HEIGHT;
  int WIDTH;
}
class ImageFilter {
  int consumer;
}
class ImageConsumer {
  int IMAGEABORTED;
  int STATICIMAGEDONE;
  int SINGLEFRAMEDONE;
  int IMAGEERROR;
  int SINGLEFRAME;
  int SINGLEPASS;
  int COMPLETESCANLINES;
  int TOPDOWNLEFTRIGHT;
  int RANDOMPIXELORDER;
}
class FilteredImageSource {
  int consumers;
  int filter;
  int ip;
}
class DirectColorModel {
}
class DataBufferUShort {
  int bankData;
  int data;
}
class DataBufferShort {
  int bankData;
  int data;
}
class DataBufferInt {
  int bankData;
  int data;
}
class DataBufferFloat {
  int bankData;
  int data;
}
class DataBufferDouble {
  int bankData;
  int data;
}
class DataBufferByte {
  int bankData;
  int data;
}
class DataBuffer {
  int offsets;
  int size;
  int offset;
  int banks;
  int dataType;
  int TYPE_UNDEFINED;
  int TYPE_DOUBLE;
  int TYPE_FLOAT;
  int TYPE_INT;
  int TYPE_SHORT;
  int TYPE_USHORT;
  int TYPE_BYTE;
}
class CropImageFilter {
  int height;
  int width;
  int y;
  int x;
}
class ConvolveOp {
  int hints;
  int edge;
  int kernel;
  int EDGE_NO_OP;
  int EDGE_ZERO_FILL;
}
class ComponentSampleModel {
  int pixelStride;
  int scanlineStride;
  int numBanks;
  int numBands;
  int bankIndices;
  int bandOffsets;
}
class ComponentColorModel {
}
class ColorModel {
  class SRGBColorModel {
  }
  int S_RGB_MODEL;
  int isAlphaPremultiplied;
  int hasAlpha;
  int transparency;
  int cspace;
  int bits;
  int transferType;
  int pixel_bits;
}
class ColorConvertOp {
  int spaces;
  int profiles;
  int hints;
}
class ByteLookupTable {
  int data;
}
class BufferedImageOp {
}
class BufferedImageFilter {
  int op;
}
class BufferedImage {
  int tileIndices;
  int type;
  int isPremultiplied;
  int properties;
  int colorModel;
  int raster;
  int tileObservers;
  int TYPE_BYTE_INDEXED;
  int TYPE_BYTE_BINARY;
  int TYPE_USHORT_GRAY;
  int TYPE_BYTE_GRAY;
  int TYPE_USHORT_555_RGB;
  int TYPE_USHORT_565_RGB;
  int TYPE_4BYTE_ABGR_PRE;
  int TYPE_4BYTE_ABGR;
  int TYPE_3BYTE_BGR;
  int TYPE_INT_BGR;
  int TYPE_INT_ARGB_PRE;
  int TYPE_INT_ARGB;
  int TYPE_INT_RGB;
  int TYPE_CUSTOM;
}
class BufferStrategy {
}
class BandedSampleModel {
  int numElems;
  int numberOfBits;
  int elemBits;
  int dataBitOffset;
  int sampleSize;
  int bitOffsets;
  int bitMasks;
}
class BandCombineOp {
  int matrix;
  int hints;
}
class AreaAveragingScaleFilter {
}
class AffineTransformOp {
  int hints;
  int transform;
  int TYPE_BICUBIC;
  int TYPE_BILINEAR;
  int TYPE_NEAREST_NEIGHBOR;
}
