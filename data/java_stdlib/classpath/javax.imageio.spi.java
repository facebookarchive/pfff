package javax.imageio.spi;
class ServiceRegistry {
  class Filter {
  }
  int constraints;
  int providers;
  int categories;
}
class RegisterableService {
}
class ImageWriterSpi {
  int readerSpiNames;
  int outputTypes;
  int STANDARD_OUTPUT_TYPE;
}
class ImageTranscoderSpi {
}
class ImageReaderWriterSpi {
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
  int pluginClassName;
  int MIMETypes;
  int suffixes;
  int names;
}
class ImageReaderSpi {
  int writerSpiNames;
  int inputTypes;
  int STANDARD_INPUT_TYPE;
}
class ImageOutputStreamSpi {
  int outputClass;
}
class ImageInputStreamSpi {
  int inputClass;
}
class IIOServiceProvider {
  int version;
  int vendorName;
}
class IIORegistry {
  int instances;
  int defaultCategories;
}
