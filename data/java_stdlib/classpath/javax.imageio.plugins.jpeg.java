package javax.imageio.plugins.jpeg;
class JPEGQTable {
  int K2Div2Chrominance;
  int K2Chrominance;
  int K1Div2Luminance;
  int K1Luminance;
  int table;
}
class JPEGImageWriteParam {
  int messages;
  int compressionQualityValues;
  int compressionQualityDescriptions;
  int optimize;
  int ACHuffmanTables;
  int DCHuffmanTables;
  int qTables;
}
class JPEGImageReadParam {
  int ACHuffmanTables;
  int DCHuffmanTables;
  int qTables;
}
class JPEGHuffmanTable {
  int StdDCLuminance;
  int StdDCChrominance;
  int StdACLuminance;
  int StdACChrominance;
  int values;
  int lengths;
}
