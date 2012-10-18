package gnu.java.awt.color;
class ToneReproductionCurve {
  int reverseTrc;
  int gamma;
  int trc;
}
class TagEntry {
  int data;
  int offset;
  int size;
  int signature;
  int entrySize;
}
class SrgbConverter {
}
class RgbProfileConverter {
  int D50;
  int fromPCS;
  int toPCS;
  int bTRC;
  int gTRC;
  int rTRC;
  int inv_matrix;
  int matrix;
}
class PyccConverter {
}
class ProfileHeader {
  int creatorSig;
  int illuminant;
  int intent;
  int attributes;
  int modelSig;
  int manufacturerSig;
  int magic;
  int flags;
  int platform;
  int timestamp;
  int profileColorSpace;
  int colorSpace;
  int profileClass;
  int minorVersion;
  int majorVersion;
  int cmmId;
  int size;
  int classMap;
  int HEADERSIZE;
  int csTypeMap;
  int icMagicNumber;
}
class LinearRGBConverter {
}
class GrayScaleConverter {
  int D50;
  int coeff;
}
class GrayProfileConverter {
  int fromPCS;
  int toPCS;
  int trc;
  int gc;
}
class ColorSpaceConverter {
}
class ColorLookUpTable {
  int outputLab;
  int inputLab;
  int offsets;
  int multiplier;
  int useMatrix;
  int inMatrix;
  int clut;
  int outTable;
  int inTable;
  int nClut;
  int gridpoints;
  int nOutTableEntries;
  int nInTableEntries;
  int nOut;
  int nIn;
  int D50;
}
class ClutProfileConverter {
  int nChannels;
  int fromPCS;
  int toPCS;
}
class CieXyzConverter {
}
