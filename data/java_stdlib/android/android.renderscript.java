package android.renderscript;
class Short4 {
  int w;
  int z;
  int y;
  int x;
}
class Short3 {
  int z;
  int y;
  int x;
}
class Short2 {
  int y;
  int x;
}
class ScriptC {
  int TAG;
}
class Script {
  class FieldBase {
    int mAllocation;
    int mElement;
  }
  class Builder {
    int mRS;
  }
}
class Sampler {
  class Builder {
    int mAniso;
    int mWrapR;
    int mWrapT;
    int mWrapS;
    int mMag;
    int mMin;
    int mRS;
  }
  int mAniso;
  int mWrapR;
  int mWrapT;
  int mWrapS;
  int mMag;
  int mMin;
  class Value {
    int CLAMP;
    int WRAP;
    int LINEAR_MIP_NEAREST;
    int LINEAR_MIP_LINEAR;
    int LINEAR;
    int NEAREST;
    int mID;
  }
}
class RenderScriptGL {
  int mSurfaceConfig;
  class SurfaceConfig {
    int mSamplesQ;
    int mSamplesPref;
    int mSamplesMin;
    int mAlphaPref;
    int mAlphaMin;
    int mColorPref;
    int mColorMin;
    int mStencilPref;
    int mStencilMin;
    int mDepthPref;
    int mDepthMin;
  }
  int mHeight;
  int mWidth;
}
class RenderScript {
  class MessageThread {
    int RS_ERROR_FATAL_UNKNOWN;
    int RS_MESSAGE_TO_CLIENT_USER;
    int RS_MESSAGE_TO_CLIENT_ERROR;
    int RS_MESSAGE_TO_CLIENT_RESIZE;
    int RS_MESSAGE_TO_CLIENT_EXCEPTION;
    int RS_MESSAGE_TO_CLIENT_NONE;
    int mAuxData;
    int mRun;
    int mRS;
  }
  class Priority {
    int NORMAL;
    int LOW;
    int mID;
  }
  int mErrorCallback;
  class RSErrorHandler {
    int mErrorNum;
    int mErrorMessage;
  }
  int mMessageCallback;
  class RSMessageHandler {
    int mLength;
    int mID;
    int mData;
  }
  int mProgramRaster_CULL_NONE;
  int mProgramRaster_CULL_FRONT;
  int mProgramRaster_CULL_BACK;
  int mProgramStore_BLEND_ALPHA_DEPTH_NO_DEPTH;
  int mProgramStore_BLEND_ALPHA_DEPTH_TEST;
  int mProgramStore_BLEND_NONE_DEPTH_NO_DEPTH;
  int mProgramStore_BLEND_NONE_DEPTH_TEST;
  int mSampler_WRAP_LINEAR_MIP_LINEAR;
  int mSampler_WRAP_LINEAR;
  int mSampler_WRAP_NEAREST;
  int mSampler_CLAMP_LINEAR_MIP_LINEAR;
  int mSampler_CLAMP_LINEAR;
  int mSampler_CLAMP_NEAREST;
  int mElement_MATRIX_2X2;
  int mElement_MATRIX_3X3;
  int mElement_MATRIX_4X4;
  int mElement_LONG_4;
  int mElement_LONG_3;
  int mElement_LONG_2;
  int mElement_ULONG_4;
  int mElement_ULONG_3;
  int mElement_ULONG_2;
  int mElement_INT_4;
  int mElement_INT_3;
  int mElement_INT_2;
  int mElement_UINT_4;
  int mElement_UINT_3;
  int mElement_UINT_2;
  int mElement_SHORT_4;
  int mElement_SHORT_3;
  int mElement_SHORT_2;
  int mElement_USHORT_4;
  int mElement_USHORT_3;
  int mElement_USHORT_2;
  int mElement_CHAR_4;
  int mElement_CHAR_3;
  int mElement_CHAR_2;
  int mElement_UCHAR_4;
  int mElement_UCHAR_3;
  int mElement_UCHAR_2;
  int mElement_DOUBLE_4;
  int mElement_DOUBLE_3;
  int mElement_DOUBLE_2;
  int mElement_FLOAT_4;
  int mElement_FLOAT_3;
  int mElement_FLOAT_2;
  int mElement_RGBA_8888;
  int mElement_RGBA_4444;
  int mElement_RGBA_5551;
  int mElement_RGB_888;
  int mElement_RGB_565;
  int mElement_A_8;
  int mElement_FONT;
  int mElement_PROGRAM_STORE;
  int mElement_PROGRAM_RASTER;
  int mElement_PROGRAM_VERTEX;
  int mElement_PROGRAM_FRAGMENT;
  int mElement_MESH;
  int mElement_SCRIPT;
  int mElement_SAMPLER;
  int mElement_ALLOCATION;
  int mElement_TYPE;
  int mElement_ELEMENT;
  int mElement_BOOLEAN;
  int mElement_F64;
  int mElement_F32;
  int mElement_I64;
  int mElement_U64;
  int mElement_I32;
  int mElement_U32;
  int mElement_I16;
  int mElement_U16;
  int mElement_I8;
  int mElement_U8;
  int mMessageThread;
  int mContext;
  int mDev;
  int mCachePath;
  int CACHE_PATH;
  int sInitialized;
  int mApplicationContext;
  int LOG_ENABLED;
  int DEBUG;
  int LOG_TAG;
}
class RSTextureView {
  int mSurfaceTexture;
  int mRS;
}
class RSSurfaceView {
  int mRS;
  int mSurfaceHolder;
}
class RSRuntimeException {
}
class RSInvalidStateException {
}
class RSIllegalArgumentException {
}
class RSDriverException {
}
class ProgramVertexFixedFunction {
  class Constants {
    int mIOBuffer;
    int mAlloc;
    int mTexture;
    int mProjection;
    int mModel;
    int TEXTURE_OFFSET;
    int PROJECTION_OFFSET;
    int MODELVIEW_OFFSET;
  }
  class Builder {
    int mRS;
    int mShader;
    int mTextureMatrixEnable;
  }
  class InternalBuilder {
  }
}
class ProgramVertex {
  class Builder {
  }
}
class ProgramStore {
  class Builder {
    int mDither;
    int mBlendDst;
    int mBlendSrc;
    int mColorMaskA;
    int mColorMaskB;
    int mColorMaskG;
    int mColorMaskR;
    int mDepthMask;
    int mDepthFunc;
    int mRS;
  }
  int mDither;
  int mBlendDst;
  int mBlendSrc;
  int mColorMaskA;
  int mColorMaskB;
  int mColorMaskG;
  int mColorMaskR;
  int mDepthMask;
  int mDepthFunc;
  class BlendDstFunc {
    int ONE_MINUS_DST_ALPHA;
    int DST_ALPHA;
    int ONE_MINUS_SRC_ALPHA;
    int SRC_ALPHA;
    int ONE_MINUS_SRC_COLOR;
    int SRC_COLOR;
    int ONE;
    int ZERO;
    int mID;
  }
  class BlendSrcFunc {
    int SRC_ALPHA_SATURATE;
    int ONE_MINUS_DST_ALPHA;
    int DST_ALPHA;
    int ONE_MINUS_SRC_ALPHA;
    int SRC_ALPHA;
    int ONE_MINUS_DST_COLOR;
    int DST_COLOR;
    int ONE;
    int ZERO;
    int mID;
  }
  class DepthFunc {
    int NOT_EQUAL;
    int EQUAL;
    int GREATER_OR_EQUAL;
    int GREATER;
    int LESS_OR_EQUAL;
    int LESS;
    int ALWAYS;
    int mID;
  }
}
class ProgramRaster {
  class Builder {
    int mCullMode;
    int mPointSprite;
    int mRS;
  }
  int mCullMode;
  int mPointSprite;
  class CullMode {
    int NONE;
    int FRONT;
    int BACK;
    int mID;
  }
}
class ProgramFragmentFixedFunction {
  class Builder {
    int mSlots;
    class Slot {
      int format;
      int env;
    }
    class Format {
      int RGBA;
      int RGB;
      int LUMINANCE_ALPHA;
      int ALPHA;
      int mID;
    }
    class EnvMode {
      int DECAL;
      int MODULATE;
      int REPLACE;
      int mID;
    }
    int mRS;
    int mShader;
    int mVaryingColorEnable;
    int mPointSpriteEnable;
    int mNumTextures;
    int MAX_TEXTURE;
  }
  class InternalBuilder {
  }
}
class ProgramFragment {
  class Builder {
  }
}
class Program {
  class BaseProgramBuilder {
    int mShader;
    int mTextureCount;
    int mConstantCount;
    int mOutputCount;
    int mInputCount;
    int mTextureNames;
    int mTextureTypes;
    int mTextures;
    int mConstants;
    int mOutputs;
    int mInputs;
    int mRS;
  }
  int mShader;
  int mTextureCount;
  int mTextureNames;
  int mTextures;
  int mConstants;
  int mOutputs;
  int mInputs;
  class ProgramParam {
    int TEXTURE_TYPE;
    int CONSTANT;
    int OUTPUT;
    int INPUT;
    int mID;
  }
  class TextureType {
    int TEXTURE_CUBE;
    int TEXTURE_2D;
    int mID;
  }
  int MAX_TEXTURE;
  int MAX_CONSTANT;
  int MAX_OUTPUT;
  int MAX_INPUT;
}
class Path {
  int mCoverageToAlpha;
  int mQuality;
  int mPrimitive;
  int mLoopBuffer;
  int mVertexBuffer;
  class Primitive {
    int CUBIC_BEZIER;
    int QUADRATIC_BEZIER;
    int mID;
  }
}
class Mesh {
  class TriangleMeshBuilder {
    int TEXTURE_0;
    int NORMAL;
    int COLOR;
    int mFlags;
    int mVtxSize;
    int mA;
    int mB;
    int mG;
    int mR;
    int mT0;
    int mS0;
    int mNZ;
    int mNY;
    int mNX;
    int mElement;
    int mRS;
    int mIndexCount;
    int mIndexData;
    int mMaxIndex;
    int mVtxCount;
    int mVtxData;
  }
  class AllocationBuilder {
    int mIndexTypes;
    int mVertexTypes;
    int mVertexTypeCount;
    class Entry {
      int prim;
      int a;
    }
    int mRS;
  }
  class Builder {
    int mIndexTypes;
    int mVertexTypes;
    int mVertexTypeCount;
    class Entry {
      int usage;
      int prim;
      int size;
      int e;
      int t;
    }
    int mUsage;
    int mRS;
  }
  int mPrimitives;
  int mIndexBuffers;
  int mVertexBuffers;
  class Primitive {
    int TRIANGLE_FAN;
    int TRIANGLE_STRIP;
    int TRIANGLE;
    int LINE_STRIP;
    int LINE;
    int POINT;
    int mID;
  }
}
class Matrix4f {
  int mMat;
}
class Matrix3f {
  int mMat;
}
class Matrix2f {
  int mMat;
}
class Long4 {
  int w;
  int z;
  int y;
  int x;
}
class Long3 {
  int z;
  int y;
  int x;
}
class Long2 {
  int y;
  int x;
}
class Int4 {
  int w;
  int z;
  int y;
  int x;
}
class Int3 {
  int z;
  int y;
  int x;
}
class Int2 {
  int y;
  int x;
}
class Font {
  class Style {
    int BOLD_ITALIC;
    int ITALIC;
    int BOLD;
    int NORMAL;
  }
  int sFontFamilyMap;
  class FontFamily {
    int mBoldItalicFileName;
    int mItalicFileName;
    int mBoldFileName;
    int mNormalFileName;
    int mNames;
  }
  int sMonoNames;
  int sSerifNames;
  int sSansNames;
}
class Float4 {
  int w;
  int z;
  int y;
  int x;
}
class Float3 {
  int z;
  int y;
  int x;
}
class Float2 {
  int y;
  int x;
}
class FileA3D {
  int mInputStream;
  int mFileEntries;
  class IndexEntry {
    int mLoadedObj;
    int mEntryType;
    int mName;
    int mID;
    int mIndex;
    int mRS;
  }
  class EntryType {
    int MESH;
    int UNKNOWN;
    int mID;
  }
}
class FieldPacker {
  int mLen;
  int mPos;
  int mData;
}
class Element {
  class Builder {
    int mSkipPadding;
    int mCount;
    int mArraySizes;
    int mElementNames;
    int mElements;
    int mRS;
  }
  class DataKind {
    int PIXEL_DEPTH;
    int PIXEL_RGBA;
    int PIXEL_RGB;
    int PIXEL_LA;
    int PIXEL_A;
    int PIXEL_L;
    int USER;
    int mID;
  }
  class DataType {
    int RS_FONT;
    int RS_PROGRAM_STORE;
    int RS_PROGRAM_RASTER;
    int RS_PROGRAM_VERTEX;
    int RS_PROGRAM_FRAGMENT;
    int RS_MESH;
    int RS_SCRIPT;
    int RS_SAMPLER;
    int RS_ALLOCATION;
    int RS_TYPE;
    int RS_ELEMENT;
    int MATRIX_2X2;
    int MATRIX_3X3;
    int MATRIX_4X4;
    int UNSIGNED_4_4_4_4;
    int UNSIGNED_5_5_5_1;
    int UNSIGNED_5_6_5;
    int BOOLEAN;
    int UNSIGNED_64;
    int UNSIGNED_32;
    int UNSIGNED_16;
    int UNSIGNED_8;
    int SIGNED_64;
    int SIGNED_32;
    int SIGNED_16;
    int SIGNED_8;
    int FLOAT_64;
    int FLOAT_32;
    int NONE;
    int mSize;
    int mID;
  }
  int mVectorSize;
  int mNormalized;
  int mKind;
  int mType;
  int mVisibleElementMap;
  int mOffsetInBytes;
  int mArraySizes;
  int mElementNames;
  int mElements;
  int mSize;
}
class Double4 {
  int w;
  int z;
  int y;
  int x;
}
class Double3 {
  int z;
  int y;
  int x;
}
class Double2 {
  int y;
  int x;
}
class Byte4 {
  int w;
  int z;
  int y;
  int x;
}
class Byte3 {
  int z;
  int y;
  int x;
}
class Byte2 {
  int y;
  int x;
}
class BaseObj {
  int mRS;
  int mName;
  int mDestroyed;
  int mID;
}
class AllocationAdapter {
}
class Allocation {
  int mBitmapOptions;
  class MipmapControl {
    int MIPMAP_ON_SYNC_TO_TEXTURE;
    int MIPMAP_FULL;
    int MIPMAP_NONE;
    int mID;
  }
  int USAGE_IO_OUTPUT;
  int USAGE_IO_INPUT;
  int USAGE_GRAPHICS_RENDER_TARGET;
  int USAGE_GRAPHICS_CONSTANTS;
  int USAGE_GRAPHICS_VERTEX;
  int USAGE_GRAPHICS_TEXTURE;
  int USAGE_SCRIPT;
  int mCurrentCount;
  int mCurrentDimZ;
  int mCurrentDimY;
  int mCurrentDimX;
  int mSelectedFace;
  int mSelectedLOD;
  int mSelectedZ;
  int mSelectedY;
  int mWriteAllowed;
  int mReadAllowed;
  int mConstrainedZ;
  int mConstrainedY;
  int mConstrainedFace;
  int mConstrainedLOD;
  int mAdaptedAllocation;
  int mUsage;
  int mBitmap;
  int mType;
}
