package com.android.scenegraph;
class VertexShader {
  class Builder {
    int mBuilder;
    int mShader;
  }
  int mField;
  int mProgram;
}
class TransformParam {
  int mLight;
  int mTransform;
}
class Transform {
  int mTransformData;
  int mField;
  int mChildren;
  int mParent;
}
class TextureRenderTarget {
}
class TextureParam {
  int mTexture;
}
class TextureCube {
  int mResourceID;
  int mFileDir;
  int mFileName;
}
class TextureBase {
  int mRsTexture;
  int mField;
  int mData;
  class SingleImageLoaderTask {
  }
}
class Texture2D {
  int mResourceID;
  int mFileDir;
  int mFileName;
}
class ShaderParam {
  int mCamera;
  int mParamName;
  int mField;
  int mData;
  int sMaxTimeStamp;
  int modelViewProj;
  int modelView;
  int model;
  int viewProj;
  int proj;
  int view;
  int lightDir;
  int lightPos;
  int lightColor;
  int cameraDir;
  int cameraPos;
}
class Shader {
  int mConstantBufferParams;
  int mConstantBuffer;
  int mTextureTypes;
  int mTextureNames;
  int mShaderTextureTypes;
  int mShaderTextureNames;
  int mSourceParams;
  int mPerShaderConstants;
  int mPerObjConstants;
}
class SceneManager {
  class SceneLoadedCallback {
    int mName;
    int mLoadedScene;
  }
  int mDefaultTransform;
  int mDefaultState;
  int mDefaultVertex;
  int mTexture;
  int mColor;
  int mDefaultCube;
  int mDefault2D;
  int sSceneManager;
  int mActiveScene;
  int mHeight;
  int mWidth;
  int mQuad;
  int mRes;
  int mRS;
  int mExportScript;
  int mTransformScript;
  int mCullScript;
  int mVertexParamsScript;
  int mFragmentParamsScript;
  int mObjectParamsScript;
  int mLightScript;
  int mCameraScript;
  int mRenderLoop;
  int mAllocationMap;
}
class SceneGraphBase {
  int mNameAlloc;
  int mName;
}
class Scene {
  int mRenderPassAlloc;
  int mRes;
  int mRS;
  int mTransformRSData;
  int mRenderableMeshMap;
  int mTextures;
  int mRenderableMap;
  int mRenderables;
  int mVertexShaders;
  int mFragmentShaders;
  int mCameras;
  int mLights;
  int mRenderPasses;
  int mTransformMap;
  int mRootTransforms;
  int TIMER_TAG;
}
class RenderableGroup {
  int mChildren;
}
class RenderableBase {
}
class Renderable {
  int mData;
  int mField;
  int mMaterialName;
  int mMeshIndexName;
  int mMeshName;
  int mTransform;
  int mRenderState;
  int mSourceParams;
}
class RenderState {
  int mField;
  int mRaster;
  int mStore;
  int mFragment;
  int mVertex;
}
class RenderPass {
  int mRsField;
  int mCamera;
  int mObjectsToDraw;
  int mShouldClearDepth;
  int mClearDepth;
  int mDepthTarget;
  int mShouldClearColor;
  int mClearColor;
  int mColorTarget;
}
class PointLight {
}
class MatrixTransform {
  int mLocalMatrix;
}
class LightBase {
  int mIntensity;
  int mColor;
  int mTransform;
  int mFieldData;
  int mField;
  int RS_LIGHT_DIRECTIONAL;
  int RS_LIGHT_POINT;
}
class FragmentShader {
  class Builder {
    int mBuilder;
    int mShader;
  }
  int mField;
  int mProgram;
}
class Float4Param {
  int mLight;
  int TAG;
}
class CompoundTransform {
  int mTransformComponents;
  int mComponentField;
  class ScaleComponent {
  }
  class RotateComponent {
  }
  class TranslateComponent {
  }
  class Component {
    int mData;
    int mParentIndex;
    int mParent;
    int mName;
  }
}
class ColladaScene {
  class A3DLoaderTask {
  }
  class ColladaLoaderTask {
    int sceneSource;
  }
  int mActiveScene;
  int mRS;
  int mRes;
  int mCallback;
  int mLoadFromSD;
  int STATE_LAST_FOCUS;
  int TAG;
  int modelName;
}
class ColladaParser {
  int mRootDir;
  int mScene;
  int mMeshIdNameMap;
  int mSamplerImageMap;
  int mImages;
  int mEffectsParams;
  int mCameras;
  int mLights;
  int mDom;
  int TAG;
}
class Camera {
  int mField;
  int mData;
  int mTransform;
}
