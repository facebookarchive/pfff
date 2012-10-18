package com.android.gl2cameraeye;
class CamRenderer {
  int GL_TEXTURE_EXTERNAL_OES;
  int TAG;
  int mContext;
  int updateSurface;
  int mCamera;
  int mSurface;
  int mLastTime;
  int mGForce;
  int mPos;
  int mVel;
  int mCameraRatio;
  int mRatio;
  int maTextureHandle;
  int maPositionHandle;
  int muCRatioHandle;
  int muSTMatrixHandle;
  int muMVPMatrixHandle;
  int mTextureID;
  int mProgram;
  int mSTMatrix;
  int mVMatrix;
  int mMMatrix;
  int mProjMatrix;
  int mMVPMatrix;
  int mFragmentShader;
  int mVertexShader;
  int mTriangleVertices;
  int mTriangleVerticesData;
  int TRIANGLE_VERTICES_DATA_UV_OFFSET;
  int TRIANGLE_VERTICES_DATA_POS_OFFSET;
  int TRIANGLE_VERTICES_DATA_STRIDE_BYTES;
  int FLOAT_SIZE_BYTES;
}
class CamGLSurfaceView {
  int mAcceleration;
  int mSensorManager;
  int mCamera;
  int mRenderer;
}
class GL2CameraEye {
  int mGLView;
}
