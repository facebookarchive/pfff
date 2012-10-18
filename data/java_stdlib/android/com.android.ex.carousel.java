package com.android.ex.carousel;
class CarouselViewUtilities {
}
class CarouselViewHelper {
  class SyncHandler {
  }
  class AsyncHandler {
  }
  class DetailTextureParameters {
    int lineOffsetY;
    int lineOffsetX;
    int textureOffsetY;
    int textureOffsetX;
  }
  class TextureParameters {
    int matrix;
  }
  int mSyncHandler;
  int mAsyncHandler;
  int HOLDOFF_DELAY;
  int DBG;
  int mCarouselView;
  int mContext;
  int mHandlerThread;
  int REQUEST_END;
  int REQUEST_GEOMETRY_N;
  int REQUEST_DETAIL_TEXTURE_N;
  int REQUEST_TEXTURE_N;
  int SET_MATRIX_N;
  int SET_GEOMETRY_N;
  int SET_DETAIL_TEXTURE_N;
  int SET_TEXTURE_N;
  int TAG;
}
class CarouselView {
  class Info {
    int resId;
  }
  class DetailAlignment {
    int HORIZONTAL_ALIGNMENT_MASK;
    int RIGHT;
    int LEFT;
    int CENTER_HORIZONTAL;
    int VERTICAL_ALIGNMENT_MASK;
    int BELOW;
    int ABOVE;
    int VIEW_BOTTOM;
    int VIEW_TOP;
    int CENTER_VERTICAL;
  }
  class InterpolationMode {
    int ACCELERATE_DECELERATE_CUBIC;
    int DECELERATE_QUADRATIC;
    int LINEAR;
  }
  int FILL_DIRECTION_CW;
  int FILL_DIRECTION_CCW;
  int DRAG_MODEL_CYLINDER_OUTSIDE;
  int DRAG_MODEL_CYLINDER_INSIDE;
  int DRAG_MODEL_PLANE;
  int DRAG_MODEL_SCREEN_DELTA;
  int mController;
  int mTracking;
  int mContext;
  int mRS;
  int mRenderScript;
  int TAG;
  int USE_DEPTH_BUFFER;
}
class CarouselRS {
  int mRsMessage;
  class CarouselCallback {
  }
  int mMultiTextureBlendingShader;
  int mMultiTextureShader;
  int mSingleTextureBlendingShader;
  int mSingleTextureShader;
  int mUp;
  int mAtPoint;
  int mEyePoint;
  int mCallback;
  int mPrefetchCardCount;
  int mRowCount;
  int mVisibleSlots;
  int mForceBlendCardsWithZ;
  int mAllocationPool;
  int mRasterProgram;
  int mVertexProgram;
  int mMultiTextureBlendingFragmentProgram;
  int mMultiTextureFragmentProgram;
  int mSingleTextureBlendingFragmentProgram;
  int mSingleTextureFragmentProgram;
  int mProgramStoresCard;
  int mFSConst;
  int mCards;
  int mScript;
  int mRes;
  int mRS;
  int DBG;
  int MIPMAP;
  int DEFAULT_SLOT_COUNT;
  int TAG;
  int FILL_DIRECTION_CW;
  int FILL_DIRECTION_CCW;
  int DRAG_MODEL_CYLINDER_OUTSIDE;
  int DRAG_MODEL_CYLINDER_INSIDE;
  int DRAG_MODEL_PLANE;
  int DRAG_MODEL_SCREEN_DELTA;
  int CMD_PING;
  int CMD_INVALIDATE_DETAIL_TEXTURE;
  int CMD_REQUEST_DETAIL_TEXTURE;
  int CMD_ANIMATION_FINISHED;
  int CMD_ANIMATION_STARTED;
  int CMD_INVALIDATE_GEOMETRY;
  int CMD_REQUEST_GEOMETRY;
  int CMD_INVALIDATE_TEXTURE;
  int CMD_REQUEST_TEXTURE;
  int CMD_CARD_LONGPRESS;
  int CMD_DETAIL_SELECTED;
  int CMD_CARD_SELECTED;
  int DEFAULT_ROW_COUNT;
  int DEFAULT_CARD_COUNT;
  int DEFAULT_VISIBLE_SLOTS;
}
class CarouselController {
  int STORE_CONFIG_DEPTH_WRITES;
  int STORE_CONFIG_DEPTH_READS;
  int STORE_CONFIG_ALPHA;
  int mStoreConfigs;
  int mFirstCardTop;
  int mFillDirection;
  int mDragModel;
  int mDetailLoadingBitmap;
  int mCardCreationFadeDuration;
  int mFadeInDuration;
  int mRezInCardCount;
  int mCarouselCallback;
  int mBackgroundColor;
  int mUp;
  int mAt;
  int mEye;
  int mRowSpacing;
  int mRowCount;
  int mSlotCount;
  int mDragFactor;
  int mFrictionCoefficient;
  int mSwaySensitivity;
  int mOverscrollSlots;
  int mCardsFaceTangent;
  int mCardRotation;
  int mRadius;
  int mCarouselRotationAngle;
  int mStartAngle;
  int mDrawRuler;
  int mForceBlendCardsWithZ;
  int mDetailTextureAlignment;
  int mPrefetchCardCount;
  int mVisibleDetails;
  int mVisibleSlots;
  int mCardCount;
  int mDefaultCardMatrix;
  int mLoadingGeometry;
  int mDefaultGeometry;
  int mDefaultLineBitmap;
  int mBackgroundBitmap;
  int mLoadingBitmap;
  int mDefaultBitmap;
  int DBG;
  int TAG;
  int mRS;
  int mRenderScript;
  int DEFAULT_DETAIL_ALIGNMENT;
  int DEFAULT_DRAG_FACTOR;
  int DEFAULT_FRICTION_COEFFICIENT;
  int DEFAULT_SWAY_SENSITIVITY;
  int DEFAULT_ROW_SPACING;
  int DEFAULT_OVERSCROLL_SLOTS;
  int DEFAULT_ROW_COUNT;
  int DEFAULT_PREFETCH_CARD_COUNT;
  int DEFAULT_VISIBLE_DETAIL_COUNT;
  int DEFAULT_RADIUS;
  int DEFAULT_SLOT_COUNT;
}
