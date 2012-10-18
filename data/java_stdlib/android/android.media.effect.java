package android.media.effect;
class SizeChangeEffect {
}
class SingleFilterEffect {
  int mOutputName;
  int mInputName;
  int mFunction;
}
class FilterGraphEffect {
  int mSchedulerClass;
  int mGraph;
  int mRunner;
  int mOutputName;
  int mInputName;
  int TAG;
}
class FilterEffect {
  int mName;
  int mEffectContext;
}
class EffectUpdateListener {
}
class EffectFactory {
  int EFFECT_VIGNETTE;
  int EFFECT_TINT;
  int EFFECT_TEMPERATURE;
  int EFFECT_STRAIGHTEN;
  int EFFECT_SHARPEN;
  int EFFECT_SEPIA;
  int EFFECT_SATURATE;
  int EFFECT_ROTATE;
  int EFFECT_REDEYE;
  int EFFECT_POSTERIZE;
  int EFFECT_NEGATIVE;
  int EFFECT_LOMOISH;
  int EFFECT_GRAYSCALE;
  int EFFECT_GRAIN;
  int EFFECT_FLIP;
  int EFFECT_FILLLIGHT;
  int EFFECT_DUOTONE;
  int EFFECT_BITMAPOVERLAY;
  int EFFECT_DOCUMENTARY;
  int EFFECT_CROSSPROCESS;
  int EFFECT_CROP;
  int EFFECT_BLACKWHITE;
  int EFFECT_AUTOFIX;
  int EFFECT_BACKDROPPER;
  int EFFECT_FISHEYE;
  int EFFECT_CONTRAST;
  int EFFECT_BRIGHTNESS;
  int EFFECT_IDENTITY;
  int EFFECT_PACKAGES;
  int mEffectContext;
}
class EffectContext {
  int mOldState;
  int mFactory;
  int mFilterContext;
  int GL_STATE_COUNT;
  int GL_STATE_ARRAYBUFFER;
  int GL_STATE_PROGRAM;
  int GL_STATE_FBO;
}
class Effect {
}
