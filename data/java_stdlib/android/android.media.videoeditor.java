package android.media.videoeditor;
class WaveformData {
  int mGains;
  int mFramesCount;
  int mFrameDurationMs;
}
class VideoEditorProfile {
  int maxOutputVideoFrameHeight;
  int maxOutputVideoFrameWidth;
  int maxInputVideoFrameHeight;
  int maxInputVideoFrameWidth;
}
class VideoEditorImpl {
  int mMallocDebug;
  int mPreviewInProgress;
  int mMANativeHelper;
  int mAspectRatio;
  int mDurationMs;
  int mTransitions;
  int mAudioTracks;
  int mMediaItems;
  int mProjectPath;
  int mLock;
  int ENGINE_ACCESS_MAX_TIMEOUT_MS;
  int ATTR_OVERLAY_RESIZED_RGB_FRAME_HEIGHT;
  int ATTR_OVERLAY_RESIZED_RGB_FRAME_WIDTH;
  int ATTR_OVERLAY_FRAME_HEIGHT;
  int ATTR_OVERLAY_FRAME_WIDTH;
  int ATTR_OVERLAY_RGB_FILENAME;
  int ATTR_IS_TRANSITION_GENERATED;
  int ATTR_GENERATED_TRANSITION_CLIP;
  int ATTR_IS_IMAGE_CLIP_GENERATED;
  int ATTR_GENERATED_IMAGE_CLIP;
  int ATTR_DUCKED_TRACK_VOLUME;
  int ATTR_DUCK_THRESHOLD;
  int ATTR_DUCK_ENABLED;
  int ATTR_MUTED;
  int ATTR_LOOP;
  int ATTR_END_RECT_BOTTOM;
  int ATTR_END_RECT_RIGHT;
  int ATTR_END_RECT_TOP;
  int ATTR_END_RECT_LEFT;
  int ATTR_START_RECT_BOTTOM;
  int ATTR_START_RECT_RIGHT;
  int ATTR_START_RECT_TOP;
  int ATTR_START_RECT_LEFT;
  int ATTR_COLOR_EFFECT_VALUE;
  int ATTR_COLOR_EFFECT_TYPE;
  int ATTR_AFTER_MEDIA_ITEM_ID;
  int ATTR_BEFORE_MEDIA_ITEM_ID;
  int ATTR_MASK;
  int ATTR_INVERT;
  int ATTR_BLENDING;
  int ATTR_DIRECTION;
  int ATTR_BEHAVIOR;
  int ATTR_VOLUME;
  int ATTR_END_TIME;
  int ATTR_BEGIN_TIME;
  int ATTR_START_TIME;
  int ATTR_DURATION;
  int ATTR_TYPE;
  int ATTR_REGENERATE_PCM;
  int ATTR_ASPECT_RATIO;
  int ATTR_RENDERING_MODE;
  int ATTR_AUDIO_WAVEFORM_FILENAME;
  int ATTR_FILENAME;
  int ATTR_ID;
  int TAG_AUDIO_TRACK;
  int TAG_AUDIO_TRACKS;
  int TAG_EFFECT;
  int TAG_EFFECTS;
  int TAG_OVERLAY_USER_ATTRIBUTES;
  int TAG_OVERLAY;
  int TAG_OVERLAYS;
  int TAG_TRANSITION;
  int TAG_TRANSITIONS;
  int TAG_MEDIA_ITEM;
  int TAG_MEDIA_ITEMS;
  int TAG_PROJECT;
  int PROJECT_FILENAME;
  int TAG;
}
class VideoEditorFactory {
}
class VideoEditor {
  class OverlayData {
    int sResizePaint;
    int mClear;
    int mRenderingMode;
    int mOverlayBitmap;
  }
  class MediaProcessingProgressListener {
    int ACTION_DECODE;
    int ACTION_ENCODE;
  }
  class ExportProgressListener {
  }
  class PreviewProgressListener {
  }
  int MAX_SUPPORTED_FILE_SIZE;
  int DURATION_OF_STORYBOARD;
  int THUMBNAIL_FILENAME;
}
class TransitionSliding {
  int mSlidingDirection;
  int DIRECTION_BOTTOM_OUT_TOP_IN;
  int DIRECTION_TOP_OUT_BOTTOM_IN;
  int DIRECTION_LEFT_OUT_RIGHT_IN;
  int DIRECTION_RIGHT_OUT_LEFT_IN;
}
class TransitionFadeBlack {
}
class TransitionCrossfade {
}
class TransitionAlpha {
  int mRGBMaskFile;
  int mHeight;
  int mWidth;
  int mIsInvert;
  int mBlendingPercent;
  int mMaskFilename;
}
class Transition {
  int mNativeHelper;
  int mFilename;
  int mDurationMs;
  int mBehavior;
  int mBeforeMediaItem;
  int mAfterMediaItem;
  int mUniqueId;
  int BEHAVIOR_MAX_VALUE;
  int BEHAVIOR_MIDDLE_FAST;
  int BEHAVIOR_MIDDLE_SLOW;
  int BEHAVIOR_LINEAR;
  int BEHAVIOR_SPEED_DOWN;
  int BEHAVIOR_SPEED_UP;
  int BEHAVIOR_MIN_VALUE;
}
class OverlayFrame {
  int sResizePaint;
  int mResizedRGBHeight;
  int mResizedRGBWidth;
  int mOFHeight;
  int mOFWidth;
  int mBitmapFileName;
  int mFilename;
  int mBitmap;
}
class Overlay {
  int mDurationMs;
  int mStartTimeMs;
  int mUserAttributes;
  int mMediaItem;
  int mUniqueId;
}
class MediaVideoItem {
  int mWaveformData;
  int mVideoRotationDegree;
  int mVideoEditor;
  int mMANativeHelper;
  int mAudioWaveformFilename;
  int mMuted;
  int mVolumePercentage;
  int mEndBoundaryTimeMs;
  int mBeginBoundaryTimeMs;
  int mAudioSamplingFrequency;
  int mAudioChannels;
  int mAudioType;
  int mFps;
  int mAudioBitrate;
  int mDurationMs;
  int mVideoBitrate;
  int mVideoLevel;
  int mVideoProfile;
  int mVideoType;
  int mFileType;
  int mAspectRatio;
  int mHeight;
  int mWidth;
}
class MediaProperties {
  int AUDIO_MAX_VOLUME_PERCENT;
  int AUDIO_MAX_TRACK_COUNT;
  int SUPPORTED_VIDEO_FILE_FORMATS;
  int UNDEFINED_VIDEO_PROFILE;
  int FILE_UNSUPPORTED;
  int FILE_M4V;
  int FILE_PNG;
  int FILE_JPEG;
  int FILE_MP3;
  int FILE_AMR;
  int FILE_MP4;
  int FILE_3GP;
  int DEFAULT_CHANNEL_COUNT;
  int DEFAULT_SAMPLING_FREQUENCY;
  int SAMPLES_PER_FRAME_AMRWB;
  int SAMPLES_PER_FRAME_AMRNB;
  int SAMPLES_PER_FRAME_MP3;
  int SAMPLES_PER_FRAME_AAC;
  int SUPPORTED_ACODECS;
  int ACODEC_OGG;
  int ACODEC_AMRWB;
  int ACODEC_EVRC;
  int ACODEC_MP3;
  int ACODEC_ENHANCED_AAC_PLUS;
  int ACODEC_AAC_PLUS;
  int ACODEC_AAC_LC;
  int ACODEC_AMRNB;
  int ACODEC_NO_AUDIO;
  class MPEG4Level {
    int MPEG4LevelUnknown;
    int MPEG4Level5;
    int MPEG4Level4a;
    int MPEG4Level4;
    int MPEG4Level3;
    int MPEG4Level2;
    int MPEG4Level1;
    int MPEG4Level0b;
    int MPEG4Level0;
  }
  class MPEG4Profile {
    int MPEG4ProfileUnknown;
    int MPEG4ProfileAdvancedSimple;
    int MPEG4ProfileAdvancedScalable;
    int MPEG4ProfileAdvancedCore;
    int MPEG4ProfileAdvancedCoding;
    int MPEG4ProfileCoreScalable;
    int MPEG4ProfileAdvancedRealTime;
    int MPEG4ProfileHybrid;
    int MPEG4ProfileBasicAnimated;
    int MPEG4ProfileSimpleFBA;
    int MPEG4ProfileSimpleFace;
    int MPEG4ProfileScalableTexture;
    int MPEG4ProfileNbit;
    int MPEG4ProfileMain;
    int MPEG4ProfileCore;
    int MPEG4ProfileSimpleScalable;
    int MPEG4ProfileSimple;
  }
  class H263Level {
    int H263LevelUnknown;
    int H263Level70;
    int H263Level60;
    int H263Level50;
    int H263Level45;
    int H263Level40;
    int H263Level30;
    int H263Level20;
    int H263Level10;
  }
  class H263Profile {
    int H263ProfileUnknown;
    int H263ProfileHighLatency;
    int H263ProfileInterlace;
    int H263ProfileInternet;
    int H263ProfileHighCompression;
    int H263ProfileISWV3;
    int H263ProfileISWV2;
    int H263ProfileBackwardCompatible;
    int H263ProfileH320Coding;
    int H263ProfileBaseline;
  }
  class H264Level {
    int H264LevelUnknown;
    int H264Level51;
    int H264Level5;
    int H264Level42;
    int H264Level41;
    int H264Level4;
    int H264Level32;
    int H264Level31;
    int H264Level3;
    int H264Level22;
    int H264Level21;
    int H264Level2;
    int H264Level13;
    int H264Level12;
    int H264Level11;
    int H264Level1b;
    int H264Level1;
  }
  class H264Profile {
    int H264ProfileUnknown;
    int H264ProfileHigh444;
    int H264ProfileHigh422;
    int H264ProfileHigh10;
    int H264ProfileHigh;
    int H264ProfileExtended;
    int H264ProfileMain;
    int H264ProfileBaseline;
  }
  int SUPPORTED_VCODECS;
  int VCODEC_MPEG4;
  int VCODEC_H264;
  int VCODEC_H263;
  int SUPPORTED_BITRATES;
  int BITRATE_8M;
  int BITRATE_5M;
  int BITRATE_2M;
  int BITRATE_800K;
  int BITRATE_512K;
  int BITRATE_384K;
  int BITRATE_256K;
  int BITRATE_192K;
  int BITRATE_128K;
  int BITRATE_96K;
  int BITRATE_64K;
  int BITRATE_40K;
  int BITRATE_28K;
  int ASPECT_RATIO_16_9_RESOLUTIONS;
  int ASPECT_RATIO_11_9_RESOLUTIONS;
  int ASPECT_RATIO_5_3_RESOLUTIONS;
  int ASPECT_RATIO_4_3_RESOLUTIONS;
  int ASPECT_RATIO_3_2_RESOLUTIONS;
  int ASPECT_RATIOS;
  int ASPECT_RATIO_11_9;
  int ASPECT_RATIO_5_3;
  int ASPECT_RATIO_4_3;
  int ASPECT_RATIO_16_9;
  int ASPECT_RATIO_3_2;
  int ASPECT_RATIO_UNDEFINED;
  int HEIGHT_1080;
  int HEIGHT_720;
  int HEIGHT_480;
  int HEIGHT_360;
  int HEIGHT_288;
  int HEIGHT_144;
}
class MediaItem {
  class GetThumbnailListCallback {
  }
  int mBlankFrameFilename;
  int mBlankFrameGenerated;
  int mRegenerateClip;
  int mGeneratedImageClip;
  int mEndTransition;
  int mBeginTransition;
  int mProjectPath;
  int mMANativeHelper;
  int mRenderingMode;
  int mOverlays;
  int mEffects;
  int mFilename;
  int mUniqueId;
  int RENDERING_MODE_CROPPING;
  int RENDERING_MODE_STRETCH;
  int RENDERING_MODE_BLACK_BORDER;
  int END_OF_FILE;
}
class MediaImageItem {
  int mMANativeHelper;
  int mFileName;
  int mGeneratedClipWidth;
  int mGeneratedClipHeight;
  int mDecodedFilename;
  int mVideoEditor;
  int mScaledFilename;
  int mScaledHeight;
  int mScaledWidth;
  int mDurationMs;
  int mAspectRatio;
  int mHeight;
  int mWidth;
  int sResizePaint;
  int TAG;
}
class MediaArtistNativeHelper {
  class NativeGetPixelsListCallback {
  }
  class Properties {
    int Id;
    int videoRotation;
    int audioVolumeValue;
    int audioSamplingFrequency;
    int audioChannels;
    int audioBitrate;
    int audioDuration;
    int audioFormat;
    int levelSupported;
    int profileSupported;
    int level;
    int profile;
    int averageFrameRate;
    int height;
    int width;
    int videoBitrate;
    int videoDuration;
    int videoFormat;
    int fileType;
    int duration;
  }
  class EditSettings {
    int primaryTrackVolume;
    int backgroundMusicSettings;
    int audioBitrate;
    int videoBitrate;
    int audioChannels;
    int maxFileSize;
    int audioSamplingFreq;
    int audioFormat;
    int videoLevel;
    int videoProfile;
    int videoFormat;
    int videoFrameSize;
    int outputFile;
    int videoFrameRate;
    int effectSettingsArray;
    int transitionSettingsArray;
    int clipSettingsArray;
  }
  class PreviewClipProperties {
    int clipProperties;
  }
  class PreviewSettings {
    int effectSettingsArray;
    int previewClipsArray;
  }
  class AudioSettings {
    int pcmFilePath;
    int bInDucking_enable;
    int ducking_lowVolume;
    int ducking_threshold;
    int loop;
    int volume;
    int fileType;
    int endCutTime;
    int beginCutTime;
    int startMs;
    int ExtendedFs;
    int Fs;
    int channels;
    int bRemoveOriginal;
    int Id;
    int pFile;
  }
  class PreviewClips {
    int mediaRendering;
    int endPlayTime;
    int beginPlayTime;
    int fileType;
    int clipPath;
  }
  class EffectSettings {
    int alphaBlendingFadeOutTimePercent;
    int alphaBlendingFadeInTimePercent;
    int alphaBlendingEndPercent;
    int alphaBlendingMiddlePercent;
    int alphaBlendingStartPercent;
    int rgb16InputColor;
    int fiftiesFrameRate;
    int textBufferHeight;
    int textBufferWidth;
    int textRenderingData;
    int text;
    int framingScaledSize;
    int framingResize;
    int topLeftY;
    int topLeftX;
    int height;
    int width;
    int bitmapType;
    int framingBuffer;
    int framingFile;
    int durationPercent;
    int startPercent;
    int audioEffectType;
    int videoEffectType;
    int duration;
    int startTime;
  }
  class AudioEffect {
    int FADE_OUT;
    int FADE_IN;
    int NONE;
  }
  class BackgroundMusicSettings {
    int isLooping;
    int lowVolume;
    int duckingThreshold;
    int enableDucking;
    int endLoop;
    int beginLoop;
    int volumePercent;
    int insertionTime;
    int fileType;
    int file;
  }
  class TransitionBehaviour {
    int FAST_MIDDLE;
    int SLOW_MIDDLE;
    int SPEED_DOWN;
    int LINEAR;
    int SPEED_UP;
  }
  class AudioTransition {
    int CROSS_FADE;
    int NONE;
  }
  class TransitionSettings {
    int slideSettings;
    int alphaSettings;
    int transitionBehaviour;
    int audioTransitionType;
    int videoTransitionType;
    int duration;
  }
  class ClipSettings {
    int rotationDegree;
    int rgbHeight;
    int rgbWidth;
    int mediaRendering;
    int panZoomTopLeftYEnd;
    int panZoomTopLeftXEnd;
    int panZoomPercentEnd;
    int panZoomTopLeftYStart;
    int panZoomTopLeftXStart;
    int panZoomPercentStart;
    int panZoomEnabled;
    int endCutPercent;
    int beginCutPercent;
    int endCutTime;
    int beginCutTime;
    int fileType;
    int clipOriginalPath;
    int clipDecodedPath;
    int clipPath;
  }
  class SlideTransitionSettings {
    int direction;
  }
  class SlideDirection {
    int BOTTOM_OUT_TOP_IN;
    int TOP_OUT_BOTTOM_IN;
    int LEFT_OUT_RIGTH_IN;
    int RIGHT_OUT_LEFT_IN;
  }
  class AlphaMagicSettings {
    int rgbHeight;
    int rgbWidth;
    int invertRotation;
    int blendingPercent;
    int file;
  }
  class VideoTransition {
    int FADE_BLACK;
    int SLIDE_TRANSITION;
    int ALPHA_MAGIC;
    int EXTERNAL;
    int CROSS_FADE;
    int NONE;
  }
  class VideoEffect {
    int GRADIENT;
    int COLORRGB16;
    int FIFTIES;
    int ZOOM_OUT;
    int ZOOM_IN;
    int TEXT;
    int FRAMING;
    int NEGATIVE;
    int SEPIA;
    int GREEN;
    int PINK;
    int BLACK_AND_WHITE;
    int EXTERNAL;
    int FADE_TO_BLACK;
    int FADE_FROM_BLACK;
    int NONE;
  }
  class VideoFrameRate {
    int FR_30_FPS;
    int FR_25_FPS;
    int FR_20_FPS;
    int FR_15_FPS;
    int FR_12_5_FPS;
    int FR_10_FPS;
    int FR_7_5_FPS;
    int FR_5_FPS;
  }
  class VideoFrameSize {
    int V1080p;
    int S720p;
    int W720p;
    int V720p;
    int WVGA16x9;
    int nHD;
    int NTSC;
    int WVGA;
    int VGA;
    int CIF;
    int QVGA;
    int QCIF;
    int QQVGA;
    int SQCIF;
    int SIZE_UNDEFINED;
  }
  class VideoFormat {
    int UNSUPPORTED;
    int NULL_VIDEO;
    int MPEG4;
    int H264;
    int H263;
    int NO_VIDEO;
  }
  class Result {
    int ERR_INTERNAL;
    int ERR_NOMORE_SPACE_FOR_FILE;
    int ERR_DECODER_H263_NOT_BASELINE;
    int ERR_DECODER_H263_PROFILE_NOT_SUPPORTED;
    int WAR_DEBLOCKING_FILTER_NOT_IMPLEMENTED;
    int WAR_VIDEORENDERER_NO_NEW_FRAME;
    int WAR_WRITER_STOP_REQ;
    int WAR_READER_INFORMATION_NOT_PRESENT;
    int WAR_READER_NO_METADATA;
    int ERR_READER_UNKNOWN_STREAM_TYPE;
    int ERR_OUTPUT_FILE_SIZE_TOO_SMALL;
    int ERR_AUDIOBITRATE_TOO_HIGH;
    int ERR_VIDEOBITRATE_TOO_HIGH;
    int ERR_AUDIOBITRATE_TOO_LOW;
    int ERR_VIDEOBITRATE_TOO_LOW;
    int ERR_MAXFILESIZE_TOO_SMALL;
    int ERR_END_CUT_SMALLER_THAN_BEGIN_CUT;
    int ERR_BEGIN_CUT_EQUALS_END_CUT;
    int ERR_AUDIO_CONVERSION_FAILED;
    int ERR_INVALID_AAC_SAMPLING_FREQUENCY;
    int ERR_H263_FORBIDDEN_IN_MP4_FILE;
    int ERR_DURATION_IS_NULL;
    int ERR_INVALID_VIDEO_FRAME_RATE_FOR_H263;
    int ERR_INVALID_VIDEO_FRAME_SIZE_FOR_H263;
    int ERR_UNDEFINED_OUTPUT_AUDIO_FORMAT;
    int ERR_UNDEFINED_OUTPUT_VIDEO_FRAME_RATE;
    int ERR_UNDEFINED_OUTPUT_VIDEO_FRAME_SIZE;
    int ERR_UNDEFINED_OUTPUT_VIDEO_FORMAT;
    int ERR_INVALID_INPUT_FILE;
    int ERR_INPUT_FILE_CONTAINS_NO_SUPPORTED_STREAM;
    int WAR_MEDIATYPE_NOT_SUPPORTED;
    int WAR_TRANSCODING_DONE;
    int ERR_THREAD_NOT_STARTED;
    int WAR_STR_NOT_FOUND;
    int WAR_STR_OVERFLOW;
    int ERR_STR_BAD_ARGS;
    int ERR_STR_OVERFLOW;
    int ERR_STR_CONV_FAILED;
    int ERR_STR_BAD_STRING;
    int ERR_FILE_INVALID_POSITION;
    int ERR_FILE_BAD_MODE_ACCESS;
    int ERR_FILE_LOCKED;
    int WAR_TOO_MUCH_STREAMS;
    int WAR_REDIRECT;
    int WAR_BUFFER_FULL;
    int WAR_TIME_OUT;
    int WAR_NO_MORE_AU;
    int WAR_INVALID_TIME;
    int WAR_NO_MORE_STREAM;
    int WAR_NO_DATA_YET;
    int ERR_UNSUPPORTED_MEDIA_TYPE;
    int ERR_NOT_IMPLEMENTED;
    int ERR_READ_ONLY;
    int ERR_WRITE_ONLY;
    int ERR_BAD_OPTION_ID;
    int ERR_BAD_STREAM_ID;
    int ERR_CONTEXT_FAILED;
    int ERR_BAD_CONTEXT;
    int ERR_ALLOC;
    int ERR_STATE;
    int ERR_PARAMETER;
    int ERR_DIR_NO_MORE_ENTRY;
    int ERR_DIR_READ_FAILED;
    int ERR_DIR_OPEN_FAILED;
    int ERR_CLOCK_BAD_REF_YEAR;
    int WAR_TIMESCALE_TOO_BIG;
    int WAR_MAX_OUTPUT_SIZE_EXCEEDED;
    int WAR_TRANSCODING_NECESSARY;
    int ERR_NO_SUPPORTED_VIDEO_STREAM_IN_FILE;
    int ERR_H263_PROFILE_NOT_SUPPORTED;
    int ERR_FEATURE_UNSUPPORTED_WITH_EVRC;
    int ERR_ONLY_AMRNB_INPUT_CAN_BE_MIXED;
    int ERR_AUDIO_CANNOT_BE_MIXED;
    int ERR_FEATURE_UNSUPPORTED_WITH_AAC;
    int ERR_FEATURE_UNSUPPORTED_WITH_AUDIO_TRACK;
    int ERR_AUDIO_MIXING_MP3_UNSUPPORTED;
    int ERR_AUDIO_MIXING_UNSUPPORTED;
    int ERR_UNSUPPORTED_ADDED_AUDIO_STREAM;
    int ERR_UNDEFINED_AUDIO_TRACK_FILE_FORMAT;
    int ERR_ADDCTS_HIGHER_THAN_VIDEO_DURATION;
    int ERR_ADDVOLUME_EQUALS_ZERO;
    int ERR_NO_SUPPORTED_STREAM_IN_FILE;
    int ERR_UNSUPPORTED_MP3_ASSEMBLY;
    int ERR_INCOMPATIBLE_VIDEO_DATA_PARTITIONING;
    int ERR_INCOMPATIBLE_VIDEO_TIME_SCALE;
    int ERR_INCOMPATIBLE_VIDEO_FRAME_SIZE;
    int ERR_INCOMPATIBLE_VIDEO_FORMAT;
    int ERR_INVALID_CLIP_ANALYSIS_PLATFORM;
    int ERR_INVALID_CLIP_ANALYSIS_VERSION;
    int ERR_EDITING_NO_SUPPORTED_VIDEO_STREAM_IN_FILE;
    int ERR_EDITING_NO_SUPPORTED_STREAM_IN_FILE;
    int ERR_EDITING_UNSUPPORTED_AUDIO_FORMAT;
    int ERR_EDITING_UNSUPPORTED_MPEG4_RVLC;
    int ERR_EDITING_UNSUPPORTED_MPEG4_PROFILE;
    int ERR_EDITING_UNSUPPORTED_H263_PROFILE;
    int ERR_EDITING_UNSUPPORTED_VIDEO_FORMAT;
    int ERR_ENCODER_ACCES_UNIT_ERROR;
    int ERR_INPUT_AUDIO_CORRUPTED_AU;
    int ERR_INPUT_AUDIO_AU_TOO_LARGE;
    int ERR_INPUT_VIDEO_AU_TOO_LARGE;
    int ERR_AMR_EDITING_UNSUPPORTED;
    int ERR_UNSUPPORTED_INPUT_AUDIO_FORMAT;
    int ERR_UNSUPPORTED_INPUT_VIDEO_FORMAT;
    int ERR_INVALID_3GPP_FILE;
    int ERR_ANALYSIS_DATA_SIZE_TOO_SMALL;
    int ERR_OVERLAPPING_TRANSITIONS;
    int ERR_BEGIN_CUT_LARGER_THAN_END_CUT;
    int ERR_BEGIN_CUT_LARGER_THAN_DURATION;
    int ERR_EXTERNAL_TRANSITION_NULL;
    int ERR_EXTERNAL_EFFECT_NULL;
    int ERR_INVALID_VIDEO_ENCODING_FRAME_RATE;
    int ERR_INVALID_AUDIO_TRANSITION_TYPE;
    int ERR_INVALID_VIDEO_TRANSITION_TYPE;
    int ERR_INVALID_AUDIO_EFFECT_TYPE;
    int ERR_INVALID_VIDEO_EFFECT_TYPE;
    int ERR_INVALID_EFFECT_KIND;
    int ERR_INVALID_FILE_TYPE;
    int ERR_BUFFER_OUT_TOO_SMALL;
    int ERR_FILE_NOT_FOUND;
    int NO_ERROR;
  }
  class MediaRendering {
    int BLACK_BORDERS;
    int CROPPING;
    int RESIZING;
  }
  class FileType {
    int UNSUPPORTED;
    int M4V;
    int PNG;
    int GIF;
    int JPG;
    int PCM;
    int MP3;
    int AMR;
    int MP4;
    int THREE_GPP;
  }
  class Bitrate {
    int BR_8_MBPS;
    int BR_5_MBPS;
    int BR_2_MBPS;
    int BR_800_KBPS;
    int BR_512_KBPS;
    int BR_384_KBPS;
    int BR_288_KBPS;
    int BR_256_KBPS;
    int BR_192_KBPS;
    int BR_128_KBPS;
    int BR_96_KBPS;
    int BR_64_KBPS;
    int BR_48_KBPS;
    int BR_32_KBPS;
    int BR_24_KBPS;
    int BR_16_KBPS;
    int BR_12_2_KBPS;
    int BR_9_2_KBPS;
    int UNDEFINED;
    int VARIABLE;
  }
  class AudioSamplingFrequency {
    int FREQ_48000;
    int FREQ_44100;
    int FREQ_32000;
    int FREQ_24000;
    int FREQ_22050;
    int FREQ_16000;
    int FREQ_12000;
    int FREQ_11025;
    int FREQ_8000;
    int FREQ_DEFAULT;
  }
  class AudioFormat {
    int UNSUPPORTED_AUDIO;
    int NULL_AUDIO;
    int PCM;
    int EVRC;
    int MP3;
    int ENHANCED_AAC_PLUS;
    int AAC_PLUS;
    int AAC;
    int AMR_NB;
    int NO_AUDIO;
  }
  class Version {
    int VIDEOEDITOR_REVISION_VERSION;
    int VIDEOEDITOR_MINOR_VERSION;
    int VIDEOEDITOR_MAJOR_VERSION;
    int revision;
    int minor;
    int major;
  }
  class OnProgressUpdateListener {
  }
  int mManualEditContext;
  int mErrorFlagSet;
  int mTotalClips;
  int mAudioTrackPCMFilePath;
  int mPreviewProgress;
  int mProjectPath;
  int mMediaProcessingProgressListener;
  int mExtractAudioWaveformProgressListener;
  int mExportProgressListener;
  int mPreviewProgressListener;
  int mProcessingObject;
  int mProcessingState;
  int PROCESSING_EXPORT;
  int PROCESSING_INTERMEDIATE3;
  int PROCESSING_INTERMEDIATE2;
  int PROCESSING_INTERMEDIATE1;
  int PROCESSING_KENBURNS;
  int PROCESSING_TRANSITION;
  int PROCESSING_AUDIO_PCM;
  int PROCESSING_NONE;
  int AUDIO_TRACK_PCM_FILE;
  int mIsFirstProgress;
  int mRenderPreviewRenderingMode;
  int mRenderPreviewOverlayFile;
  int mProgressToApp;
  int mExportAudioCodec;
  int mExportVideoCodec;
  int mExportFilename;
  int mRegenerateAudio;
  int mInvalidatePreviewArray;
  int mAudioTrack;
  int mAudioSettings;
  int mPreviewEditSettings;
  int mClipProperties;
  int mOutputFilename;
  int mStoryBoardSettings;
  int mLock;
  int mVideoEditor;
  int sResizePaint;
  int TASK_ENCODING;
  int TASK_LOADING_SETTINGS;
  int MAX_THUMBNAIL_PERMITTED;
  int TAG;
}
class ExtractAudioWaveformProgressListener {
}
class EffectKenBurns {
  int mEndRect;
  int mStartRect;
}
class EffectColor {
  int mColor;
  int mType;
  int GRAY;
  int PINK;
  int GREEN;
  int TYPE_FIFTIES;
  int TYPE_NEGATIVE;
  int TYPE_SEPIA;
  int TYPE_GRADIENT;
  int TYPE_COLOR;
}
class Effect {
  int mStartTimeMs;
  int mDurationMs;
  int mMediaItem;
  int mUniqueId;
}
class AudioTrack {
  int mWaveformData;
  int mAudioWaveformFilename;
  int mIsDuckingEnabled;
  int mDuckedTrackVolume;
  int mDuckingThreshold;
  int mAudioSamplingFrequency;
  int mAudioBitrate;
  int mAudioType;
  int mAudioChannels;
  int mDurationMs;
  int mMuted;
  int mLoop;
  int mEndBoundaryTimeMs;
  int mBeginBoundaryTimeMs;
  int mVolumePercent;
  int mTimelineDurationMs;
  int mStartTimeMs;
  int mFilename;
  int mUniqueId;
  int mMANativeHelper;
}
