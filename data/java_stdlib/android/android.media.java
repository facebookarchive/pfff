package android.media;
class ToneGenerator {
  int mNativeContext;
  int MIN_VOLUME;
  int MAX_VOLUME;
  int TONE_CDMA_SIGNAL_OFF;
  int TONE_CDMA_ABBR_ALERT;
  int TONE_CDMA_NETWORK_BUSY_ONE_SHOT;
  int TONE_CDMA_CALLDROP_LITE;
  int TONE_CDMA_SOFT_ERROR_LITE;
  int TONE_CDMA_ALERT_CALL_GUARD;
  int TONE_CDMA_EMERGENCY_RINGBACK;
  int TONE_CDMA_ALERT_INCALL_LITE;
  int TONE_CDMA_PRESSHOLDKEY_LITE;
  int TONE_CDMA_KEYPAD_VOLUME_KEY_LITE;
  int TONE_CDMA_ONE_MIN_BEEP;
  int TONE_CDMA_ALERT_AUTOREDIAL_LITE;
  int TONE_CDMA_ALERT_NETWORK_LITE;
  int TONE_CDMA_LOW_PBX_S_X4;
  int TONE_CDMA_MED_PBX_S_X4;
  int TONE_CDMA_HIGH_PBX_S_X4;
  int TONE_CDMA_LOW_PBX_SLS;
  int TONE_CDMA_MED_PBX_SLS;
  int TONE_CDMA_HIGH_PBX_SLS;
  int TONE_CDMA_LOW_PBX_SSL;
  int TONE_CDMA_MED_PBX_SSL;
  int TONE_CDMA_HIGH_PBX_SSL;
  int TONE_CDMA_LOW_PBX_SS;
  int TONE_CDMA_MED_PBX_SS;
  int TONE_CDMA_HIGH_PBX_SS;
  int TONE_CDMA_LOW_PBX_L;
  int TONE_CDMA_MED_PBX_L;
  int TONE_CDMA_HIGH_PBX_L;
  int TONE_CDMA_LOW_S_X4;
  int TONE_CDMA_MED_S_X4;
  int TONE_CDMA_HIGH_S_X4;
  int TONE_CDMA_LOW_SLS;
  int TONE_CDMA_MED_SLS;
  int TONE_CDMA_HIGH_SLS;
  int TONE_CDMA_LOW_SS_2;
  int TONE_CDMA_MED_SS_2;
  int TONE_CDMA_HIGH_SS_2;
  int TONE_CDMA_LOW_SSL;
  int TONE_CDMA_MED_SSL;
  int TONE_CDMA_HIGH_SSL;
  int TONE_CDMA_LOW_SS;
  int TONE_CDMA_MED_SS;
  int TONE_CDMA_HIGH_SS;
  int TONE_CDMA_LOW_L;
  int TONE_CDMA_MED_L;
  int TONE_CDMA_HIGH_L;
  int TONE_CDMA_CALL_SIGNAL_ISDN_PAT7;
  int TONE_CDMA_CALL_SIGNAL_ISDN_PAT6;
  int TONE_CDMA_CALL_SIGNAL_ISDN_PAT5;
  int TONE_CDMA_CALL_SIGNAL_ISDN_PING_RING;
  int TONE_CDMA_CALL_SIGNAL_ISDN_PAT3;
  int TONE_CDMA_CALL_SIGNAL_ISDN_SP_PRI;
  int TONE_CDMA_CALL_SIGNAL_ISDN_INTERGROUP;
  int TONE_CDMA_CALL_SIGNAL_ISDN_NORMAL;
  int TONE_CDMA_PIP;
  int TONE_CDMA_NETWORK_CALLWAITING;
  int TONE_CDMA_ANSWER;
  int TONE_CDMA_CONFIRM;
  int TONE_CDMA_NETWORK_BUSY;
  int TONE_CDMA_ABBR_REORDER;
  int TONE_CDMA_REORDER;
  int TONE_CDMA_ABBR_INTERCEPT;
  int TONE_CDMA_INTERCEPT;
  int TONE_CDMA_NETWORK_USA_RINGBACK;
  int TONE_CDMA_DIAL_TONE_LITE;
  int TONE_SUP_PIP;
  int TONE_SUP_CONFIRM;
  int TONE_SUP_CONGESTION_ABBREV;
  int TONE_SUP_INTERCEPT_ABBREV;
  int TONE_SUP_INTERCEPT;
  int TONE_PROP_BEEP2;
  int TONE_PROP_PROMPT;
  int TONE_PROP_NACK;
  int TONE_PROP_ACK;
  int TONE_PROP_BEEP;
  int TONE_SUP_RINGTONE;
  int TONE_SUP_CALL_WAITING;
  int TONE_SUP_ERROR;
  int TONE_SUP_RADIO_NOTAVAIL;
  int TONE_SUP_RADIO_ACK;
  int TONE_SUP_CONGESTION;
  int TONE_SUP_BUSY;
  int TONE_SUP_DIAL;
  int TONE_DTMF_D;
  int TONE_DTMF_C;
  int TONE_DTMF_B;
  int TONE_DTMF_A;
  int TONE_DTMF_P;
  int TONE_DTMF_S;
  int TONE_DTMF_9;
  int TONE_DTMF_8;
  int TONE_DTMF_7;
  int TONE_DTMF_6;
  int TONE_DTMF_5;
  int TONE_DTMF_4;
  int TONE_DTMF_3;
  int TONE_DTMF_2;
  int TONE_DTMF_1;
  int TONE_DTMF_0;
}
class TimedText {
  class HyperText {
    int altString;
    int URL;
    int endChar;
    int startChar;
  }
  class Karaoke {
    int endChar;
    int startChar;
    int endTimeMs;
    int startTimeMs;
  }
  class Font {
    int name;
    int ID;
  }
  class Style {
    int colorRGBA;
    int fontSize;
    int isUnderlined;
    int isItalic;
    int isBold;
    int fontID;
    int endChar;
    int startChar;
  }
  class Justification {
    int verticalJustification;
    int horizontalJustification;
  }
  class CharPos {
    int endChar;
    int startChar;
  }
  int mJustification;
  int mTextChars;
  int mTextBounds;
  int mHyperTextList;
  int mStyleList;
  int mFontList;
  int mKaraokeList;
  int mHighlightPosList;
  int mBlinkingPosList;
  int mWrapText;
  int mScrollDelay;
  int mHighlightColorRGBA;
  int mBackgroundColorRGBA;
  int mDisplayFlags;
  int mKeyObjectMap;
  int TAG;
  int LAST_PRIVATE_KEY;
  int KEY_TEXT_COLOR_RGBA;
  int KEY_FONT_SIZE;
  int KEY_FONT_ID;
  int KEY_END_CHAR;
  int KEY_START_CHAR;
  int KEY_LOCAL_SETTING;
  int KEY_GLOBAL_SETTING;
  int FIRST_PRIVATE_KEY;
  int LAST_PUBLIC_KEY;
  int KEY_STRUCT_TEXT;
  int KEY_STRUCT_JUSTIFICATION;
  int KEY_STRUCT_TEXT_POS;
  int KEY_STRUCT_STYLE_LIST;
  int KEY_STRUCT_KARAOKE_LIST;
  int KEY_STRUCT_HYPER_TEXT_LIST;
  int KEY_STRUCT_HIGHLIGHT_LIST;
  int KEY_STRUCT_FONT_LIST;
  int KEY_STRUCT_BLINKING_TEXT_LIST;
  int KEY_START_TIME;
  int KEY_WRAP_TEXT;
  int KEY_SCROLL_DELAY;
  int KEY_HIGHLIGHT_COLOR_RGBA;
  int KEY_BACKGROUND_COLOR_RGBA;
  int KEY_STYLE_FLAGS;
  int KEY_DISPLAY_FLAGS;
  int FIRST_PUBLIC_KEY;
}
class ThumbnailUtils {
  class SizedThumbnailBitmap {
    int mThumbnailHeight;
    int mThumbnailWidth;
    int mBitmap;
    int mThumbnailData;
  }
  int TARGET_SIZE_MICRO_THUMBNAIL;
  int TARGET_SIZE_MINI_THUMBNAIL;
  int OPTIONS_RECYCLE_INPUT;
  int OPTIONS_SCALE_UP;
  int OPTIONS_NONE;
  int UNCONSTRAINED;
  int MAX_NUM_PIXELS_MICRO_THUMBNAIL;
  int MAX_NUM_PIXELS_THUMBNAIL;
  int TAG;
}
class SoundPool {
  class EventHandler {
    int mSoundPool;
  }
  class OnLoadCompleteListener {
  }
  int SAMPLE_LOADED;
  int mLock;
  int mOnLoadCompleteListener;
  int mEventHandler;
  int mNativeContext;
  int DEBUG;
  int TAG;
}
class RingtoneManager {
  int mIncludeDrm;
  int mPreviousRingtone;
  int mStopPreviousRingtone;
  int mFilterColumns;
  int mType;
  int mCursor;
  int mContext;
  int mActivity;
  int URI_COLUMN_INDEX;
  int TITLE_COLUMN_INDEX;
  int ID_COLUMN_INDEX;
  int MEDIA_COLUMNS;
  int DRM_COLUMNS;
  int INTERNAL_COLUMNS;
  int EXTRA_RINGTONE_PICKED_URI;
  int EXTRA_RINGTONE_TITLE;
  int EXTRA_RINGTONE_TYPE;
  int EXTRA_RINGTONE_DEFAULT_URI;
  int EXTRA_RINGTONE_EXISTING_URI;
  int EXTRA_RINGTONE_INCLUDE_DRM;
  int EXTRA_RINGTONE_SHOW_SILENT;
  int EXTRA_RINGTONE_SHOW_DEFAULT;
  int ACTION_RINGTONE_PICKER;
  int TYPE_ALL;
  int TYPE_ALARM;
  int TYPE_NOTIFICATION;
  int TYPE_RINGTONE;
  int TAG;
}
class Ringtone {
  int mStreamType;
  int mTitle;
  int mUri;
  int mLocalPlayer;
  int mRemoteToken;
  int mRemotePlayer;
  int mAllowRemote;
  int mAudioManager;
  int mContext;
  int DRM_COLUMNS;
  int MEDIA_COLUMNS;
  int LOGD;
  int TAG;
}
class ResampleInputStream {
  int mOneByte;
  int mFirLength;
  int mBufCount;
  int mBuf;
  int mRateOut;
  int mRateIn;
  int mInputStream;
  int TAG;
}
class RemoteControlClient {
  int sService;
  class EventHandler {
  }
  int MSG_UNPLUG_DISPLAY;
  int MSG_PLUG_DISPLAY;
  int MSG_NEW_CURRENT_CLIENT_GEN;
  int MSG_NEW_INTERNAL_CLIENT_GEN;
  int MSG_REQUEST_ARTWORK;
  int MSG_REQUEST_TRANSPORTCONTROL;
  int MSG_REQUEST_METADATA;
  int MSG_REQUEST_PLAYBACK_STATE;
  int mEventHandler;
  int mRcseId;
  int RCSE_ID_UNREGISTERED;
  int mIRCC;
  int mRcDisplay;
  int mRcMediaIntent;
  int mInternalClientGenId;
  int mCurrentClientGenId;
  int mMetadata;
  int mTransportControlFlags;
  int mArtworkExpectedHeight;
  int mArtworkExpectedWidth;
  int ARTWORK_INVALID_SIZE;
  int ARTWORK_DEFAULT_SIZE;
  int mArtwork;
  int mPlaybackStateChangeTimeMs;
  int mPlaybackState;
  int mCacheLock;
  int mPlaybackStream;
  int mPlaybackVolumeHandling;
  int mPlaybackVolume;
  int mPlaybackVolumeMax;
  int mPlaybackType;
  int DEFAULT_PLAYBACK_VOLUME;
  int DEFAULT_PLAYBACK_VOLUME_HANDLING;
  class MetadataEditor {
    int METADATA_KEY_ARTWORK;
    int BITMAP_KEY_ARTWORK;
    int mApplied;
    int mEditorMetadata;
    int mEditorArtwork;
    int mArtworkChanged;
    int mMetadataChanged;
  }
  int METADATA_KEYS_TYPE_LONG;
  int METADATA_KEYS_TYPE_STRING;
  int FLAG_INFORMATION_REQUEST_ALBUM_ART;
  int FLAG_INFORMATION_REQUEST_PLAYSTATE;
  int FLAG_INFORMATION_REQUEST_KEY_MEDIA;
  int FLAG_INFORMATION_REQUEST_METADATA;
  int FLAGS_KEY_MEDIA_NONE;
  int FLAG_KEY_MEDIA_NEXT;
  int FLAG_KEY_MEDIA_FAST_FORWARD;
  int FLAG_KEY_MEDIA_STOP;
  int FLAG_KEY_MEDIA_PAUSE;
  int FLAG_KEY_MEDIA_PLAY_PAUSE;
  int FLAG_KEY_MEDIA_PLAY;
  int FLAG_KEY_MEDIA_REWIND;
  int FLAG_KEY_MEDIA_PREVIOUS;
  int PLAYBACKINFO_PLAYSTATE;
  int PLAYBACKINFO_USES_STREAM;
  int PLAYBACKINFO_VOLUME_HANDLING;
  int PLAYBACKINFO_VOLUME_MAX;
  int PLAYBACKINFO_VOLUME;
  int PLAYBACKINFO_PLAYBACK_TYPE;
  int PLAYBACKINFO_INVALID_VALUE;
  int PLAYBACK_VOLUME_VARIABLE;
  int PLAYBACK_VOLUME_FIXED;
  int PLAYBACK_TYPE_MAX;
  int PLAYBACK_TYPE_MIN;
  int PLAYBACK_TYPE_REMOTE;
  int PLAYBACK_TYPE_LOCAL;
  int PLAYSTATE_NONE;
  int PLAYSTATE_ERROR;
  int PLAYSTATE_BUFFERING;
  int PLAYSTATE_SKIPPING_BACKWARDS;
  int PLAYSTATE_SKIPPING_FORWARDS;
  int PLAYSTATE_REWINDING;
  int PLAYSTATE_FAST_FORWARDING;
  int PLAYSTATE_PLAYING;
  int PLAYSTATE_PAUSED;
  int PLAYSTATE_STOPPED;
  int TAG;
}
class MiniThumbFile {
  int sThumbFiles;
  int mBuffer;
  int mChannel;
  int mMiniThumbFile;
  int mUri;
  int HEADER_SIZE;
  int BYTES_PER_MINTHUMB;
  int MINI_THUMB_DATA_FILE_VERSION;
  int TAG;
}
class Metadata {
  int mKeyToPosMap;
  int mParcel;
  int kMetaMarker;
  int kRecordHeaderSize;
  int kMetaHeaderSize;
  int kInt32Size;
  int TAG;
  int LAST_TYPE;
  int BYTE_ARRAY_VAL;
  int DATE_VAL;
  int DOUBLE_VAL;
  int LONG_VAL;
  int BOOLEAN_VAL;
  int INTEGER_VAL;
  int STRING_VAL;
  int MATCH_ALL;
  int MATCH_NONE;
  int FIRST_CUSTOM;
  int LAST_SYSTEM;
  int DRM_CRIPPLED;
  int NUM_TRACKS;
  int VIDEO_WIDTH;
  int VIDEO_HEIGHT;
  int VIDEO_CODEC;
  int AUDIO_CODEC;
  int MIME_TYPE;
  int VIDEO_FRAME_RATE;
  int AUDIO_SAMPLE_RATE;
  int VIDEO_BIT_RATE;
  int AUDIO_BIT_RATE;
  int BIT_RATE;
  int VIDEO_FRAME;
  int ALBUM_ART;
  int RATING;
  int CD_TRACK_MAX;
  int CD_TRACK_NUM;
  int DURATION;
  int DATE;
  int GENRE;
  int COMPOSER;
  int AUTHOR;
  int ARTIST;
  int ALBUM;
  int COPYRIGHT;
  int COMMENT;
  int TITLE;
  int SEEK_AVAILABLE;
  int SEEK_FORWARD_AVAILABLE;
  int SEEK_BACKWARD_AVAILABLE;
  int PAUSE_AVAILABLE;
  int ANY;
}
class MediaSyncEvent {
  int mAudioSession;
  int mType;
  int SYNC_EVENT_PRESENTATION_COMPLETE;
  int SYNC_EVENT_NONE;
}
class MediaScannerConnection {
  class ClientProxy {
    int mNextPath;
    int mConnection;
    int mClient;
    int mMimeTypes;
    int mPaths;
  }
  class MediaScannerConnectionClient {
  }
  class OnScanCompletedListener {
  }
  int mListener;
  int mConnected;
  int mService;
  int mClient;
  int mContext;
  int TAG;
}
class MediaScannerClient {
}
class MediaScanner {
  class WplHandler {
    int playListDirectory;
    int handler;
  }
  class MediaBulkDeleter {
    int mBaseUri;
    int mProvider;
    int whereArgs;
    int whereClause;
  }
  class MyMediaScannerClient {
    int mHeight;
    int mWidth;
    int mNoMedia;
    int mIsDrm;
    int mCompilation;
    int mWriter;
    int mFileSize;
    int mLastModified;
    int mPath;
    int mDuration;
    int mYear;
    int mTrack;
    int mFileType;
    int mMimeType;
    int mGenre;
    int mComposer;
    int mTitle;
    int mAlbum;
    int mAlbumArtist;
    int mArtist;
  }
  int mClient;
  int mDrmManagerClient;
  int mPlayLists;
  int mMediaInserter;
  int mPlaylistEntries;
  class PlaylistEntry {
    int bestmatchlevel;
    int bestmatchid;
    int path;
  }
  class FileEntry {
    int mLastModifiedChanged;
    int mFormat;
    int mLastModified;
    int mPath;
    int mRowId;
  }
  int mBitmapOptions;
  int mCaseInsensitivePaths;
  int DEFAULT_RINGTONE_PROPERTY_PREFIX;
  int mDefaultAlarmAlertFilename;
  int mDefaultNotificationFilename;
  int mDefaultRingtoneFilename;
  int mDefaultAlarmSet;
  int mDefaultNotificationSet;
  int mDefaultRingtoneSet;
  int mWasEmptyPriorToScan;
  int mOriginalCount;
  int ENABLE_BULK_INSERTS;
  int mExternalStoragePath;
  int mMtpObjectHandle;
  int mProcessGenres;
  int mProcessPlaylists;
  int mFilesUri;
  int mPlaylistsUri;
  int mThumbsUri;
  int mImagesUri;
  int mVideoUri;
  int mAudioUri;
  int mMediaProvider;
  int mContext;
  int mNativeContext;
  int ID3_GENRES;
  int PODCAST_DIR;
  int MUSIC_DIR;
  int ALARMS_DIR;
  int NOTIFICATIONS_DIR;
  int RINGTONES_DIR;
  int DATE_MODIFIED_PLAYLISTS_COLUMN_INDEX;
  int PATH_PLAYLISTS_COLUMN_INDEX;
  int ID_PLAYLISTS_COLUMN_INDEX;
  int PLAYLIST_MEMBERS_PROJECTION;
  int FILES_PRESCAN_DATE_MODIFIED_COLUMN_INDEX;
  int FILES_PRESCAN_FORMAT_COLUMN_INDEX;
  int FILES_PRESCAN_PATH_COLUMN_INDEX;
  int FILES_PRESCAN_ID_COLUMN_INDEX;
  int ID_PROJECTION;
  int FILES_PRESCAN_PROJECTION;
  int TAG;
}
class MediaRouter {
  class VolumeChangeReceiver {
  }
  class VolumeCallback {
  }
  class VolumeCallbackInfo {
    int route;
    int vcb;
  }
  class SimpleCallback {
  }
  class Callback {
  }
  class CallbackInfo {
    int router;
    int cb;
    int type;
  }
  class RouteCategory {
    int mGroupable;
    int mTypes;
    int mNameResId;
    int mName;
  }
  class RouteGroup {
    int mUpdateName;
    int mRoutes;
  }
  class UserRouteInfo {
    int mRcc;
  }
  class RouteInfo {
    int mRemoteVolObserver;
    int PLAYBACK_VOLUME_VARIABLE;
    int PLAYBACK_VOLUME_FIXED;
    int PLAYBACK_TYPE_REMOTE;
    int PLAYBACK_TYPE_LOCAL;
    int mTag;
    int mVcb;
    int mPlaybackStream;
    int mVolumeHandling;
    int mVolume;
    int mVolumeMax;
    int mPlaybackType;
    int mIcon;
    int mCategory;
    int mGroup;
    int mSupportedTypes;
    int mStatus;
    int mNameResId;
    int mName;
  }
  int sRouters;
  int ROUTE_TYPE_USER;
  int ROUTE_TYPE_LIVE_AUDIO;
  int sStatic;
  class Static {
    int mRoutesObserver;
    int mSelectedRoute;
    int mBluetoothA2dpRoute;
    int mDefaultAudio;
    int mCurRoutesInfo;
    int mSystemCategory;
    int mCategories;
    int mRoutes;
    int mCallbacks;
    int mHandler;
    int mAudioService;
    int mResources;
  }
  int TAG;
}
class MediaRecorder {
  class EventHandler {
    int MEDIA_RECORDER_TRACK_EVENT_LIST_END;
    int MEDIA_RECORDER_TRACK_EVENT_INFO;
    int MEDIA_RECORDER_TRACK_EVENT_ERROR;
    int MEDIA_RECORDER_TRACK_EVENT_LIST_START;
    int MEDIA_RECORDER_EVENT_LIST_END;
    int MEDIA_RECORDER_EVENT_INFO;
    int MEDIA_RECORDER_EVENT_ERROR;
    int MEDIA_RECORDER_EVENT_LIST_START;
    int mMediaRecorder;
  }
  class OnInfoListener {
  }
  int MEDIA_RECORDER_TRACK_INFO_LIST_END;
  int MEDIA_RECORDER_TRACK_INFO_DATA_KBYTES;
  int MEDIA_RECORDER_TRACK_INFO_START_OFFSET_MS;
  int MEDIA_RECORDER_TRACK_INFO_INITIAL_DELAY_MS;
  int MEDIA_RECORDER_TRACK_INTER_CHUNK_TIME_MS;
  int MEDIA_RECORDER_TRACK_INFO_ENCODED_FRAMES;
  int MEDIA_RECORDER_TRACK_INFO_MAX_CHUNK_DUR_MS;
  int MEDIA_RECORDER_TRACK_INFO_DURATION_MS;
  int MEDIA_RECORDER_TRACK_INFO_TYPE;
  int MEDIA_RECORDER_TRACK_INFO_PROGRESS_IN_TIME;
  int MEDIA_RECORDER_TRACK_INFO_COMPLETION_STATUS;
  int MEDIA_RECORDER_TRACK_INFO_LIST_START;
  int MEDIA_RECORDER_INFO_MAX_FILESIZE_REACHED;
  int MEDIA_RECORDER_INFO_MAX_DURATION_REACHED;
  int MEDIA_RECORDER_INFO_UNKNOWN;
  class OnErrorListener {
  }
  int MEDIA_RECORDER_ERROR_UNKNOWN;
  class VideoEncoder {
    int MPEG_4_SP;
    int H264;
    int H263;
    int DEFAULT;
  }
  class AudioEncoder {
    int AAC_ELD;
    int HE_AAC;
    int AAC;
    int AMR_WB;
    int AMR_NB;
    int DEFAULT;
  }
  class OutputFormat {
    int OUTPUT_FORMAT_MPEG2TS;
    int OUTPUT_FORMAT_RTP_AVP;
    int AAC_ADTS;
    int AAC_ADIF;
    int AMR_WB;
    int AMR_NB;
    int RAW_AMR;
    int MPEG_4;
    int THREE_GPP;
    int DEFAULT;
  }
  class VideoSource {
    int GRALLOC_BUFFER;
    int CAMERA;
    int DEFAULT;
  }
  class AudioSource {
    int VOICE_COMMUNICATION;
    int VOICE_RECOGNITION;
    int CAMCORDER;
    int VOICE_CALL;
    int VOICE_DOWNLINK;
    int VOICE_UPLINK;
    int MIC;
    int DEFAULT;
  }
  int mOnInfoListener;
  int mOnErrorListener;
  int mEventHandler;
  int mFd;
  int mPath;
  int mSurface;
  int mNativeContext;
  int TAG;
}
class MediaPlayer {
  int mOnInfoListener;
  class OnInfoListener {
  }
  int MEDIA_INFO_TIMED_TEXT_ERROR;
  int MEDIA_INFO_METADATA_UPDATE;
  int MEDIA_INFO_NOT_SEEKABLE;
  int MEDIA_INFO_BAD_INTERLEAVING;
  int MEDIA_INFO_BUFFERING_END;
  int MEDIA_INFO_BUFFERING_START;
  int MEDIA_INFO_VIDEO_TRACK_LAGGING;
  int MEDIA_INFO_STARTED_AS_NEXT;
  int MEDIA_INFO_UNKNOWN;
  int mOnErrorListener;
  class OnErrorListener {
  }
  int MEDIA_ERROR_NOT_VALID_FOR_PROGRESSIVE_PLAYBACK;
  int MEDIA_ERROR_SERVER_DIED;
  int MEDIA_ERROR_UNKNOWN;
  int mOnTimedTextListener;
  class OnTimedTextListener {
  }
  int mOnVideoSizeChangedListener;
  class OnVideoSizeChangedListener {
  }
  int mOnSeekCompleteListener;
  class OnSeekCompleteListener {
  }
  int mOnBufferingUpdateListener;
  class OnBufferingUpdateListener {
  }
  int mOnCompletionListener;
  class OnCompletionListener {
  }
  int mOnPreparedListener;
  class OnPreparedListener {
  }
  class EventHandler {
    int mMediaPlayer;
  }
  int MEDIA_INFO;
  int MEDIA_ERROR;
  int MEDIA_TIMED_TEXT;
  int MEDIA_SET_VIDEO_SIZE;
  int MEDIA_SEEK_COMPLETE;
  int MEDIA_BUFFERING_UPDATE;
  int MEDIA_PLAYBACK_COMPLETE;
  int MEDIA_PREPARED;
  int MEDIA_NOP;
  int MEDIA_MIMETYPE_TEXT_SUBRIP;
  class TrackInfo {
    int CREATOR;
    int mLanguage;
    int mTrackType;
    int MEDIA_TRACK_TYPE_TIMEDTEXT;
    int MEDIA_TRACK_TYPE_AUDIO;
    int MEDIA_TRACK_TYPE_VIDEO;
    int MEDIA_TRACK_TYPE_UNKNOWN;
  }
  int VIDEO_SCALING_MODE_SCALE_TO_FIT_WITH_CROPPING;
  int VIDEO_SCALING_MODE_SCALE_TO_FIT;
  int INVOKE_ID_SET_VIDEO_SCALE_MODE;
  int INVOKE_ID_DESELECT_TRACK;
  int INVOKE_ID_SELECT_TRACK;
  int INVOKE_ID_ADD_EXTERNAL_SOURCE_FD;
  int INVOKE_ID_ADD_EXTERNAL_SOURCE;
  int INVOKE_ID_GET_TRACK_INFO;
  int mStayAwake;
  int mScreenOnWhilePlaying;
  int mWakeLock;
  int mEventHandler;
  int mSurfaceHolder;
  int mListenerContext;
  int mNativeSurfaceTexture;
  int mNativeContext;
  int IMEDIA_PLAYER;
  int TAG;
  int BYPASS_METADATA_FILTER;
  int APPLY_METADATA_FILTER;
  int METADATA_ALL;
  int METADATA_UPDATE_ONLY;
}
class MediaMetadataRetriever {
  int METADATA_KEY_LOCATION;
  int METADATA_KEY_IS_DRM;
  int METADATA_KEY_TIMED_TEXT_LANGUAGES;
  int METADATA_KEY_BITRATE;
  int METADATA_KEY_VIDEO_HEIGHT;
  int METADATA_KEY_VIDEO_WIDTH;
  int METADATA_KEY_HAS_VIDEO;
  int METADATA_KEY_HAS_AUDIO;
  int METADATA_KEY_COMPILATION;
  int METADATA_KEY_DISC_NUMBER;
  int METADATA_KEY_ALBUMARTIST;
  int METADATA_KEY_MIMETYPE;
  int METADATA_KEY_WRITER;
  int METADATA_KEY_NUM_TRACKS;
  int METADATA_KEY_DURATION;
  int METADATA_KEY_YEAR;
  int METADATA_KEY_TITLE;
  int METADATA_KEY_GENRE;
  int METADATA_KEY_DATE;
  int METADATA_KEY_COMPOSER;
  int METADATA_KEY_AUTHOR;
  int METADATA_KEY_ARTIST;
  int METADATA_KEY_ALBUM;
  int METADATA_KEY_CD_TRACK_NUMBER;
  int OPTION_CLOSEST;
  int OPTION_CLOSEST_SYNC;
  int OPTION_NEXT_SYNC;
  int OPTION_PREVIOUS_SYNC;
  int EMBEDDED_PICTURE_TYPE_ANY;
  int mNativeContext;
}
class MediaInserter {
  int mBufferSizePerUri;
  int mProvider;
  int mPriorityRowMap;
  int mRowMap;
}
class MediaFormat {
  int KEY_FLAC_COMPRESSION_LEVEL;
  int KEY_AAC_PROFILE;
  int KEY_CHANNEL_MASK;
  int KEY_IS_ADTS;
  int KEY_DURATION;
  int KEY_SLICE_HEIGHT;
  int KEY_STRIDE;
  int KEY_I_FRAME_INTERVAL;
  int KEY_FRAME_RATE;
  int KEY_COLOR_FORMAT;
  int KEY_BIT_RATE;
  int KEY_MAX_INPUT_SIZE;
  int KEY_HEIGHT;
  int KEY_WIDTH;
  int KEY_CHANNEL_COUNT;
  int KEY_SAMPLE_RATE;
  int KEY_MIME;
  int mMap;
}
class MediaFile {
  int sFormatToMimeTypeMap;
  int sMimeTypeToFormatMap;
  int sFileTypeToFormatMap;
  int sMimeTypeMap;
  int sFileTypeMap;
  class MediaFileType {
    int mimeType;
    int fileType;
  }
  int FILE_TYPE_ZIP;
  int FILE_TYPE_MS_POWERPOINT;
  int FILE_TYPE_MS_EXCEL;
  int FILE_TYPE_MS_WORD;
  int FILE_TYPE_XML;
  int FILE_TYPE_PDF;
  int FILE_TYPE_HTML;
  int FILE_TYPE_TEXT;
  int LAST_DRM_FILE_TYPE;
  int FIRST_DRM_FILE_TYPE;
  int FILE_TYPE_FL;
  int LAST_PLAYLIST_FILE_TYPE;
  int FIRST_PLAYLIST_FILE_TYPE;
  int FILE_TYPE_HTTPLIVE;
  int FILE_TYPE_WPL;
  int FILE_TYPE_PLS;
  int FILE_TYPE_M3U;
  int LAST_IMAGE_FILE_TYPE;
  int FIRST_IMAGE_FILE_TYPE;
  int FILE_TYPE_WEBP;
  int FILE_TYPE_WBMP;
  int FILE_TYPE_BMP;
  int FILE_TYPE_PNG;
  int FILE_TYPE_GIF;
  int FILE_TYPE_JPEG;
  int LAST_VIDEO_FILE_TYPE2;
  int FIRST_VIDEO_FILE_TYPE2;
  int FILE_TYPE_MP2PS;
  int LAST_VIDEO_FILE_TYPE;
  int FIRST_VIDEO_FILE_TYPE;
  int FILE_TYPE_WEBM;
  int FILE_TYPE_AVI;
  int FILE_TYPE_MP2TS;
  int FILE_TYPE_MKV;
  int FILE_TYPE_ASF;
  int FILE_TYPE_WMV;
  int FILE_TYPE_3GPP2;
  int FILE_TYPE_3GPP;
  int FILE_TYPE_M4V;
  int FILE_TYPE_MP4;
  int LAST_MIDI_FILE_TYPE;
  int FIRST_MIDI_FILE_TYPE;
  int FILE_TYPE_IMY;
  int FILE_TYPE_SMF;
  int FILE_TYPE_MID;
  int LAST_AUDIO_FILE_TYPE;
  int FIRST_AUDIO_FILE_TYPE;
  int FILE_TYPE_FLAC;
  int FILE_TYPE_MKA;
  int FILE_TYPE_AAC;
  int FILE_TYPE_OGG;
  int FILE_TYPE_WMA;
  int FILE_TYPE_AWB;
  int FILE_TYPE_AMR;
  int FILE_TYPE_WAV;
  int FILE_TYPE_M4A;
  int FILE_TYPE_MP3;
}
class MediaExtractor {
  int mNativeContext;
  int SAMPLE_FLAG_ENCRYPTED;
  int SAMPLE_FLAG_SYNC;
  int SEEK_TO_CLOSEST_SYNC;
  int SEEK_TO_NEXT_SYNC;
  int SEEK_TO_PREVIOUS_SYNC;
}
class MediaCryptoException {
}
class MediaCrypto {
  int mNativeContext;
}
class MediaCodecList {
}
class MediaCodecInfo {
  class CodecProfileLevel {
    int level;
    int profile;
    int AACObjectELD;
    int AACObjectHE_PS;
    int AACObjectLD;
    int AACObjectERLC;
    int AACObjectScalable;
    int AACObjectHE;
    int AACObjectLTP;
    int AACObjectSSR;
    int AACObjectLC;
    int AACObjectMain;
    int MPEG4Level5;
    int MPEG4Level4a;
    int MPEG4Level4;
    int MPEG4Level3;
    int MPEG4Level2;
    int MPEG4Level1;
    int MPEG4Level0b;
    int MPEG4Level0;
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
    int H263Level70;
    int H263Level60;
    int H263Level50;
    int H263Level45;
    int H263Level40;
    int H263Level30;
    int H263Level20;
    int H263Level10;
    int H263ProfileHighLatency;
    int H263ProfileInterlace;
    int H263ProfileInternet;
    int H263ProfileHighCompression;
    int H263ProfileISWV3;
    int H263ProfileISWV2;
    int H263ProfileBackwardCompatible;
    int H263ProfileH320Coding;
    int H263ProfileBaseline;
    int AVCLevel51;
    int AVCLevel5;
    int AVCLevel42;
    int AVCLevel41;
    int AVCLevel4;
    int AVCLevel32;
    int AVCLevel31;
    int AVCLevel3;
    int AVCLevel22;
    int AVCLevel21;
    int AVCLevel2;
    int AVCLevel13;
    int AVCLevel12;
    int AVCLevel11;
    int AVCLevel1b;
    int AVCLevel1;
    int AVCProfileHigh444;
    int AVCProfileHigh422;
    int AVCProfileHigh10;
    int AVCProfileHigh;
    int AVCProfileExtended;
    int AVCProfileMain;
    int AVCProfileBaseline;
  }
  class CodecCapabilities {
    int colorFormats;
    int COLOR_QCOM_FormatYUV420SemiPlanar;
    int COLOR_TI_FormatYUV420PackedSemiPlanar;
    int COLOR_Format24BitABGR6666;
    int COLOR_Format24BitARGB6666;
    int COLOR_Format18BitBGR666;
    int COLOR_FormatYUV422PackedSemiPlanar;
    int COLOR_FormatYUV420PackedSemiPlanar;
    int COLOR_FormatL32;
    int COLOR_FormatL24;
    int COLOR_FormatL16;
    int COLOR_FormatL8;
    int COLOR_FormatL4;
    int COLOR_FormatL2;
    int COLOR_FormatRawBayer8bitcompressed;
    int COLOR_FormatRawBayer10bit;
    int COLOR_FormatRawBayer8bit;
    int COLOR_FormatYUV444Interleaved;
    int COLOR_FormatCrYCbY;
    int COLOR_FormatCbYCrY;
    int COLOR_FormatYCrYCb;
    int COLOR_FormatYCbYCr;
    int COLOR_FormatYUV422SemiPlanar;
    int COLOR_FormatYUV422PackedPlanar;
    int COLOR_FormatYUV422Planar;
    int COLOR_FormatYUV420SemiPlanar;
    int COLOR_FormatYUV420PackedPlanar;
    int COLOR_FormatYUV420Planar;
    int COLOR_FormatYUV411PackedPlanar;
    int COLOR_FormatYUV411Planar;
    int COLOR_Format32bitARGB8888;
    int COLOR_Format32bitBGRA8888;
    int COLOR_Format25bitARGB1888;
    int COLOR_Format24bitARGB1887;
    int COLOR_Format24bitBGR888;
    int COLOR_Format24bitRGB888;
    int COLOR_Format19bitARGB1666;
    int COLOR_Format18bitARGB1665;
    int COLOR_Format18bitRGB666;
    int COLOR_Format16bitBGR565;
    int COLOR_Format16bitRGB565;
    int COLOR_Format16bitARGB1555;
    int COLOR_Format16bitARGB4444;
    int COLOR_Format12bitRGB444;
    int COLOR_Format8bitRGB332;
    int COLOR_FormatMonochrome;
    int profileLevels;
  }
  int mIndex;
}
class MediaCodec {
  int mNativeContext;
  int VIDEO_SCALING_MODE_SCALE_TO_FIT_WITH_CROPPING;
  int VIDEO_SCALING_MODE_SCALE_TO_FIT;
  int INFO_OUTPUT_BUFFERS_CHANGED;
  int INFO_OUTPUT_FORMAT_CHANGED;
  int INFO_TRY_AGAIN_LATER;
  class CryptoInfo {
    int mode;
    int iv;
    int key;
    int numBytesOfEncryptedData;
    int numBytesOfClearData;
    int numSubSamples;
  }
  int CRYPTO_MODE_AES_CTR;
  int CRYPTO_MODE_UNENCRYPTED;
  class CryptoException {
    int mErrorCode;
  }
  int CONFIGURE_FLAG_ENCODE;
  int BUFFER_FLAG_END_OF_STREAM;
  int BUFFER_FLAG_CODEC_CONFIG;
  int BUFFER_FLAG_SYNC_FRAME;
  class BufferInfo {
    int flags;
    int presentationTimeUs;
    int size;
    int offset;
  }
}
class MediaActionSound {
  int mLoadCompleteListener;
  int SOUND_NOT_LOADED;
  int STOP_VIDEO_RECORDING;
  int START_VIDEO_RECORDING;
  int FOCUS_COMPLETE;
  int SHUTTER_CLICK;
  int TAG;
  int SOUND_FILES;
  int mSoundIdToPlay;
  int mSoundIds;
  int mSoundPool;
  int NUM_MEDIA_SOUND_STREAMS;
}
class JetPlayer {
  int TAG;
  class OnJetEventListener {
  }
  class NativeEventHandler {
    int mJet;
  }
  int mNativePlayerInJavaObj;
  int singletonRef;
  int mJetEventListener;
  int mEventListenerLock;
  int mInitializationLooper;
  int mEventHandler;
  int JET_OUTPUT_CHANNEL_CONFIG;
  int JET_OUTPUT_RATE;
  int JET_EVENT_SEG_SHIFT;
  int JET_EVENT_TRACK_SHIFT;
  int JET_EVENT_CHAN_SHIFT;
  int JET_EVENT_CTRL_SHIFT;
  int JET_EVENT_SEG_MASK;
  int JET_EVENT_TRACK_MASK;
  int JET_EVENT_CHAN_MASK;
  int JET_EVENT_CTRL_MASK;
  int JET_EVENT_VAL_MASK;
  int JET_PAUSE_UPDATE;
  int JET_NUMQUEUEDSEGMENT_UPDATE;
  int JET_USERID_UPDATE;
  int JET_EVENT;
  int MAXTRACKS;
}
class FaceDetector {
  int mBWBuffer;
  int mMaxFaces;
  int mHeight;
  int mWidth;
  int mDCR;
  int mSDK;
  int mFD;
  int sInitialized;
  class Face {
    int mPoseEulerZ;
    int mPoseEulerY;
    int mPoseEulerX;
    int mEyesDist;
    int mMidPointY;
    int mMidPointX;
    int mConfidence;
    int EULER_Z;
    int EULER_Y;
    int EULER_X;
    int CONFIDENCE_THRESHOLD;
  }
}
class ExifInterface {
  int sLock;
  int mHasThumbnail;
  int mAttributes;
  int mFilename;
  int sFormatter;
  int WHITEBALANCE_MANUAL;
  int WHITEBALANCE_AUTO;
  int ORIENTATION_ROTATE_270;
  int ORIENTATION_TRANSVERSE;
  int ORIENTATION_ROTATE_90;
  int ORIENTATION_TRANSPOSE;
  int ORIENTATION_FLIP_VERTICAL;
  int ORIENTATION_ROTATE_180;
  int ORIENTATION_FLIP_HORIZONTAL;
  int ORIENTATION_NORMAL;
  int ORIENTATION_UNDEFINED;
  int TAG_GPS_PROCESSING_METHOD;
  int TAG_FOCAL_LENGTH;
  int TAG_WHITE_BALANCE;
  int TAG_GPS_DATESTAMP;
  int TAG_GPS_TIMESTAMP;
  int TAG_GPS_ALTITUDE_REF;
  int TAG_GPS_ALTITUDE;
  int TAG_ISO;
  int TAG_APERTURE;
  int TAG_EXPOSURE_TIME;
  int TAG_GPS_LONGITUDE_REF;
  int TAG_GPS_LATITUDE_REF;
  int TAG_GPS_LONGITUDE;
  int TAG_GPS_LATITUDE;
  int TAG_IMAGE_LENGTH;
  int TAG_IMAGE_WIDTH;
  int TAG_FLASH;
  int TAG_MODEL;
  int TAG_MAKE;
  int TAG_DATETIME;
  int TAG_ORIENTATION;
}
class EncoderCapabilities {
  class AudioEncoderCap {
    int mMaxBitRate;
    int mMinBitRate;
    int mMaxSampleRate;
    int mMinSampleRate;
    int mMaxChannels;
    int mMinChannels;
    int mCodec;
  }
  class VideoEncoderCap {
    int mMaxFrameHeight;
    int mMinFrameHeight;
    int mMaxFrameWidth;
    int mMinFrameWidth;
    int mMaxFrameRate;
    int mMinFrameRate;
    int mMaxBitRate;
    int mMinBitRate;
    int mCodec;
  }
  int TAG;
}
class DecoderCapabilities {
  class AudioDecoder {
    int AUDIO_DECODER_WMA;
  }
  class VideoDecoder {
    int VIDEO_DECODER_WMV;
  }
}
class CameraProfile {
  int sCache;
  int QUALITY_HIGH;
  int QUALITY_MEDIUM;
  int QUALITY_LOW;
}
class CamcorderProfile {
  int audioChannels;
  int audioSampleRate;
  int audioBitRate;
  int audioCodec;
  int videoFrameHeight;
  int videoFrameWidth;
  int videoFrameRate;
  int videoBitRate;
  int videoCodec;
  int fileFormat;
  int quality;
  int duration;
  int QUALITY_TIME_LAPSE_LIST_END;
  int QUALITY_TIME_LAPSE_LIST_START;
  int QUALITY_TIME_LAPSE_QVGA;
  int QUALITY_TIME_LAPSE_1080P;
  int QUALITY_TIME_LAPSE_720P;
  int QUALITY_TIME_LAPSE_480P;
  int QUALITY_TIME_LAPSE_CIF;
  int QUALITY_TIME_LAPSE_QCIF;
  int QUALITY_TIME_LAPSE_HIGH;
  int QUALITY_TIME_LAPSE_LOW;
  int QUALITY_LIST_END;
  int QUALITY_LIST_START;
  int QUALITY_QVGA;
  int QUALITY_1080P;
  int QUALITY_720P;
  int QUALITY_480P;
  int QUALITY_CIF;
  int QUALITY_QCIF;
  int QUALITY_HIGH;
  int QUALITY_LOW;
}
class AudioTrack {
  class NativeEventHandlerDelegate {
    int mHandler;
    int mAudioTrack;
  }
  class OnPlaybackPositionUpdateListener {
  }
  int SUPPORTED_OUT_CHANNELS;
  int mJniData;
  int mNativeTrackInJavaObj;
  int mSessionId;
  int mAudioFormat;
  int mChannelConfiguration;
  int mDataLoadMode;
  int mStreamType;
  int mChannels;
  int mChannelCount;
  int mSampleRate;
  int mInitializationLooper;
  int mEventHandlerDelegate;
  int mNativeBufferSizeInBytes;
  int mPositionListenerLock;
  int mPositionListener;
  int mPlayStateLock;
  int mPlayState;
  int mState;
  int TAG;
  int NATIVE_EVENT_NEW_POS;
  int NATIVE_EVENT_MARKER;
  int ERROR_NATIVESETUP_NATIVEINITFAILED;
  int ERROR_NATIVESETUP_INVALIDSTREAMTYPE;
  int ERROR_NATIVESETUP_INVALIDFORMAT;
  int ERROR_NATIVESETUP_INVALIDCHANNELMASK;
  int ERROR_NATIVESETUP_AUDIOSYSTEM;
  int ERROR_INVALID_OPERATION;
  int ERROR_BAD_VALUE;
  int ERROR;
  int SUCCESS;
  int STATE_NO_STATIC_DATA;
  int STATE_INITIALIZED;
  int STATE_UNINITIALIZED;
  int MODE_STREAM;
  int MODE_STATIC;
  int PLAYSTATE_PLAYING;
  int PLAYSTATE_PAUSED;
  int PLAYSTATE_STOPPED;
  int VOLUME_MAX;
  int VOLUME_MIN;
}
class AudioSystem {
  int SYNC_EVENT_PRESENTATION_COMPLETE;
  int SYNC_EVENT_NONE;
  int NUM_FORCE_USE;
  int FOR_DOCK;
  int FOR_RECORD;
  int FOR_MEDIA;
  int FOR_COMMUNICATION;
  int FORCE_DEFAULT;
  int NUM_FORCE_CONFIG;
  int FORCE_NO_BT_A2DP;
  int FORCE_DIGITAL_DOCK;
  int FORCE_ANALOG_DOCK;
  int FORCE_BT_DESK_DOCK;
  int FORCE_BT_CAR_DOCK;
  int FORCE_WIRED_ACCESSORY;
  int FORCE_BT_A2DP;
  int FORCE_BT_SCO;
  int FORCE_HEADPHONES;
  int FORCE_SPEAKER;
  int FORCE_NONE;
  int PHONE_STATE_INCALL;
  int PHONE_STATE_RINGING;
  int PHONE_STATE_OFFCALL;
  int DEVICE_OUT_USB_DEVICE_NAME;
  int DEVICE_OUT_USB_ACCESSORY_NAME;
  int DEVICE_OUT_DGTL_DOCK_HEADSET_NAME;
  int DEVICE_OUT_ANLG_DOCK_HEADSET_NAME;
  int DEVICE_OUT_AUX_DIGITAL_NAME;
  int DEVICE_OUT_BLUETOOTH_A2DP_SPEAKER_NAME;
  int DEVICE_OUT_BLUETOOTH_A2DP_HEADPHONES_NAME;
  int DEVICE_OUT_BLUETOOTH_A2DP_NAME;
  int DEVICE_OUT_BLUETOOTH_SCO_CARKIT_NAME;
  int DEVICE_OUT_BLUETOOTH_SCO_HEADSET_NAME;
  int DEVICE_OUT_BLUETOOTH_SCO_NAME;
  int DEVICE_OUT_WIRED_HEADPHONE_NAME;
  int DEVICE_OUT_WIRED_HEADSET_NAME;
  int DEVICE_OUT_SPEAKER_NAME;
  int DEVICE_OUT_EARPIECE_NAME;
  int NUM_DEVICE_STATES;
  int DEVICE_STATE_AVAILABLE;
  int DEVICE_STATE_UNAVAILABLE;
  int DEVICE_IN_DEFAULT;
  int DEVICE_IN_AUX_DIGITAL;
  int DEVICE_IN_WIRED_HEADSET;
  int DEVICE_IN_BLUETOOTH_SCO_HEADSET;
  int DEVICE_IN_MIC_ARRAY;
  int DEVICE_IN_BUILTIN_MIC2;
  int DEVICE_IN_BUILTIN_MIC1;
  int DEVICE_IN_AMBIENT;
  int DEVICE_IN_COMMUNICATION;
  int DEVICE_OUT_ALL_USB;
  int DEVICE_OUT_ALL_SCO;
  int DEVICE_OUT_ALL_A2DP;
  int DEVICE_OUT_ALL;
  int DEVICE_OUT_DEFAULT;
  int DEVICE_OUT_USB_DEVICE;
  int DEVICE_OUT_USB_ACCESSORY;
  int DEVICE_OUT_DGTL_DOCK_HEADSET;
  int DEVICE_OUT_ANLG_DOCK_HEADSET;
  int DEVICE_OUT_AUX_DIGITAL;
  int DEVICE_OUT_BLUETOOTH_A2DP_SPEAKER;
  int DEVICE_OUT_BLUETOOTH_A2DP_HEADPHONES;
  int DEVICE_OUT_BLUETOOTH_A2DP;
  int DEVICE_OUT_BLUETOOTH_SCO_CARKIT;
  int DEVICE_OUT_BLUETOOTH_SCO_HEADSET;
  int DEVICE_OUT_BLUETOOTH_SCO;
  int DEVICE_OUT_WIRED_HEADPHONE;
  int DEVICE_OUT_WIRED_HEADSET;
  int DEVICE_OUT_SPEAKER;
  int DEVICE_OUT_EARPIECE;
  class ErrorCallback {
  }
  int mErrorCallback;
  int AUDIO_STATUS_SERVER_DIED;
  int AUDIO_STATUS_ERROR;
  int AUDIO_STATUS_OK;
  int ROUTE_ALL;
  int ROUTE_BLUETOOTH_A2DP;
  int ROUTE_HEADSET;
  int ROUTE_BLUETOOTH_SCO;
  int ROUTE_BLUETOOTH;
  int ROUTE_SPEAKER;
  int ROUTE_EARPIECE;
  int NUM_MODES;
  int MODE_IN_COMMUNICATION;
  int MODE_IN_CALL;
  int MODE_RINGTONE;
  int MODE_NORMAL;
  int MODE_CURRENT;
  int MODE_INVALID;
  int NUM_STREAM_TYPES;
  int NUM_STREAMS;
  int STREAM_TTS;
  int STREAM_DTMF;
  int STREAM_SYSTEM_ENFORCED;
  int STREAM_BLUETOOTH_SCO;
  int STREAM_NOTIFICATION;
  int STREAM_ALARM;
  int STREAM_MUSIC;
  int STREAM_RING;
  int STREAM_SYSTEM;
  int STREAM_VOICE_CALL;
}
class AudioService {
  class RcDisplayDeathHandler {
    int mCb;
  }
  int mArtworkExpectedHeight;
  int mArtworkExpectedWidth;
  int mRcDisplayDeathHandler;
  int mRcDisplay;
  int mMediaReceiverForCalls;
  int mRCStack;
  class RemoteControlStackEntry {
    int mRemoteVolumeObs;
    int mPlaybackState;
    int mPlaybackStream;
    int mPlaybackVolumeHandling;
    int mPlaybackVolumeMax;
    int mPlaybackVolume;
    int mPlaybackType;
    int mRcClientDeathHandler;
    int mRcClient;
    int mCallingUid;
    int mCallingPackageName;
    int mReceiverComponent;
    int mMediaIntent;
    int mRccId;
  }
  int mHasRemotePlayback;
  int mMainRemoteIsActive;
  int mMainRemote;
  class RemotePlaybackState {
    int mVolumeHandling;
    int mVolumeMax;
    int mVolume;
    int mRccId;
  }
  int sLastRccId;
  class RcClientDeathHandler {
    int mMediaIntent;
    int mCb;
  }
  int mCurrentRcClientGen;
  int RC_INFO_ALL;
  int RC_INFO_NONE;
  int mCurrentRcClient;
  int mCurrentRcLock;
  int mKeyEventDone;
  int EXTRA_WAKELOCK_ACQUIRED;
  int WAKELOCK_RELEASE_ON_FINISHED;
  int mMediaEventWakeLock;
  int mVoiceButtonHandled;
  int mVoiceButtonDown;
  int mVoiceEventLock;
  int VOICEBUTTON_ACTION_SIMULATE_KEY_PRESS;
  int VOICEBUTTON_ACTION_START_VOICE_INPUT;
  int VOICEBUTTON_ACTION_DISCARD_CURRENT_KEY_PRESS;
  class AudioFocusDeathHandler {
    int mCb;
  }
  int mFocusStack;
  class FocusStackEntry {
    int mCallingUid;
    int mPackageName;
    int mHandler;
    int mFocusChangeType;
    int mClientId;
    int mSourceRef;
    int mFocusDispatcher;
    int mStreamType;
  }
  int mPhoneStateListener;
  int mRingingLock;
  int mAudioFocusLock;
  int IN_VOICE_COMM_FOCUS_ID;
  class AudioServiceBroadcastReceiver {
  }
  int mDockAddress;
  int mBecomingNoisyIntentDevices;
  class SettingsObserver {
  }
  class AudioHandler {
  }
  class AudioSystemThread {
  }
  class VolumeStreamState {
    class VolumeDeathHandler {
      int mMuteCount;
      int mICallback;
    }
    int mDeathHandlers;
    int mLastAudibleIndex;
    int mIndex;
    int mIndexMax;
    int mLastAudibleVolumeIndexSettingName;
    int mVolumeIndexSettingName;
    int mStreamType;
  }
  int mBluetoothProfileServiceListener;
  class ScoClient {
    int mStartcount;
    int mCreatorPid;
    int mCb;
  }
  class SoundPoolCallback {
    int mLastSample;
    int mStatus;
  }
  class SoundPoolListenerThread {
  }
  class SetModeDeathHandler {
    int mMode;
    int mPid;
    int mCb;
  }
  class ForceControlStreamClient {
    int mCb;
  }
  int STREAM_REMOTE_MUSIC;
  int mRoutesObservers;
  int mCurAudioRoutes;
  int mBluetoothA2dpEnabledLock;
  int mBluetoothA2dpEnabled;
  int mDeviceOrientation;
  int mRingtonePlayer;
  int mForceControlStreamClient;
  int mForceControlStreamLock;
  int mVolumeControlStream;
  int mKeyguardManager;
  int mPrevVolDirection;
  int NOTIFICATION_VOLUME_DELAY_MS;
  int SOUND_EFFECT_VOLUME_DB;
  int mSoundPoolLooper;
  int mSoundPoolListenerThread;
  int mSoundPoolCallBack;
  int mBootCompleted;
  int mScoConnectionState;
  int SCO_STATE_DEACTIVATE_EXT_REQ;
  int SCO_STATE_ACTIVE_EXTERNAL;
  int SCO_STATE_DEACTIVATE_REQ;
  int SCO_STATE_ACTIVE_INTERNAL;
  int SCO_STATE_ACTIVATE_REQ;
  int SCO_STATE_INACTIVE;
  int mScoAudioState;
  int mBluetoothHeadsetDevice;
  int mBluetoothHeadset;
  int mScoClients;
  int mSetModeDeathHandlers;
  int mMasterVolumeRamp;
  int mUseMasterVolume;
  int mForcedUseForComm;
  int mConnectedDevices;
  int mIsRinging;
  int mReceiver;
  int mHasVibrator;
  int mVibrateSetting;
  int mMuteAffectedStreams;
  int mRingerModeMutedStreams;
  int mRingerModeAffectedStreams;
  int mRingerMode;
  int mAudioSystemCallback;
  int STREAM_NAMES;
  int mStreamVolumeAlias;
  int STREAM_VOLUME_ALIAS_NON_VOICE;
  int STREAM_VOLUME_ALIAS;
  int MAX_STREAM_VOLUME;
  int SOUND_EFFECT_FILES_MAP;
  int SOUND_EFFECT_FILES;
  int SOUND_EFFECTS_PATH;
  int MAX_BATCH_VOLUME_ADJUST_STEPS;
  int MAX_MASTER_VOLUME;
  int NUM_SOUNDPOOL_CHANNELS;
  int mSoundEffectsLock;
  int mSoundPool;
  int mMediaServerOk;
  int mSettingsLock;
  int mMode;
  int mSettingsObserver;
  int mStreamStates;
  int mAudioHandler;
  int mAudioSystemThread;
  int BT_HEADSET_CNCT_TIMEOUT_MS;
  int BTA2DP_DOCK_TIMEOUT_MILLIS;
  int PERSIST_LAST_AUDIBLE;
  int PERSIST_CURRENT;
  int MSG_SET_A2DP_CONNECTION_STATE;
  int MSG_SET_WIRED_DEVICE_CONNECTION_STATE;
  int MSG_RCC_NEW_VOLUME_OBS;
  int MSG_RCC_NEW_PLAYBACK_INFO;
  int MSG_REEVALUATE_REMOTE;
  int MSG_REPORT_NEW_ROUTES;
  int MSG_PERSIST_MASTER_VOLUME_MUTE;
  int MSG_SET_ALL_VOLUMES;
  int MSG_RCDISPLAY_UPDATE;
  int MSG_RCDISPLAY_CLEAR;
  int MSG_BT_HEADSET_CNCT_FAILED;
  int MSG_PERSIST_MEDIABUTTONRECEIVER;
  int MSG_SET_FORCE_USE;
  int MSG_LOAD_SOUND_EFFECTS;
  int MSG_BTA2DP_DOCK_TIMEOUT;
  int MSG_PLAY_SOUND_EFFECT;
  int MSG_MEDIA_SERVER_STARTED;
  int MSG_MEDIA_SERVER_DIED;
  int MSG_PERSIST_RINGER_MODE;
  int MSG_PERSIST_MASTER_VOLUME;
  int MSG_PERSIST_VOLUME;
  int MSG_SET_DEVICE_VOLUME;
  int SENDMSG_QUEUE;
  int SENDMSG_NOOP;
  int SENDMSG_REPLACE;
  int mVolumePanel;
  int mVoiceCapable;
  int mContentResolver;
  int mContext;
  int PERSIST_DELAY;
  int DEBUG_VOL;
  int DEBUG_RC;
  int TAG;
}
class AudioRoutesInfo {
  int CREATOR;
  int mMainType;
  int mBluetoothName;
  int MAIN_HDMI;
  int MAIN_DOCK_SPEAKERS;
  int MAIN_HEADPHONES;
  int MAIN_HEADSET;
  int MAIN_SPEAKER;
}
class AudioRecord {
  class NativeEventHandler {
    int mAudioRecord;
  }
  class OnRecordPositionUpdateListener {
  }
  int mSessionId;
  int mNativeBufferSizeInBytes;
  int mInitializationLooper;
  int mEventHandler;
  int mPositionListenerLock;
  int mPositionListener;
  int mRecordingStateLock;
  int mRecordingState;
  int mState;
  int mRecordSource;
  int mAudioFormat;
  int mChannelConfiguration;
  int mChannels;
  int mChannelCount;
  int mSampleRate;
  int mNativeCallbackCookie;
  int mNativeRecorderInJavaObj;
  int TAG;
  int NATIVE_EVENT_NEW_POS;
  int NATIVE_EVENT_MARKER;
  int AUDIORECORD_ERROR_SETUP_NATIVEINITFAILED;
  int AUDIORECORD_ERROR_SETUP_INVALIDSOURCE;
  int AUDIORECORD_ERROR_SETUP_INVALIDFORMAT;
  int AUDIORECORD_ERROR_SETUP_INVALIDCHANNELMASK;
  int AUDIORECORD_ERROR_SETUP_ZEROFRAMECOUNT;
  int ERROR_INVALID_OPERATION;
  int ERROR_BAD_VALUE;
  int ERROR;
  int SUCCESS;
  int RECORDSTATE_RECORDING;
  int RECORDSTATE_STOPPED;
  int STATE_INITIALIZED;
  int STATE_UNINITIALIZED;
}
class AudioManager {
  int DEVICE_OUT_DEFAULT;
  int DEVICE_OUT_USB_DEVICE;
  int DEVICE_OUT_USB_ACCESSORY;
  int DEVICE_OUT_DGTL_DOCK_HEADSET;
  int DEVICE_OUT_ANLG_DOCK_HEADSET;
  int DEVICE_OUT_AUX_DIGITAL;
  int DEVICE_OUT_BLUETOOTH_A2DP_SPEAKER;
  int DEVICE_OUT_BLUETOOTH_A2DP_HEADPHONES;
  int DEVICE_OUT_BLUETOOTH_A2DP;
  int DEVICE_OUT_BLUETOOTH_SCO_CARKIT;
  int DEVICE_OUT_BLUETOOTH_SCO_HEADSET;
  int DEVICE_OUT_BLUETOOTH_SCO;
  int DEVICE_OUT_WIRED_HEADPHONE;
  int DEVICE_OUT_WIRED_HEADSET;
  int DEVICE_OUT_SPEAKER;
  int DEVICE_OUT_EARPIECE;
  int mICallBack;
  int EXTRA_REMOTE_CONTROL_CLIENT_INFO_CHANGED;
  int EXTRA_REMOTE_CONTROL_EVENT_RECEIVER;
  int EXTRA_REMOTE_CONTROL_CLIENT_NAME;
  int EXTRA_REMOTE_CONTROL_CLIENT_GENERATION;
  int REMOTE_CONTROL_CLIENT_CHANGED;
  int AUDIOFOCUS_REQUEST_GRANTED;
  int AUDIOFOCUS_REQUEST_FAILED;
  int mAudioFocusDispatcher;
  class FocusEventHandlerDelegate {
    int mHandler;
  }
  int mAudioFocusEventHandlerDelegate;
  int mFocusListenerLock;
  int mAudioFocusIdListenerMap;
  class OnAudioFocusChangeListener {
  }
  int AUDIOFOCUS_LOSS_TRANSIENT_CAN_DUCK;
  int AUDIOFOCUS_LOSS_TRANSIENT;
  int AUDIOFOCUS_LOSS;
  int AUDIOFOCUS_GAIN_TRANSIENT_MAY_DUCK;
  int AUDIOFOCUS_GAIN_TRANSIENT;
  int AUDIOFOCUS_GAIN;
  int NUM_SOUND_EFFECTS;
  int FX_KEYPRESS_RETURN;
  int FX_KEYPRESS_DELETE;
  int FX_KEYPRESS_SPACEBAR;
  int FX_KEYPRESS_STANDARD;
  int FX_FOCUS_NAVIGATION_RIGHT;
  int FX_FOCUS_NAVIGATION_LEFT;
  int FX_FOCUS_NAVIGATION_DOWN;
  int FX_FOCUS_NAVIGATION_UP;
  int FX_KEY_CLICK;
  int ROUTE_ALL;
  int ROUTE_BLUETOOTH_A2DP;
  int ROUTE_HEADSET;
  int ROUTE_BLUETOOTH_SCO;
  int ROUTE_BLUETOOTH;
  int ROUTE_SPEAKER;
  int ROUTE_EARPIECE;
  int MODE_IN_COMMUNICATION;
  int MODE_IN_CALL;
  int MODE_RINGTONE;
  int MODE_NORMAL;
  int MODE_CURRENT;
  int MODE_INVALID;
  int SCO_AUDIO_STATE_ERROR;
  int SCO_AUDIO_STATE_CONNECTING;
  int SCO_AUDIO_STATE_CONNECTED;
  int SCO_AUDIO_STATE_DISCONNECTED;
  int EXTRA_SCO_AUDIO_PREVIOUS_STATE;
  int EXTRA_SCO_AUDIO_STATE;
  int ACTION_SCO_AUDIO_STATE_UPDATED;
  int ACTION_SCO_AUDIO_STATE_CHANGED;
  int sService;
  int USE_DEFAULT_STREAM_TYPE;
  int VIBRATE_SETTING_ONLY_SILENT;
  int VIBRATE_SETTING_ON;
  int VIBRATE_SETTING_OFF;
  int VIBRATE_TYPE_NOTIFICATION;
  int VIBRATE_TYPE_RINGER;
  int RINGER_MODE_MAX;
  int RINGER_MODE_NORMAL;
  int RINGER_MODE_VIBRATE;
  int RINGER_MODE_SILENT;
  int FLAG_VIBRATE;
  int FLAG_REMOVE_SOUND_AND_VIBRATE;
  int FLAG_PLAY_SOUND;
  int FLAG_ALLOW_RINGER_MODES;
  int FLAG_SHOW_UI;
  int ADJUST_SAME;
  int ADJUST_LOWER;
  int ADJUST_RAISE;
  int DEFAULT_STREAM_VOLUME;
  int NUM_STREAMS;
  int STREAM_TTS;
  int STREAM_DTMF;
  int STREAM_SYSTEM_ENFORCED;
  int STREAM_BLUETOOTH_SCO;
  int STREAM_NOTIFICATION;
  int STREAM_ALARM;
  int STREAM_MUSIC;
  int STREAM_RING;
  int STREAM_SYSTEM;
  int STREAM_VOICE_CALL;
  int EXTRA_MASTER_VOLUME_MUTED;
  int EXTRA_PREV_MASTER_VOLUME_VALUE;
  int EXTRA_MASTER_VOLUME_VALUE;
  int EXTRA_PREV_VOLUME_STREAM_VALUE;
  int EXTRA_VOLUME_STREAM_VALUE;
  int EXTRA_VOLUME_STREAM_TYPE;
  int EXTRA_VIBRATE_TYPE;
  int EXTRA_VIBRATE_SETTING;
  int MASTER_MUTE_CHANGED_ACTION;
  int MASTER_VOLUME_CHANGED_ACTION;
  int VOLUME_CHANGED_ACTION;
  int VIBRATE_SETTING_CHANGED_ACTION;
  int EXTRA_RINGER_MODE;
  int RINGER_MODE_CHANGED_ACTION;
  int ACTION_AUDIO_BECOMING_NOISY;
  int TAG;
  int mUseMasterVolume;
  int mVolumeKeyUpTime;
  int mContext;
}
class AudioFormat {
  int CHANNEL_IN_STEREO;
  int CHANNEL_IN_MONO;
  int CHANNEL_IN_VOICE_DNLINK;
  int CHANNEL_IN_VOICE_UPLINK;
  int CHANNEL_IN_Z_AXIS;
  int CHANNEL_IN_Y_AXIS;
  int CHANNEL_IN_X_AXIS;
  int CHANNEL_IN_PRESSURE;
  int CHANNEL_IN_BACK_PROCESSED;
  int CHANNEL_IN_FRONT_PROCESSED;
  int CHANNEL_IN_RIGHT_PROCESSED;
  int CHANNEL_IN_LEFT_PROCESSED;
  int CHANNEL_IN_BACK;
  int CHANNEL_IN_FRONT;
  int CHANNEL_IN_RIGHT;
  int CHANNEL_IN_LEFT;
  int CHANNEL_IN_DEFAULT;
  int CHANNEL_OUT_7POINT1_SURROUND;
  int CHANNEL_OUT_7POINT1;
  int CHANNEL_OUT_5POINT1;
  int CHANNEL_OUT_SURROUND;
  int CHANNEL_OUT_QUAD;
  int CHANNEL_OUT_STEREO;
  int CHANNEL_OUT_MONO;
  int CHANNEL_OUT_TOP_BACK_RIGHT;
  int CHANNEL_OUT_TOP_BACK_CENTER;
  int CHANNEL_OUT_TOP_BACK_LEFT;
  int CHANNEL_OUT_TOP_FRONT_RIGHT;
  int CHANNEL_OUT_TOP_FRONT_CENTER;
  int CHANNEL_OUT_TOP_FRONT_LEFT;
  int CHANNEL_OUT_TOP_CENTER;
  int CHANNEL_OUT_SIDE_RIGHT;
  int CHANNEL_OUT_SIDE_LEFT;
  int CHANNEL_OUT_BACK_CENTER;
  int CHANNEL_OUT_FRONT_RIGHT_OF_CENTER;
  int CHANNEL_OUT_FRONT_LEFT_OF_CENTER;
  int CHANNEL_OUT_BACK_RIGHT;
  int CHANNEL_OUT_BACK_LEFT;
  int CHANNEL_OUT_LOW_FREQUENCY;
  int CHANNEL_OUT_FRONT_CENTER;
  int CHANNEL_OUT_FRONT_RIGHT;
  int CHANNEL_OUT_FRONT_LEFT;
  int CHANNEL_OUT_DEFAULT;
  int CHANNEL_INVALID;
  int CHANNEL_CONFIGURATION_STEREO;
  int CHANNEL_CONFIGURATION_MONO;
  int CHANNEL_CONFIGURATION_DEFAULT;
  int CHANNEL_CONFIGURATION_INVALID;
  int ENCODING_PCM_8BIT;
  int ENCODING_PCM_16BIT;
  int ENCODING_DEFAULT;
  int ENCODING_INVALID;
}
class AsyncPlayer {
  int mState;
  int mWakeLock;
  int mPlayer;
  int mThread;
  int mTag;
  class Thread {
  }
  int mCmdQueue;
  class Command {
    int requestTime;
    int stream;
    int looping;
    int uri;
    int context;
    int code;
  }
  int mDebug;
  int STOP;
  int PLAY;
}
class AmrInputStream {
  int mOneByte;
  int mBufOut;
  int mBufIn;
  int mBuf;
  int mGae;
  int mInputStream;
  int SAMPLES_PER_FRAME;
  int TAG;
}
