#include <stdio.h>
#include <unistd.h>

#include <Foundation/Foundation.h>
#include <CoreAudio/CoreAudio.h>

#include <ruby.h>

#include <AudioToolbox/AudioToolbox.h>

#include "coreaudio.h"


VALUE rb_cAudioFile;

static VALUE sym_read, sym_write, sym_format;
static VALUE sym_rate, sym_file_rate, sym_channels, sym_file_channels;
static VALUE sym_wav, sym_m4a;

static void
setASBD(AudioStreamBasicDescription *asbd,
        Float64 rate,
        UInt32 format,
        UInt32 flags,
        UInt32 channels,
        UInt32 bitsPerChannel,
        UInt32 framePerPacket)
{
    asbd->mSampleRate = rate;
    asbd->mFormatID = format;
    asbd->mFormatFlags = flags;
    asbd->mBitsPerChannel = bitsPerChannel;
    asbd->mChannelsPerFrame = channels;
    asbd->mFramesPerPacket = framePerPacket;
    asbd->mBytesPerFrame = bitsPerChannel/8*channels;
    asbd->mBytesPerPacket = bitsPerChannel/8*channels*framePerPacket;
}

typedef struct {
  AudioStreamBasicDescription file_desc;
  AudioStreamBasicDescription inner_desc;
  Boolean                     for_write;
  ExtAudioFileRef             file;
} ca_audio_file_t;

static void
ca_audio_file_free(void *ptr)
{
    ca_audio_file_t *data = ptr;

    if ( data->file ) {
      ExtAudioFileDispose(data->file);
      data->file = NULL;
    }
}

static size_t
ca_audio_file_memsize(const void *ptr)
{
    (void)ptr;
    return sizeof(ca_audio_file_t);
}

static const rb_data_type_t ca_audio_file_type = {
  "ca_audio_file",
  {NULL, ca_audio_file_free, ca_audio_file_memsize}
};

static VALUE
ca_audio_file_alloc(VALUE klass)
{
    VALUE obj;
    ca_audio_file_t *ptr;

    obj = TypedData_Make_Struct(klass, ca_audio_file_t, &ca_audio_file_type, ptr);

    return obj;
}

static void
parse_audio_file_options(VALUE opt, Boolean for_write,
                         Float64 *rate, Float64 *file_rate,
                         UInt32 *channels, UInt32 *file_channels)
{
    if (NIL_P(opt) || NIL_P(rb_hash_aref(opt, sym_rate))) {
      if (for_write)
        *rate = 44100.0;
      else
        *rate = *file_rate;
    } else {
      *rate = NUM2DBL(rb_hash_aref(opt, sym_rate));
    }
    if (NIL_P(opt) || NIL_P(rb_hash_aref(opt, sym_channels))) {
      if (for_write)
        *channels = 2;
      else
        *channels = *file_channels;
    } else {
      *channels = NUM2UINT(rb_hash_aref(opt, sym_channels));
    }

    if (for_write) {
      if (NIL_P(opt) || NIL_P(rb_hash_aref(opt, sym_file_rate))) {
        *file_rate = *rate;
      } else {
        *file_rate = NUM2DBL(rb_hash_aref(opt, sym_file_rate));
      }
      if (NIL_P(opt) || NIL_P(rb_hash_aref(opt, sym_file_channels))) {
        *file_channels = *channels;
      } else {
        *file_channels = NUM2UINT(rb_hash_aref(opt, sym_file_channels));
      }
    }
}

/*
 * call-seq:
 *   AudioFile.new(filepath, mode, opt)
 *
 * open audio file at +filepath+. +mode+ should be :read or :write
 * corresponding to file mode argument of Kernel#open.
 * +opt+ must contain :format key.
 *
 * 'client data' means audio data pass to AudioFile#write or from
 * AudioFile#read method.
 *
 * :format :: audio file format. currently audio file format and
 *            codec type is hardcoded. (:wav, :m4a)
 * :rate :: sample rate of data pass from AudioFile#read or to AudioFile#write
 *          If not specified, :file_rate value is used. (Float)
 * :channels :: number of channels
 * :file_rate :: file data sample rate. only work when open for output. (Float)
 * :file_channels :: file data number of channels. only work when open for
 *                  output.
 */
static VALUE
ca_audio_file_initialize(int argc, VALUE *argv, VALUE self)
{
    ca_audio_file_t *data;
    VALUE path, mode, opt, format;
    Float64 rate, file_rate;
    UInt32 channels, file_channels;
    CFURLRef url = NULL;
    AudioFileTypeID filetype = 0;
    OSStatus err = noErr;

    TypedData_Get_Struct(self, ca_audio_file_t, &ca_audio_file_type, data);

    rb_scan_args(argc, argv, "12", &path, &mode, &opt);

    StringValue(path);

    /* check mode */
    if (NIL_P(mode) || mode == sym_read)
      data->for_write = FALSE;
    else if (mode == sym_write)
      data->for_write = TRUE;
    else
      rb_raise(rb_eArgError, "coreaudio: mode should be :read or :write");

    if (data->for_write) {
      /* when open for write, parse options before open ExtAudioFile */
      parse_audio_file_options(opt, data->for_write, &rate, &file_rate,
                               &channels, &file_channels);

      format = rb_hash_aref(opt, sym_format);
      if (NIL_P(format))
        rb_raise(rb_eArgError, "coreaudio: :format option must be specified");

      if (format == sym_wav) {
        filetype = kAudioFileWAVEType;
        setASBD(&data->file_desc, file_rate, kAudioFormatLinearPCM,
                kLinearPCMFormatFlagIsSignedInteger |
                kAudioFormatFlagIsPacked,
                file_channels, 16, 1);
      } else if (format == sym_m4a) {
        filetype = kAudioFileM4AType;
        setASBD(&data->file_desc, file_rate, kAudioFormatMPEG4AAC,
                0, file_channels, 0, 0);
      } else {
        volatile VALUE str = rb_inspect(format);
        RB_GC_GUARD(str);
        rb_raise(rb_eArgError, "coreaudio: unsupported format (%s)",
                 RSTRING_PTR(str));
      }
    }

    /* create URL represent the target filepath */
    url = CFURLCreateFromFileSystemRepresentation(
                NULL, StringValueCStr(path), (CFIndex)RSTRING_LEN(path), FALSE);

    /* open ExtAudioFile */
    if (data->for_write)
      err = ExtAudioFileCreateWithURL(url, filetype, &data->file_desc,
                                      NULL, kAudioFileFlags_EraseFile,
                                      &data->file);
    else
      err = ExtAudioFileOpenURL(url, &data->file);
    CFRelease(url);
    url = NULL;
    if (err != noErr) {
      rb_raise(rb_eArgError,
               "coreaudio: fail to open ExtAudioFile: %d", (int)err);
    }

    /* get Audio Stream Basic Description (ASBD) from input file */
    if (!data->for_write) {
      UInt32 size = sizeof(data->file_desc);
      err = ExtAudioFileGetProperty(data->file,
                                    kExtAudioFileProperty_FileDataFormat,
                                    &size, &data->file_desc);
      if (err != noErr)
        rb_raise(rb_eRuntimeError,
                 "coreaudio: fail to Get ExtAudioFile Property %d", err);

      /* parse options */
      file_rate = data->file_desc.mSampleRate;
      file_channels = data->file_desc.mChannelsPerFrame;
      parse_audio_file_options(opt, data->for_write, &rate, &file_rate,
                               &channels, &file_channels);
    }

    setASBD(&data->inner_desc, rate, kAudioFormatLinearPCM,
            kLinearPCMFormatFlagIsSignedInteger | kAudioFormatFlagIsPacked,
            channels, 16, 1);

    err = ExtAudioFileSetProperty(
            data->file, kExtAudioFileProperty_ClientDataFormat,
            sizeof(data->inner_desc), &data->inner_desc);
    if (err != noErr) {
      ExtAudioFileDispose(data->file);
      data->file = NULL;
      rb_raise(rb_eArgError, "coreaudio: fail to set client data format: %d",
               (int)err);
    }

    return self;
}

static VALUE
ca_audio_file_close(VALUE self)
{
    ca_audio_file_t *data;

    TypedData_Get_Struct(self, ca_audio_file_t, &ca_audio_file_type, data);
    if (data->file) {
      ExtAudioFileDispose(data->file);
      data->file = NULL;
    }

    return self;
}

static VALUE
ca_audio_file_write(VALUE self, VALUE data)
{
    ca_audio_file_t *file;
    UInt32 n_ch;
    int rank;
    short *buf = NULL;
    AudioBufferList buf_list;
    UInt32 frames;
    size_t alloc_size;
    volatile VALUE tmpstr = Qundef;
    OSStatus err = noErr;

    /* cast to NArray of SINT and check rank of NArray */
    data = na_cast_object(data, NA_SINT);
    rank = NA_RANK(data);
    if (rank > 3)
      rb_raise(rb_eArgError, "coreaudio: audio buffer rank must be 1 or 2.");

    TypedData_Get_Struct(self, ca_audio_file_t, &ca_audio_file_type, file);

    if (file->file == NULL)
      rb_raise(rb_eIOError, "coreaudio: already closed file");

    if (!file->for_write)
      rb_raise(rb_eRuntimeError, "coreaudio: audio file opened for reading");

    n_ch = file->inner_desc.mChannelsPerFrame;

    if (rank == 2 && NA_SHAPE0(data) != (int)n_ch)
      rb_raise(rb_eArgError,
               "coreaudio: audio buffer size of first dimension must be "
               "equal to number of channels");

    frames = rank == 1 ? NA_SHAPE0(data) : NA_SHAPE1(data);
    alloc_size = (file->inner_desc.mBitsPerChannel/8) * frames * n_ch;

    /* prepare interleaved audio buffer */
    buf_list.mNumberBuffers = 1;
    buf_list.mBuffers[0].mNumberChannels = n_ch;
    buf_list.mBuffers[0].mDataByteSize = (UInt32)alloc_size;

    if ((rank == 1 && n_ch == 1) || rank == 2) {
      /* no need to allocate buffer. NArray buffer can be used as mData */
      buf_list.mBuffers[0].mData = NA_PTR_TYPE(data, void *);
    } else {
      UInt32 i, j;
      short *na_buf = NA_PTR_TYPE(data, short *);

      buf_list.mBuffers[0].mData = rb_alloc_tmp_buffer(&tmpstr, alloc_size);
      buf = buf_list.mBuffers[0].mData;

      for (i = 0; i < frames; i++) {
        for (j = 0; j < n_ch; j++) {
          buf[i*n_ch+j] = na_buf[i];
        }
      }
    }

    err = ExtAudioFileWrite(file->file, frames, &buf_list);

    if (tmpstr != Qundef)
      rb_free_tmp_buffer(&tmpstr);

    if (err != noErr) {
      rb_raise(rb_eRuntimeError,
               "coreaudio: ExtAudioFileWrite() fails: %d", (int)err);
    }

    return self;
}

static VALUE
ca_audio_file_read_frames(VALUE self, VALUE frame_val)
{
    ca_audio_file_t *file;
    UInt32 channels, frames, read_frames;
    AudioBufferList buf_list;
    size_t alloc_size;
    VALUE nary;
    int shape[2];
    OSStatus err = noErr;

    frames = NUM2UINT(frame_val);

    TypedData_Get_Struct(self, ca_audio_file_t, &ca_audio_file_type, file);

    if (file->file == NULL)
      rb_raise(rb_eIOError, "coreaudio: already closend file");

    if (file->for_write)
      rb_raise(rb_eRuntimeError, "coreaudio: audio file open for writing");

    channels = file->inner_desc.mChannelsPerFrame;

    shape[0] = channels;
    shape[1] = frames;
    nary = na_make_object(NA_SINT, 2, shape, cNArray);

    alloc_size = (file->inner_desc.mBitsPerChannel/8) * channels * frames;

    /* prepare interleaved audio buffer */
    buf_list.mNumberBuffers = 1;
    buf_list.mBuffers[0].mNumberChannels = channels;
    buf_list.mBuffers[0].mDataByteSize = (UInt32)alloc_size;
    buf_list.mBuffers[0].mData = NA_PTR_TYPE(nary, void *);

    read_frames = frames;
    err = ExtAudioFileRead(file->file, &read_frames, &buf_list);

    if (err != noErr) {
      rb_raise(rb_eRuntimeError,
               "coreaudio: ExtAudioFileRead() fails: %d", (int)err);
    }

    if (read_frames == 0)
      return Qnil;

    NA_SHAPE1(nary) = read_frames;

    return nary;
}

static VALUE
ca_audio_file_rate(VALUE self)
{
    ca_audio_file_t *data;

    TypedData_Get_Struct(self, ca_audio_file_t, &ca_audio_file_type, data);

    return DBL2NUM(data->file_desc.mSampleRate);
}

static VALUE
ca_audio_file_channels(VALUE self)
{
    ca_audio_file_t *data;

    TypedData_Get_Struct(self, ca_audio_file_t, &ca_audio_file_type, data);

    return UINT2NUM((unsigned int)data->file_desc.mChannelsPerFrame);
}

static VALUE
ca_audio_file_inner_rate(VALUE self)
{
    ca_audio_file_t *data;

    TypedData_Get_Struct(self, ca_audio_file_t, &ca_audio_file_type, data);

    return DBL2NUM(data->inner_desc.mSampleRate);
}

static VALUE
ca_audio_file_inner_channels(VALUE self)
{
    ca_audio_file_t *data;

    TypedData_Get_Struct(self, ca_audio_file_t, &ca_audio_file_type, data);

    return UINT2NUM((unsigned int)data->inner_desc.mChannelsPerFrame);
}

void
Init_coreaudio_audiofile(void)
{
    sym_read = ID2SYM(rb_intern("read"));
    sym_write = ID2SYM(rb_intern("write"));
    sym_format = ID2SYM(rb_intern("format"));
    sym_rate = ID2SYM(rb_intern("rate"));
    sym_file_rate = ID2SYM(rb_intern("file_rate"));
    sym_channels = ID2SYM(rb_intern("channels"));
    sym_file_channels = ID2SYM(rb_intern("file_channels"));
    sym_wav = ID2SYM(rb_intern("wav"));
    sym_m4a = ID2SYM(rb_intern("m4a"));

    rb_cAudioFile = rb_define_class_under(rb_mCoreAudio, "AudioFile",
                                          rb_cObject);

    rb_define_alloc_func(rb_cAudioFile, ca_audio_file_alloc);
    rb_define_method(rb_cAudioFile, "initialize", ca_audio_file_initialize, -1);
    rb_define_method(rb_cAudioFile, "close", ca_audio_file_close, 0);
    rb_define_method(rb_cAudioFile, "write", ca_audio_file_write, 1);
    rb_define_method(rb_cAudioFile, "read_frames", ca_audio_file_read_frames, 1);
    rb_define_method(rb_cAudioFile, "rate", ca_audio_file_rate, 0);
    rb_define_method(rb_cAudioFile, "channels", ca_audio_file_channels, 0);
    rb_define_method(rb_cAudioFile, "inner_rate", ca_audio_file_inner_rate, 0);
    rb_define_method(rb_cAudioFile, "inner_channels", ca_audio_file_inner_channels, 0);
}
