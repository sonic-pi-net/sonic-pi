#include <stdio.h>
#include <unistd.h>

#include <pthread.h>

#include <Foundation/Foundation.h>
#include <CoreAudio/CoreAudio.h>

#include <ruby.h>

#include "coreaudio.h"

VALUE rb_mCoreAudio;

static VALUE rb_cAudioDevice;
static VALUE rb_cAudioStream;
static VALUE rb_cOutLoop;
static VALUE rb_cAudioBuffer;
static VALUE rb_cOutputBuffer;
static VALUE rb_cInputBuffer;
static ID sym_iv_devid;
static ID sym_iv_name;
static ID sym_iv_available_sample_rate;
static ID sym_iv_nominal_rate;
static ID sym_iv_input_stream;
static ID sym_iv_output_stream;
static ID sym_iv_channels;
static ID sym_iv_buffer_frame_size;

/* utility macro */
#define PropertyAddress { \
  .mScope = kAudioObjectPropertyScopeGlobal,    \
  .mElement = kAudioObjectPropertyElementMaster \
}

/*--- CoreAudio::AudioStream ---*/
static VALUE
ca_get_stream_channel_num(AudioDeviceID devID,
                          AudioObjectPropertyScope scope)
{
    AudioObjectPropertyAddress address = PropertyAddress;
    UInt32 size;
    AudioChannelLayout *layout;
    OSStatus status;
    UInt32 ch_num;

    address.mSelector = kAudioDevicePropertyPreferredChannelLayout;
    address.mScope = scope;
    if (!AudioObjectHasProperty(devID, &address))
      return INT2NUM(0);

    status = AudioObjectGetPropertyDataSize(devID, &address, 0, NULL, &size);

    if (status != noErr) {
      rb_raise(rb_eArgError,
               "coreaudio: get preferred channel layout size failed: %d", status);
    }

    layout = alloca(size);

    status = AudioObjectGetPropertyData(devID, &address, 0, NULL, &size, layout);
    if (status != noErr) {
      rb_raise(rb_eArgError,
               "coreaudio: get preferred channel layout failed: %d", status);
    }

    if (layout->mChannelLayoutTag == kAudioChannelLayoutTag_UseChannelDescriptions) {
      ch_num = layout->mNumberChannelDescriptions;
    } else if (layout->mChannelLayoutTag == kAudioChannelLayoutTag_UseChannelBitmap) {
      UInt32 i;
      ch_num = 0;
      for ( i = 0; i < sizeof(layout->mChannelBitmap)*8; i++ ) {
        if ( (layout->mChannelBitmap >> i) & 0x01 )
          ch_num++;
      }
    } else {
      ch_num = AudioChannelLayoutTag_GetNumberOfChannels(layout->mChannelLayoutTag);
    }
    return UINT2NUM(ch_num);
}

static VALUE
ca_get_stream_buffer_frame(AudioDeviceID devID, AudioObjectPropertyScope scope)
{
    AudioObjectPropertyAddress address = PropertyAddress;
    UInt32 size, framesize;
    OSStatus status;

    address.mSelector = kAudioDevicePropertyBufferFrameSize;
    address.mScope = scope;

    if (!AudioObjectHasProperty(devID, &address))
      return INT2NUM(0);

    size = sizeof(framesize);
    status = AudioObjectGetPropertyData(devID, &address, 0, NULL, &size, &framesize);
    if (status != noErr) {
      rb_raise(rb_eArgError,
               "coreaudio: get buffer frame size failed: %d", status);
    }
    return UINT2NUM(framesize);
}

static VALUE
ca_stream_initialize(VALUE self, VALUE devid_val, VALUE is_input)
{
    AudioDeviceID devID = (AudioDeviceID)NUM2UINT(devid_val);
    AudioObjectPropertyScope scope;

    if (RTEST(is_input))
      scope = kAudioDevicePropertyScopeInput;
    else
      scope = kAudioDevicePropertyScopeOutput;
    rb_ivar_set(self, sym_iv_channels, ca_get_stream_channel_num(devID, scope));
    rb_ivar_set(self, sym_iv_buffer_frame_size, ca_get_stream_buffer_frame(devID, scope));

    return self;
}

static VALUE
ca_stream_new(VALUE devid, VALUE is_input)
{
    VALUE stream;
    stream = rb_obj_alloc(rb_cAudioStream);
    ca_stream_initialize(stream, devid, is_input);
    return stream;
}

/*--- CoreAudio::AudioDevice ---*/
static VALUE
ca_get_device_name(AudioDeviceID devID)
{
    AudioObjectPropertyAddress address = PropertyAddress;
    UInt32 size;
    OSStatus status;
    CFStringRef deviceName = NULL;
    char cbuf[256];
    VALUE str;

    address.mSelector = kAudioObjectPropertyName;
    size = sizeof(deviceName);

    status = AudioObjectGetPropertyData(devID, &address, 0, NULL, &size, &deviceName);
    if ( status != noErr ) {
      rb_raise(rb_eArgError, "coreaudio: get device name failed: %d", status);
    }
    if (!CFStringGetCString(deviceName, cbuf, (CFIndex)sizeof(cbuf),
                            kCFStringEncodingUTF8)) {
      /* String conversion failed. Ignore an error and return empty String */
      cbuf[0] = '\0';
    }
    CFRelease(deviceName);
    str = rb_str_new2(cbuf);

    return str;
}

static VALUE
ca_get_device_available_sample_rate(AudioDeviceID devID)
{
    AudioObjectPropertyAddress address = PropertyAddress;
    UInt32 size;
    UInt32 n_rates;
    AudioValueRange *sample_rates;
    OSStatus status;
    VALUE ary;
    UInt32 i;

    address.mSelector = kAudioDevicePropertyAvailableNominalSampleRates;
    status = AudioObjectGetPropertyDataSize(devID, &address, 0, NULL, &size);

    if (status != noErr) {
      rb_raise(rb_eArgError,
               "coreaudio: get available sample rates size failed: %d", status);
    }

    n_rates = size / (UInt32)sizeof(AudioValueRange);
    sample_rates = ALLOCA_N(AudioValueRange, n_rates);

    status = AudioObjectGetPropertyData(devID, &address, 0, NULL, &size, sample_rates);
    if (status != noErr) {
      rb_raise(rb_eArgError,
               "coreaudio: get available sample rates failed: %d", status);
    }

    ary = rb_ary_new();
    for ( i = 0; i < n_rates; i++ ) {
      rb_ary_push(ary,
                  rb_ary_new3(2,
                              DBL2NUM((double)sample_rates[i].mMinimum),
                              DBL2NUM((double)sample_rates[i].mMaximum)));
    }

    return ary;
}

static VALUE
ca_set_device_nominal_sample_rate(AudioDeviceID devID, VALUE sampleRateVal)
{
    AudioObjectPropertyAddress address = PropertyAddress;
    UInt32 size;
    OSStatus status;
    Float64 rate = NUM2DBL(sampleRateVal);

    address.mSelector = kAudioDevicePropertyNominalSampleRate;
    status = AudioObjectGetPropertyDataSize(devID, &address, 0, NULL, &size);

    if (status != noErr) {
      rb_raise(rb_eArgError,
               "coreaudio: get nominal sample rates size failed: %d", status);
    }
    status = AudioObjectSetPropertyData(devID, &address, 0, NULL, size, &rate);
    if (status != noErr) {
      rb_raise(rb_eArgError,
               "coreaudio: set nominal sample rates failed: %d", status);
    }
    return sampleRateVal;
}

static VALUE
ca_get_device_nominal_sample_rate(AudioDeviceID devID)
{
    AudioObjectPropertyAddress address = PropertyAddress;
    UInt32 size;
    Float64 rate;
    OSStatus status;

    address.mSelector = kAudioDevicePropertyNominalSampleRate;
    status = AudioObjectGetPropertyDataSize(devID, &address, 0, NULL, &size);

    if (status != noErr) {
      rb_raise(rb_eArgError,
               "coreaudio: get nominal sample rates size failed: %d", status);
    }

    status = AudioObjectGetPropertyData(devID, &address, 0, NULL, &size, &rate);
    if (status != noErr) {
      rb_raise(rb_eArgError,
               "coreaudio: get nominal sample rates failed: %d", status);
    }
    return DBL2NUM((double)rate);
}

static VALUE
ca_get_device_actual_sample_rate(VALUE self)
{
    AudioDeviceID devID = NUM2UINT(rb_ivar_get(self, sym_iv_devid));
    AudioObjectPropertyAddress address = PropertyAddress;
    UInt32 size;
    Float64 rate;
    OSStatus status;

    address.mSelector = kAudioDevicePropertyActualSampleRate;
    status = AudioObjectGetPropertyDataSize(devID, &address, 0, NULL, &size);

    size = sizeof(rate);
    status = AudioObjectGetPropertyData(devID, &address, 0, NULL, &size, &rate);
    if (status != noErr) {
      rb_raise(rb_eArgError,
               "coreaudio: get actual sample rates failed: %d", status);
    }
    return DBL2NUM((double)rate);
}

static VALUE
ca_device_initialize(VALUE self, VALUE devIdVal, VALUE options)
{
    AudioDeviceID devID = (AudioDeviceID)NUM2LONG(devIdVal);
    VALUE device_name;
    VALUE available_sample_rate;
    VALUE nominal_rate;
    VALUE input_stream, output_stream;
    VALUE option_nominal_rate;

    device_name = ca_get_device_name(devID);
    available_sample_rate = ca_get_device_available_sample_rate(devID);
    rb_obj_freeze(available_sample_rate);

    if (options != Qnil) {
      option_nominal_rate = rb_funcall(options, rb_intern("[]"), 1, ID2SYM(rb_intern("nominal_rate")));
      if (option_nominal_rate != Qnil) {
        nominal_rate = ca_set_device_nominal_sample_rate(devID, option_nominal_rate);
      }
    } else {
      nominal_rate = ca_get_device_nominal_sample_rate(devID);
    }

    input_stream = ca_stream_new(devIdVal, Qtrue);
    output_stream = ca_stream_new(devIdVal, Qfalse);

    rb_ivar_set(self, sym_iv_devid, devIdVal);
    rb_ivar_set(self, sym_iv_name, device_name);
    rb_ivar_set(self, sym_iv_available_sample_rate, available_sample_rate);
    rb_ivar_set(self, sym_iv_nominal_rate, nominal_rate);
    rb_ivar_set(self, sym_iv_input_stream, input_stream);
    rb_ivar_set(self, sym_iv_output_stream, output_stream);

    return self;
}

static VALUE
ca_device_new(AudioDeviceID devid, VALUE options)
{
    VALUE devIdVal = UINT2NUM(devid);
    VALUE device;

    device = rb_obj_alloc(rb_cAudioDevice);
    ca_device_initialize(device, devIdVal, options);

    return device;
}

/*
 * Document-method: CoreAudio.devices
 * call-seq:
 *   CoreAudio.devices(options = nil)
 *
 * Get available all audio devices (CoreAudio::AudioDevice object).
 * if options[:nominal_rate] is given, set device nominal rate as given one.
 */
static VALUE
ca_devices(int argc, VALUE *argv, VALUE mod)
{
    AudioObjectPropertyAddress address = PropertyAddress;
    AudioDeviceID *devIDs = NULL;
    UInt32 size = 0, devnum = 0;
    OSStatus status = noErr;
    VALUE ary;
    UInt32 i;
    VALUE options;

    rb_scan_args(argc, argv, "01", &options);

    address.mSelector = kAudioHardwarePropertyDevices;

    status = AudioObjectGetPropertyDataSize(kAudioObjectSystemObject,
                                            &address, 0, NULL, &size);
    if (status != noErr)
      rb_raise(rb_eRuntimeError, "coreaudio: get devices size failed: %d", status);

    devnum = size / (UInt32)sizeof(AudioDeviceID);
    devIDs = ALLOCA_N(AudioDeviceID, devnum);

    status = AudioObjectGetPropertyData(kAudioObjectSystemObject,
                                        &address, 0, NULL, &size, devIDs);
    if (status != noErr)
      rb_raise(rb_eRuntimeError, "coreaudio: get devices failed: %d", status);

    ary = rb_ary_new();
    for (i = 0; i < devnum; i++) {
      rb_ary_push(ary, ca_device_new(devIDs[i], options));
    }
    return ary;
}

/*
 * Document-method: CoreAudio.default_input_device
 * call-seq:
 *   CoreAudio.default_input_device(options = nil)
 *
 * Get system default audio input device as CoreAudio::AudioDevice object.
 * if options[:nominal_rate] is given, set device nominal rate as given one.
 */
static VALUE
ca_default_input_device(int argc, VALUE *argv, VALUE mod)
{
    AudioDeviceID devID;
    AudioObjectPropertyAddress address = PropertyAddress;
    UInt32 size;
    OSStatus status;
    VALUE options;

    rb_scan_args(argc, argv, "01", &options);

    address.mSelector = kAudioHardwarePropertyDefaultInputDevice;
    size = sizeof(devID);

    status = AudioObjectGetPropertyData(kAudioObjectSystemObject,
                                        &address, 0, NULL, &size, &devID);

    if (status != noErr)
      rb_raise(rb_eArgError, "coreaudio: get default input device failed: %d", status);

    return ca_device_new(devID, options);
}


/*
 * Document-method: CoreAudio.default_output_device
 * call-seq:
 *   CoreAudio.default_output_device(options = nil)
 *
 * Get system default audio output device as CoreAudio::AudioDevice object.
 * if options[:nominal_rate] is given, set device nominal rate as given one.
 */
static VALUE
ca_default_output_device(int argc, VALUE *argv, VALUE mod)
{
    AudioDeviceID devID;
    AudioObjectPropertyAddress address = PropertyAddress;
    UInt32 size;
    OSStatus status;
    VALUE options;

    rb_scan_args(argc, argv, "01", &options);

    address.mSelector = kAudioHardwarePropertyDefaultOutputDevice;
    size = sizeof(devID);

    status = AudioObjectGetPropertyData(kAudioObjectSystemObject,
                                        &address, 0, NULL, &size, &devID);

    if (status != noErr)
      rb_raise(rb_eArgError, "coreaudio: get default output device failed: %d", status);

    return ca_device_new(devID, options);
}

/*
 * Document-method: CoreAudio.set_default_output_device
 * call-seq:
 *   CoreAudio.set_default_output_device(audio_device)
 *
 * Set system default audio output device as CoreAudio::AudioDevice object.
 */
static VALUE
ca_set_default_output_device(VALUE self, VALUE device)
{
    AudioDeviceID devID = NUM2UINT(rb_ivar_get(device, sym_iv_devid));    
    AudioObjectPropertyAddress address = PropertyAddress;
    UInt32 size;
    OSStatus status;

    address.mSelector = kAudioHardwarePropertyDefaultOutputDevice;
    address.mScope    = kAudioObjectPropertyScopeGlobal;
    address.mElement  = kAudioObjectPropertyElementMaster;

    status = AudioObjectSetPropertyData(kAudioObjectSystemObject, &address, 0, NULL, sizeof(devID), &devID);
    if (status != noErr) {
      rb_raise(rb_eArgError,
               "coreaudio: set default output deveice failed: %d", status);
    }

    return Qtrue;
}

/*
 * Document-class: CoreAudio::OutLoop
 *
 * CoreAudio::OutLoop is an class for loop waveform to output audio stream.
 */
typedef struct {
  AudioDeviceID       devID;
  AudioDeviceIOProcID procID;
  UInt32              frame;
  UInt32              channels;
  short               *buf;
} ca_out_loop_data;

static void
ca_out_loop_data_free(void *ptr)
{
    if (ptr) {
      ca_out_loop_data *data = ptr;
      if (data->procID)
        AudioDeviceDestroyIOProcID(data->devID, data->procID);
      if (data->buf)
        free(data->buf);
      free(ptr);
    }
}

static size_t
ca_out_loop_data_memsize(const void *ptr)
{
    const ca_out_loop_data *data = ptr;
    return sizeof(ca_out_loop_data) + data->channels * data->frame * sizeof(short);
}

static const rb_data_type_t ca_out_loop_data_type = {
  "ca_out_loop_data",
  {NULL, ca_out_loop_data_free, ca_out_loop_data_memsize},
};

static OSStatus
ca_out_loop_proc(
        AudioDeviceID           inDevice,
        const AudioTimeStamp*   inNow,
        const AudioBufferList*  inInputData,
        const AudioTimeStamp*   inInputTime,
        AudioBufferList*        outOutputData,
        const AudioTimeStamp*   inOutputTime,
        void*                   inClientData)
{
    NSUInteger i;
    UInt32 buffers = outOutputData->mNumberBuffers;
    ca_out_loop_data *loop_data = inClientData;
    UInt32 channels = loop_data->channels;

    for (i = 0; i < buffers; i++) {
      float *ptr = outOutputData->mBuffers[i].mData;
      UInt32 size = outOutputData->mBuffers[i].mDataByteSize / (UInt32)sizeof(float) / channels;
      UInt32 offset = (UInt32)inOutputTime->mSampleTime % loop_data->frame;
      UInt32 copied = 0;

      if (outOutputData->mBuffers[i].mNumberChannels != channels) {
        memset(ptr, 0, size * channels * sizeof(float));
        continue;
      }

      while ( copied < size ) {
        UInt32 len = loop_data->frame - offset;
        UInt32 j;
        if ( len > size - copied )
          len = size - copied;
        for (j = 0; j < len*channels; j++) {
          ptr[copied*channels+j] = SHORT2FLOAT(loop_data->buf[offset*channels+j]);
        }
        offset = (offset + len) % loop_data->frame;
        copied += len;
      }
    }

    return 0;
}

static VALUE
ca_out_loop_data_alloc(VALUE klass)
{
    VALUE obj;
    ca_out_loop_data *ptr;

    obj = TypedData_Make_Struct(klass, ca_out_loop_data, &ca_out_loop_data_type, ptr);
    return obj;
}

static VALUE
ca_out_loop_data_initialize(VALUE self, VALUE devID, VALUE frame, VALUE channels)
{
    ca_out_loop_data *data;
    OSStatus status;

    TypedData_Get_Struct(self, ca_out_loop_data, &ca_out_loop_data_type, data);
    data->devID = NUM2UINT(devID);
    status = AudioDeviceCreateIOProcID(data->devID, ca_out_loop_proc, data, &data->procID);
    if ( status != noErr )
    {
      rb_raise(rb_eRuntimeError, "coreaudio: create proc ID fail: %d", status);
    }
    data->frame = NUM2UINT(frame);
    data->channels = NUM2UINT(channels);
    data->buf = malloc(sizeof(short)*data->frame*data->channels);
    if (data->buf == NULL)
      rb_raise(rb_eNoMemError, "coreaudio: fail to alloc out loop data buffer");
    return self;
}

/*
 * Document-method: CoreAudio::AudioDevice#out_loop
 * call-seq:
 *   device.out_loop(frame)
 *
 * Create output audio loop buffer.
 *
 * == Parameters
 * * +frame+ is an integer value indicate loop buffer size in number of
 *   sample/frame. The number of channel is considered automatically.
 */
static VALUE
ca_device_create_out_loop_proc(VALUE self, VALUE frame)
{
    VALUE proc;
    VALUE out_stream = rb_ivar_get(self, sym_iv_output_stream);

    proc = ca_out_loop_data_alloc(rb_cOutLoop);
    ca_out_loop_data_initialize(proc, rb_ivar_get(self, sym_iv_devid), frame,
                                rb_ivar_get(out_stream, sym_iv_channels));
    return proc;
}

/*
 * call-seq:
 *   outloop.start
 *
 * Start play of output audio loop.
 */
static VALUE
ca_out_loop_data_start(VALUE self)
{
    ca_out_loop_data *data;
    OSStatus status;

    TypedData_Get_Struct(self, ca_out_loop_data, &ca_out_loop_data_type, data);

    status = AudioDeviceStart(data->devID, data->procID);
    if ( status != noErr )
    {
      rb_raise(rb_eRuntimeError, "coreaudio: audio device start fail: %d", status);
    }
    return self;
}

/*
 * call-seq:
 *   outloop.stop
 *
 * Stop play of output audio loop.
 */
static VALUE
ca_out_loop_data_stop(VALUE self)
{
    ca_out_loop_data *data;
    OSStatus status;

    TypedData_Get_Struct(self, ca_out_loop_data, &ca_out_loop_data_type, data);

    status = AudioDeviceStop(data->devID, data->procID);
    if ( status != noErr )
    {
      rb_raise(rb_eRuntimeError, "coreaudio: audio device stop fail: %d", status);
    }
    return self;
}

/*
 * call-seq:
 *   outloop[frame] = sample
 *   outloop[frame] = [sample1, sample2]
 *
 * Assign audio loop buffer frame value.
 * If assigned value is an Fixnum (-32767..32767, signed 16bit),
 * the value is stored to all channels.
 * The +sample+ should be normalize to -32767 <= sample <= 32767 range.
 * If assigned value is an Array of Fixnum, each value is stored to each
 * correponding channel. If size of array is not equal to the AudioDevice's
 * output stream channel number, raise ArgumentError.
 */
static VALUE
ca_out_loop_data_assign(VALUE self, VALUE index, VALUE val)
{
    ca_out_loop_data *data;
    size_t idx;
    UInt32 i;

    TypedData_Get_Struct(self, ca_out_loop_data, &ca_out_loop_data_type, data);

    idx = NUM2UINT(index) % data->frame;
    if (TYPE(val) == T_ARRAY) {
      if (RARRAY_LEN(val) != data->channels) {
        rb_raise(rb_eArgError, "size of array and channel size mismatch");
      }
      for (i = 0; i < data->channels; i++) {
        data->buf[idx*data->channels+i] = (short)NUM2INT(RARRAY_PTR(val)[i]);
      }
    } else {
      for (i = 0; i < data->channels; i++) {
        data->buf[idx*data->channels+i] = (short)NUM2INT(val);
      }
    }
    return val;
}

typedef struct {
  AudioDeviceID       devID;
  AudioDeviceIOProcID procID;
  UInt32              frame;
  UInt32              channels;
  short               *buf;
  UInt32              start;
  UInt32              end;
  long                dropped_frame;
  pthread_mutex_t     mutex;
  pthread_cond_t      cond;
} ca_buffer_data;

static void
ca_buffer_data_free(void *ptr)
{
    if (ptr) {
      ca_buffer_data *data = ptr;
      if (data->procID)
        AudioDeviceDestroyIOProcID(data->devID, data->procID);
      pthread_cond_destroy(&data->cond);
      pthread_mutex_destroy(&data->mutex);
      if (data->buf)
        free(data->buf);
      free(ptr);
    }
}

static size_t
ca_buffer_data_memsize(const void *ptr)
{
    const ca_buffer_data *data = ptr;
    return sizeof(ca_buffer_data) + data->channels * data->frame * sizeof(short);
}

static const rb_data_type_t ca_buffer_data_type = {
  "ca_buffer_data",
  {NULL, ca_buffer_data_free, ca_buffer_data_memsize},
};

static VALUE
ca_buffer_data_alloc(VALUE klass)
{
    VALUE obj;
    ca_buffer_data *ptr;

    obj = TypedData_Make_Struct(klass, ca_buffer_data, &ca_buffer_data_type, ptr);
    pthread_mutex_init(&ptr->mutex, NULL);
    pthread_cond_init(&ptr->cond, NULL);
    return obj;
}

static VALUE
ca_buffer_data_start(VALUE self)
{
    ca_buffer_data *data;
    OSStatus status;

    TypedData_Get_Struct(self, ca_buffer_data, &ca_buffer_data_type, data);

    data->dropped_frame = 0;
    status = AudioDeviceStart(data->devID, data->procID);
    if ( status != noErr )
    {
      rb_raise(rb_eRuntimeError, "coreaudio: audio device start fail: %d", status);
    }
    return self;
}

static VALUE
ca_buffer_data_stop(VALUE self)
{
    ca_buffer_data *data;
    OSStatus status;

    TypedData_Get_Struct(self, ca_buffer_data, &ca_buffer_data_type, data);

    status = AudioDeviceStop(data->devID, data->procID);
    if ( status != noErr )
    {
      rb_raise(rb_eRuntimeError, "coreaudio: audio device stop fail: %d", status);
    }
    return self;
}

static VALUE
ca_buffer_wait(void *ptr)
{
    ca_buffer_data *data = ptr;
    int ret;

    ret = pthread_cond_wait(&data->cond, &data->mutex);

    return (VALUE)ret;
}

#if 0
/* use pthread_mutex_lock in unblocking function cause deadlock.
 * because calling thread have GVL lock and interrupted thread could
 * be waiting mutex lock for GVL.
 * So use RUBY_UBF_IO for unblocking function.
 * Although pthread_cond_wait() shouldn't return EINTR acoording to POSIX,
 * on Mac OS X pthread_cond_wait() actually returns when received signals. */
static void
ca_buffer_unblocking_func(void *ptr)
{
    ca_buffer_data *data = ptr;

    pthread_mutex_lock(&data->mutex);
    pthread_cond_broadcast(&data->cond);
    pthread_mutex_unlock(&data->mutex);
}
#endif

static VALUE
ca_buffer_wait_blocking(VALUE value)
{
    void *ptr = (void *)value;
#if 0
    return rb_thread_call_without_gvl(ca_buffer_wait, ptr,
                                      ca_buffer_unblocking_func, ptr);
#endif
    return rb_thread_call_without_gvl(ca_buffer_wait, ptr,
                                      RUBY_UBF_IO, NULL);
}

static size_t
buffer_space(ca_buffer_data *data)
{
    UInt32 nxt;
    size_t space;

    pthread_mutex_lock(&data->mutex);
    nxt = (data->end+1) % data->frame;
    if ( nxt <= data->start )
      space = (size_t)(data->start - nxt);
    else
      space = (size_t)(data->start + data->frame - nxt);
    pthread_mutex_unlock(&data->mutex);
    return space;
}

static VALUE
ca_buffer_space(VALUE self)
{
    ca_buffer_data *data;
    size_t space;

    TypedData_Get_Struct(self, ca_buffer_data, &ca_buffer_data_type, data);

    space = buffer_space(data);
    return SIZET2NUM(space);
}

static VALUE
ca_buffer_dropped_frame(VALUE self)
{
    ca_buffer_data *data;
    long dropped;

    TypedData_Get_Struct(self, ca_buffer_data, &ca_buffer_data_type, data);

    pthread_mutex_lock(&data->mutex);
    dropped = data->dropped_frame;
    pthread_mutex_unlock(&data->mutex);
    return LONG2NUM(dropped);
}

static VALUE
ca_buffer_reset_dropped_frame(VALUE self)
{
    ca_buffer_data *data;
    long dropped;

    TypedData_Get_Struct(self, ca_buffer_data, &ca_buffer_data_type, data);

    pthread_mutex_lock(&data->mutex);
    dropped = data->dropped_frame;
    data->dropped_frame = 0;
    pthread_mutex_unlock(&data->mutex);
    return LONG2NUM(dropped);
}

/*
 * Document-class: CoreAudio::OutputBuffer
 *
 * CoreAudio::OutputBuffer is a class for stream waveform to output audio stream.
 */
static OSStatus
ca_out_buffer_proc(
        AudioDeviceID           inDevice,
        const AudioTimeStamp*   inNow,
        const AudioBufferList*  inInputData,
        const AudioTimeStamp*   inInputTime,
        AudioBufferList*        outOutputData,
        const AudioTimeStamp*   inOutputTime,
        void*                   inClientData)
{
    NSUInteger n_buf;
    UInt32 buffers = outOutputData->mNumberBuffers;
    ca_buffer_data *buffer_data = inClientData;
    UInt32 channels = buffer_data->channels;

    for (n_buf = 0; n_buf < buffers; n_buf++) {
      float *ptr = outOutputData->mBuffers[n_buf].mData;
      UInt32 size = outOutputData->mBuffers[n_buf].mDataByteSize / (UInt32)sizeof(float) / channels;
      UInt32 copied = 0;
      UInt32 i;

      if (outOutputData->mBuffers[n_buf].mNumberChannels != channels) {
        memset(ptr, 0, size * channels * sizeof(float));
        continue;
      }

      pthread_mutex_lock(&buffer_data->mutex);
      for ( copied = 0, i = buffer_data->start; copied < size && i != buffer_data->end; copied++, i = (i+1) % buffer_data->frame ) {
        UInt32 ch;
        for (ch = 0; ch < channels; ch++) {
          ptr[copied*channels+ch] = SHORT2FLOAT(buffer_data->buf[i*channels+ch]);
        }
      }
      buffer_data->start = i;
      pthread_cond_broadcast(&buffer_data->cond);
      pthread_mutex_unlock(&buffer_data->mutex);
      if ( copied < size ) {
        memset(ptr+(copied*channels), 0, sizeof(float)*channels*(size-copied));
        buffer_data->dropped_frame += size - copied;
      }
    }

    return 0;
}

static VALUE
ca_out_buffer_data_initialize(VALUE self, VALUE devID, VALUE frame, VALUE channels)
{
    ca_buffer_data *data;
    OSStatus status;

    TypedData_Get_Struct(self, ca_buffer_data, &ca_buffer_data_type, data);
    data->devID = NUM2UINT(devID);
    status = AudioDeviceCreateIOProcID(data->devID, ca_out_buffer_proc, data, &data->procID);
    if ( status != noErr )
    {
      rb_raise(rb_eRuntimeError, "coreaudio: create proc ID fail: %d", status);
    }
    data->frame = NUM2UINT(frame);
    data->channels = NUM2UINT(channels);
    data->buf = malloc(sizeof(short)*data->frame*data->channels);
    if (data->buf == NULL)
      rb_raise(rb_eNoMemError, "coreaudio: fail to alloc out buffer data buffer");
    return self;
}

static VALUE
ca_device_create_out_buffer_proc(VALUE self, VALUE frame)
{
    VALUE proc;
    VALUE out_stream = rb_ivar_get(self, sym_iv_output_stream);

    proc = ca_buffer_data_alloc(rb_cOutputBuffer);
    ca_out_buffer_data_initialize(proc, rb_ivar_get(self, sym_iv_devid), frame,
                                  rb_ivar_get(out_stream, sym_iv_channels));
    return proc;
}

static VALUE
ca_out_buffer_data_append(VALUE self, VALUE nary)
{
    ca_buffer_data *data;
    int rank;
    short *buf;
    UInt32 frames;
    UInt32 idx;
    long i;
    UInt32 j;

    TypedData_Get_Struct(self, ca_buffer_data, &ca_buffer_data_type, data);

    nary = na_cast_object(nary, NA_SINT);
    rank = NA_RANK(nary);
    if (rank == 1) {
      frames = NA_SHAPE0(nary);
    } else if (rank == 2) {
      frames = NA_SHAPE1(nary);
      if (NA_SHAPE0(nary) != (int)data->channels)
        rb_raise(rb_eArgError,
                 "coreaudio: audio buffer NArray size of first dim. must be "
                 "number of channels");
    } else {
      rb_raise(rb_eArgError,
               "coreaudio: audio buffer NArray rank must be 1 or 2");
    }
    buf = NA_PTR_TYPE(nary, short *);
    pthread_mutex_lock(&data->mutex);
    idx = (data->end + 1) % data->frame;
    for ( i = 0; i < frames; i++, idx = (idx+1)%data->frame) {
      while ( idx == data->start ) {
        int ret, state;
        data->end = (idx == 0) ? data->frame-1 : idx - 1;
        ret = (int)rb_protect(ca_buffer_wait_blocking, (VALUE)data, &state);
        if (state) {
          pthread_mutex_unlock(&data->mutex);
          rb_jump_tag(state);
        }
        switch(ret) {
          case 0:
          case EINTR:
          case EAGAIN:
            break;
          default:
            pthread_mutex_unlock(&data->mutex);
            rb_sys_fail("pthread_cond_wait");
            break;
        }
      }

      if (rank == 2) {
        memcpy(data->buf + idx * data->channels,
               buf + i * data->channels,
               sizeof(short) * data->channels);
      } else {
        for (j = 0; j < data->channels; j++) {
          data->buf[idx*data->channels+j] = buf[i];
        }
      }
      data->end = idx;
    }
    pthread_mutex_unlock(&data->mutex);
    return self;
}

/*
 * Document-class: CoreAudio::InputBuffer
 *
 * CoreAudio::InputBuffer is a class for stream waveform to input audio stream.
 */
static OSStatus
ca_in_buffer_proc(
        AudioDeviceID           inDevice,
        const AudioTimeStamp*   inNow,
        const AudioBufferList*  inInputData,
        const AudioTimeStamp*   inInputTime,
        AudioBufferList*        outOutputData,
        const AudioTimeStamp*   inOutputTime,
        void*                   inClientData)
{
    NSUInteger n_buf;
    UInt32 buffers = inInputData->mNumberBuffers;
    ca_buffer_data *buffer_data = inClientData;
    UInt32 channels = buffer_data->channels;

    for (n_buf = 0; n_buf < buffers; n_buf++) {
      float *ptr = inInputData->mBuffers[n_buf].mData;
      UInt32 size = inInputData->mBuffers[n_buf].mDataByteSize / (UInt32)sizeof(float) / channels;
      UInt32 copied, idx;

      if (inInputData->mBuffers[n_buf].mNumberChannels != channels) {
        continue;
      }

      pthread_mutex_lock(&buffer_data->mutex);

      copied = 0;
      for (idx = buffer_data->end;
           copied < size && (idx+1) % buffer_data->frame != buffer_data->start;
           copied++, idx = (idx+1) % buffer_data->frame) {
        UInt32 ch;
        for (ch = 0; ch < channels; ch++) {
          buffer_data->buf[idx*channels+ch] = FLOAT2SHORT(ptr[copied*channels+ch]);
        }
      }
      buffer_data->end = idx;
      buffer_data->dropped_frame += size - copied;

      pthread_cond_broadcast(&buffer_data->cond);
      pthread_mutex_unlock(&buffer_data->mutex);
    }

    return 0;
}

static VALUE
ca_in_buffer_data_initialize(VALUE self, VALUE devID, VALUE frame, VALUE channels)
{
    ca_buffer_data *data;
    OSStatus status;

    TypedData_Get_Struct(self, ca_buffer_data, &ca_buffer_data_type, data);
    data->devID = NUM2UINT(devID);
    status = AudioDeviceCreateIOProcID(data->devID, ca_in_buffer_proc, data, &data->procID);
    if ( status != noErr )
    {
      rb_raise(rb_eRuntimeError, "coreaudio: create proc ID fail: %d", status);
    }
    data->frame = NUM2UINT(frame);
    data->channels = NUM2UINT(channels);
    data->buf = malloc(sizeof(short)*data->frame*data->channels);
    if (data->buf == NULL)
      rb_raise(rb_eNoMemError, "coreaudio: fail to alloc input buffer data buffer");
    return self;
}

static VALUE
ca_device_create_in_buffer_proc(VALUE self, VALUE frame)
{
    VALUE proc;
    VALUE in_stream = rb_ivar_get(self, sym_iv_input_stream);

    proc = ca_buffer_data_alloc(rb_cInputBuffer);
    ca_in_buffer_data_initialize(proc, rb_ivar_get(self, sym_iv_devid), frame,
                                 rb_ivar_get(in_stream, sym_iv_channels));
    return proc;
}

static VALUE
ca_in_buffer_data_read(VALUE self, VALUE num)
{
    ca_buffer_data *data;
    UInt32 frame = NUM2UINT(num);
    VALUE nary;
    int shape[2];
    short *buf;
    UInt32 i;

    TypedData_Get_Struct(self, ca_buffer_data, &ca_buffer_data_type, data);

    shape[0] = data->channels;
    shape[1] = frame;
    nary = na_make_object(NA_SINT, 2, shape, cNArray);
    buf = NA_PTR_TYPE(nary, short *);

    pthread_mutex_lock(&data->mutex);
    for ( i = 0; i < frame; i++, data->start = (data->start+1)%data->frame) {
      while (data->start == data->end) {
        int ret, state;
        ret = (int)rb_protect(ca_buffer_wait_blocking, (VALUE)data, &state);
        if (state) {
          pthread_mutex_unlock(&data->mutex);
          rb_jump_tag(state);
        }
        switch(ret) {
          case 0:
          case EINTR:
          case EAGAIN:
            break;
          default:
            pthread_mutex_unlock(&data->mutex);
            rb_sys_fail("pthread_cond_wait");
            break;
        }
      }
      memcpy(buf + i * data->channels, data->buf + data->start*data->channels,
             sizeof(short) * data->channels);
    }
    pthread_mutex_unlock(&data->mutex);
    return nary;
}

void
Init_coreaudio_ext(void)
{
    sym_iv_devid = rb_intern("@devid");
    sym_iv_name = rb_intern("@name");
    sym_iv_available_sample_rate = rb_intern("@available_sample_rate");
    sym_iv_nominal_rate = rb_intern("@nominal_rate");
    sym_iv_input_stream = rb_intern("@input_stream");
    sym_iv_output_stream = rb_intern("@output_stream");
    sym_iv_channels = rb_intern("@channels");
    sym_iv_buffer_frame_size = rb_intern("@buffer_frame_size");

    rb_mCoreAudio = rb_define_module("CoreAudio");
    rb_cAudioDevice = rb_define_class_under(rb_mCoreAudio, "AudioDevice", rb_cObject);
    rb_cAudioStream = rb_define_class_under(rb_mCoreAudio, "AudioStream", rb_cObject);
    rb_cOutLoop = rb_define_class_under(rb_mCoreAudio, "OutLoop", rb_cObject);
    rb_cAudioBuffer = rb_define_class_under(rb_mCoreAudio, "AudioBuffer", rb_cObject);
    rb_cOutputBuffer = rb_define_class_under(rb_mCoreAudio, "OutputBuffer", rb_cAudioBuffer);
    rb_cInputBuffer = rb_define_class_under(rb_mCoreAudio, "InputBuffer", rb_cAudioBuffer);

    rb_define_method(rb_cAudioDevice, "initialize", ca_device_initialize, 2);
    rb_define_method(rb_cAudioDevice, "actual_rate", ca_get_device_actual_sample_rate, 0);
    rb_define_method(rb_cAudioDevice, "output_loop", ca_device_create_out_loop_proc, 1);
    rb_define_method(rb_cAudioDevice, "output_buffer", ca_device_create_out_buffer_proc, 1);
    rb_define_method(rb_cAudioDevice, "input_buffer", ca_device_create_in_buffer_proc, 1);
    rb_define_attr(rb_cAudioDevice, "devid", 1, 0);
    rb_define_attr(rb_cAudioDevice, "name", 1, 0);
    rb_define_attr(rb_cAudioDevice, "available_sample_rate", 1, 0);
    rb_define_attr(rb_cAudioDevice, "nominal_rate", 1, 0);
    rb_define_attr(rb_cAudioDevice, "input_stream", 1, 0);
    rb_define_attr(rb_cAudioDevice, "output_stream", 1, 0);

    rb_define_method(rb_cAudioStream, "initialize", ca_stream_initialize, 2);
    rb_define_attr(rb_cAudioStream, "channels", 1, 0);
    rb_define_attr(rb_cAudioStream, "buffer_frame_size", 1, 0);

    rb_define_singleton_method(rb_mCoreAudio, "devices", ca_devices, -1);
    rb_define_singleton_method(rb_mCoreAudio, "default_input_device", ca_default_input_device, -1);
    rb_define_singleton_method(rb_mCoreAudio, "default_output_device", ca_default_output_device, -1);
    rb_define_singleton_method(rb_mCoreAudio, "set_default_output_device", ca_set_default_output_device, 1);

    rb_define_method(rb_cOutLoop, "[]=", ca_out_loop_data_assign, 2);
    rb_define_method(rb_cOutLoop, "start", ca_out_loop_data_start, 0);
    rb_define_method(rb_cOutLoop, "stop", ca_out_loop_data_stop, 0);

    rb_define_method(rb_cAudioBuffer, "start", ca_buffer_data_start, 0);
    rb_define_method(rb_cAudioBuffer, "stop", ca_buffer_data_stop, 0);
    rb_define_method(rb_cAudioBuffer, "dropped_frame", ca_buffer_dropped_frame, 0);
    rb_define_method(rb_cAudioBuffer, "reset_dropped_frame", ca_buffer_reset_dropped_frame, 0);
    rb_define_method(rb_cAudioBuffer, "space",  ca_buffer_space, 0);

    rb_define_method(rb_cOutputBuffer, "<<", ca_out_buffer_data_append, 1);

    rb_define_method(rb_cInputBuffer, "read", ca_in_buffer_data_read, 1);

    Init_coreaudio_audiofile();
}
