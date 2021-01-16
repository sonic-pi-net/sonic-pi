/*
  Copyright (C) 2013 Paul Brossier <piem@aubio.org>

  This file is part of aubio.

  aubio is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  aubio is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with aubio.  If not, see <http://www.gnu.org/licenses/>.

*/

#include "aubio_priv.h"
#ifdef HAVE_AUDIO_UNIT

#include "fvec.h"
#include "fmat.h"
#include "io/audio_unit.h"

#include <AudioToolbox/AudioToolbox.h>

#define AU_IOS_MAX_OUT 64
#define AU_IOS_MAX_FRAMES AU_IOS_MAX_OUT * 16 * 2
#define PREFERRED_LATENCY 0.010
#define MAX_FPS 4096

#define INT_TO_FLOAT 3.0517578125e-05 // 1. / 32768.

struct _aubio_audio_unit_t {
  AudioUnit audio_unit;
  uint_t samplerate;
  uint_t blocksize;
  uint_t sw_input_channels;
  uint_t sw_output_channels;
  uint_t hw_output_channels;
  uint_t hw_input_channels;
  Float32 latency;
  sint_t total_frames;
  fmat_t *input_frames;
  fmat_t *output_frames;
  SInt16 *au_ios_inbuf;
  SInt16 *au_ios_outbuf;
  aubio_device_callback_t callback;
  void *callback_closure;
  int dio_error; // flag to check if we had a read error
  bool input_enabled;
  bool prevent_feedback;
  bool verbose;
  int au_ios_start;
  int au_ios_end;
  AURenderCallbackStruct au_ios_cb_struct;
};


static OSStatus
aubio_audio_unit_process(void *closure, AudioUnitRenderActionFlags * action_flags,
    const AudioTimeStamp * time_stamp, UInt32 bus_number, UInt32 inNumber_frames,
    AudioBufferList * input_output);

static int aubio_audio_unit_blocking(aubio_audio_unit_t *o);

static void audio_unit_check_audio_route(aubio_audio_unit_t *o);

static void audio_unit_interruption_listener(void *closure, UInt32 inInterruptionState);
static void audio_unit_route_change_listener(void *closure, AudioSessionPropertyID
    inID, UInt32 dataSize, const void *inData);
static OSStatus audio_unit_set_audio_session_category(bool has_input, bool verbose);
static UInt32 audio_unit_get_audio_session_category ();

aubio_audio_unit_t * new_aubio_audio_unit(uint_t samplerate,
    uint_t sw_input_channels, uint_t sw_output_channels,
    uint_t blocksize)
{
  aubio_audio_unit_t * o = AUBIO_NEW(aubio_audio_unit_t);
  o->hw_output_channels = 2;
  o->hw_input_channels = 2;
  o->sw_output_channels = sw_output_channels;
  o->sw_input_channels = sw_input_channels;
  o->samplerate = samplerate;
  o->latency = PREFERRED_LATENCY;
  o->blocksize = blocksize;

  o->au_ios_start = 0;
  o->au_ios_end = 0;

  o->verbose = 0;
  o->input_enabled = true;
  o->prevent_feedback = 1;
  o->dio_error = 0;

  o->total_frames = 0;

  /* the integers coming from and to the audio unit */
  o->au_ios_outbuf = AUBIO_ARRAY(SInt16, AU_IOS_MAX_FRAMES * o->hw_output_channels);
  o->au_ios_inbuf = AUBIO_ARRAY(SInt16, AU_IOS_MAX_FRAMES * o->hw_input_channels);

  /* the floats coming from and to the device callback */
  o->output_frames = new_fmat(sw_output_channels, blocksize);
  o->input_frames = new_fmat(sw_input_channels, blocksize);

  /* check for some sizes */
  if ( o->hw_output_channels != o->output_frames->height ) {
    AUBIO_ERR ("got hw_output_channels = %d, but output_frames has %d rows\n",
        o->hw_output_channels, o->output_frames->height);
  }
  if ( o->blocksize != o->output_frames->length ) {
    AUBIO_ERR ("got blocksize = %d, but output_frames has length %d\n",
        o->blocksize, o->output_frames->length);
  }
  if ( o->hw_input_channels != o->input_frames->height ) {
    AUBIO_ERR ("got hw_input_channels = %d, but input_frames has %d rows\n",
        o->hw_input_channels, o->input_frames->height);
  }
  if ( o->blocksize != o->input_frames->length ) {
    AUBIO_ERR ("got blocksize = %d, but input_frames has length %d\n",
        o->blocksize, o->input_frames->length);
  }

  return o;
}

sint_t aubio_audio_unit_set_preferred_latency (aubio_audio_unit_t *o, smpl_t latency)
{
  o->latency = latency;
  return 0;
}

sint_t aubio_audio_unit_set_prevent_feedback (aubio_audio_unit_t *o, uint_t prevent_feedback)
{
  o->prevent_feedback = prevent_feedback;
  return 0;
}

sint_t aubio_audio_unit_set_verbose (aubio_audio_unit_t *o, uint_t verbose)
{
  o->verbose = verbose;
  return 0;
}


sint_t aubio_audio_unit_init (aubio_audio_unit_t *o)
{
  OSStatus err = noErr;
  Float32 latency = o->latency;
  Float64 samplerate = (Float64)o->samplerate;

  o->au_ios_cb_struct.inputProc = aubio_audio_unit_process;
  o->au_ios_cb_struct.inputProcRefCon = o;

  /* setting up audio session with interruption listener */
  err = AudioSessionInitialize(NULL, NULL, audio_unit_interruption_listener, o);
  if (err) { AUBIO_ERR("audio_unit: could not initialize audio session (%d)\n", (int)err); goto fail; }

  audio_unit_set_audio_session_category(o->input_enabled, o->verbose);
  audio_unit_check_audio_route(o);

  /* add route change listener */
  err = AudioSessionAddPropertyListener(kAudioSessionProperty_AudioRouteChange,
      audio_unit_route_change_listener, o);
  if (err) { AUBIO_ERR("audio_unit: could not set route change listener (%d)\n", (int)err); goto fail; }

  /* set latency */
  err = AudioSessionSetProperty(kAudioSessionProperty_PreferredHardwareIOBufferDuration,
      sizeof(latency), &latency);
  if (err) { AUBIO_ERR("audio_unit: could not set preferred latency (%d)\n", (int)err); goto fail; }

#if 0 // only for iphone OS >= 3.1
  UInt32 val = 1; // set to 0 (default) to use ear speaker in voice application
  err = AudioSessionSetProperty(kAudioSessionProperty_OverrideCategoryDefaultToSpeaker,
      sizeof(UInt32), &val);
  if (err) { AUBIO_ERR("audio_unit: could not set session property to default to speaker\n"); }
#endif

  /* setting up audio unit */
  AudioComponentDescription desc;
  desc.componentManufacturer = kAudioUnitManufacturer_Apple;
  desc.componentSubType = kAudioUnitSubType_RemoteIO;
  desc.componentType = kAudioUnitType_Output;
  desc.componentFlags = 0;
  desc.componentFlagsMask = 0;

  AudioStreamBasicDescription audioFormat;

  /* look for a component that match the description */
  AudioComponent comp = AudioComponentFindNext(NULL, &desc);

  /* create the audio component */
  AudioUnit *audio_unit = &(o->audio_unit);

  err = AudioComponentInstanceNew(comp, &(o->audio_unit));
  if (err) { AUBIO_ERR("audio_unit: failed creating the audio unit\n"); goto fail; }

  /* enable IO */
  UInt32 enabled = 1;
  err = AudioUnitSetProperty (*audio_unit, kAudioOutputUnitProperty_EnableIO,
      kAudioUnitScope_Input, 1, &enabled, sizeof(enabled));
  if (err) {
    AUBIO_ERR("audio_unit: failed enabling input of audio unit\n");
    goto fail;
  }

  /* set max fps */
  UInt32 max_fps = MIN(o->blocksize, MAX_FPS);
  err = AudioUnitSetProperty (*audio_unit, kAudioUnitProperty_MaximumFramesPerSlice,
      kAudioUnitScope_Global, 0, &max_fps, sizeof(max_fps));
  if (err) {
    AUBIO_ERR("audio_unit: could not set maximum frames per slice property (%d)\n", (int)err);
    goto fail;
  }

  AudioUnitSetProperty (*audio_unit, kAudioUnitProperty_SetRenderCallback,
      kAudioUnitScope_Input, 0, &(o->au_ios_cb_struct), sizeof(o->au_ios_cb_struct));
  if (err) { AUBIO_ERR("audio_unit: failed setting audio unit render callback\n"); goto fail; }

#if 0
  err = AudioUnitSetProperty (*audio_unit, kAudioUnitProperty_SampleRate,
      kAudioUnitScope_Input, 0, &samplerate, sizeof(Float64));
  if (err) { AUBIO_ERR("audio_unit: could not set audio input sample rate\n"); goto fail; }
  err = AudioUnitSetProperty (*audio_unit, kAudioUnitProperty_SampleRate,
      kAudioUnitScope_Output, 1, &samplerate, sizeof(Float64));
  if (err) { AUBIO_ERR("audio_unit: could not set audio input sample rate\n"); goto fail; }
#endif

  audioFormat.mSampleRate = (Float64)samplerate;
  audioFormat.mChannelsPerFrame = 2;
  audioFormat.mFormatID = kAudioFormatLinearPCM;
  audioFormat.mFormatFlags = kAudioFormatFlagIsSignedInteger | kAudioFormatFlagsNativeEndian | kAudioFormatFlagIsPacked;
  audioFormat.mFramesPerPacket = 1;
  audioFormat.mBitsPerChannel = 8 * sizeof(SInt16);
#if 1  // interleaving
  audioFormat.mBytesPerFrame = 2 * sizeof(SInt16);
  audioFormat.mBytesPerPacket = 2 * sizeof(SInt16);
#else
  audioFormat.mBytesPerPacket = audioFormat.mBytesPerFrame = sizeof(SInt32);
  audioFormat.mFormatFlags |= kAudioFormatFlagIsNonInterleaved;
#endif

  err = AudioUnitSetProperty (*audio_unit, kAudioUnitProperty_StreamFormat,
      kAudioUnitScope_Input, 0, &audioFormat, sizeof(audioFormat));
  if (err) { AUBIO_ERR("audio_unit: could not set audio output format\n"); goto fail; }
  err = AudioUnitSetProperty (*audio_unit, kAudioUnitProperty_StreamFormat,
      kAudioUnitScope_Output, 1, &audioFormat, sizeof(audioFormat));
  if (err) { AUBIO_ERR("audio_unit: could not set audio input format\n"); goto fail; }

#if 0
  AudioStreamBasicDescription thruFormat;
  thissize = sizeof(thruFormat);
  err = AudioUnitGetProperty (*audio_unit, kAudioUnitProperty_StreamFormat,
      kAudioUnitScope_Input, 0, &thruFormat, &thissize);
  if (err) { AUBIO_ERR("audio_unit: could not get speaker output format, err: %d\n", (int)err); goto fail; }
  err = AudioUnitSetProperty (*audio_unit, kAudioUnitProperty_StreamFormat,
      kAudioUnitScope_Output, 1, &thruFormat, sizeof(thruFormat));
  if (err) { AUBIO_ERR("audio_unit: could not set input audio format, err: %d\n", (int)err); goto fail; }
#endif

  /* time to initialize the unit */
  err = AudioUnitInitialize(*audio_unit);
  if (err) { AUBIO_ERR("audio_unit: failed initializing audio, err: %d\n", (int)err); goto fail; }

  return 0;

fail:
  return err;
}

/* perform function */
OSStatus
aubio_audio_unit_process(void *closure, AudioUnitRenderActionFlags * action_flags,
    const AudioTimeStamp * time_stamp, UNUSED UInt32 bus_number, UInt32 inNumber_frames,
    AudioBufferList * input_output)
{
  UInt32 b; int err = 0;
  aubio_audio_unit_t *o = (aubio_audio_unit_t *)closure;
  AudioUnit thisUnit = o->audio_unit;

  if (o->input_enabled) {
    err = AudioUnitRender(thisUnit, action_flags, time_stamp, 1,
        inNumber_frames, input_output);
    if (err) {
      AUBIO_ERR("audio_unit: error performing AudioUnitRender (%d)\n", err);
      return err;
    }
  }

  // get the number of frames from the audio buffer list, NOT inNumber_frames
  UInt32 number_frames = input_output->mBuffers[0].mDataByteSize/ sizeof(SInt16) / 2;

  // FIXME find out why this happens
  if (number_frames < 10) {
    AUBIO_ERR("audio_unit: got number_frames %d\n", (int)number_frames);
    return -1;
  }

  if (o->total_frames >= (signed)number_frames) {

    SInt16 *data;
    if (o->au_ios_start + number_frames > AU_IOS_MAX_FRAMES) {
      // start reminder samples writing at reminder
      int reminder = AU_IOS_MAX_FRAMES - o->au_ios_start;
      int starter = (o->au_ios_start + number_frames) - AU_IOS_MAX_FRAMES;
      for (b = 0; b < input_output->mNumberBuffers; b++) {
        data = (SInt16 *)(input_output->mBuffers[b].mData);
        /* copy microphone output to input buffer */
        memcpy (o->au_ios_inbuf + o->au_ios_start * 2, data, reminder * 2 * sizeof(SInt16));
        memcpy (o->au_ios_inbuf, data + reminder * 2, starter * 2 * sizeof(SInt16));
        /* silence data before copying from output */
        //memset (data, 0, input_output->mBuffers[b].mDataByteSize);
        /* copy output buffer to speakers */
        memcpy (data, o->au_ios_outbuf + o->au_ios_start * 2, reminder * 2 * sizeof(SInt16));
        memcpy (data + reminder * 2, o->au_ios_outbuf, starter * 2 * sizeof(SInt16));
      }
    } else {
      for (b = 0; b < input_output->mNumberBuffers; b++) {
        data = (SInt16 *)(input_output->mBuffers[b].mData);
        /* copy microphone samples to au_ios_inbuf */
        memcpy(o->au_ios_inbuf + o->au_ios_start * 2, data, number_frames * 2 * sizeof(SInt16));
        /* silence data before copying from output */
        //memset (data, 0, input_output->mBuffers[b].mDataByteSize);
        /* copy output buffer to speakers */
        memcpy(data, o->au_ios_outbuf + o->au_ios_start * 2, number_frames * 2 * sizeof(SInt16));
      }
    }
    o->au_ios_start += number_frames;
    o->au_ios_start %= AU_IOS_MAX_FRAMES;
    o->total_frames -= number_frames;

#if 1
  } else {
    if (o->total_frames > 0) o->dio_error = 1;
    for (b = 0; b < input_output->mNumberBuffers; b++) {
      memset (input_output->mBuffers[b].mData, 0,
          input_output->mBuffers[b].mDataByteSize);
    }
    //total_frames = 0;
#endif
  }

  // now call callback
  while ( o->total_frames < (signed)number_frames ) {
    //AUBIO_DBG ("audio_unit: total_frames = %d, number_frames = %d, o->au_ios_start = %d, o->au_ios_end = %d",
    //    o->total_frames, number_frames, o->au_ios_start, o->au_ios_end);
    aubio_audio_unit_blocking(o);
  }

  return err;
}

int aubio_audio_unit_blocking(aubio_audio_unit_t *o)
{
  uint_t sw_output_channels, sw_input_channels,
         hw_output_channels, hw_input_channels,
         i, j, blocksize;
  if (! o->callback) return -1;

  smpl_t ** tbuf;

  sw_output_channels = o->sw_output_channels;
  sw_input_channels = o->sw_input_channels;
  hw_output_channels = o->hw_output_channels;
  hw_input_channels = o->hw_input_channels;
  blocksize = o->blocksize;

  if (!sw_input_channels && !sw_output_channels) goto fail;

  if (o->dio_error) {
    AUBIO_WRN("audio_unit: dio error %d\n", o->total_frames);
    o->dio_error = 0;
  }

  if (o->au_ios_inbuf) {
    /* copy samples from input buffer */
    tbuf = o->input_frames->data;
    if (o->input_enabled) {
      for (j = 0; j < blocksize;j++) {
        for (i = 0; i < sw_input_channels && i < hw_input_channels; i++) {
          //tbuf[i][j] =
          //   (smpl_t)(o->au_ios_inbuf[i + (j + o->au_ios_end) * sw_input_channels] / 32768.);
          // on iphone, input is mono, copy right to left channel
          tbuf[i][j] =
            (smpl_t) o->au_ios_inbuf[0 + (j + o->au_ios_end) * hw_input_channels]
            * INT_TO_FLOAT;
        }
      }
    } else {
      // input is disabled, fill with zeroes
      for (j = 0; j < blocksize; j++) {
        for (i = 0; i < sw_input_channels && i < hw_input_channels; i++) {
          tbuf[i][j] = 0;
        }
      }
    }
  }

  o->callback(o->callback_closure, o->input_frames, o->output_frames);

  /* copy samples to output buffer */
  tbuf = o->output_frames->data;
  for (i = 0; i < o->output_frames->height; i++)	{
    for (j = 0; j < o->output_frames->length; j++) {
      smpl_t val = tbuf[i][j];
      if (val < -1.0) val = -1.0;
      if (val > 1.0) val = 1.0;
      o->au_ios_outbuf[i + (j + o->au_ios_end) * hw_output_channels ] = (SInt16)(val * 32767);
    }
  }

  o->au_ios_end += blocksize;
  o->au_ios_end %= AU_IOS_MAX_FRAMES;
  o->total_frames += blocksize;

  return 0;

fail:
  AUBIO_ERR("audio_unit: callback() failed\n");
  o->total_frames += AU_IOS_MAX_FRAMES;
  return 1;
}

sint_t aubio_audio_unit_get_info (aubio_audio_unit_t *o)
{
  UInt32 thissize, input_hw_channels, output_hw_channels, max_fps;
  Float32 latency, input_latency, output_latency, input_hw_volume, output_hw_volume;
  Float64 samplerate;
  OSStatus err = 0;

  // Show some info about the opened unit

  /* get sampling rate */
  thissize = sizeof(samplerate);
  err = AudioUnitGetProperty (o->audio_unit, kAudioUnitProperty_SampleRate,
      kAudioUnitScope_Output, 1, &samplerate, &thissize);
  if (err) { AUBIO_ERR("audio_unit: could not get audio unit sample rate (%d)\n",
      (int)err); goto fail; }

  /* get hardware input channels */
  thissize = sizeof(input_hw_channels);
  err = AudioSessionGetProperty(kAudioSessionProperty_CurrentHardwareInputNumberChannels,
      &thissize, &input_hw_channels);
  if (err) { AUBIO_ERR("audio_unit: could not get hardware input channels (%d)\n",
      (int)err); goto fail; }

  /* get hardware output channels */
  thissize = sizeof(output_hw_channels);
  err = AudioSessionGetProperty(kAudioSessionProperty_CurrentHardwareOutputNumberChannels,
      &thissize, &output_hw_channels);
  if (err) { AUBIO_ERR("audio_unit: could not get hardware output channels (%d)\n",
      (int)err); goto fail; }

  /* get hardware input volume */
  thissize = sizeof(input_hw_volume);
  err = AudioSessionGetProperty(kAudioSessionProperty_InputGainScalar,
      &thissize, &input_hw_volume);
  if (err) { AUBIO_ERR("audio_unit: could not get hardware input volume (%d)\n",
      (int)err); goto fail; }

  /* get hardware output volume */
  thissize = sizeof(output_hw_volume);
  err = AudioSessionGetProperty(kAudioSessionProperty_CurrentHardwareOutputVolume,
      &thissize, &output_hw_volume);
  if (err) { AUBIO_ERR("audio_unit: could not get hardware output volume (%d)\n",
      (int)err); goto fail; }

  AUBIO_MSG("audio_unit: opened at %.0fHz, sw channels %din/%dout, hw channels %din/%dout, hw vol %.2fin/%.2fout\n",
      samplerate,
      o->sw_input_channels, o->sw_output_channels,
      (unsigned int)input_hw_channels, (unsigned int)output_hw_channels,
      input_hw_volume, output_hw_volume);

  /* get max frames per slice */
  thissize = sizeof(max_fps);
  err = AudioUnitGetProperty (o->audio_unit, kAudioUnitProperty_MaximumFramesPerSlice,
      kAudioUnitScope_Global, 0, &max_fps, &thissize);
  if (err) { AUBIO_ERR("audio_unit: could not get maximum frames per slice property %d\n",
      (int)err); goto fail; }

  /* get hardware latency */
  thissize = sizeof(latency);
  err = AudioSessionGetProperty(kAudioSessionProperty_CurrentHardwareIOBufferDuration,
      &thissize, &latency);
  if (err) { AUBIO_ERR("audio_unit: could not get hardware latency %d\n",
      (int)err); goto fail; }

  /* get input latency */
  thissize = sizeof(input_latency);
  err = AudioSessionGetProperty(kAudioSessionProperty_CurrentHardwareInputLatency,
      &thissize, &input_latency);
  if (err) { AUBIO_ERR("audio_unit: could not get input latency %d\n",
      (int)err); goto fail; }

  /* get output harlatency */
  thissize = sizeof(output_latency);
  err = AudioSessionGetProperty(kAudioSessionProperty_CurrentHardwareOutputLatency,
      &thissize, &output_latency);
  if (err) { AUBIO_ERR("audio_unit: could not get output latency %d\n",
      (int)err); goto fail; }

  AUBIO_MSG("audio_unit: I/O latency: %.2fms, %d frames, (%.2fms, %d frames in, %.2fms %d frames out)\n",
      latency*1000., (sint_t)round(latency*samplerate),
      input_latency*1000., (sint_t)ROUND(input_latency*samplerate),
      output_latency*1000., (sint_t)ROUND(output_latency*samplerate));

fail:
  return err;
}

sint_t aubio_audio_unit_start(aubio_audio_unit_t *o) {
  OSStatus err = 0;

  if (o->verbose) {
    // print some info about the current settings
    aubio_audio_unit_get_info (o);
  }

  /* time to start the unit */
  err = AudioOutputUnitStart (o->audio_unit);
  if (err) { AUBIO_ERR("audio_unit: could not start unit (%d)\n", (int)err); }
  return err;
}

sint_t aubio_audio_unit_stop(aubio_audio_unit_t *o)
{
  if (o->audio_unit == NULL) return -1;
  OSStatus err = AudioOutputUnitStop (o->audio_unit);
  if (err) { AUBIO_WRN("audio_unit: failed stopping audio unit (%d)\n", (int)err); }
  err = AudioUnitUninitialize (o->audio_unit);
  if (err) { AUBIO_WRN("audio_unit: failed unitializing audio unit (%d)\n", (int)err); }
  err = AudioSessionSetActive(false);
  if (err) { AUBIO_WRN("audio_unit: failed stopping audio session (%d)\n", (int)err); }
  return err;
}

uint_t aubio_audio_unit_set_callback(aubio_audio_unit_t *o,
    aubio_device_callback_t callback, void *closure) {
  o->callback = callback;
  o->callback_closure = closure;
  return 0;
}

/* interruption listeners */
void audio_unit_interruption_listener(void *closure, UInt32 inInterruptionState)
{
  OSStatus err = 0;
  aubio_audio_unit_t *o = (aubio_audio_unit_t *) closure;
  AudioUnit this_unit = o->audio_unit;

  if (inInterruptionState == kAudioSessionEndInterruption) {
    AUBIO_WRN("audio_unit: session interruption ended\n");
    err = AudioSessionSetActive(true);
    if (err) {
      AUBIO_ERR("audio_unit: could not make session active after interruption (%d)\n", (int)err);
      goto fail;
    }
    err = AudioOutputUnitStart(this_unit);
    if (err) {
      AUBIO_ERR("audio_unit: failed starting unit (%d)\n", (int)err);
      goto fail;
    }
  }
  if (inInterruptionState == kAudioSessionBeginInterruption) {
    AUBIO_WRN("audio_unit: session interruption started\n");
    err = AudioOutputUnitStop(this_unit);
    if (err) {
      AUBIO_ERR("audio_unit: could not stop unit at interruption (%d)\n", (int)err);
      goto fail;
    }
    err = AudioSessionSetActive(false);
    if (err) {
      AUBIO_ERR("audio_unit: could not make session inactive after interruption (%d)\n", (int)err);
      goto fail;
    }
  }
fail:
  return;
}

UInt32 audio_unit_get_audio_session_category () {
  UInt32 category, thissize;
  thissize = sizeof(category);
  OSStatus err = AudioSessionGetProperty(kAudioSessionProperty_AudioCategory,
      &thissize, &category);
  if (err) {
    AUBIO_ERR("audio_unit: could not get audio category (%d)\n", (int)err);
    return err;
  }
  if (category == kAudioSessionCategory_AmbientSound) {
    AUBIO_MSG("audio_unit: session category is AmbiantSound\n");
  } else if (category == kAudioSessionCategory_SoloAmbientSound) {
    AUBIO_MSG("audio_unit: session category is SoloAmbiantSound\n");
  } else if (category == kAudioSessionCategory_MediaPlayback) {
    AUBIO_MSG("audio_unit: session category is MediaPlayback\n");
  } else if (category == kAudioSessionCategory_RecordAudio) {
    AUBIO_MSG("audio_unit: session category is RecordAudio\n");
  } else if (category == kAudioSessionCategory_PlayAndRecord) {
    AUBIO_MSG("audio_unit: session category is PlayAndRecord\n");
  } else if (category == kAudioSessionCategory_AudioProcessing) {
    AUBIO_MSG("audio_unit: session category is AudioProcessing\n");
  }
  return category;
}

OSStatus audio_unit_set_audio_session_category(bool has_input, bool verbose)
{
  //if we have input, set the session category accordingly
  OSStatus err = 0;
  UInt32 category;
  if (has_input) {
    category = kAudioSessionCategory_PlayAndRecord;
    if (verbose) AUBIO_MSG("audio_unit: setting category to PlayAndRecord\n");
  } else {
    category = kAudioSessionCategory_MediaPlayback;
    if (verbose) AUBIO_MSG("audio_unit: setting category to MediaPlayback\n");
  }
  err = AudioSessionSetProperty(kAudioSessionProperty_AudioCategory,
      sizeof(category), &category);
  if (err) {
    AUBIO_ERR("audio_unit: could not set audio category\n");
  }

  // Audiob.us style
  UInt32 allowMixing = 1;
  AudioSessionSetProperty(kAudioSessionProperty_OverrideCategoryMixWithOthers,
      sizeof (allowMixing), &allowMixing);
  if (err) {
    AUBIO_ERR("audio_unit: could not set audio session to mix with others\n");
  }

  return err;
}

void audio_unit_check_audio_route(aubio_audio_unit_t *o) {
  CFStringRef currentRoute;
  UInt32 val, thissize = sizeof(currentRoute);
  OSStatus err = AudioSessionGetProperty(kAudioSessionProperty_AudioRoute, &thissize, &currentRoute);
  if (err) { AUBIO_ERR("audio_unit: could not get current route\n"); goto fail; }
  else {
    char *route = (char *)CFStringGetCStringPtr ( currentRoute, kCFStringEncodingUTF8);
    if (route == NULL) {
      int bufferSize = 25;
      route = calloc(bufferSize, sizeof(char));
      CFStringGetCString ( currentRoute, route, bufferSize,
          kCFStringEncodingUTF8);
    }
    if (o->verbose) {
      AUBIO_MSG ("audio_unit: current route is %s\n", route);
    }
    //free(route);
  }
  if( currentRoute ) {
    if( CFStringCompare( currentRoute, CFSTR("Headset"), 0 ) == kCFCompareEqualTo ) {
      val = kAudioSessionOverrideAudioRoute_None;
    } else if( CFStringCompare( currentRoute, CFSTR("Receiver" ), 0 ) == kCFCompareEqualTo ) {
      val = kAudioSessionOverrideAudioRoute_Speaker;
    } else if( CFStringCompare( currentRoute, CFSTR("ReceiverAndMicrophone" ), 0 ) == kCFCompareEqualTo ) {
      val = kAudioSessionOverrideAudioRoute_Speaker;
    } else if( CFStringCompare( currentRoute, CFSTR("SpeakerAndMicrophone" ), 0 ) == kCFCompareEqualTo ) {
      val = kAudioSessionOverrideAudioRoute_Speaker;
    } else if( CFStringCompare( currentRoute, CFSTR("HeadphonesAndMicrophone" ), 0 ) == kCFCompareEqualTo ) {
      val = kAudioSessionOverrideAudioRoute_None;
    } else if( CFStringCompare( currentRoute, CFSTR("HeadsetInOut" ), 0 ) == kCFCompareEqualTo ) {
      val = kAudioSessionOverrideAudioRoute_None;
    } else {
      val = kAudioSessionOverrideAudioRoute_None;
    }

    o->input_enabled = true;
    if (val == kAudioSessionOverrideAudioRoute_Speaker) {
      if (o->prevent_feedback) {
        o->input_enabled = false;
        if (o->verbose) {
          AUBIO_MSG ("audio_unit: disabling input to avoid feedback\n");
        }
      } else {
        AUBIO_WRN ("audio_unit: input not disabled as prevent_feedback set to 0, risking feedback\n");
      }
    }

    err = AudioSessionSetProperty(kAudioSessionProperty_OverrideAudioRoute,
        sizeof(UInt32), &val);
    if (err) { AUBIO_ERR("audio_unit: could not set session OverrideAudioRoute to Speaker\n"); }

  }

fail:
  if ( currentRoute ) free((void*)currentRoute);
  return;

}

SInt32
audio_unit_get_route_change_reason(CFDictionaryRef routeChangeDic) {
  CFNumberRef routeChangeReasonRef = (CFNumberRef)CFDictionaryGetValue(routeChangeDic,
      CFSTR(kAudioSession_AudioRouteChangeKey_Reason));
  SInt32 change_reason_number;
  CFNumberGetValue ( routeChangeReasonRef, kCFNumberSInt32Type, &change_reason_number);
  switch (change_reason_number) {
    case kAudioSessionRouteChangeReason_NewDeviceAvailable:
      AUBIO_MSG("audio_unit: route changed to NewDeviceAvailable\n");
      break;
    case kAudioSessionRouteChangeReason_OldDeviceUnavailable:
      AUBIO_MSG("audio_unit: route changed to OldDeviceUnavailable\n");
      break;
    case kAudioSessionRouteChangeReason_CategoryChange:
      AUBIO_MSG("audio_unit: route changed to CategoryChange\n");
      audio_unit_get_audio_session_category();
      break;
    case kAudioSessionRouteChangeReason_Override:
      AUBIO_MSG("audio_unit: route changed to Override\n");
      break;
    case kAudioSessionRouteChangeReason_WakeFromSleep:
      AUBIO_MSG("audio_unit: route changed to WakeFromSleep\n");
      break;
    case kAudioSessionRouteChangeReason_NoSuitableRouteForCategory:
      AUBIO_MSG("audio_unit: route changed to NoSuitableRouteForCategory\n");
      break;
    case kAudioSessionRouteChangeReason_Unknown:
    default:
      AUBIO_ERR("audio_unit: route changed for an unknown reason!?\n");
      break;
  }
  return change_reason_number;
}

/* route change listeners */
void
audio_unit_route_change_listener(void *closure, AudioSessionPropertyID inID,
    UInt32 dataSize, const void *inData)
{

  UNUSED aubio_audio_unit_t *o = (aubio_audio_unit_t *)closure;
  UNUSED UInt32 size = dataSize;
  if (inID == kAudioSessionProperty_AudioRouteChange) {

    // OSStatus err = 0;
    //AudioUnit audio_unit = o->audio_unit;

    if (o->verbose) {
      // show route change reason
      audio_unit_get_route_change_reason((CFDictionaryRef)inData);
    }

    // check current audio route, changing it to prevent feedback as needed
    audio_unit_check_audio_route(o);

    if (o->verbose) {
      // print some info about the current settings
      aubio_audio_unit_get_info(o);
    }

  }

}

/* delete object */
uint_t del_aubio_audio_unit(aubio_audio_unit_t *o)
{
  int err = 0;
  err = aubio_audio_unit_stop(o);
  if (o->au_ios_inbuf) free(o->au_ios_inbuf);
  o->au_ios_inbuf = NULL;
  if (o->au_ios_outbuf) free(o->au_ios_outbuf);
  o->au_ios_outbuf = NULL;
  del_fmat (o->input_frames);
  del_fmat (o->output_frames);
  o->audio_unit = NULL;
  return (int)err;
}

#endif /* HAVE_AUDIO_UNIT */
