/*
  Copyright (C) 2012 Paul Brossier <piem@aubio.org>

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

#ifdef HAVE_SOURCE_APPLE_AUDIO

#include "fvec.h"
#include "fmat.h"
#include "ioutils.h"
#include "io/source_apple_audio.h"

// ExtAudioFileRef, AudioStreamBasicDescription, AudioBufferList, ...
#include <AudioToolbox/AudioToolbox.h>

#define RT_BYTE1( a )      ( (a) & 0xff )
#define RT_BYTE2( a )      ( ((a) >> 8) & 0xff )
#define RT_BYTE3( a )      ( ((a) >> 16) & 0xff )
#define RT_BYTE4( a )      ( ((a) >> 24) & 0xff )

struct _aubio_source_apple_audio_t {
  uint_t channels;
  uint_t samplerate;          //< requested samplerate
  uint_t source_samplerate;   //< actual source samplerate
  uint_t block_size;

  char_t *path;

  ExtAudioFileRef audioFile;
  AudioBufferList bufferList;
};

extern int createAudioBufferList(AudioBufferList *bufferList, int channels, int max_source_samples);
extern void freeAudioBufferList(AudioBufferList *bufferList);
extern CFURLRef createURLFromPath(const char * path);
char_t *getPrintableOSStatusError(char_t *str, OSStatus error);

uint_t aubio_source_apple_audio_open (aubio_source_apple_audio_t *s, const char_t * path);

aubio_source_apple_audio_t * new_aubio_source_apple_audio(const char_t * path, uint_t samplerate, uint_t block_size)
{
  aubio_source_apple_audio_t * s = AUBIO_NEW(aubio_source_apple_audio_t);

  if (path == NULL || strnlen(path, PATH_MAX) < 1) {
    AUBIO_ERROR("source_apple_audio: Aborted opening null path\n");
    goto beach;
  }

  if ( (sint_t)block_size <= 0 ) {
    AUBIO_ERROR("source_apple_audio: Can not open %s with null or negative block_size %d\n",
        path, block_size);
    goto beach;
  }

  if ( (sint_t)samplerate < 0 ) {
    AUBIO_ERROR("source_apple_audio: Can not open %s with negative samplerate %d\n",
        path, samplerate);
    goto beach;
  }

  s->block_size = block_size;
  s->samplerate = samplerate;

  if ( aubio_source_apple_audio_open ( s, path ) ) {
    goto beach;
  }
  return s;

beach:
  del_aubio_source_apple_audio(s);
  return NULL;
}

uint_t aubio_source_apple_audio_open (aubio_source_apple_audio_t *s, const char_t * path)
{
  OSStatus err = noErr;
  UInt32 propSize;

  s->path = AUBIO_ARRAY(char_t, strnlen(path, PATH_MAX) + 1);
  strncpy(s->path, path, strnlen(path, PATH_MAX) + 1);

  // open the resource url
  CFURLRef fileURL = createURLFromPath(s->path);
  err = ExtAudioFileOpenURL(fileURL, &s->audioFile);
  CFRelease(fileURL);
  if (err == -43) {
    AUBIO_ERR("source_apple_audio: Failed opening %s, "
        "file not found, or no read access\n", s->path);
    goto beach;
  } else if (err) {
    char_t errorstr[20];
    AUBIO_ERR("source_apple_audio: Failed opening %s, "
        "error in ExtAudioFileOpenURL (%s)\n", s->path,
        getPrintableOSStatusError(errorstr, err));
    goto beach;
  }

  // create an empty AudioStreamBasicDescription
  AudioStreamBasicDescription fileFormat;
  propSize = sizeof(fileFormat);
  memset(&fileFormat, 0, sizeof(AudioStreamBasicDescription));

  // fill it with the file description
  err = ExtAudioFileGetProperty(s->audioFile,
      kExtAudioFileProperty_FileDataFormat, &propSize, &fileFormat);
  if (err) {
    char_t errorstr[20];
    AUBIO_ERROR("source_apple_audio: Failed opening %s, "
        "error in ExtAudioFileGetProperty (%s)\n", s->path,
        getPrintableOSStatusError(errorstr, err));
    goto beach;
  }

  if (s->samplerate == 0) {
    s->samplerate = fileFormat.mSampleRate;
    //AUBIO_DBG("sampling rate set to 0, automagically adjusting to %d\n", samplerate);
  }

  s->source_samplerate = fileFormat.mSampleRate;
  s->channels = fileFormat.mChannelsPerFrame;

  AudioStreamBasicDescription clientFormat;
  propSize = sizeof(AudioStreamBasicDescription);
  memset(&clientFormat, 0, sizeof(AudioStreamBasicDescription));
  clientFormat.mFormatID         = kAudioFormatLinearPCM;
  clientFormat.mSampleRate       = (Float64)(s->samplerate);
  clientFormat.mFormatFlags      = kAudioFormatFlagIsFloat;
  clientFormat.mChannelsPerFrame = s->channels;
  clientFormat.mBitsPerChannel   = sizeof(smpl_t) * 8;
  clientFormat.mFramesPerPacket  = 1;
  clientFormat.mBytesPerFrame    = clientFormat.mBitsPerChannel * clientFormat.mChannelsPerFrame / 8;
  clientFormat.mBytesPerPacket   = clientFormat.mFramesPerPacket * clientFormat.mBytesPerFrame;

  // set the client format description
  err = ExtAudioFileSetProperty(s->audioFile, kExtAudioFileProperty_ClientDataFormat,
      propSize, &clientFormat);
  if (err) {
    char_t errorstr[20];
    AUBIO_ERROR("source_apple_audio: Failed opening %s, "
        "error in ExtAudioFileSetProperty (%s)\n", s->path,
        getPrintableOSStatusError(errorstr, err));
#if 0
  // print client and format descriptions
  AUBIO_DBG("Opened %s\n", s->path);
  AUBIO_DBG("file/client Format.mFormatID:        : %3c%c%c%c / %c%c%c%c\n",
      (int)RT_BYTE4(fileFormat.mFormatID),   (int)RT_BYTE3(fileFormat.mFormatID),   (int)RT_BYTE2(fileFormat.mFormatID),   (int)RT_BYTE1(fileFormat.mFormatID),
      (int)RT_BYTE4(clientFormat.mFormatID), (int)RT_BYTE3(clientFormat.mFormatID), (int)RT_BYTE2(clientFormat.mFormatID), (int)RT_BYTE1(clientFormat.mFormatID)
      );

  AUBIO_DBG("file/client Format.mSampleRate       : %6.0f / %.0f\n",     fileFormat.mSampleRate      ,      clientFormat.mSampleRate);
  AUBIO_DBG("file/client Format.mFormatFlags      : %6d / %d\n",    (int)fileFormat.mFormatFlags     , (int)clientFormat.mFormatFlags);
  AUBIO_DBG("file/client Format.mChannelsPerFrame : %6d / %d\n",    (int)fileFormat.mChannelsPerFrame, (int)clientFormat.mChannelsPerFrame);
  AUBIO_DBG("file/client Format.mBitsPerChannel   : %6d / %d\n",    (int)fileFormat.mBitsPerChannel  , (int)clientFormat.mBitsPerChannel);
  AUBIO_DBG("file/client Format.mFramesPerPacket  : %6d / %d\n",    (int)fileFormat.mFramesPerPacket , (int)clientFormat.mFramesPerPacket);
  AUBIO_DBG("file/client Format.mBytesPerFrame    : %6d / %d\n",    (int)fileFormat.mBytesPerFrame   , (int)clientFormat.mBytesPerFrame);
  AUBIO_DBG("file/client Format.mBytesPerPacket   : %6d / %d\n",    (int)fileFormat.mBytesPerPacket  , (int)clientFormat.mBytesPerPacket);
  AUBIO_DBG("file/client Format.mReserved         : %6d / %d\n",    (int)fileFormat.mReserved        , (int)clientFormat.mReserved);
#endif
      goto beach;
  }

  smpl_t ratio = s->source_samplerate * 1. / s->samplerate;
  if (ratio < 1.) {
    AUBIO_WRN("source_apple_audio: up-sampling %s from %0dHz to %0dHz\n",
        s->path, s->source_samplerate, s->samplerate);
  }

  // allocate the AudioBufferList
  freeAudioBufferList(&s->bufferList);
  if (createAudioBufferList(&s->bufferList, s->channels, s->block_size * s->channels)) {
    AUBIO_ERR("source_apple_audio: failed creating bufferList\n");
    goto beach;
  }

beach:
  return err;
}

static UInt32 aubio_source_apple_audio_read_frame(aubio_source_apple_audio_t *s)
{
  UInt32 loadedPackets = s->block_size;
  OSStatus err = ExtAudioFileRead(s->audioFile, &loadedPackets, &s->bufferList);
  if (err) {
    char_t errorstr[20];
    AUBIO_ERROR("source_apple_audio: error while reading %s "
        "with ExtAudioFileRead (%s)\n", s->path,
        getPrintableOSStatusError(errorstr, err));
  }
  return loadedPackets;
}

void aubio_source_apple_audio_do(aubio_source_apple_audio_t *s, fvec_t * read_to,
    uint_t * read) {
  uint_t c, v;
  UInt32 loadedPackets = aubio_source_apple_audio_read_frame(s);
  uint_t length = aubio_source_validate_input_length("source_apple_audio",
      s->path, s->block_size, read_to->length);
  smpl_t *data = (smpl_t*)s->bufferList.mBuffers[0].mData;

  length = MIN(loadedPackets, length);

  for (v = 0; v < length; v++) {
    read_to->data[v] = 0.;
    for (c = 0; c < s->channels; c++) {
      read_to->data[v] += data[ v * s->channels + c];
    }
    read_to->data[v] /= (smpl_t)s->channels;
  }
  // short read, fill with zeros
  aubio_source_pad_output(read_to, length);

  *read = (uint_t)length;
}

void aubio_source_apple_audio_do_multi(aubio_source_apple_audio_t *s, fmat_t * read_to, uint_t * read) {
  uint_t c, v;
  uint_t length = aubio_source_validate_input_length("source_apple_audio",
      s->path, s->block_size, read_to->length);
  uint_t channels = aubio_source_validate_input_channels("source_apple_audio",
      s->path, s->channels, read_to->height);
  UInt32 loadedPackets = aubio_source_apple_audio_read_frame(s);
  smpl_t *data = (smpl_t*)s->bufferList.mBuffers[0].mData;

  length = MIN(loadedPackets, length);

  for (v = 0; v < length; v++) {
    for (c = 0; c < channels; c++) {
      read_to->data[c][v] = data[ v * s->channels + c];
    }
  }

  aubio_source_pad_multi_output(read_to, s->channels, (uint_t)length);

  *read = (uint_t)length;
}

uint_t aubio_source_apple_audio_close (aubio_source_apple_audio_t *s)
{
  OSStatus err = noErr;
  if (!s->audioFile) { return AUBIO_OK; }
  err = ExtAudioFileDispose(s->audioFile);
  s->audioFile = NULL;
  if (err) {
    char_t errorstr[20];
    AUBIO_ERROR("source_apple_audio: error while closing %s "
        "in ExtAudioFileDispose (%s)\n", s->path,
        getPrintableOSStatusError(errorstr, err));
    return err;
  }
  return AUBIO_OK;
}

void del_aubio_source_apple_audio(aubio_source_apple_audio_t * s){
  AUBIO_ASSERT(s);
  aubio_source_apple_audio_close (s);
  if (s->path) AUBIO_FREE(s->path);
  freeAudioBufferList(&s->bufferList);
  AUBIO_FREE(s);
}

uint_t aubio_source_apple_audio_seek (aubio_source_apple_audio_t * s, uint_t pos) {
  OSStatus err = noErr;
  if ((sint_t)pos < 0) {
    AUBIO_ERROR("source_apple_audio: error while seeking in %s "
        "(can not seek at negative position %d)\n",
        s->path, pos);
    err = -1;
    goto beach;
  }
  // check if we are not seeking out of the file
  uint_t fileLengthFrames = aubio_source_apple_audio_get_duration(s);
  // compute position in the source file, before resampling
  smpl_t ratio = s->source_samplerate * 1. / s->samplerate;
  SInt64 resampled_pos = (SInt64)ROUND( pos * ratio );
  if (resampled_pos > fileLengthFrames) {
    AUBIO_ERR("source_apple_audio: trying to seek in %s at pos %d, "
        "but file has only %d frames\n",
        s->path, pos, (uint_t)(fileLengthFrames / ratio));
    err = -1;
    goto beach;
  }
  // after a short read, the bufferList size needs to resetted to prepare for a full read
  AudioBufferList *bufferList = &s->bufferList;
  bufferList->mBuffers[0].mDataByteSize = s->block_size * s->channels * sizeof (smpl_t);
  // do the actual seek
  err = ExtAudioFileSeek(s->audioFile, resampled_pos);
  if (err) {
    char_t errorstr[20];
    AUBIO_ERROR("source_apple_audio: error while seeking %s at %d "
        "in ExtAudioFileSeek (%s)\n", s->path, pos,
        getPrintableOSStatusError(errorstr, err));
  }
#if 0
  // check position after seek
  {
    SInt64 outFrameOffset = 0;
    err = ExtAudioFileTell(s->audioFile, &outFrameOffset);
    if (err) {
      char_t errorstr[20];
      AUBIO_ERROR("source_apple_audio: error while seeking %s at %d "
          "in ExtAudioFileTell (%s)\n", s->path, pos,
          getPrintableOSStatusError(errorstr, err));
    }
    AUBIO_DBG("source_apple_audio: asked seek at %d, tell got %d\n",
        pos, (uint_t)(outFrameOffset / ratio + .5));
  }
#endif
beach:
  return err;
}

uint_t aubio_source_apple_audio_get_samplerate(const aubio_source_apple_audio_t * s) {
  return s->samplerate;
}

uint_t aubio_source_apple_audio_get_channels(const aubio_source_apple_audio_t * s) {
  return s->channels;
}

uint_t aubio_source_apple_audio_get_duration(const aubio_source_apple_audio_t * s) {
  SInt64 fileLengthFrames = 0;
  UInt32 propSize = sizeof(fileLengthFrames);
  OSStatus err = ExtAudioFileGetProperty(s->audioFile,
      kExtAudioFileProperty_FileLengthFrames, &propSize, &fileLengthFrames);
  if (err) {
    char_t errorstr[20];
    AUBIO_ERROR("source_apple_audio: Failed getting %s duration, "
        "error in ExtAudioFileGetProperty (%s)\n", s->path,
        getPrintableOSStatusError(errorstr, err));
    return 0;
  }
  return (uint_t)fileLengthFrames;
}

#endif /* HAVE_SOURCE_APPLE_AUDIO */
