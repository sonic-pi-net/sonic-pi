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

#ifdef HAVE_LIBAV

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#if defined(HAVE_SWRESAMPLE)
#include <libswresample/swresample.h>
#elif defined(HAVE_AVRESAMPLE)
#include <libavresample/avresample.h>
#endif
#include <libavutil/opt.h>

// determine whether we use libavformat from ffmpeg or from libav
#define FFMPEG_LIBAVFORMAT (LIBAVFORMAT_VERSION_MICRO > 99 )
// max_analyze_duration2 was used from ffmpeg libavformat 55.43.100 -> 57.2.100
#define FFMPEG_LIBAVFORMAT_MAX_DUR2 FFMPEG_LIBAVFORMAT && ( \
      (LIBAVFORMAT_VERSION_MAJOR == 55 && LIBAVFORMAT_VERSION_MINOR >= 43) \
      || (LIBAVFORMAT_VERSION_MAJOR == 56) \
      || (LIBAVFORMAT_VERSION_MAJOR == 57 && LIBAVFORMAT_VERSION_MINOR < 2) \
      )

// backward compatibility with libavcodec55
#if LIBAVCODEC_VERSION_INT < AV_VERSION_INT(57,0,0)
#define HAVE_AUBIO_LIBAVCODEC_DEPRECATED 1
#endif

#if LIBAVCODEC_VERSION_INT < AV_VERSION_INT(58,3,102)
#define HAVE_AUBIO_LIBAVCODEC_TIMEBASE_FIX 1
#endif

#if LIBAVCODEC_VERSION_INT < AV_VERSION_INT(55,28,1)
#warning "libavcodec < 56 is deprecated"
#define av_frame_alloc  avcodec_alloc_frame
#define av_frame_free avcodec_free_frame
#define av_packet_unref av_free_packet
#endif

#include "aubio_priv.h"
#include "fvec.h"
#include "fmat.h"
#include "ioutils.h"
#include "source_avcodec.h"

#if LIBAVCODEC_VERSION_INT < AV_VERSION_INT(56, 56, 0)
#define AUBIO_AVCODEC_MAX_BUFFER_SIZE FF_MIN_BUFFER_SIZE
#else
#define AUBIO_AVCODEC_MAX_BUFFER_SIZE AV_INPUT_BUFFER_MIN_SIZE
#endif

struct _aubio_source_avcodec_t {
  uint_t hop_size;
  uint_t samplerate;
  uint_t channels;

  // some data about the file
  char_t *path;
  uint_t input_samplerate;
  uint_t input_channels;

  // avcodec stuff
  AVFormatContext *avFormatCtx;
  AVCodecContext *avCodecCtx;
  AVFrame *avFrame;
  AVPacket avPacket;
#ifdef HAVE_AVRESAMPLE
  AVAudioResampleContext *avr;
#elif defined(HAVE_SWRESAMPLE)
  SwrContext *avr;
#endif
  smpl_t *output;
  uint_t read_samples;
  uint_t read_index;
  sint_t selected_stream;
  uint_t eof;
};

// create or re-create the context when _do or _do_multi is called
void aubio_source_avcodec_reset_resampler(aubio_source_avcodec_t * s);
// actually read a frame
void aubio_source_avcodec_readframe(aubio_source_avcodec_t *s,
    uint_t * read_samples);

uint_t aubio_source_avcodec_has_network_url(aubio_source_avcodec_t *s);

uint_t aubio_source_avcodec_has_network_url(aubio_source_avcodec_t *s) {
  char proto[20], authorization[256], hostname[128], uripath[256];
  int proto_size = 20, authorization_size = 256, hostname_size = 128,
      *port_ptr = 0, path_size = 256;
  av_url_split(proto, proto_size, authorization, authorization_size, hostname,
      hostname_size, port_ptr, uripath, path_size, s->path);
  if (strlen(proto)) {
    return 1;
  }
  return 0;
}


aubio_source_avcodec_t * new_aubio_source_avcodec(const char_t * path,
    uint_t samplerate, uint_t hop_size) {
  aubio_source_avcodec_t * s = AUBIO_NEW(aubio_source_avcodec_t);
  AVFormatContext *avFormatCtx = NULL;
  AVCodecContext *avCodecCtx = NULL;
  AVFrame *avFrame = NULL;
  sint_t selected_stream = -1;
#if FF_API_LAVF_AVCTX
  AVCodecParameters *codecpar;
#endif
  AVCodec *codec;
  uint_t i;
  int err;
  if (path == NULL) {
    AUBIO_ERR("source_avcodec: Aborted opening null path\n");
    goto beach;
  }
  if ((sint_t)samplerate < 0) {
    AUBIO_ERR("source_avcodec: Can not open %s with samplerate %d\n",
        path, samplerate);
    goto beach;
  }
  if ((sint_t)hop_size <= 0) {
    AUBIO_ERR("source_avcodec: Can not open %s with hop_size %d\n",
        path, hop_size);
    goto beach;
  }

  s->hop_size = hop_size;
  s->channels = 1;

  s->path = AUBIO_ARRAY(char_t, strnlen(path, PATH_MAX) + 1);
  strncpy(s->path, path, strnlen(path, PATH_MAX) + 1);

#if LIBAVFORMAT_VERSION_INT < AV_VERSION_INT(58,0,0)
  // register all formats and codecs
  av_register_all();
#endif

  if (aubio_source_avcodec_has_network_url(s)) {
    avformat_network_init();
  }

  // try opening the file and get some info about it
  avFormatCtx = NULL;
  if ( (err = avformat_open_input(&avFormatCtx, s->path, NULL, NULL) ) < 0 ) {
    char errorstr[256];
    av_strerror (err, errorstr, sizeof(errorstr));
    AUBIO_ERR("source_avcodec: Failed opening %s (%s)\n", s->path, errorstr);
    goto beach;
  }

  // try to make sure max_analyze_duration is big enough for most songs
#if FFMPEG_LIBAVFORMAT_MAX_DUR2
  avFormatCtx->max_analyze_duration2 *= 100;
#else
  avFormatCtx->max_analyze_duration *= 100;
#endif

  // retrieve stream information
  if ( (err = avformat_find_stream_info(avFormatCtx, NULL)) < 0 ) {
    char errorstr[256];
    av_strerror (err, errorstr, sizeof(errorstr));
    AUBIO_ERR("source_avcodec: Could not find stream information "
        "for %s (%s)\n", s->path, errorstr);
    goto beach;
  }

  // dump information about file onto standard error
  //av_dump_format(avFormatCtx, 0, s->path, 0);

  // look for the first audio stream
  for (i = 0; i < avFormatCtx->nb_streams; i++) {
#if FF_API_LAVF_AVCTX
    if (avFormatCtx->streams[i]->codecpar->codec_type == AVMEDIA_TYPE_AUDIO) {
#else
    if (avFormatCtx->streams[i]->codec->codec_type == AVMEDIA_TYPE_AUDIO) {
#endif
      if (selected_stream == -1) {
        selected_stream = i;
      } else {
        AUBIO_WRN("source_avcodec: More than one audio stream in %s, "
            "taking the first one\n", s->path);
      }
    }
  }
  if (selected_stream == -1) {
    AUBIO_ERR("source_avcodec: No audio stream in %s\n", s->path);
    goto beach;
  }
  //AUBIO_DBG("Taking stream %d in file %s\n", selected_stream, s->path);
  s->selected_stream = selected_stream;

#if FF_API_LAVF_AVCTX
  codecpar = avFormatCtx->streams[selected_stream]->codecpar;
  if (codecpar == NULL) {
    AUBIO_ERR("source_avcodec: Could not find decoder for %s", s->path);
    goto beach;
  }
  codec = avcodec_find_decoder(codecpar->codec_id);

  /* Allocate a codec context for the decoder */
  avCodecCtx = avcodec_alloc_context3(codec);
  if (!avCodecCtx) {
    AUBIO_ERR("source_avcodec: Failed to allocate the %s codec context "
        "for path %s\n", av_get_media_type_string(AVMEDIA_TYPE_AUDIO),
        s->path);
    goto beach;
  }
#else
  avCodecCtx = avFormatCtx->streams[selected_stream]->codec;
  codec = avcodec_find_decoder(avCodecCtx->codec_id);
#endif
  if (codec == NULL) {
    AUBIO_ERR("source_avcodec: Could not find decoder for %s", s->path);
    goto beach;
  }

#if FF_API_LAVF_AVCTX
  /* Copy codec parameters from input stream to output codec context */
  if ((err = avcodec_parameters_to_context(avCodecCtx, codecpar)) < 0) {
    AUBIO_ERR("source_avcodec: Failed to copy %s codec parameters to "
        "decoder context for %s\n",
        av_get_media_type_string(AVMEDIA_TYPE_AUDIO), s->path);
    goto beach;
  }
#if HAVE_AUBIO_LIBAVCODEC_TIMEBASE_FIX
  // avoids 'skipped frames warning' with avecodec < 58, deprecated after
  av_codec_set_pkt_timebase(avCodecCtx,
      avFormatCtx->streams[selected_stream]->time_base);
#endif
#endif

  if ( ( err = avcodec_open2(avCodecCtx, codec, NULL) ) < 0) {
    char errorstr[256];
    av_strerror (err, errorstr, sizeof(errorstr));
    AUBIO_ERR("source_avcodec: Could not load codec for %s (%s)\n", s->path,
        errorstr);
    goto beach;
  }

  /* get input specs */
  s->input_samplerate = avCodecCtx->sample_rate;
  s->input_channels   = avCodecCtx->channels;
  //AUBIO_DBG("input_samplerate: %d\n", s->input_samplerate);
  //AUBIO_DBG("input_channels: %d\n", s->input_channels);

  if (samplerate == 0) {
    s->samplerate = s->input_samplerate;
  } else {
    s->samplerate = samplerate;
  }

  if (s->samplerate >  s->input_samplerate) {
    AUBIO_WRN("source_avcodec: upsampling %s from %d to %d\n", s->path,
        s->input_samplerate, s->samplerate);
  }

  avFrame = av_frame_alloc();
  if (!avFrame) {
    AUBIO_ERR("source_avcodec: Could not allocate frame for (%s)\n", s->path);
  }

  /* allocate output for avr */
  s->output = (smpl_t *)av_malloc(AUBIO_AVCODEC_MAX_BUFFER_SIZE
      * sizeof(smpl_t));

  s->read_samples = 0;
  s->read_index = 0;

  s->avFormatCtx = avFormatCtx;
  s->avCodecCtx = avCodecCtx;
  s->avFrame = avFrame;

  aubio_source_avcodec_reset_resampler(s);

  if (s->avr == NULL) goto beach;

  s->eof = 0;

  //av_log_set_level(AV_LOG_QUIET);

  return s;

beach:
  //AUBIO_ERR("can not read %s at samplerate %dHz with a hop_size of %d\n",
  //    s->path, s->samplerate, s->hop_size);
  del_aubio_source_avcodec(s);
  return NULL;
}

void aubio_source_avcodec_reset_resampler(aubio_source_avcodec_t * s)
{
  // create or reset resampler to/from mono/multi-channel
  if ( s->avr == NULL ) {
    int err;
    int64_t input_layout = av_get_default_channel_layout(s->input_channels);
    int64_t output_layout = av_get_default_channel_layout(s->input_channels);
#ifdef HAVE_AVRESAMPLE
    AVAudioResampleContext *avr = avresample_alloc_context();
#elif defined(HAVE_SWRESAMPLE)
    SwrContext *avr = swr_alloc();
#endif /* HAVE_AVRESAMPLE || HAVE_SWRESAMPLE */

    av_opt_set_int(avr, "in_channel_layout",  input_layout,              0);
    av_opt_set_int(avr, "out_channel_layout", output_layout,             0);
    av_opt_set_int(avr, "in_sample_rate",     s->input_samplerate,       0);
    av_opt_set_int(avr, "out_sample_rate",    s->samplerate,             0);
    av_opt_set_int(avr, "in_sample_fmt",      s->avCodecCtx->sample_fmt, 0);
#if HAVE_AUBIO_DOUBLE
    av_opt_set_int(avr, "out_sample_fmt",     AV_SAMPLE_FMT_DBL,         0);
#else
    av_opt_set_int(avr, "out_sample_fmt",     AV_SAMPLE_FMT_FLT,         0);
#endif
    // TODO: use planar?
    //av_opt_set_int(avr, "out_sample_fmt",     AV_SAMPLE_FMT_FLTP,      0);
#ifdef HAVE_AVRESAMPLE
    if ( ( err = avresample_open(avr) ) < 0)
#elif defined(HAVE_SWRESAMPLE)
    if ( ( err = swr_init(avr) ) < 0)
#endif /* HAVE_AVRESAMPLE || HAVE_SWRESAMPLE */
    {
      char errorstr[256];
      av_strerror (err, errorstr, sizeof(errorstr));
      AUBIO_ERR("source_avcodec: Could not open resampling context"
         " for %s (%s)\n", s->path, errorstr);
      return;
    }
    s->avr = avr;
  }
}

void aubio_source_avcodec_readframe(aubio_source_avcodec_t *s,
    uint_t * read_samples)
{
  AVFormatContext *avFormatCtx = s->avFormatCtx;
  AVCodecContext *avCodecCtx = s->avCodecCtx;
  AVFrame *avFrame = s->avFrame;
  AVPacket avPacket = s->avPacket;
#ifdef HAVE_AVRESAMPLE
  AVAudioResampleContext *avr = s->avr;
#elif defined(HAVE_SWRESAMPLE)
  SwrContext *avr = s->avr;
#endif /* HAVE_AVRESAMPLE || HAVE_SWRESAMPLE */
  int got_frame = 0;
#ifdef HAVE_AVRESAMPLE
  int in_linesize = 0;
  int in_samples = avFrame->nb_samples;
  int out_linesize = 0;
  int max_out_samples = AUBIO_AVCODEC_MAX_BUFFER_SIZE;
  int out_samples = 0;
#elif defined(HAVE_SWRESAMPLE)
  int in_samples = avFrame->nb_samples;
  int max_out_samples = AUBIO_AVCODEC_MAX_BUFFER_SIZE / avCodecCtx->channels;
  int out_samples = 0;
#endif /* HAVE_AVRESAMPLE || HAVE_SWRESAMPLE */
  smpl_t *output = s->output;
#ifndef FF_API_LAVF_AVCTX
  int len = 0;
#else
  int ret = 0;
#endif
  av_init_packet (&avPacket);
  *read_samples = 0;

  do
  {
    int err = av_read_frame (avFormatCtx, &avPacket);
    if (err == AVERROR_EOF) {
      s->eof = 1;
      goto beach;
    }
    if (err != 0) {
      char errorstr[256];
      av_strerror (err, errorstr, sizeof(errorstr));
      AUBIO_ERR("source_avcodec: could not read frame in %s (%s)\n",
          s->path, errorstr);
      s->eof = 1;
      goto beach;
    }
  } while (avPacket.stream_index != s->selected_stream);

#if FF_API_LAVF_AVCTX
  ret = avcodec_send_packet(avCodecCtx, &avPacket);
  if (ret < 0 && ret != AVERROR_EOF) {
    AUBIO_ERR("source_avcodec: error when sending packet for %s\n", s->path);
    goto beach;
  }
  ret = avcodec_receive_frame(avCodecCtx, avFrame);
  if (ret >= 0) {
    got_frame = 1;
  }
  if (ret < 0) {
    if (ret == AVERROR(EAGAIN)) {
      //AUBIO_WRN("source_avcodec: output is not available right now - "
      //    "user must try to send new input\n");
      goto beach;
    } else if (ret == AVERROR_EOF) {
      AUBIO_WRN("source_avcodec: the decoder has been fully flushed, "
          "and there will be no more output frames\n");
    } else {
      AUBIO_ERR("source_avcodec: decoding errors on %s\n", s->path);
      goto beach;
    }
  }
#else
  len = avcodec_decode_audio4(avCodecCtx, avFrame, &got_frame, &avPacket);

  if (len < 0) {
    AUBIO_ERR("source_avcodec: error while decoding %s\n", s->path);
    goto beach;
  }
#endif
  if (got_frame == 0) {
    AUBIO_WRN("source_avcodec: did not get a frame when reading %s\n",
        s->path);
    goto beach;
  }

#if LIBAVUTIL_VERSION_MAJOR > 52
  if (avFrame->channels != (sint_t)s->input_channels) {
    AUBIO_WRN ("source_avcodec: trying to read from %d channel(s),"
        "but configured for %d; is '%s' corrupt?\n",
        avFrame->channels, s->input_channels, s->path);
    goto beach;
  }
#else
#warning "avutil < 53 is deprecated, crashes might occur on corrupt files"
#endif

#ifdef HAVE_AVRESAMPLE
  in_linesize = 0;
  av_samples_get_buffer_size(&in_linesize, avCodecCtx->channels,
      avFrame->nb_samples, avCodecCtx->sample_fmt, 1);
  in_samples = avFrame->nb_samples;
  out_linesize = 0;
  max_out_samples = AUBIO_AVCODEC_MAX_BUFFER_SIZE;
  out_samples = avresample_convert ( avr,
        (uint8_t **)&output, out_linesize, max_out_samples,
        (uint8_t **)avFrame->data, in_linesize, in_samples);
#elif defined(HAVE_SWRESAMPLE)
  in_samples = avFrame->nb_samples;
  max_out_samples = AUBIO_AVCODEC_MAX_BUFFER_SIZE / avCodecCtx->channels;
  out_samples = swr_convert( avr,
      (uint8_t **)&output, max_out_samples,
      (const uint8_t **)avFrame->data, in_samples);
#endif /* HAVE_AVRESAMPLE || HAVE_SWRESAMPLE */
  if (out_samples < 0) {
    AUBIO_WRN("source_avcodec: error while resampling %s (%d)\n",
        s->path, out_samples);
    goto beach;
  }

  *read_samples = out_samples;

beach:
  av_packet_unref(&avPacket);
}

void aubio_source_avcodec_do(aubio_source_avcodec_t * s, fvec_t * read_data,
    uint_t * read) {
  uint_t i, j;
  uint_t end = 0;
  uint_t total_wrote = 0;
  uint_t length = aubio_source_validate_input_length("source_avcodec", s->path,
      s->hop_size, read_data->length);
  if (!s->avr || !s->avFormatCtx || !s->avCodecCtx) {
    AUBIO_ERR("source_avcodec: could not read from %s (file was closed)\n",
        s->path);
    *read= 0;
    return;
  }
  while (total_wrote < length) {
    end = MIN(s->read_samples - s->read_index, length - total_wrote);
    for (i = 0; i < end; i++) {
      read_data->data[i + total_wrote] = 0.;
      for (j = 0; j < s->input_channels; j++) {
        read_data->data[i + total_wrote] +=
          s->output[(i + s->read_index) * s->input_channels + j];
      }
      read_data->data[i + total_wrote] *= 1./s->input_channels;
    }
    total_wrote += end;
    if (total_wrote < length) {
      uint_t avcodec_read = 0;
      aubio_source_avcodec_readframe(s, &avcodec_read);
      s->read_samples = avcodec_read;
      s->read_index = 0;
      if (s->eof) {
        break;
      }
    } else {
      s->read_index += end;
    }
  }

  aubio_source_pad_output(read_data, total_wrote);

  *read = total_wrote;
}

void aubio_source_avcodec_do_multi(aubio_source_avcodec_t * s,
    fmat_t * read_data, uint_t * read) {
  uint_t i,j;
  uint_t end = 0;
  uint_t total_wrote = 0;
  uint_t length = aubio_source_validate_input_length("source_avcodec", s->path,
      s->hop_size, read_data->length);
  uint_t channels = aubio_source_validate_input_channels("source_avcodec",
      s->path, s->input_channels, read_data->height);
  if (!s->avr || !s->avFormatCtx || !s->avCodecCtx) {
    AUBIO_ERR("source_avcodec: could not read from %s (file was closed)\n",
        s->path);
    *read= 0;
    return;
  }
  while (total_wrote < length) {
    end = MIN(s->read_samples - s->read_index, length - total_wrote);
    for (j = 0; j < channels; j++) {
      for (i = 0; i < end; i++) {
        read_data->data[j][i + total_wrote] =
          s->output[(i + s->read_index) * s->input_channels + j];
      }
    }
    total_wrote += end;
    if (total_wrote < length) {
      uint_t avcodec_read = 0;
      aubio_source_avcodec_readframe(s, &avcodec_read);
      s->read_samples = avcodec_read;
      s->read_index = 0;
      if (s->eof) {
        break;
      }
    } else {
      s->read_index += end;
    }
  }

  aubio_source_pad_multi_output(read_data, s->input_channels, total_wrote);

  *read = total_wrote;
}

uint_t aubio_source_avcodec_get_samplerate(const aubio_source_avcodec_t * s) {
  return s->samplerate;
}

uint_t aubio_source_avcodec_get_channels(const aubio_source_avcodec_t * s) {
  return s->input_channels;
}

uint_t aubio_source_avcodec_seek (aubio_source_avcodec_t * s, uint_t pos) {
  int64_t resampled_pos =
    (uint_t)ROUND(pos * (s->input_samplerate * 1. / s->samplerate));
  int64_t min_ts = MAX(resampled_pos - 2000, 0);
  int64_t max_ts = MIN(resampled_pos + 2000, INT64_MAX);
  int seek_flags = AVSEEK_FLAG_FRAME | AVSEEK_FLAG_ANY;
  int ret = AUBIO_FAIL;
  if (s->avFormatCtx != NULL && s->avr != NULL) {
    ret = AUBIO_OK;
  } else {
    AUBIO_ERR("source_avcodec: failed seeking in %s (file not opened?)",
        s->path);
    return ret;
  }
  if ((sint_t)pos < 0) {
    AUBIO_ERR("source_avcodec: could not seek %s at %d (seeking position"
       " should be >= 0)\n", s->path, pos);
    return AUBIO_FAIL;
  }
  ret = avformat_seek_file(s->avFormatCtx, s->selected_stream,
      min_ts, resampled_pos, max_ts, seek_flags);
  if (ret < 0) {
    AUBIO_ERR("source_avcodec: failed seeking to %d in file %s",
        pos, s->path);
  }
  // reset read status
  s->eof = 0;
  s->read_index = 0;
  s->read_samples = 0;
#ifdef HAVE_AVRESAMPLE
  // reset the AVAudioResampleContext
  avresample_close(s->avr);
  avresample_open(s->avr);
#elif defined(HAVE_SWRESAMPLE)
  swr_close(s->avr);
  swr_init(s->avr);
#endif
  return ret;
}

uint_t aubio_source_avcodec_get_duration (aubio_source_avcodec_t * s) {
  if (s && &(s->avFormatCtx) != NULL) {
    int64_t duration = s->avFormatCtx->duration;
    return s->samplerate * ((uint_t)duration / 1e6 );
  }
  return 0;
}

uint_t aubio_source_avcodec_close(aubio_source_avcodec_t * s) {
  if (s->avr != NULL) {
#ifdef HAVE_AVRESAMPLE
    avresample_close( s->avr );
    av_free ( s->avr );
#elif defined(HAVE_SWRESAMPLE)
    swr_close ( s->avr );
    swr_free ( &s->avr );
#endif
  }
  s->avr = NULL;
  if (s->avCodecCtx != NULL) {
#ifndef HAVE_AUBIO_LIBAVCODEC_DEPRECATED
    avcodec_free_context( &s->avCodecCtx );
#else
    avcodec_close ( s->avCodecCtx );
#endif
  }
  s->avCodecCtx = NULL;
  if (s->avFormatCtx != NULL) {
    avformat_close_input(&s->avFormatCtx);
    s->avFormatCtx = NULL;
  }
  av_packet_unref(&s->avPacket);
  return AUBIO_OK;
}

void del_aubio_source_avcodec(aubio_source_avcodec_t * s){
  AUBIO_ASSERT(s);
  aubio_source_avcodec_close(s);
  if (s->output != NULL) {
    av_free(s->output);
  }
  s->output = NULL;
  if (s->avFrame != NULL) {
    av_frame_free( &(s->avFrame) );
  }
  s->avFrame = NULL;
  if (s->path) {
    AUBIO_FREE(s->path);
  }
  s->path = NULL;
  AUBIO_FREE(s);
}

#endif /* HAVE_LIBAV */
