/* Copyright (c) 2017 Google Inc. */
/*
   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   - Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

   - Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "opus.h"
#include "opus_types.h"

#define MAX_FRAME_SAMP 5760
#define MAX_PACKET 1500

/* 4 bytes: packet length, 4 bytes: encoder final range */
#define SETUP_BYTE_COUNT 8

typedef struct {
    int fs;
    int channels;
} TocInfo;

static void ParseToc(const uint8_t *toc, TocInfo *const info) {
    const int samp_freqs[5] = {8000, 12000, 16000, 24000, 48000};
    const int bandwidth = opus_packet_get_bandwidth(toc);

    info->fs = samp_freqs[bandwidth - OPUS_BANDWIDTH_NARROWBAND];
    info->channels = opus_packet_get_nb_channels(toc);
}

/* Treats the input data as concatenated packets encoded by opus_demo,
 * structured as
 *    bytes 0..3: packet length
 *    bytes 4..7: encoder final range
 *    bytes 8+  : Opus packet, including ToC
 */
int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    OpusDecoder *dec;
    opus_int16 *pcm;
    uint8_t *packet;
    TocInfo toc;
    int i, err;

    /* Not enough data to setup the decoder (+1 for the ToC) */
    if (size < SETUP_BYTE_COUNT + 1) {
        return 0;
    }

    /* Create decoder based on info from the first ToC available */
    ParseToc(&data[SETUP_BYTE_COUNT], &toc);

    dec = opus_decoder_create(toc.fs, toc.channels, &err);
    if (err != OPUS_OK | dec == NULL) {
        return 0;
    }

    pcm = (opus_int16*) malloc(sizeof(*pcm) * MAX_FRAME_SAMP * toc.channels);
    packet = (uint8_t*) calloc(MAX_PACKET, sizeof(*packet));

    i = 0;
    while (1) {
        int len, fec;

        if (i + SETUP_BYTE_COUNT >= size) {
            break;
        }

        len = (opus_uint32) data[i    ] << 24 |
              (opus_uint32) data[i + 1] << 16 |
              (opus_uint32) data[i + 2] <<  8 |
              (opus_uint32) data[i + 3];
        if (len > MAX_PACKET || len < 0) {
            break;
        }

        /* Bytes 4..7 represent encoder final range, but are unused here.
         * Instead, byte 4 is repurposed to determine if FEC is used. */
        fec = data[i + 4] & 1;

        /* Lost packet */
        if (len == 0) {
            int frame_size;
            opus_decoder_ctl(dec, OPUS_GET_LAST_PACKET_DURATION(&frame_size));
            (void) opus_decode(dec, NULL, size, pcm, frame_size, fec);
        } else {
            if (i + SETUP_BYTE_COUNT + len > size) {
                break;
            }
            memcpy(pcm, &data[i + SETUP_BYTE_COUNT], len);
            (void) opus_decode(dec, data, size, pcm, MAX_FRAME_SAMP, fec);
        }

        i += SETUP_BYTE_COUNT + len;
    }

    opus_decoder_destroy(dec);
    free(pcm);
    free(packet);

    return 0;
}
