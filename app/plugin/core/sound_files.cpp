// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
#include "sound_files.h"

#include <aubio.h>
#include <clockwork_audio_file.h>

namespace SonicPi::sound_files
{

bool describe(const std::string& path, SoundFacts& out)
{
    ClockworkAudioInfo info{};
    info.struct_bytes = sizeof info;
    if (clockwork_audio_probe_file(path.c_str(), &info) != CLOCKWORK_OK) return false;
    if (info.channels == 0 || info.sample_rate == 0) return false;
    out.frames = int64_t(info.frames);
    out.channels = int(info.channels);
    out.sampleRate = int(info.sample_rate);
    return true;
}

std::optional<std::vector<double>> onsets(const std::string& path)
{
    ClockworkAudioInfo info{};
    info.struct_bytes = sizeof info;
    float* interleaved = nullptr;
    if (clockwork_audio_decode_file(path.c_str(), &info, &interleaved) != CLOCKWORK_OK || !interleaved) return std::nullopt;
    const uint32_t channels = info.channels ? info.channels : 1;
    const uint64_t frames = info.frames;
    const uint32_t rate = info.sample_rate ? info.sample_rate : 44100;

    // aubio_onset's defaults as Sonic Pi's aubio_onset program fixed them
    const uint_t winSize = 1024, hopSize = 512;
    aubio_onset_t* o = new_aubio_onset(const_cast<char_t*>("default"), winSize, hopSize, rate);
    if (!o)
    {
        clockwork_audio_free(interleaved);
        return std::nullopt;
    }
    aubio_onset_set_threshold(o, 0.3f);
    aubio_onset_set_minioi_s(o, 0.012f);
    fvec_t* in = new_fvec(hopSize);
    fvec_t* onset = new_fvec(1);
    std::vector<double> out;
    // hop by hop, as aubio's source reads the file: each channel's mean, the last block padded with silence
    for (uint64_t at = 0; at < frames; at += hopSize)
    {
        for (uint_t k = 0; k < hopSize; k++)
        {
            const uint64_t f = at + k;
            float v = 0.0f;
            if (f < frames)
            {
                for (uint32_t c = 0; c < channels; c++) v += interleaved[f * channels + c];
                v /= float(channels);
            }
            in->data[k] = v;
        }
        aubio_onset_do(o, in, onset);
        if (onset->data[0] != 0.0f) out.push_back(double(aubio_onset_get_last_s(o)));
    }
    del_fvec(onset);
    del_fvec(in);
    del_aubio_onset(o);
    clockwork_audio_free(interleaved);
    return out;
}

} // namespace SonicPi::sound_files
