//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// API audio processing thread for clients to get waveform data
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include <algorithm>
#include <cmath>
#include <set>
#include <vector>

#include "api/audio/audio_processor.h"
#include "api/logger.h"
#include "api/sonicpi_api.h"

namespace
{
const int FrameSamples = 4096;
const float FFTDecibelRange = 70.0f;
const float AudioProcessorRefreshRate = 50.0f;

/// Creates a Hamming Window for FFT
/// FFT requires a window function to get smooth results
inline std::vector<float> createWindow(uint32_t size)
{
    const float PI = float(std::atan(1.0) * 4);
    std::vector<float> ret(size);
    for (uint32_t i = 0; i < size; i++)
    {
        ret[i] = (0.5f * (1 - cos(2.0f * PI * (i / (float)(size - 1)))));
    }

    return ret;
}
} // namespace

namespace SonicPi
{

AudioProcessor::AudioProcessor(SonicPi::IAPIClient* pClient, int synthPort)
    : m_scSynthPort(synthPort)
    , m_pClient(pClient)
{
    SetupFFT();
    m_thread = std::thread([&]() {
        Run();
    });
}

AudioProcessor::~AudioProcessor()
{
    Quit();
}

ProcessedAudio& AudioProcessor::GetCurrentProcessedAudio()
{
    return m_processedAudio;
}

void AudioProcessor::Quit()
{
    m_quit.store(true);
    m_thread.join();
}

void AudioProcessor::SetConsumed(bool consumed)
{
    m_consumed.store(consumed);
}

void AudioProcessor::SetMaxBuckets(int maxBuckets)
{
    m_maxBuckets.store(maxBuckets);
}

void AudioProcessor::SetupFFT()
{
    m_processedAudio.m_samples[0].resize(FrameSamples, 0.0);
    m_processedAudio.m_samples[1].resize(FrameSamples, 0.0);
    m_processedAudio.m_monoSamples.resize(FrameSamples, 0.0);

    // FFT output is half the size of the input
    m_processedAudio.m_spectrum[0].resize(FrameSamples / 2, (0));
    m_processedAudio.m_spectrum[1].resize(FrameSamples / 2, (0));

    // Hamming window
    m_window = createWindow(FrameSamples);
    m_totalWin = 0.0;
    for (auto& win : m_window)
    {
        m_totalWin += win;
    }

    // Imaginary part of audio input always 0.
    for (int i = 0; i < 2; i++)
    {
        m_fftIn[i].resize(FrameSamples, std::complex<float>{ 0.0, 0.0 });
        m_fftOut[i].resize(FrameSamples);
        m_fftMag[i].resize(FrameSamples);
    }

    m_cfg = kiss_fft_alloc(FrameSamples, 0, 0, 0);
}

// Generate a sequentially increasing space of numbers.
// The idea here is to generate partitions of the frequency spectrum that cover the whole
// range of values, but concentrate the results on the 'interesting' frequencies at the bottom end
// This code ported from the python below:
// https://stackoverflow.com/questions/12418234/logarithmically-spaced-integers
void AudioProcessor::GenLogSpace(uint32_t limit, uint32_t n)
{
    if (m_lastSpectrumPartitions == std::make_pair(limit, n) && !m_spectrumPartitions.empty())
    {
        return;
    }

    // Remember what we did last
    m_lastSpectrumPartitions = std::make_pair(limit, n);

    m_spectrumPartitions.clear();

    // Generate buckets using a power factor, with each bucket advancing on the last
    uint32_t lastValue = 0;
    for (float fVal = 0.0f; fVal <= 1.0f; fVal += 1.0f / float(n))
    {
        const float curveSharpness = 8.0f;
        auto step = uint32_t(limit * std::pow(fVal, curveSharpness));
        step = std::max(step, lastValue + 1);
        lastValue = step;
        m_spectrumPartitions.push_back(float(step));
    }
}

// This is a simple linear partitioning of the frequencies
void AudioProcessor::GenLinSpace(uint32_t limit, uint32_t n)
{
    if (m_lastSpectrumPartitions == std::make_pair(limit, n) && !m_spectrumPartitions.empty())
    {
        return;
    }

    // Remember what we did last
    m_lastSpectrumPartitions = std::make_pair(limit, n);

    m_spectrumPartitions.resize(n);
    for (uint32_t i = 0; i < n; i++)
    {
        m_spectrumPartitions[i] = float(n) / (float(limit));
    }
}

void AudioProcessor::CalculateFFT(ProcessedAudio& audio)
{
    if (!m_calculateFFT.load())
    {
        return;
    }

    // Magnitude
    const float ref = 1.0f; // Source reference value, but we are +/1.0f

    if (m_fftOut[0].size() == 0)
    {
        return;
    }

    for (int channel = 0; channel < 2; channel++)
    {
        for (uint32_t i = 0; i < FrameSamples; i++)
        {
            // Hamming window * audio
            m_fftIn[channel][i] = std::complex<float>(audio.m_samples[channel][i] * m_window[i], 0.0f);
        }

        // Do the FFT
        kiss_fft(m_cfg, (const kiss_fft_cpx*)&m_fftIn[channel][0], (kiss_fft_cpx*)&m_fftOut[channel][0]);

        // Sample 0 is the all frequency component
        m_fftOut[channel][0] = std::complex<float>(0.0f, 0.0f);

        for (uint32_t i = 0; i < FrameSamples / 2; i++)
        {
            // Magnitude
            m_fftMag[channel][i] = std::abs(m_fftOut[channel][i]);

            audio.m_spectrum[channel][i] = m_fftMag[channel][i] * 2.0f / m_totalWin;
            audio.m_spectrum[channel][i] = std::max(audio.m_spectrum[channel][i], std::numeric_limits<float>::min());

            // Log based on a reference value of 1
            audio.m_spectrum[channel][i] = 20 * std::log10(audio.m_spectrum[channel][i] / ref);

            // Normalize by moving up and dividing
            // Decibels are now positive from 0->1;
            audio.m_spectrum[channel][i] += FFTDecibelRange;
            audio.m_spectrum[channel][i] /= FFTDecibelRange;
            audio.m_spectrum[channel][i] = std::max(0.0f, audio.m_spectrum[channel][i]);
            audio.m_spectrum[channel][i] = std::min(1.0f, audio.m_spectrum[channel][i]);
        }

        // Quantize into bigger buckets; filtering helps smooth the graph, and gives a more pleasant effect
        uint32_t SpectrumSamples = uint32_t(audio.m_spectrum[channel].size());

        // Make less buckets on a big window, but at least 4
        uint32_t buckets = std::min(SpectrumSamples / 8, uint32_t(m_maxBuckets.load()));
        buckets = std::max(buckets, uint32_t(4));

        // Linear space shows lower frequencies, log space shows all freqencies but focused
        // on the lower buckets more
//#define LINEAR_SPACE
#ifdef LINEAR_SPACE
        GenLinSpace(SpectrumSamples / 4, buckets);
#else
        GenLogSpace(SpectrumSamples, buckets);
#endif
        auto itrPartition = m_spectrumPartitions.begin();

        if (buckets > 0)
        {
            float countPerBucket = (float)SpectrumSamples / (float)buckets;
            uint32_t currentBucket = 0;

            float av = 0.0f;
            uint32_t averageCount = 0;

            audio.m_spectrumQuantized[channel].resize(buckets);

            // Ignore the first spectrum sample
            for (uint32_t i = 1; i < SpectrumSamples; i++)
            {
                av += audio.m_spectrum[channel][i];
                averageCount++;

                if (i >= *itrPartition)
                {
                    audio.m_spectrumQuantized[channel][currentBucket++] = av / (float)averageCount;
                    av = 0.0f; // reset sum for next average
                    averageCount = 0;
                    itrPartition++;
                }

                // Sanity
                if (itrPartition == m_spectrumPartitions.end()
                    || currentBucket >= buckets)
                    break;
            }
        }
    }
}

shm_audio_buffer* AudioProcessor::GetAudioBufferSlot(unsigned int slot)
{
    if (!m_shmClient) return nullptr;
    return m_shmClient->get_audio_buffer(slot);
}

const std::atomic<uint32_t>* AudioProcessor::GetMetrics()
{
    if (!m_shmClient) return nullptr;
    return m_shmClient->get_metrics();
}

ring_view AudioProcessor::GetInRing()
{
    if (!m_shmClient) return ring_view{};
    return m_shmClient->get_in_ring();
}

ring_view AudioProcessor::GetOutRing()
{
    if (!m_shmClient) return ring_view{};
    return m_shmClient->get_out_ring();
}

ring_view AudioProcessor::GetDebugRing()
{
    if (!m_shmClient) return ring_view{};
    return m_shmClient->get_debug_ring();
}

node_tree_view AudioProcessor::GetNodeTree()
{
    if (!m_shmClient) return node_tree_view{};
    return m_shmClient->get_node_tree();
}

native_stats AudioProcessor::GetNativeStats()
{
    if (!m_shmClient) return native_stats{};
    return m_shmClient->get_native_stats();
}

// Re-opens the shm segment and re-binds the scope reader. The segment
// is owned by supersonic and survives cold swaps; re-opening by name
// covers both in-place cold swap and supersonic-restart. Validity
// transitions are logged from Run() (see m_shmReaderLastValid).
void AudioProcessor::ResetConnection()
{
    try
    {
        m_shmClient.reset(new server_shared_memory_client(m_scSynthPort));
        m_shmReader = m_shmClient->get_scope_buffer_reader(0);
    }
    catch (const std::exception&)
    {
        // Segment not yet created by supersonic — expected at boot races.
        m_shmClient.reset();
        m_shmReader = shm_scope_buffer_reader();
    }

    // Clear any stale consumed=false left from a prior torn-down session.
    SetConsumed(true);
}

// Consumer-side loop. ResetConnection is called externally from the GUI
// lifecycle (onSpiderReady / onSupersonicSetup); pull() returns 0 frames
// gracefully whether the scope buffer is mid-reallocation or supersonic
// is simply silent.
void AudioProcessor::Run()
{
    for (;;)
    {
        if (m_quit.load()) break;

        auto startTime = std::chrono::high_resolution_clock::now();
        auto nextTime = startTime + std::chrono::milliseconds(int(1000.0f / AudioProcessorRefreshRate));

        // Log validity transitions once per change. The buffer the
        // reader points at transitions free → initialized asynchronously
        // when spider's Phase 5 runs. Above the m_running gate so the
        // status is logged whether or not the scope window is open.
        const bool nowValid = m_shmReader.valid();
        if (nowValid != m_shmReaderLastValid)
        {
            if (nowValid)
            {
                LOG(INFO, "Scope reader attached (scope buffer ready)");
            }
            else
            {
                LOG(INFO, "Scope reader unavailable "
                          "(scope buffer not initialised — studio "
                          "booting, mid-cold-swap, or wedged)");
            }
            m_shmReaderLastValid = nowValid;
        }

        if (!m_running.load())
        {
            std::this_thread::sleep_for(std::chrono::seconds(1));
            continue;
        }

        if (!nowValid)
        {
            std::this_thread::sleep_until(nextTime);
            continue;
        }

        // If the GUI hasn't consumed the previous frame yet, yield the
        // rest of this slice. Avoids spinning while the UI is busy.
        if (!m_consumed.load())
        {
            std::this_thread::sleep_until(nextTime);
            continue;
        }

        unsigned int frames;
        if (m_shmReader.pull(frames))
        {
            float* data = m_shmReader.data();
            for (unsigned int j = 0; j < 2; ++j)
            {
                unsigned int offset = m_shmReader.max_frames() * j;
                for (unsigned int i = 0; i < FrameSamples - frames; ++i)
                {
                    m_processedAudio.m_samples[j][i] = m_processedAudio.m_samples[j][i + frames];
                    if (j == 0)
                    {
                        m_processedAudio.m_monoSamples[i] = m_processedAudio.m_monoSamples[i + frames];
                    }
                }

                for (unsigned int i = 0; i < frames; ++i)
                {
                    m_processedAudio.m_samples[j][FrameSamples - frames + i] = data[i + offset];
                    auto d = data[i + offset] + 1.0;
                    if (j == 0)
                    {
                        m_processedAudio.m_monoSamples[FrameSamples - frames + i] = float(d * d);
                    }
                    else
                    {
                        m_processedAudio.m_monoSamples[FrameSamples - frames + i] += float(d * d);
                        m_processedAudio.m_monoSamples[FrameSamples - frames + i] /= 2.0f;
                        m_processedAudio.m_monoSamples[FrameSamples - frames + i] = sqrt(m_processedAudio.m_monoSamples[FrameSamples - frames + i]) - 1.0f;
                    }
                }
            }

            CalculateFFT(m_processedAudio);
            m_pClient->AudioDataAvailable(m_processedAudio);
        }

        std::this_thread::sleep_until(nextTime);
    }

    LOG(DBG, "Shutting down audio thread");
}

void AudioProcessor::Enable(bool enable)
{
    SetConsumed(true);
    m_running.store(enable);
}

void AudioProcessor::EnableFFT(bool enable)
{
    SetConsumed(true);
    m_calculateFFT.store(enable);
}

} // namespace SonicPi
