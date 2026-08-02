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
#include <chrono>
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
// 60 to match typical display refresh; 50 beat against 60Hz vsync,
// double-presenting then skipping frames (visible judder).
const float AudioProcessorRefreshRate = 60.0f;

// Spectrum display range and ballistics. Values are normalized 0..1 over
// FFTDecibelRange, so release/fall rates convert from dB/s via
// (dB/s) / FFTDecibelRange / AudioProcessorRefreshRate.
const float SpectrumFreqMin = 30.0f;
const float SpectrumFreqMaxLimit = 20000.0f;
const float SpectrumReleasePerFrame = 60.0f / FFTDecibelRange / AudioProcessorRefreshRate;
const float SpectrumPeakFallPerFrame = 45.0f / FFTDecibelRange / AudioProcessorRefreshRate;
const int SpectrumPeakHoldFrames = int(AudioProcessorRefreshRate / 2); // ~0.5s

/// Creates a Hann Window for FFT
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
    kiss_fftr_free(m_cfg);
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

void AudioProcessor::SetSampleRate(int sampleRate)
{
    if (sampleRate > 0)
    {
        m_sampleRate.store(sampleRate);
    }
}

void AudioProcessor::SetupFFT()
{
    m_processedAudio.m_samples[0].resize(FrameSamples, 0.0);
    m_processedAudio.m_samples[1].resize(FrameSamples, 0.0);
    m_processedAudio.m_monoSamples.resize(FrameSamples, 0.0);

    // Hann window
    m_window = createWindow(FrameSamples);
    m_totalWin = 0.0;
    for (auto& win : m_window)
    {
        m_totalWin += win;
    }

    // Real-input FFT: N real samples in, N/2+1 complex bins out.
    for (int i = 0; i < 2; i++)
    {
        m_fftIn[i].resize(FrameSamples, 0.0f);
        m_fftOut[i].resize(FrameSamples / 2 + 1);
        m_fftPower[i].resize(FrameSamples / 2 + 1);
    }

    m_cfg = kiss_fftr_alloc(FrameSamples, 0, 0, 0);
}

// Bucket edges (bin indices) log-spaced in frequency between
// SpectrumFreqMin and the Nyquist-clamped SpectrumFreqMaxLimit, so every
// octave gets equal display width.
void AudioProcessor::GenFreqPartitions(uint32_t buckets, int sampleRate)
{
    if (m_lastSpectrumPartitions == std::make_pair(buckets, uint32_t(sampleRate))
        && !m_spectrumPartitions.empty())
    {
        return;
    }
    m_lastSpectrumPartitions = std::make_pair(buckets, uint32_t(sampleRate));

    const float fHi = std::min(SpectrumFreqMaxLimit, sampleRate * 0.5f);
    const uint32_t maxBin = FrameSamples / 2;

    m_spectrumPartitions.resize(buckets + 1);
    uint32_t lastBin = 0;
    for (uint32_t k = 0; k <= buckets; k++)
    {
        float f = SpectrumFreqMin * std::pow(fHi / SpectrumFreqMin, k / float(buckets));
        uint32_t bin = uint32_t(f * FrameSamples / float(sampleRate));
        bin = std::max(bin, lastBin + 1);
        bin = std::min(bin, maxBin);
        m_spectrumPartitions[k] = bin;
        lastBin = bin;
    }
}

void AudioProcessor::CalculateFFT(ProcessedAudio& audio)
{
    if (!m_calculateFFT.load())
    {
        return;
    }

    if (m_fftOut[0].size() == 0)
    {
        return;
    }

    const uint32_t spectrumSamples = FrameSamples / 2;
    const int sampleRate = m_sampleRate.load();

    // Make less buckets on a big window, but at least 4
    uint32_t buckets = std::min(spectrumSamples / 8, uint32_t(m_maxBuckets.load()));
    buckets = std::max(buckets, uint32_t(4));

    GenFreqPartitions(buckets, sampleRate);

    // Consumed once per frame, ahead of the per-channel loop.
    const bool resetBallistics = m_resetSpectrum.exchange(false);
    const int silentGap = m_pendingSilentFrames;
    m_pendingSilentFrames = 0;

    audio.m_spectrumFreqMin = SpectrumFreqMin;
    audio.m_spectrumFreqMax = std::min(SpectrumFreqMaxLimit, sampleRate * 0.5f);

    for (int channel = 0; channel < 2; channel++)
    {
        for (uint32_t i = 0; i < FrameSamples; i++)
        {
            // Hann window * audio
            m_fftIn[channel][i] = audio.m_samples[channel][i] * m_window[i];
        }

        // Do the FFT
        kiss_fftr(m_cfg, &m_fftIn[channel][0], (kiss_fft_cpx*)&m_fftOut[channel][0]);

        // Sample 0 is the all frequency component
        m_fftOut[channel][0] = std::complex<float>(0.0f, 0.0f);

        for (uint32_t i = 0; i < spectrumSamples; i++)
        {
            // Amplitude-corrected power per bin; bucket averaging happens
            // in the power domain so narrow peaks read truthfully
            float amp = std::abs(m_fftOut[channel][i]) * 2.0f / m_totalWin;
            m_fftPower[channel][i] = amp * amp;
        }

        // Reset ballistics state when the bucket count changes, or when the
        // feed has just (re)started with stale state left from the last run
        if (m_bucketSmoothed[channel].size() != buckets || resetBallistics)
        {
            m_bucketSmoothed[channel].assign(buckets, 0.0f);
            m_bucketPeak[channel].assign(buckets, 0.0f);
            m_bucketPeakAge[channel].assign(buckets, 0);
        }
        else if (silentGap > 0)
        {
            // The stream stalls while the engine is self-paused on silence;
            // run the decay those frames would have delivered so the old
            // run's bars and peaks don't reappear when frames resume.
            for (uint32_t k = 0; k < buckets; k++)
            {
                float& s = m_bucketSmoothed[channel][k];
                s = std::max(0.0f, s - silentGap * SpectrumReleasePerFrame);
                int& age = m_bucketPeakAge[channel][k];
                const int falling =
                    std::max(0, age + silentGap - std::max(age, SpectrumPeakHoldFrames));
                age += silentGap;
                m_bucketPeak[channel][k] = std::max(
                    s, m_bucketPeak[channel][k] - falling * SpectrumPeakFallPerFrame);
            }
        }

        audio.m_spectrumQuantized[channel].resize(buckets);
        audio.m_spectrumPeaks[channel].resize(buckets);

        for (uint32_t k = 0; k < buckets; k++)
        {
            uint32_t b0 = m_spectrumPartitions[k];
            uint32_t b1 = std::max(b0 + 1, m_spectrumPartitions[k + 1]);
            b1 = std::min(b1, spectrumSamples);
            b0 = std::min(b0, b1 - 1);

            float power = 0.0f;
            for (uint32_t i = b0; i < b1; i++)
            {
                power += m_fftPower[channel][i];
            }
            power /= float(b1 - b0);
            power = std::max(power, std::numeric_limits<float>::min());

            // dB (power domain), normalized to 0..1 over FFTDecibelRange
            float v = 10.0f * std::log10(power);
            v = (v + FFTDecibelRange) / FFTDecibelRange;
            v = std::min(1.0f, std::max(0.0f, v));

            // Instant attack, timed release
            float smoothed = std::max(v, m_bucketSmoothed[channel][k] - SpectrumReleasePerFrame);
            m_bucketSmoothed[channel][k] = smoothed;
            audio.m_spectrumQuantized[channel][k] = smoothed;

            // Peak-hold: sit at the recent maximum, then fall slowly
            if (v >= m_bucketPeak[channel][k])
            {
                m_bucketPeak[channel][k] = v;
                m_bucketPeakAge[channel][k] = 0;
            }
            else if (++m_bucketPeakAge[channel][k] > SpectrumPeakHoldFrames)
            {
                m_bucketPeak[channel][k] = std::max(smoothed,
                    m_bucketPeak[channel][k] - SpectrumPeakFallPerFrame);
            }
            audio.m_spectrumPeaks[channel][k] = m_bucketPeak[channel][k];
        }
    }
}

shm_audio_buffer* AudioProcessor::GetAudioBufferSlot(unsigned int slot)
{
    if (!m_shmClient) return nullptr;
    return m_shmClient->get_audio_buffer(slot);
}

shm_scope_stream_reader AudioProcessor::GetScopeReader(unsigned int index)
{
    if (!m_shmClient) return shm_scope_stream_reader();
    return m_shmClient->get_scope_stream_reader(index);
}

sample_clock_view AudioProcessor::GetSampleClock()
{
    if (!m_shmClient) return sample_clock_view();
    return m_shmClient->get_sample_clock();
}

const std::atomic<uint32_t>* AudioProcessor::GetMetrics()
{
    if (!m_shmClient) return nullptr;
    return m_shmClient->get_metrics_flat();
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
    return m_shmClient->get_nrt_out_ring();
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

bool AudioProcessor::HasNativeStats()
{
    return m_shmClient && m_shmClient->has_native_stats();
}

// Re-opens the shm segment and re-binds the scope reader. The segment
// is owned by supersonic and survives cold swaps; re-opening by name
// covers both in-place cold swap and supersonic-restart. Validity
// transitions are logged from Run() (see m_shmReaderLastValid).
void AudioProcessor::ResetConnection()
{
    // Pairs with Run()'s lock: the reset destroys the client (unmapping the
    // segment), which must not interleave with the scope thread's reads.
    std::lock_guard<std::mutex> lock(m_mutex);
    AttachLocked();
}

// Attach body. Caller holds m_mutex — Run() retries from inside its own lock.
void AudioProcessor::AttachLocked()
{
    try
    {
        m_shmClient.reset(new server_shared_memory_client(m_scSynthPort));
        m_shmReader = m_shmClient->get_scope_stream_reader(0);
        m_lastAttachError.clear();   // a later recurrence is worth reporting again
    }
    catch (const std::exception& e)
    {
        // Two very different failures land here.
        //
        // The segment simply not existing yet is the boot race Run() retries
        // out of — silent by design, and the common case.
        //
        // But a segment that EXISTS and fails validation (bad MAGIC, wrong
        // size, layout mismatch) is version skew between the engine binary and
        // this reader. No amount of retrying resolves it: the scope, node
        // tree, metrics and debug rings just stay silently empty while audio
        // keeps working over OSC, which reads as a GUI bug rather than a build
        // one. Say so, once.
        const std::string what = e.what();
        if (!sonic_pi::audio::IsShmAttachRetryable(what) && what != m_lastAttachError)
        {
            LOG(ERR, "Shared memory attach failed: "
                         << what
                         << " | The scope, node tree and debug panes will stay empty. "
                            "The engine binary and this GUI were most likely built from "
                            "different sources — rebuild both from the same tree.");
            m_lastAttachError = what;
        }
        m_shmClient.reset();
        m_shmReader = shm_scope_stream_reader();
    }
    m_lastEndCursor = 0;
    m_stalledTicks = 0;
    m_pendingSilentFrames = 0;
    // A new session's spectrum starts from rest, not the old session's decay.
    m_resetSpectrum.store(true);

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

        // m_mutex: ResetConnection() (GUI thread) destroys and reassigns
        // m_shmClient/m_shmReader; every access to them on this thread must
        // hold the lock or the client can be unmapped mid-call.
        std::unique_lock<std::mutex> lock(m_mutex);

        // Unattached: keep trying, once a second. The lifecycle triggers
        // (onSpiderReady / onSupersonicSetup) are one-shot, so a client that
        // misses both — spider announcing ready before this processor exists,
        // or supersonic restarting without a cold-swap setup — would otherwise
        // stay detached forever, leaving scope, node tree, metrics and the
        // ring readers permanently empty.
        if (!m_shmClient)
        {
            const auto now = std::chrono::steady_clock::now();
            if (now - m_lastAttachAttempt >= std::chrono::seconds(1))
            {
                m_lastAttachAttempt = now;
                AttachLocked();
                if (m_shmClient)
                    LOG(INFO, "Shared memory segment attached");
            }
        }

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
            lock.unlock();
            std::this_thread::sleep_for(std::chrono::seconds(1));
            continue;
        }

        if (!nowValid)
        {
            lock.unlock();
            std::this_thread::sleep_until(nextTime);
            continue;
        }

        // If the GUI hasn't consumed the previous frame yet, yield the
        // rest of this slice. Avoids spinning while the UI is busy.
        if (!m_consumed.load())
        {
            lock.unlock();
            std::this_thread::sleep_until(nextTime);
            continue;
        }

        // Window the stream on what the listener is hearing, via the
        // engine's sample clock.
        const uint64_t endCursor =
            (m_shmClient ? m_shmClient->get_sample_clock() : sample_clock_view())
                .audible_end(m_shmReader);

        if (endCursor != m_lastEndCursor)
        {
            m_pendingSilentFrames = m_stalledTicks;
            m_stalledTicks = 0;
            m_lastEndCursor = endCursor;
            // Scratch sized for the max stride; copy_window reports the one
            // it used (a separate channels() read would race re-activation).
            m_windowScratch.resize(static_cast<size_t>(FrameSamples) * SHM_SCOPE_STREAM_CHANNELS);
            uint32_t ch = 1;
            m_shmReader.copy_window(endCursor, FrameSamples, m_windowScratch.data(), &ch);

            for (unsigned int i = 0; i < FrameSamples; ++i)
            {
                const float l = m_windowScratch[i * ch];
                const float r = m_windowScratch[i * ch + (ch - 1)];
                m_processedAudio.m_samples[0][i] = l;
                m_processedAudio.m_samples[1][i] = r;
                // Mono display curve: RMS-of-offset-squares (the mono
                // scope's established shape — keep as-is).
                const double dl = l + 1.0;
                const double dr = r + 1.0;
                m_processedAudio.m_monoSamples[i] =
                    float(sqrt((dl * dl + dr * dr) / 2.0) - 1.0);
            }

            CalculateFFT(m_processedAudio);
            // One copy, made on this thread; the GUI shares the snapshot
            // instead of copying it again through the queued connection.
            m_pClient->AudioDataAvailable(
                std::make_shared<const ProcessedAudio>(m_processedAudio));
        }
        else if (m_stalledTicks < int(AudioProcessorRefreshRate) * 60)
        {
            m_stalledTicks++;
        }

        lock.unlock();
        std::this_thread::sleep_until(nextTime);
    }

    LOG(DBG, "Shutting down audio thread");
}

void AudioProcessor::Enable(bool enable)
{
    SetConsumed(true);
    const bool was = m_running.exchange(enable);
    if (enable && !was)
    {
        m_resetSpectrum.store(true);
    }
}

void AudioProcessor::EnableFFT(bool enable)
{
    SetConsumed(true);
    const bool was = m_calculateFFT.exchange(enable);
    if (enable && !was)
    {
        m_resetSpectrum.store(true);
    }
}

} // namespace SonicPi
