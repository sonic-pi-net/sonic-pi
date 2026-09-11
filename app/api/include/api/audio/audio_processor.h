//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2016 by Adrian Cheater
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#pragma once

#include <complex>
#include <memory>
#include <string>
#include <thread>
#include <mutex>
#include <vector>
#include "server_shm.hpp"

#include "kiss_fftr.h"

#include "api/audio/shm_attach_diagnostics.hpp"
#include "api/sonicpi_api.h"

namespace SonicPi
{

struct IAPIClient;

class AudioProcessor
{
public:
    AudioProcessor(IAPIClient* pAPI, int synthPort);
    ~AudioProcessor();

    void Run();

    ProcessedAudio& GetCurrentProcessedAudio();
    void EnableFFT(bool enable);
    void Enable(bool start);
    void SetConsumed(bool consumed);
    void SetMaxBuckets(int maxBuckets);
    void SetSampleRate(int sampleRate);
    void Quit();
    // Force re-attach to the scope shared memory — public so callers can
    // invoke after a cold-swap device change to refresh the stale reader.
    void ResetConnection();

    // Direct pointer to one of the engine's audio taps: SHM_AUDIO_OUT_SLOT
    // is the master mix as it left for the device, SHM_AUDIO_IN_SLOT what
    // arrived from it, both written by the engine from boot. Returns
    // nullptr if the shm client is not initialised. The pointer is into
    // the engine's shm mapping, so callers must keep the owning
    // AudioProcessor alive for its lifetime.
    shm_audio_buffer* GetAudioBufferSlot(unsigned int slot);

    // Reader onto scope stream slot `index`. Slot 0 is the master scope
    // (the full mix); slots 1..N are fed by fx_scope_out / ScopeOut2 taps.
    // Invalid reader if the shm client is not connected or the index is out
    // of range. Same lifetime caveat as GetAudioBufferSlot — points into
    // supersonic's shm mapping.
    shm_scope_stream_reader GetScopeReader(unsigned int index);

    // Seqlock snapshot of the engine sample clock (sample position ↔ DAC NTP);
    // invalid when disconnected or before the engine's first publish.
    sample_clock_view GetSampleClock();

    // Flat pointer to the PerformanceMetrics region in supersonic's shm
    // mapping (METRICS_FIELD_COUNT contiguous uint32 fields), or nullptr
    // if the shm client is not connected. Same lifetime caveat as
    // GetAudioBufferSlot — the pointer is owned by this AudioProcessor.
    const std::atomic<uint32_t>* GetMetrics();

    // Passive views onto the OSC/debug transport rings and the node-tree
    // mirror, for the SuperSonic observability panel. Empty (null bases) when
    // the shm client is not connected. Call on the GUI thread (same thread as
    // ResetConnection); the views point into the shm mapping.
    ring_view GetInRing();      // OSC host→engine (what Sonic Pi sent)
    ring_view GetOutRing();     // OSC engine→host (replies)
    ring_view GetDebugRing();   // engine debug/log text
    node_tree_view GetNodeTree();
    native_stats GetNativeStats();  // synthdef count, allocated buffers + bytes
    bool HasNativeStats();          // false on a web-origin segment (render "-", not 0)

private:
    void GenFreqPartitions(uint32_t buckets, int sampleRate);
    void SetupFFT();
    void CalculateFFT(ProcessedAudio& audio);

private:
    // Opens the segment and binds the scope reader. Caller holds m_mutex.
    void AttachLocked();

    std::unique_ptr<shm_segment_client> m_shmClient;
    shm_scope_stream_reader m_shmReader;

    // Rate-limits Run()'s reattach attempts while unattached.
    std::chrono::steady_clock::time_point m_lastAttachAttempt{};
    // Previous validity for transition-only logging in Run().
    bool m_shmReaderLastValid = false;
    // Last non-retryable attach failure, so a permanent one is reported once
    // rather than on every retry tick (see AttachLocked).
    std::string m_lastAttachError;
    // Slot-local cursor of the last emitted window (skip repaints when the
    // stream hasn't advanced) and the interleaved copy-out scratch.
    uint64_t m_lastEndCursor = 0;
    std::vector<float> m_windowScratch;

    int m_scSynthPort = 0;

    std::atomic<bool> m_calculateFFT = { false };
    std::atomic<bool> m_running = { false };
    std::atomic<int> m_maxBuckets = { 0 };
    std::atomic<bool> m_quit = { false };
    std::atomic<bool> m_consumed = { false };
    std::atomic<int> m_sampleRate = { 48000 };

    // FFT (real-input: N real samples in, N/2+1 complex bins out)
    kiss_fftr_cfg m_cfg;
    std::vector<kiss_fft_scalar> m_fftIn[2];
    std::vector<std::complex<float>> m_fftOut[2];
    // Per-bin power (|X|^2 amplitude-corrected), bucket-averaged in the
    // power domain before conversion to dB
    std::vector<float> m_fftPower[2];
    std::vector<float> m_window;
    // Bucket edges in bin space (buckets+1 entries), log-spaced in frequency
    std::vector<uint32_t> m_spectrumPartitions;
    std::pair<uint32_t, uint32_t> m_lastSpectrumPartitions = { 0, 0 };

    // Display ballistics: instant attack / timed release per bucket, plus
    // slowly-falling peak-hold markers
    std::vector<float> m_bucketSmoothed[2];
    std::vector<float> m_bucketPeak[2];
    std::vector<int> m_bucketPeakAge[2];
    // One-shot: zero the ballistics before the next FFT pass. Set when the
    // feed or FFT is (re)started, so the spectrum rises from rest instead of
    // resuming the decay of whatever the previous session last showed.
    std::atomic<bool> m_resetSpectrum = { false };
    // Consumer-thread only: ticks the frame stream has been stalled (the
    // engine pauses its sample clock when silent, freezing the ballistics
    // mid-decay). Handed to CalculateFFT when frames resume so the missed
    // decay is applied instead of the old run's bars reappearing.
    int m_stalledTicks = 0;
    int m_pendingSilentFrames = 0;

    // Output data, double buffered
    ProcessedAudio m_processedAudio;

    float m_totalWin = 0.0;
    bool m_paused = false;

    std::thread m_thread;
    std::mutex m_mutex;
    IAPIClient* m_pClient;
};

} // Sonic Pi

